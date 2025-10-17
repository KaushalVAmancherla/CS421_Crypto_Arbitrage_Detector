
# CS421 Crypto Cross-Exchange Arbitrage Simulator — Technical Overview

>This document provides a detailed technical overview of the two main pipelines in the project: **Dataset Builder** and **Arbitrage Detector**. All diagrams and architectural features are preserved for clarity.

---

## Table of Contents
1. [Dataset Builder Pipeline](#dataset-builder-pipeline)
2. [Arbitrage Detector Pipeline](#arbitrage-detector-pipeline)
3. [Key Architectural Features & Design Choices](#key-architectural-features--design-choices)

---

## 1. Dataset Builder Pipeline

The Dataset Builder pipeline generates per-minute OHLC (Open, High, Low, Close) data for a curated set of cryptocurrencies, grouped by exchange, for a single UTC day. Data is sourced from the [Twelve Data API](https://twelvedata.com/cryptocurrencies).

### Pipeline Steps

**Step 1: Build Symbol-to-Exchange Mapping**

*Script:* `ds_scripts/symbol_to_exchange.py`

Creates `datasets/symbol_to_exchange.json`, a dictionary mapping each symbol (e.g., `XLM/USD`) to the exchanges it is traded on:

```json
{
  "XLM/USD": ["Binance", "Coinbase Pro", "Huobi", "OKEx"],
  ...
}
```

Only symbols listed on at least `min_exchanges` exchanges are included (default: 2). This ensures arbitrage is possible.

**Step 2: Download Per-Symbol, Per-Exchange OHLC Data**

*Script:* `ds_scripts/get_historical_price.py`

For each (symbol, exchange) tuple, downloads 1-minute OHLC data for the specified day. Output files:

```
datasets/crypto_timeseries_data/<DAY>/<SYM>/<SYM>_<EXCH>.ndjson
```

Each file contains 1440 lines (one per minute), e.g.:

```json
{"datetime": "2025-10-13 00:00:00", "symbol": "ADA/TRY", "exchange": "BTCTurk", "open": "29.68100", "high": "29.68100", "low": "29.68100", "close": "29.68100"}
```

Directories with fewer than 2 files are deleted to enforce the minimum exchange rule.

**Step 3: Assemble Per-Exchange Compressed Snapshots**

*Script:* `ds_scripts/create_snapshots.py`

Aggregates all per-symbol NDJSON files for each exchange into a single compressed snapshot:

```
datasets/crypto_snapshot_data/<DAY>/<EXCH>.ndjson.zst
```

Each line is a JSON object for a minute, containing OHLC data for all symbols traded on that exchange:

```json
{
  "datetime": "2025-10-13 00:14:00",
  "exchange": "Binance",
  "ADA/USD": {"open": "0.6988", "high": "0.7", "low": "0.6987", "close": "0.6997"},
  "XLM/USD": {...},
  ...
}
```

**Orchestration**

*Script:* `ds_scripts/build_dataset.py`

Runs all three steps above, validating arguments and environment variables. Key flags:

- `--day` (required): UTC day (YYYY-MM-DD)
- `--min-exchanges` (default: 2): Minimum exchanges for a symbol
- `--compression-level` (default: 3): zstd compression level (1-22)

---

## 2. Arbitrage Detector Pipeline

The Arbitrage Detector pipeline simulates real-time receipt of per-minute OHLC data from multiple exchanges and detects cross-exchange arbitrage opportunities.

### Architecture Overview

**Producer Threads**

- Each producer reads a compressed NDJSON snapshot file for one exchange (`datasets/crypto_snapshot_data/<DAY>/<EXCH>.ndjson.zst`).
- Streams and decompresses lines, parses each into a `Snapshot` object, and inserts it atomically into a shared `BatchBuffer`.
- A fixed delay (default: 0.01s) is used between lines to simulate real-time data arrival.

**Snapshot Object**

*File:* `src/Model/Snapshot.hs`

```haskell
data Snapshot = Snapshot
  {
    datetime :: Text,
    exchange :: Text,
    ohlc :: Map Text Tick
  } deriving Show

data Tick = Tick
  { open :: Double, high :: Double, low :: Double, close :: Double }
```

**BatchBuffer (Central Synchronization)**

*File:* `src/Pipeline/BatchBuffer.hs`

Thread-safe buffer using STM (Software Transactional Memory) for atomic, lock-free access. Key fields:

- `totalExs`: Number of exchanges
- `accumVar`: STM TVar holding incomplete batches (timestamp → exchange → snapshot)
- `heapVar`: STM TVar min-heap of completed batches (timestamp, [(exchange, snapshot)])
- `producersLeft`: STM TVar tracking active producers

When all exchanges for a timestamp are received, the batch is moved to the heap for processing.

**Producer <-> BatchBuffer Workflow**

Producers stream data line-by-line, adding snapshots to the buffer. When a batch for a timestamp is complete (all exchanges present), it is pushed to the heap. The min-heap ensures batches are processed in chronological order.

![Producer<->Buffer Workflow](/images/producer-buffer-workflow.jpg)

**Why a Min-Heap?**

The min-heap guarantees that batches are processed strictly in timestamp order, preventing out-of-order arbitrage detection and ensuring reproducibility.

**Consumer Thread**

*File:* `src/Pipeline/Consumer.hs`

- Atomically pops batches from the heap.
- Processes each batch using parallel map-reduce (see below).
- Validates and logs arbitrage opportunities to `outputs/arbitrage.log`.
- Monitors timing to report if processing is on schedule, early, or delayed.

Sample console output:

```text
BATCH: "2025-10-13 00:10:00" ARRIVAL TIME: 2025-10-16 22:53:23.426487 UTC
[END EARLY] target=2025-10-16 22:53:23.426587 UTC actual=2025-10-16 22:53:23.426578 UTC rem=0.000009s work_time=0.000091s
BATCH: "2025-10-13 00:11:00" ARRIVAL TIME: 2025-10-16 22:53:23.427658 UTC
[END WARN ] target=2025-10-16 22:53:23.427758 UTC actual=2025-10-16 22:53:23.428174 UTC delay=0.000416s work_time=0.000516s
```

- target → Target time batch should finish
- actual → Actual finish time
- rem/delay → Early or late by this amount
- work_time → Processing time for the batch

Please note, we are simulating historical data against real-time, hence the batch time and arrival time will obviously differ. 

![Consumer Workflow](/images/consumer-workflow.jpg)

**Parallel Batch Processing**

The parallel processing of each batch (via Haskell Parallel Strategies) follows a map-reduce paradigm:

1. **Chunking:** Input list of (exchange, snapshot) pairs is chunked by number of CPU cores.
2. **Map:** Each chunk finds local best buy/sell prices for each symbol.
3. **Reduce:** Merge chunk results to find global best buy/sell prices.
4. **Validation:** Only opportunities where sell price > buy price are logged.

![Batch processing Workflow](/images/batch_parallel_processing.jpg)

**Opportunity Object**

*File:* `src/Model/Opportunity.hs`

```haskell
data Opportunity = Opportunity
  { arSymbol :: Text
  , arBuyEx  :: Text
  , arBuyPx  :: Double
  , arSellEx :: Text
  , arSellPx :: Double
  }
```

**End-to-End Arbitrage Detection Flow**

![Batch processing Workflow](/images/e2e_arbitrage_detection_flow.jpg)

---

## 3. Key Architectural Features & Design Choices

### STM & Atomicity

All shared state (batch buffer, heap, producer count) is managed using STM (Software Transactional Memory) for atomic, lock-free concurrency. This ensures thread safety and reproducibility, even under parallel execution.

### Data Parallelism

- **Outer parallelism:** Multiple producer threads stream data concurrently.
- **Inner parallelism:** Batch processing uses Haskell's parallel strategies to analyze data across CPU cores.

### Map-Reduce Paradigm

Batch processing is designed as a map-reduce workflow for scalability. Each chunk is processed in parallel, and results are merged to find global arbitrage opportunities.

### Design Rationale

- **Min-heap for batch ordering:** Guarantees strict chronological processing, essential for time-sensitive arbitrage detection.
- **STM for concurrency:** Chosen for its composability and safety in multi-threaded environments.
- **Parallel batch processing:** Demonstrates scalable design for large datasets, even if overhead outweighs speedup for small batch sizes.

### Summary of Key Pipeline Architectural Features

- **Data Parallelism:** Batch processing map-reduce for scalable arbitrage detection.
- **STM:** Atomic transactions and shared memory variables for thread-safe, concurrent access.
- **Concurrency:** Producer-consumer architecture with lock-free synchronization.
- **Outer Parallelism:** Multiple producer threads stream and synchronize data in real time.
- **Inner Parallelism:** Parallel batch processing leverages all available CPU cores.

---
