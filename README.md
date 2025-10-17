# CS421 Crypto Cross-Exchange Arbitrage Simulator

[![Haskell](https://img.shields.io/badge/Haskell-Stack-5e5086?logo=haskell&logoColor=white)](#)
[![Python](https://img.shields.io/badge/Python-3.10%2B-3776ab?logo=python&logoColor=white)](#)
[![License: MIT](https://img.shields.io/badge/License-MIT-success.svg)](LICENSE)

This repository contains the code and docs for my CS 421 (Programming Languages & Compilers) honors project at UIUC. The repository has two main components:

- **Dataset Builder (Python)** — fetches historical per-minute OHLC data for a single simulation day and materializes exchange-grouped snapshots.
- **Arbitrage Simulator (Haskell/Stack)** — consumes those snapshots and, using concurrency/parallelism + STM, scans all exchanges minute-by-minute to surface cross-exchange arbitrage opportunities.

> **Public-artifact friendly** — large generated data (`datasets/`, `outputs/`) and secrets are **.gitignored**.

---

## Table of Contents

- [Repo layout](#repo-layout)  
- [Prerequisites](#prerequisites)  
- [Environment variables](#environment-variables)  
- [Quick start](#quick-start)  
  - [1) Create the dataset](#1-create-the-dataset-for-a-single-simulation-day)  
  - [2) Build & run the Haskell simulator](#2-build--run-the-haskell-simulator)  
- [Unit tests](#unit-tests)  
- [Technical overview](#technical-overview)  
- [Troubleshooting](#troubleshooting)  
- [License](#license)

---

## Repo layout

```
ds_scripts/                # Python dataset pipeline scripts
  build_dataset.py         # builds the simulation dataset   (moved here)
  requirements.txt         # Python runtime deps for dataset builder
haskell_env/cross-exch-arbitrage-simulator/
                           # Haskell Stack project for the simulator
datasets/                  # (ignored) output snapshots when creating dataset 
outputs/                   # (ignored) logbook of per-min arbitrage opportunities
```

## Prerequisites

- **Python 3.10+**  
  Install from `ds_scripts/requirements.txt`, (use of virtual environment is recommended)
- **TwelveData API key** (free tier OK; 8 req/min, 800/day)
- **Stack** (Haskell build tool): <a href="https://docs.haskellstack.org/" target="_blank" rel="noopener noreferrer">docs.haskellstack.org</a>


## Environment variables

Create a `.env` at the repo root (not committed) to include your key:

```dotenv
TWELVEDATA_API_KEY=<your_key_here>
```

The dataset builder reads this via `python-dotenv`.

---

# Quick start

## 1) Create the dataset (for a single simulation day)

The simulator operates on one **UTC** day (00:00–23:59, per-minute bars). From the repo root:

```bash
# (Recommended) create and activate a virtual environment
python -m venv .venv
# macOS/Linux:
source .venv/bin/activate
# Windows (PowerShell):
# .\.venv\Scripts\Activate.ps1

# Upgrade pip and install Python dependencies
python -m pip install --upgrade pip
python -m pip install -r ds_scripts/requirements.txt

# Build the dataset for a day (YYYY-MM-DD)

# NOTE:
# ds_scripts/build_dataset.py accepts --min-exchanges and --compression-level
# as optional arguments. See the Dataset Builder section of
# TECHNICAL_OVERVIEW.md for an explanation of these parameters.
python ds_scripts/build_dataset.py --day 2025-10-13

```

**What this does**
- Fetches per-minute OHLCV for a curated symbol list, across supported exchanges (subject to API availability).
- Writes **symbol-grouped** files:  
  `datasets/crypto_symbol_data/<DAY>/<SYMBOL>_<EXCH>.ndjson`
- Writes **exchange-grouped** snapshots (compressed):  
  `datasets/crypto_snapshot_data/<DAY>/<EXCH>.ndjson.zst`

**Rate-limit notes**
- Free tier is **8 calls/min** and **800/day**. The script is designed for this plan, so
data fetching may be slow.

## 2) Build & run the Haskell simulator

Build the project

```bash
cd haskell_env/cross-exch-arbitrage-simulator
stack build
```

Run the simulator for a specific day (reads the exchange snapshots produced in step 1):

```bash
stack run cross-exch-arbitrage-simulator -- --day 2025-10-13 +RTS -N -s
```

- `--day` selects the dataset day.  
- `+RTS -N` lets the runtime use all capabilities (cores). You could also pass `+RTS -N4` to pin to 4 cores, for example.
- `-s` prints RTS stats at exit (useful for benchmarking).

### Example output (truncated)

```text
"2025-10-13 00:00:00"
  found 11 opportunity(ies):
    ADA/TRY  buy $29.516 on Paribu    sell $29.681 on BTCTurk
    BTC/EUR  buy $99126.4 on Kraken   sell $99236.23 on CoinbasePro
    ETH/EUR  buy $3572.64 on Kraken   sell $3573.97 on Binance
    ...
"2025-10-13 00:01:00"
  found 10 opportunity(ies):
    ADA/USD  buy $0.697192 on Huobi   sell $0.70021802 on HitBTC
    XRP/USD  buy $2.5197 on Bitrue    sell $2.52001 on Huobi
    ...
```

Results are saved at `outputs/arbitrage.log`

---

## Unit tests

The Haskell simulator includes a test suite. From the simulator directory:

```bash
cd haskell_env/cross-exch-arbitrage-simulator
stack test
```

- This compiles the test targets and runs them under Stack’s test runner.

---

## Technical overview

For a deeper dive into data formats, concurrency/parallelism, and STM design decisions, see:  
**[TECHNICAL_OVERVIEW.md](TECHNICAL_OVERVIEW.md)**

---

## Troubleshooting

- **Simulator can’t find data**  
  Ensure `datasets/crypto_snapshot_data/<DAY>/` exists (run Step 1 again) and your `--day` matches the created day.

---

## License

MIT — see [`LICENSE`](LICENSE).
