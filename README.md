# CS421 Crypto Cross‑Exchange Arbitrage Simulator

This repository contains the code and docs for my CS 421 (Programming Languages & Compilers) honors project at UIUC. It has two major pieces:

- **Dataset Builder (Python)** — fetches historical per‑minute OHLC data for a single simulation day and materializes exchange‑grouped snapshots.
- **Arbitrage Simulator (Haskell/Stack)** — consumes those snapshots and, using concurrency/parallelism + STM, scans all exchanges minute‑by‑minute to surface cross‑exchange arbitrage opportunities.

> Public‑artifact friendly: large generated data (under `datasets/` and `outputs/`) and secrets are **.gitignored**.


## Repo layout

```
ds_scripts/                # Python dataset pipeline scripts
  create_dataset.py        # (moved here) build the simulation dataset
haskell_env/cross-exch-arbitrage-simulator/
                           # Haskell Stack project for the simulator
metadata/                  # small docs about symbols/exchanges (no data)
outputs/                   # (ignored) example reports produced by simulator
```

## Prerequisites

- **Python 3.10+**
  - `pip install -r ds_scripts/requirements.txt` (or install the few libs shown below)
- **TwelveData API key** (free tier OK; 8 req/min, 800/day)
- **Stack** (Haskell build tool): https://docs.haskellstack.org/

### Environment variables
Create a `.env` at repo root (not committed):
```
TWELVEDATA_API_KEY=<your key>
```
The dataset builder reads this via `python-dotenv`.

---

# Quick start

## 1) Create the dataset (for a single simulation day)

The simulator operates on one UTC day (00:00–23:59, per‑minute bars). From repo root:

```bash
# install deps (minimal set)
python -m pip install python-dotenv requests zstandard tqdm

# build the dataset for a day (YYYY-MM-DD)
python ds_scripts/create_dataset.py --day 2025-10-13
```

**What this does**
- fetches per‑minute OHLCV for a curated symbol list, across supported exchanges (subject to API availability);
- writes **symbol‑grouped** files: `datasets/crypto_symbol_data/<DAY>/<SYMBOL>_<EXCH>.ndjson`
- writes **exchange‑grouped** snapshots (compressed): `datasets/crypto_snapshot_data/<DAY>/<EXCH>.ndjson.zst`

**Notes on rate limits**
- Free tier is **8 calls/min** and **800/day**. The script batches requests and sleeps as needed; concurrency will still be bottlenecked by the ceiling, but overlapping I/O helps hide latency.

## 2) Build & run the Haskell simulator

```bash
cd haskell_env/cross-exch-arbitrage-simulator
stack build
```

Run the simulator for a specific day (reads exchange snapshots produced in step 1):

```bash
# use all cores; pass -N to enable multicore RTS
stack run cross-exch-arbitrage-simulator -- --day 2025-10-13 +RTS -N -s
```

- `--day` selects the dataset day.
- `+RTS -N` lets the runtime use all capabilities (cores). You can also pass `+RTS -N4` to pin to 4 cores.
- `-s` prints RTS stats at exit (useful for benchmarking).

### Example output (truncated)
```
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

---

## Repro tips

- If `datasets/crypto_snapshot_data/<DAY>/` is missing, the simulator will exit early—run **Step 1**.
- You can keep the folder structure in‑repo using `.gitkeep` files while keeping large data out of Git (already configured).

## Development

- Python style: small, single‑file scripts under `ds_scripts/`. Feel free to move to a package later.
- Haskell style: emphasizes **STM** for minute‑level coordination and **`async`/parallel** for per‑exchange work. See the technical doc for details.

## License
MIT — see `LICENSE`.
