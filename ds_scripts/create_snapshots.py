#!/usr/bin/env python3
"""
create_snapshots.py
-------------------
Assembles per-exchange snapshot files (NDJSON.zst) from historical per-symbol OHLC time series data.

Reads per-symbol NDJSON time series (minute OHLC bars) and reorganizes them into per-exchange, zstd-compressed NDJSON snapshot files. Each output file contains one JSON object per line, structured as:
    { "datetime": "YYYY-MM-DDTHH:MM:SS", "exchange": "Exch", "SYM": {"open":..., "high":..., "low":..., "close":...}, ... }

These snapshots are used by the simulator to emulate exchange data streams for a given day.

Copyright (c) 2025 Kaushala Amancherla — MIT License
"""


from __future__ import annotations

import argparse
import json
import os
from pathlib import Path
from collections import defaultdict

import zstandard as zstd

# Directory paths
parent_dir = Path(__file__).resolve().parent
datasets_dir = parent_dir.parent / "datasets"
in_root = datasets_dir / "crypto_timeseries_data"
out_root = datasets_dir / "crypto_snapshot_data"

def map_exchange_to_ndjson(day: str, compressor: zstd.ZstdCompressor) -> None:
    """
    Build per-exchange snapshot files for the given day.

    Scans the per-symbol NDJSON files under `in_root/day/`,
    groups them by exchange (using filename suffixes), and writes a
    compressed NDJSON snapshot file for each exchange.
    """
    in_dir = in_root / day
    out_dir = out_root / day
    os.makedirs(out_dir, exist_ok=True)

    # Map: exchange -> list of symbol file paths
    exchange_to_fps: dict[str, list[Path]] = defaultdict(list)

    if not in_dir.exists():
        raise SystemExit(f"Input directory not found: {in_dir}")

    for sym_path in in_dir.iterdir():
        if not sym_path.is_dir():
            continue
        symbol = sym_path.name
        # For each symbol, files are named like "{symbol}_{exchange}.ndjson"
        for src in sym_path.glob(f"{symbol}_*.ndjson"):
            exchange = src.name.split("_")[-1].split(".ndjson")[0]
            exchange_to_fps[exchange].append(src)

    for exchange, paths in exchange_to_fps.items():
        outfile = out_dir / f"{exchange}.ndjson.zst"

        # Map: timestamp -> { symbol: {open,high,low,close}, ... }
        timestamp_to_data: dict[str, dict] = defaultdict(dict)

        for src in paths:
            with open(src, "r", encoding="utf-8") as f:
                for line in f:
                    obj = json.loads(line)
                    sym = obj["symbol"]
                    ts = obj["datetime"]  # string timestamp
                    new_obj = {
                        "open": obj["open"],
                        "high": obj["high"],
                        "low": obj["low"],
                        "close": obj["close"],
                    }
                    timestamp_to_data[ts][sym] = new_obj

        # Write compressed per-exchange snapshot
        with zstd.open(outfile, mode="wt", encoding="utf-8", cctx=compressor) as w:
            for ts in sorted(timestamp_to_data):
                payload = {"datetime": ts, "exchange": exchange, **timestamp_to_data[ts]}
                w.write(json.dumps(payload) + "\n")

    print(f"✅ Done. Snapshots at: {out_dir}")

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--day",
        required=True,
        help="UTC day: YYYY-MM-DD (e.g., 2025-04-22)"
    )
    parser.add_argument(
        "--compression-level",
        default="3",
        help="Compression level for zstd (1-22); higher = more compression but slower. Default: 3"
    )
    args = parser.parse_args()

    compressor = zstd.ZstdCompressor(level=int(args.compression_level))
    map_exchange_to_ndjson(args.day, compressor)

if __name__ == "__main__":
    main()