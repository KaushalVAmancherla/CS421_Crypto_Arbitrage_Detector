#!/usr/bin/env python3
"""
build_dataset.py
----------------
Orchestrates the end-to-end dataset build pipeline for the Crypto Arbitrage Detector project.

This script sequentially runs the three core dataset build steps:
    1. symbol_to_exchange.py    - Build symbol-to-exchanges mapping
    2. get_historical_price.py  - Download historical minute bars for a given day
    3. create_snapshots.py      - Assemble per-exchange NDJSON.zst snapshots

Validates input arguments and required environment variables for reproducible, auditable dataset generation.
Intended for public release as part of the project artifact.

Copyright (c) 2025 Kaushala Amancherla — MIT License
"""


from __future__ import annotations

import argparse
import os
import subprocess
import sys
from pathlib import Path
from datetime import datetime, timedelta, date
from zoneinfo import ZoneInfo

from dotenv import load_dotenv, find_dotenv


# Repo root = parent of this script’s folder
root = Path(__file__).resolve().parent

# Timezone for local day calculation
CHI = ZoneInfo("America/Chicago")

# Load environment variables from .env if present
load_dotenv(dotenv_path=root / ".env") or load_dotenv(find_dotenv())

def parse_day(s: str) -> date:
    """Parse a date string in YYYY-MM-DD format."""
    try:
        return datetime.strptime(s, "%Y-%m-%d").date()
    except ValueError:
        raise argparse.ArgumentTypeError("Use YYYY-MM-DD (e.g., 2025-04-22)")

def validate_day(target: date) -> None:
    """Ensure the target day is within the allowed 30-day window (not today/future, not too old)."""
    today_local = datetime.now(CHI).date()
    yesterday = today_local - timedelta(days=1)
    floor = yesterday - timedelta(days=30)

    if target > yesterday:
        raise SystemExit(f"--day {target} must be ≤ {yesterday} (cannot be today or future).")
    if target < floor:
        raise SystemExit(f"--day {target} too old; must be ≥ {floor} (30-day window).")

def validate_num_exchanges(n: int) -> None:
    """Require at least two exchanges for a symbol to be included."""
    if n < 2:
        raise SystemExit(f"--min-exchanges {n} must be at least 2.")

def validate_compression_level(level):
    """Validate zstd compression level is an integer between 1 and 22 inclusive."""
    try:
        n = int(level)
        if n < 1 or n > 22:
            raise ValueError()
    except ValueError:
        raise SystemExit(f"--compression-level {level} must be an integer between 1 and 22 inclusive.")

def require_env(var: str) -> str:
    """Require that a given environment variable is set, else exit."""
    val = os.getenv(var)
    if not val:
        raise SystemExit(f"Environment variable {var} not set (put it in .env or export it).")
    return val

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--day",
        type=parse_day,
        default=None,
        help="UTC trading day (YYYY-MM-DD). Defaults to yesterday (America/Chicago)."
    )
    parser.add_argument(
        "--min-exchanges",
        type=int,
        default=2,
        help="Keep symbols listed on at least this many exchanges (used primarily for testing)."
    )
    parser.add_argument(
        "--compression-level",
        default="3",
        help="Compression level for zstd (1-22); higher = more compression but slower. Default: 3"
    )
    args = parser.parse_args()

    # Compute default day = yesterday (local), then validate 30-day window
    if args.day is None:
        args.day = (datetime.now(CHI).date() - timedelta(days=1))
    validate_day(args.day)
    day_str = args.day.strftime("%Y-%m-%d")

    # Ensure API key present before doing work
    require_env("TWELVEDATA_API_KEY")

    # Validate min-exchanges is >= 2
    validate_num_exchanges(args.min_exchanges)

    # Validate compression-level is between 1 and 22 inclusive
    validate_compression_level(args.compression_level)

    # Script paths
    p_sym = root / "ds_scripts" / "symbol_to_exchange.py"
    p_hist = root / "ds_scripts" / "get_historical_price.py"
    p_proc = root / "ds_scripts" / "create_snapshots.py"

    # Sanity check: all three scripts must exist
    for p in [p_sym, p_hist, p_proc]:
        if not p.exists():
            raise SystemExit(f"Missing script: {p}")

    # 1) Build mapping (idempotent; overwrites the mapping output file)
    print(f"[1/3] Creating (symbol -> [exchanges]) dictionary …")
    subprocess.run(
        [sys.executable, str(p_sym), "--min-exchanges", str(args.min_exchanges)],
        cwd=root, check=True
    )

    # 2) Download historical minute bars for the chosen day
    print(f"[2/3] Downloading historical 1-min bars for {day_str}…")
    subprocess.run(
        [sys.executable, str(p_hist), "--day", day_str],
        cwd=root, check=True
    )

    # 3) Assemble per-exchange NDJSON.zst snapshots
    print(f"[3/3] Building snapshots (NDJSON.zst) for {day_str}…")
    subprocess.run(
        [sys.executable, str(p_proc), "--day", day_str, "--compression-level", str(args.compression_level)],
        cwd=root, check=True
    )

if __name__ == "__main__":
    main()
