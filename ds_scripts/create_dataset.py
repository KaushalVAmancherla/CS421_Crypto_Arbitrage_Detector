#!/usr/bin/env python3
"""

Run the full dataset build pipeline in sequence:

  1) symbol_to_exchange.py
  2) get_historical_price.py  (historical, for the chosen day)
  3) process_raw_json.py      (assemble per-exchange NDJSON.zst snapshots)

Date rules:
- The target --day must be <= yesterday (local America/Chicago), i.e., NOT today to ensure a full
24-hour day of data.
- The target --day must be >= (yesterday - 30 days) to ensure data is available from Twelve Data.

"""

from __future__ import annotations
from pathlib import Path
from datetime import datetime, timedelta, date
from zoneinfo import ZoneInfo
import argparse, os, subprocess, sys, json
from dotenv import load_dotenv, find_dotenv

# Repo root = parent of this script’s folder
root = Path(__file__).resolve().parent

CHI = ZoneInfo("America/Chicago")

load_dotenv(dotenv_path=root / ".env") or load_dotenv(find_dotenv())

def parse_day(s: str) -> date:
    try:
        return datetime.strptime(s, "%Y-%m-%d").date()
    except ValueError:
        raise argparse.ArgumentTypeError("Use YYYY-MM-DD (e.g., 2025-04-22)")

def validate_day(target: date) -> None:
    today_local = datetime.now(CHI).date()
    yesterday   = today_local - timedelta(days=1)
    floor       = yesterday - timedelta(days=30)

    if target > yesterday:
        raise SystemExit(f"--day {target} must be ≤ {yesterday} (cannot be today or future).")
    if target < floor:
        raise SystemExit(f"--day {target} too old; must be ≥ {floor} (30-day window).")

def validate_num_exchanges(n: int) -> None:
    if n < 2:
        raise SystemExit(f"--min-exchanges {n} must be at least 2.")

def validate_compression_level(level):
    try:
        n = int(level)
        if n < 1 or n > 22:
            raise ValueError()
    except ValueError:
        raise SystemExit(f"--compression-level {level} must be an integer between 1 and 22 inclusive.")

def require_env(var: str) -> str:
    val = os.getenv(var)
    if not val:
        raise SystemExit(f"Environment variable {var} not set (put it in .env or export it).")
    return val

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--day", type=parse_day, default=None,
                        help="UTC trading day (YYYY-MM-DD). Defaults to yesterday (America/Chicago).")
    parser.add_argument("--min-exchanges", type=int, default=2,
                        help="Keep symbols listed on at least this many exchange (USED PRIMARILY FOR TESTING).")
    parser.add_argument("--compression-level", default="3",
                        help="Compression level for zstd (1-22); higher = more compression but slower. Default: 3s")
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
    p_proc = root / "ds_scripts" / "process_raw_json.py"

    # Sanity checks: the three scripts must exist
    for p in [p_sym, p_hist, p_proc]:
        if not p.exists():
            raise SystemExit(f"Missing script: {p}")

    # 1) Build mapping (idempotent; overwrites file)
    print(f"[1/3] Creating (symbol -> [exchanges]) dictionary …")
    subprocess.run(
        [sys.executable, str(p_sym), 
        "--min-exchanges", str(args.min_exchanges)],
        cwd=root, check=True
    )

    # 2) Download historical minute bars for the chosen day
    print(f"[2/3] Downloading historical 1-min bars for {day_str}…")
    subprocess.run(
        [sys.executable, str(p_hist),
         "--day", day_str],
        cwd=root, check=True
    )

    # 3) Assemble per-exchange NDJSON.zst snapshots
    print(f"[3/3] Building snapshots (NDJSON.zst) for {day_str}…")
    # Expectation: process_raw_json.py supports --day and writes to metadata/crypto_snapshot_data/<day>/
    subprocess.run(
        [sys.executable, str(p_proc),
         "--day", day_str,
         "--compression-level", str(args.compression_level)],
        cwd=root, check=True
    )

if __name__ == "__main__":
    main()
