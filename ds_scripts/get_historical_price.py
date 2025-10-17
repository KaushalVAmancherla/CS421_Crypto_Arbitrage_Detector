
#!/usr/bin/env python3
"""
get_historical_price.py
----------------------
Fetches per-minute OHLC data for each (symbol, exchange) tuple listed in datasets/symbol_to_exchange.json for a given UTC day.

Handles API rate limiting for the Twelve Data API (8 requests/minute on free tier) by sequentially spacing requests.
Writes one NDJSON file per (symbol, exchange) for the specified day, suitable for downstream snapshot assembly and simulation.

Intended for public release as part of the Crypto Arbitrage Detector artifact.

Copyright (c) 2025 Kaushala Amancherla — MIT License
"""

import argparse
import json
import os
from pathlib import Path
from time import monotonic, sleep
import requests
import shutil
from dotenv import load_dotenv


load_dotenv()


API_KEY = os.getenv("TWELVEDATA_API_KEY")
SPACING = 60.0 / 8  # 8 requests per minute

parent_dir = Path(__file__).resolve().parent
datasets_dir = parent_dir.parent / "datasets"
TD_URL = "https://api.twelvedata.com/time_series"


def remove_dir_if_few_items(dir_path):
    """
    If a symbol directory has fewer than 2 NDJSON files after download/post-processing,
    delete it (the symbol does not have valid data on >=2 exchanges).
    """
    p = Path(dir_path)
    children = list(p.iterdir())
    if len(children) < 2:
        print(f"Removing directory {p} with only 1 item")
        shutil.rmtree(p, ignore_errors=True)

class RateLimiter:
    def __init__(self, spacing_sec: float):
        self.spacing = spacing_sec  # e.g., 60/8 = 7.5
        self._last_sent = None  # type: float | None

    def wait_before_send(self) -> None:
        """Wait if needed to respect the API rate limit before sending the next request."""
        if self._last_sent is None:
            return
        elapsed = monotonic() - self._last_sent
        wait = self.spacing - elapsed
        if wait > 0:
            sleep(wait)

    def mark_sent(self) -> None:
        """Mark the time a request is sent (call immediately before requests.get)."""
        self._last_sent = monotonic()

def normalize_sym(sym: str) -> str:
    """Normalize symbol string for use in filenames."""
    return sym.replace("/", "_").replace("-", "_")

def normalize_exch(exch: str) -> str:
    """Normalize exchange string for use in filenames."""
    return exch.replace(" ","").replace(".","")

def write_symbol_data(symbol, exchanges, day, limiter, session):
    """
    Download and write per-minute OHLC data for a given symbol across all exchanges for the specified day.
    Atomically writes NDJSON files, skipping any that already exist.
    """
    start_date = f"{day} 00:00:00"
    end_date = f"{day} 23:59:59"

    sym = normalize_sym(symbol)
    out_dir = datasets_dir / "crypto_timeseries_data" / day / sym
    os.makedirs(out_dir, exist_ok=True)

    for exch in exchanges:
        exch_norm = normalize_exch(exch)
        final_fp = out_dir / f"{sym}_{exch_norm}.ndjson"
        tmp_fp = out_dir / f"{sym}_{exch_norm}.ndjson.tmp"

        # Skip existing files (atomic write guarantees validity)
        if final_fp.exists():
            print(f"Skipping existing file: {final_fp}")
            continue

        API_URL = (
            f"{TD_URL}?symbol={symbol}"
            f"&exchange={exch}"
            f"&interval=1min"
            f"&start_date={start_date}"
            f"&end_date={end_date}"
            f"&timezone=UTC"
            f"&outputsize=1440"  # Full day of 1-min bars
            f"&apikey={API_KEY}"
        )

        # Throttle before sending the request if needed
        limiter.wait_before_send()
        limiter.mark_sent()

        r = session.get(API_URL)
        r.raise_for_status()
        payload = r.json()
        values = payload.get("values") or []

        if not values:
            print(f"Warning: No data for {symbol} on {exch} for {day} returned by API")
            continue

        # Expect exactly 1440 rows for a full day of 1-min bars
        if len(values) != 1440:
            print(f"Warning: Incomplete data for {symbol} on {exch} for {day}: only {len(values)} rows")
            continue

        # Ensure ascending order (TwelveData returns newest-first)
        if values[0].get("datetime") > values[-1].get("datetime"):
            values = list(reversed(values))

        # Atomically write the NDJSON data
        with tmp_fp.open("w", encoding="utf-8") as f:
            for v in values:
                f.write(json.dumps({
                    "datetime": v.get("datetime"),
                    "symbol": symbol,   # keep original symbol text; normalize later if needed
                    "exchange": exch,   # keep original exchange text; normalize later if needed
                    "open": str(v.get("open")),
                    "high": str(v.get("high")),
                    "low": str(v.get("low")),
                    "close": str(v.get("close")),
                }) + "\n")

        tmp_fp.replace(final_fp)

    remove_dir_if_few_items(out_dir)

def write_data(symbol_to_exchanges_dict, day, limiter):
    """Write OHLC data for all (symbol, exchanges) for the given day."""
    with requests.Session() as session:
        for symbol, exchanges in symbol_to_exchanges_dict.items():
            write_symbol_data(symbol, exchanges, day, limiter, session)

def load_data(fp):
    """Load JSON data from a file path."""
    with open(fp, 'r') as f:
        return json.load(f)
    
def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--day",
        required=True,
        help="UTC day: YYYY-MM-DD (e.g., 2025-04-22)"
    )
    args = parser.parse_args()

    limiter = RateLimiter(spacing_sec=60.0 / 8)
    symbol_to_exchange_fp = datasets_dir / "symbol_to_exchange.json"
    symbol_to_exchange_dict = load_data(symbol_to_exchange_fp)
    write_data(symbol_to_exchange_dict, args.day, limiter)

if __name__ == "__main__":
    main()