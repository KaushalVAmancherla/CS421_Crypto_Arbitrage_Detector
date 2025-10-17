
#!/usr/bin/env python3
"""
symbol_to_exchange.py
---------------------
Builds a mapping from cryptocurrency symbols to the list of exchanges where each symbol is listed.

Filters for coins listed on at least a minimum number of exchanges (default: 2),
to identify viable arbitrage candidates for the project.

Intended for public release as part of the Crypto Arbitrage Detector artifact.

Copyright (c) 2025 Kaushala Amancherla — MIT License
"""

import argparse
import json
from pathlib import Path
import requests

API_URL = "https://api.twelvedata.com/cryptocurrencies"

def fetch_data():
    """Fetch cryptocurrency metadata from the Twelve Data API."""
    return requests.get(API_URL).json()["data"]

def compute_exchanges(rows, min_exchanges):
    """Return a dict mapping symbol -> list of exchanges, filtered by min_exchanges."""
    groups = {}
    for row in rows:
        if len(row['available_exchanges']) >= min_exchanges:
            groups[row['symbol']] = row['available_exchanges']
    return groups


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--min-exchanges",
        type=int,
        default=2,
        help="Keep only symbols listed on at least this many exchanges (default: 2)."
    )
    args = parser.parse_args()

    rows = fetch_data()
    groups = compute_exchanges(rows, int(args.min_exchanges))

    script_dir = Path(__file__).resolve().parent
    out_dir = script_dir.parent / "datasets"
    out_dir.mkdir(parents=True, exist_ok=True)
    out_path = out_dir / "symbol_to_exchange.json"

    with open(out_path, "w", encoding="utf-8") as f:
        json.dump(groups, f, indent=2, sort_keys=True)

if __name__ == "__main__":
    main()