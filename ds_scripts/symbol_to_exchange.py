#!/usr/bin/env python3
"""
Build a symbol → exchanges map for arbitrage candidates.

For arbitraging, we consider coins listed on >= 2 exchanges at minimumum.
"""

import json, requests, argparse
from pathlib import Path

API_URL = "https://api.twelvedata.com/cryptocurrencies"
parent_dir = "/Users/kaushalamancherla/cs421-honors-project/"

def fetch_data():
    return requests.get(API_URL).json()["data"]

def compute_exchanges(rows, min_exchanges):
    groups = {}
    
    for row in rows:        
        if len(row['available_exchanges']) >= min_exchanges: 
            groups[row['symbol']] = row['available_exchanges']

    return groups

def main():
    p = argparse.ArgumentParser()

    p.add_argument("--min-exchanges", required=True, help="UTC day: YYYY-MM-DD (e.g., 2025-04-22)")

    args = p.parse_args()

    rows   = fetch_data()
    groups = compute_exchanges(rows,int(args.min_exchanges))

    script_dir = Path(__file__).resolve().parent
    out_dir = script_dir.parent / "datasets"
    out_dir.mkdir(parents=True, exist_ok=True)

    out_path = out_dir / "symbol_to_exchange.json"

    with open(out_path, "w", encoding="utf-8") as f:
        json.dump(groups, f, indent=2, sort_keys=True)

if __name__ == "__main__":
    main()