'''
From the historical ohcl data we sourced from the API, we filter it to only include data for 00:00:00 to 23:59:00 on our chosen
date 4-22

This outputs two things:
1. Folder with ndjson of coin:[files] where each file is an ndjson file of the exchange the coin appears on with the ohlc data for 4-22
2. Snapshot data: Output level-3 zstd compressed ndjson files of an exchange with all the coins in that exchange and each coin's per minute ohlc data.
    The snapshots are used to simulate the data streams (i.e. what the stock data endpoint would return & what data stream we need to injest)
'''

import json,os
import zstandard as zstd

import shutil
from pathlib import Path

from datetime import datetime, date
from collections import defaultdict

day_str = "22"
month_str = "04"
year_str = "2025"

day_prefix = year_str + "-" + month_str + "-" + day_str

parent_dir = "/Users/kaushalamancherla/cs421-honors-project"
base_dir = os.path.join(f"{parent_dir}/metadata","crypto_timeseries_data")
base_out_dir = os.path.join(f"{parent_dir}/metadata","crypto_timeseries_data_4_22")
snapshot_out_dir = os.path.join(f"{parent_dir}/metadata","crypto_snapshot_data")

BASE = Path(base_dir)
SLICED = Path(base_out_dir)
SNAPSHOT = Path(snapshot_out_dir)

compressor = zstd.ZstdCompressor(level=3)

def remove_dir_if_few_items(dir_path, min_items = 2):
    p = Path(dir_path)

    children = list(p.iterdir())

    if len(children) < min_items:
        shutil.rmtree(p, ignore_errors=True)

def slice_json():
    for sym_dir in BASE.iterdir():
        if not sym_dir.is_dir(): continue
        
        symbol = sym_dir.name

        out_dir = os.path.join(base_out_dir,sym_dir.name)
        if not os.path.isdir(out_dir): os.makedirs(out_dir, exist_ok=True)

        for src in sym_dir.glob(f"{symbol}_*.json"):
            exchange = src.name.split("_")[-1].split(".json")[0].replace(" ","").replace(".","")
            outfile = os.path.join(out_dir,f"{symbol}_{exchange}.ndjson")

            with open(src,"r") as f:
                data = json.load(f)

            rows = []

            for row in data['values']:
                if not row["datetime"].startswith(day_prefix): continue

                row['symbol'] = symbol
                row['exchange'] = exchange

                rows.append(row)

            if len(rows) != 1440: continue #no data for 4-22 00:00:00 to 23:59:00 was found

            rows = rows[::-1]

            with open(outfile,'w',encoding='utf-8') as f:
                for row in rows:
                    f.write(json.dumps(row) + "\n")

        remove_dir_if_few_items(out_dir)

def map_exchange_to_ndjson(day_prefix):
    exchange_to_ndjson = defaultdict(list)

    for sym_dir in SLICED.iterdir():
        if not sym_dir.is_dir(): continue
        
        symbol = sym_dir.name

        for src in sym_dir.glob(f"{symbol}_*.ndjson"):
            exchange = src.name.split("_")[-1].split(".ndjson")[0]
            exchange_to_ndjson[exchange].append(src)

    dir = os.path.join(snapshot_out_dir,day_prefix)
    if not os.path.isdir(dir): os.makedirs(dir, exist_ok=True)

    for exchange, paths in exchange_to_ndjson.items():
        outfile = os.path.join(dir,f"{exchange}.ndjson.zst")

        timestamp_to_data = defaultdict(dict)
        
        for src in paths:
            with open(src,'r') as f:
                for line in f:
                    obj = json.loads(line)

                    symbol = obj['symbol']
                    datetime = obj['datetime']

                    new_obj = {
                        "open": obj["open"],
                        "high": obj["high"],
                        "low": obj["low"],
                        "close": obj["close"],
                    }

                    timestamp_to_data[datetime][obj['symbol']] = new_obj

        with zstd.open(outfile, mode="wt", encoding="utf-8") as f:
            for ts in sorted(timestamp_to_data):
                payload = { "datetime": ts, "exchange" : exchange, **timestamp_to_data[ts] }

                f.write(json.dumps(payload) + "\n")

def main():
    slice_json()
    map_exchange_to_ndjson(day_prefix)

if __name__ == "__main__":
    main()