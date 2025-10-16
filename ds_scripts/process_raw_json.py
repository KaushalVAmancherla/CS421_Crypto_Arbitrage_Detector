'''
From the historical ohcl data we sourced from the API, we filter it to only include data for 
00:00:00 to 23:59:00 on the chosen date

This outputs two things:
1. Folder with ndjson of coin:[files] where each file is an ndjson file of the exchange the coin appears on with the ohlc data for 4-22
2. Snapshot data: Output level-3 zstd compressed ndjson files of an exchange with all the coins in that exchange and each coin's per minute ohlc data.
    The snapshots are used to simulate the data streams (i.e. what the stock data endpoint would return & what data stream we need to injest)
'''

import json,os
import zstandard as zstd

import shutil, argparse
from pathlib import Path

from datetime import datetime, date
from collections import defaultdict

parent_dir = Path(__file__).resolve().parent
datasets_dir = parent_dir.parent / "datasets"
spanshot_out_dir = datasets_dir / "crypto_snapshot_data"

in_root = datasets_dir / "crypto_timeseries_data"
out_root = datasets_dir / "crypto_snapshot_data"

def map_exchange_to_ndjson(day,compressor):
    in_dir = in_root / day
    exchange_to_ndjson = defaultdict(list)

    out_dir = out_root / day
    os.makedirs(out_dir, exist_ok=True)

    exchange_to_fps: dict[str, list[Path]] = defaultdict(list)

    for sym_path in in_dir.iterdir():
        if not sym_path.is_dir(): continue
        
        symbol = sym_path.name

        '''
        For each ndjson file, the filename (not including .ndjson) is ALWAYS
        suffixed with the normalized exchange name, so we can use that to
        group by exchange.
        '''

        for src in sym_path.glob(f"{symbol}_*.ndjson"):
            exchange = src.name.split("_")[-1].split(".ndjson")[0]
            exchange_to_fps[exchange].append(src)

    for exchange, paths in exchange_to_fps.items():
        outfile = out_dir / f"{exchange}.ndjson.zst"

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

        with zstd.open(outfile, mode="wt", encoding="utf-8", cctx=compressor) as w:
            for ts in sorted(timestamp_to_data):
                payload = { "datetime": ts, "exchange" : exchange, **timestamp_to_data[ts] }

                w.write(json.dumps(payload) + "\n")

    print(f"✅ Done. Snapshots at: {out_dir}")

def main():
    p = argparse.ArgumentParser()

    p.add_argument("--day", required=True, help="UTC day: YYYY-MM-DD (e.g., 2025-04-22)")
    p.add_argument("--compression-level", default="3",
                        help="Compression level for zstd (1-22); higher = more compression but slower. Default: 3s")
    args = p.parse_args()

    compressor = zstd.ZstdCompressor(level=int(args.compression_level))

    map_exchange_to_ndjson(args.day,compressor)

if __name__ == "__main__":
    main()