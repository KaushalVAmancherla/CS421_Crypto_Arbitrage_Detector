import zstandard as zstd, json, pathlib

snap = pathlib.Path("/Users/kaushalamancherla/CS421_Crypto_Arbitrage_Detector/datasets/crypto_snapshot_data/2025-10-13/BTCTurk.ndjson.zst")
with zstd.open(snap, "rt", encoding="utf-8") as f:
    for i, line in zip(range(15), f):
        print(json.loads(line))

