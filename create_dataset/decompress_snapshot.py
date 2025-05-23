import zstandard as zstd, json, pathlib

snap = pathlib.Path("/Users/kaushalamancherla/cs421-honors-project//metadata/crypto_snapshot_data/2025-04-22/Binance.ndjson.zst")
with zstd.open(snap, "rt", encoding="utf-8") as f:
    for i, line in zip(range(15), f):
        print(json.loads(line))