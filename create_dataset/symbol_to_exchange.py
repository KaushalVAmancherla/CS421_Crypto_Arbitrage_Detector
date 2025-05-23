'''
For arbitraging, we consider coins listed on at least 2 exchanges
This script uses the statiic data from the API to get a list of supported exchanges for each currency and filters through them
'''

import os, json, requests, collections
from pprint import pprint

API_URL = "https://api.twelvedata.com/cryptocurrencies"
parent_dir = "/Users/kaushalamancherla/cs421-honors-project/"

def fetch_data():
    return requests.get(API_URL).json()["data"]

def compute_exchanges(rows):
    groups = {}
    
    num_crypto, num_req = 0, 0

    for row in rows:        
        if len(row['available_exchanges']) > 2: 
            num_crypto += 1
            num_req += len(row['available_exchanges'])
            groups[row['symbol']] = row['available_exchanges']

    return groups, num_crypto, num_req

def main() -> None:
    rows   = fetch_data()
    groups, num_crypto, num_req = compute_exchanges(rows)

    serialisable = {symbol: exchanges for symbol, exchanges in groups.items()}

    OUT = os.path.join(f"{parent_dir}/metadata/","symbol_to_exchange.json")

    with open(OUT, "w", encoding="utf-8") as f:
        json.dump(serialisable, f, indent=2, sort_keys=True)
        
if __name__ == "__main__":
    main()