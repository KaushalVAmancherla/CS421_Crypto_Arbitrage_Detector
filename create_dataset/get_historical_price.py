'''
Given the shortlist of coins to consider, as we create our dataset for 4-22 (this is the date we simulate on)
we get the historical per-minute ohlc data for each coin in our list
'''

from dotenv import load_dotenv

import os, json, requests
import time

load_dotenv()

API_URL = "https://api.twelvedata.com/cryptocurrencies"
API_KEY = os.getenv("TWELVEDATA_API_KEY")

SLEEP = 60.0 / 8

parent_dir = "/Users/kaushalamancherla/cs421-honors-project/"

def write_symbol_data(symbol,exchanges):
    new_sym = symbol.replace("/","_")
    out_dir = os.path.join(f"{parent_dir}/metadata/crypto_timeseries_data", new_sym)

    if not os.path.isdir(out_dir): os.makedirs(out_dir, exist_ok=True)
    
    for exchange in exchanges:
        API_URL = f"https://api.twelvedata.com/time_series?symbol={symbol}&exchange={exchange}&interval=1min&outputsize=5000&apikey={API_KEY}"

        data = requests.get(API_URL).json()

        out_path = os.path.join(out_dir, f"{new_sym}_{exchange}.json")

        with open(out_path, "w", encoding="utf-8") as f:
            json.dump(data, f, indent=2)
        
        time.sleep(SLEEP)

def write_data(symbol_to_exchanges_dict):
    for symbol,exchanges in symbol_to_exchanges_dict.items():
        write_symbol_data(symbol,exchanges)

def load_data(fp):
    with open(fp,'r') as f:
        return json.load(f)
    
def main():
    data_fp = os.path.join(f"{parent_dir}/metadata/","symbol_to_exchange.json")

    symbol_to_exchange_dict = load_data(data_fp)
    write_data(symbol_to_exchange_dict)

if __name__ == "__main__":
    main()