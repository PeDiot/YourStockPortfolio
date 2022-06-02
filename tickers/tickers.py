from stocksymbol import StockSymbol

from rich import print
import pandas as pd

from typing import Dict, List

BACKUP = "C:/Users/piediot/OneDrive - Publicis Groupe/03 - PERSO/Trading/YourStockPortfolio/tickers/backup/"
API_KEY = "dee928bc-556a-4753-a8f6-1cf01a3a45dc"

ss = StockSymbol(API_KEY)

market_list = ss.market_list
all_index_list = ss.index_list

def get_market_index_list(market: str) -> List: 
    return [
        index 
        for index in all_index_list
        if index["market"] == market
    ]

def get_symbols_by_index(index_list: List) -> Dict:
    return {
        index["indexName"]: pd.DataFrame(data=ss.get_symbol_list(index=index["indexId"]))
        for index in index_list
    }

def save_symbols_by_index(file_name: str, index_dat: Dict):
    path = BACKUP + file_name

    for ix, index in enumerate(index_dat.keys()):
        if ix > 0: mode = "a"
        else: mode = "w"
        with pd.ExcelWriter(path, engine="openpyxl", mode=mode) as writer:
            dat = index_dat[index] 
            dat.to_excel(writer, sheet_name=index, index=False)

def run(market: str): 
    mkt_index_list = get_market_index_list(market)
    mkt_symbols = get_symbols_by_index(mkt_index_list)
    print(mkt_symbols)
    file_name = "tickers_" + market + ".xlsx"
    save_symbols_by_index(file_name, mkt_symbols)

for mkt in ["america", "unitedkingdom", "germany"]: 
    run(market=mkt)