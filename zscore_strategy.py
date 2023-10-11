# carregando bibliotecas
import requests
import pandas as pd
import numpy as np
import time
import math

from datetime import datetime
from scipy.stats import iqr

import warnings

warnings.filterwarnings('ignore')

def get_bitstamp_data(currency_pair: str, start: str) -> pd.DataFrame:

    # Convert start date to timestamp because bitstamp API uses UNIX timestamp
    start_date = datetime.strptime(start, "%d-%m-%Y")
    start_timestamp = int(start_date.timestamp())
    days = (datetime.now() - start_date).days
    num_requests = math.ceil(days * (86400 / (24*60*60)) / 1000)

    # Initialize an empty list to store all the data
    data_list = []
    current_start = start_timestamp

    for _ in range(num_requests):
        # Make the API request to get the data
        url = f"https://www.bitstamp.net/api/v2/ohlc/{currency_pair}/?start={current_start}&step={86400}&limit={1000}"
        response = requests.get(url)

        if response.status_code == 200:
            data = response.json()
            if 'data' in data and 'ohlc' in data['data']:
                ohlc_data = data['data']['ohlc']
                df = pd.DataFrame(ohlc_data, columns=['timestamp', 'open', 'high', 'low', 'close', 'volume'])
                df['timestamp'] = pd.to_datetime(df['timestamp'], unit='s')

                # Append the data to the list
                data_list.append(df)

                # Update current_start for the next request
                current_start = str(int(df['timestamp'].iloc[-1].timestamp()))
            else:
                print("Error: Invalid data format received.")
                return None
        else:
            print(f"Error occurred while making the API request. Status Code: {response.status_code}")
            return None

    df_all = pd.concat(data_list, ignore_index=True)

    df_all.rename({"timestamp":"date"}, axis = 1, inplace = True)

    df_all["open"] = df_all["open"].astype(float)
    df_all["high"] = df_all["high"].astype(float)
    df_all["low"] = df_all["low"].astype(float)
    df_all["close"] = df_all["close"].astype(float)
    df_all["volume"] = df_all["volume"].astype(float)

    return df_all


# getting all bitcoin historical OHLC data
currency_pair = "btcusd"
start = "01-01-2010"

df = get_bitstamp_data(currency_pair, start)

# calculating the variables
df["daily_return"] = df["close"]/df["open"]-1

mm = df["high"].rolling(5).mean()
std = df["high"].rolling(5).std()

df["zscore"] = (df["high"]-mm)/std

df["hv"] = 100* (df["close"]/df["open"]-1).rolling(10).std()*np.sqrt(252)

df.dropna(axis = 0, inplace = True)

df = df.reset_index()
df.drop("index", axis = 1, inplace = True)

df["zscore_vol"] = df["zscore"]*df["hv"]

# Building the target and the returns to calculate results
df["target"] = df["close"].shift(-5)/df["open"].shift(-1)-1

df["returns"] = df["close"].shift(-5)/df["open"].shift(-1)-1

df.fillna(0, inplace = True)

df["date"] = pd.to_datetime(df["date"])

df_total = df

# Set the date column as the index
df.set_index('date', inplace=True)

df["sinal"] = "FICAR FORA"
df["sinal"] = np.where((df["zscore_vol"]>0) & (df["hv"]<=20),"COMPRA",df["sinal"])
df["sinal"] = np.where((df["zscore_vol"]<0) & (df["hv"]>=100),"COMPRA",df["sinal"])

df[["open","high","low","close","sinal"]].tail(5)
