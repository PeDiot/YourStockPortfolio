# Load data ------------------------------------------------------------------

load(file = "cac40_assets.RData")
load(file = "stoxx600_assets.RData")
load(file = "dowJ_assets.RData")
load(file = "sp100_assets.RData")

# Stocks ------------------------------------------------------------------

stock_tickers_ <- structure(list(
  tickers = c(cac40_assets$tickers, 
              stoxx600_assets$tickers, 
              dowJ_assets$tickers, 
              sp100_assets$tickers, 
              "COIN",
              "UNI1-EUR", 
              "GFC.PA")
),
class = "data.frame", 
row.names = c(rownames(cac40_assets), 
              rownames(stoxx600_assets),
              rownames(dowJ_assets), 
              rownames(sp100_assets), 
              "COINBASE", 
              "UNISWAP", 
              "GECINA") 
) %>% 
  distinct(tickers, .keep_all = T) 

stock_tickers_["META-PLATFORMS", "tickers"] <- "META"

stock_tickers <- stock_tickers_$tickers
stock_names <- rownames(stock_tickers_)
names(stock_tickers) <- stock_names

# Crypto ------------------------------------------------------------------

crypto_tickers <- c("BTC-EUR", 
                    "ETH-EUR", 
                    "BNB-EUR", 
                    "SOL-EUR", 
                    "MATIC-EUR", 
                    "MANA-EUR", 
                    "DOGE-EUR", 
                    "BAT-EUR", 
                    "REP-EUR", 
                    "SC-EUR", 
                    "DOT-EUR",
                    "XRP-EUR",
                    "LTC-EUR", 
                    "USDT-EUR", 
                    "USDC-EUR", 
                    "ADA-EUR", 
                    "AVAX-EUR")

crypto_names <- c("Bitcoin", 
                  "Ethereum", 
                  "Binance Coin", 
                  "Solana", 
                  "Polygon (MATIC)", 
                  "Decentraland (MANA)", 
                  "Dogecoin", 
                  "Basic Attention Token", 
                  "Augur", 
                  "Siacoin", 
                  "Polkadot", 
                  "Ripple", 
                  "Litecoin",
                  "Tether", 
                  "USDC", 
                  "cardano", 
                  "avalanche") %>% str_to_upper()

names(crypto_tickers) <- crypto_names


# ETFs ------------------------------------------------------------------

etf_tickers <- c("SPY", 
                 "VTI", 
                 "SWDA.L", 
                 "IEMG", 
                 "BND",
                 "ARKX",
                 "GLD", 
                 "SH",
                 "ACWI", 
                 "ITOT",
                 "VOO",
                 "SPYG",
                 "AMUN.PA", 
                 "AFK", 
                 "EZA", 
                 "BNK.PA", 
                 "RTWO.AS") 

etf_names <- c("SPDR S&P 500 Trust", 
               "Vanguard Total Stock Market",
               "iShares Core MSCI World UCITS ETF", 
               "iShares Core MSCI Emerging Markets", 
               "Vanguard Total Bond Market", 
               "ARK Space Exploration & Innovation", 
               "SPDR Gold", 
               "ProShares Short S&P 500", 
               "iShares MSCI ACWI Index Fund", 
               "iShares Core S&P Total U.S. Stock Market", 
               "Vanguard S&P 500 ETF", 
               "SPDR Portfolio S&P 500 Growth", 
               "Amundi SA", 
               "Market Vectors Africa", 
               "iShares MSCI South Africa", 
               "Lyxor STOXX Europe 600 Banks", 
               "L&G Russell 2000 US Small Cap UCITS") 

names(etf_tickers) <- etf_names

# Data ------------------------------------------------------------------

symbols <- structure(list(
  tickers = c(crypto_tickers, 
              stock_tickers, 
              etf_tickers)
),
class = "data.frame", 
row.names = c(crypto_names, 
              stock_names, 
              etf_names)
) %>% 
  distinct(tickers, .keep_all = T) 

# My assets ------------------------------------------------------------------

## Tickers in the portfolio ------------------------------------------------------------------
my_tickers <- c("BTC-EUR", 
                "ETH-EUR", 
                "MATIC-EUR", 
                "BNB-EUR", 
                "META", 
                "GFC.PA", 
                "AAPL", 
                "VOO", 
                "SWDA.L")

my_tickers_ix <- symbols %>%
  mutate(idxs = 1:nrow(.)) %>% 
  filter(tickers %in% my_tickers) %>%
  pull(idxs)

names(my_tickers) <- lapply(X = my_tickers, FUN = get_company_name) %>% unlist()

my_sold_tickers <- "SU.PA"

my_active_tickers <- setdiff(my_tickers, my_sold_tickers)
names(my_active_tickers) <- lapply(X = my_active_tickers, FUN = get_company_name) %>% unlist()

## Transactions ------------------------------------------------------------------

n_btc_tx <- 11
btc_tx <- lapply(X = 1:n_btc_tx, 
                 FUN = function(i) paste("BTC-EUR", i, sep = "_")) %>% unlist()

n_eth_tx <- 5
eth_tx <- lapply(X = 1:n_eth_tx, 
                 FUN = function(i) paste("ETH-EUR", i, sep = "_")) %>% unlist()

n_appl_tx <- 2
appl_tx <- lapply(X = 1:n_appl_tx, 
                FUN = function(i) paste("AAPL", i, sep = "_")) %>% unlist()

my_tickers_tx <- c(
  btc_tx, 
  eth_tx,  
  "MATIC-EUR", 
  "BNB-EUR", 
  "META", 
  "GFC.PA", 
  appl_tx, 
  "VOO", 
  "SWDA.L"
)

n_tx <- length(my_tickers_tx)

my_buy_dates <- c(
  # BTC-EUR
  "2022-05-30", 
  "2022-06-06", 
  "2022-06-13",
  "2022-06-20", 
  "2022-06-27", 
  "2022-07-11",
  "2022-07-29",
  "2022-09-18", 
  "2022-09-27", 
  "2022-12-04", 
  "2022-12-22", 
  # ETH-EUR
  "2022-07-05", 
  "2022-07-25", 
  "2022-08-25",
  "2022-11-20", 
  "2022-12-21", 
  # MATIC-EUR
  "2022-05-23", 
  # BNB-EUR
  "2022-10-20", 
  # META, 
  "2022-08-01", 
  # GFC.PA
  "2022-07-01", 
  # AAPL
  "2022-09-08", 
  "2022-11-09", 
  # VOO
  "2022-10-17", 
  # SWDA.L
  "2022-12-07"
)
names(my_buy_dates) <- my_tickers_tx

my_sale_dates <- rep("", n_tx)
names(my_sale_dates) <- my_tickers_tx 
  
my_num_shares <- c(
  # BTC-EUR
  0.00041181, 
  0.00025110, 
  0.00031765,
  0.00041653, 
  0.00061189,
  0.00071752, 
  0.00041181,
  0.00073444,
  0.00069181,
  0.00090067, 
  0.00092447, 
  # ETH-EUR
  0.01296357,
  0.00982442,
  0.00853483, 
  0.01280709,
  0.0126839, 
  # MATIC-EUR
  10.7270573, 
  # BNB-EUR
  0.05197610, 
  # META
  0.1906, 
  # GFC.PA
  0.334, 
  # AAPL
  0.32014, 
  0.34, 
  # VOO
  0.14812, 
  # SWDA.L
  0.67
)

names(my_num_shares) <- my_tickers_tx


# USD assets ------------------------------------------------------------------

usd_tickers <- setdiff(
  x = c(stock_tickers, etf_tickers), 
  y = c(cac40_assets$tickers, 
        stoxx600_assets$tickers, 
       "GFC.PA", 
       "BNK.PA", 
       "SWDA.L")
)

gbx_tickers <- c("SWDA.L")

my_usd_assets <- intersect(x = usd_tickers, y = my_tickers)
