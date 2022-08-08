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

etf_tickers <- c("^FCHI", 
                 "^GSPC", 
                 "AFK", 
                 "EZA", 
                 "BNK.PA", 
                 "RTWO.AS") 

etf_names <- c("CAC40", 
               "S&P 500", 
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
                "MANA-EUR", 
                "COIN", 
                "AMZN", 
                "META", 
                "NVDA", 
                "GFC.PA", 
                "SU.PA")

my_tickers_ix <- symbols %>%
  mutate(idxs = 1:nrow(.)) %>% 
  filter(tickers %in% my_tickers) %>%
  pull(idxs)

names(my_tickers) <- symbols[my_tickers_ix, ] %>%
  names()

## Transactions ------------------------------------------------------------------

n_btc_tx <- 9
btc_tx <- lapply(X = 1:n_btc_tx, 
                 FUN = function(i) paste("BTC-EUR", i, sep = "_")) %>% unlist()

n_eth_tx <- 3
eth_tx <- lapply(X = 1:n_eth_tx, 
                 FUN = function(i) paste("ETH-EUR", i, sep = "_")) %>% unlist()

n_amzn_tx <- 2
amzn_tx <- lapply(X = 1:n_amzn_tx, 
                  FUN = function(i) paste("AMZN", i, sep = "_")) %>% unlist()

n_meta_tx <- 2
meta_tx <- lapply(X = 1:n_meta_tx, 
                  FUN = function(i) paste("META", i, sep = "_")) %>% unlist()

n_su_tx <- 2
su_tx <- lapply(X = 1:n_su_tx, 
                FUN = function(i) paste("SU.PA", i, sep = "_")) %>% unlist()

my_tickers_tx <- c(btc_tx, 
                   eth_tx,  
                   "MATIC-EUR", 
                   "MANA-EUR", 
                   "COIN", 
                   amzn_tx, 
                   meta_tx, 
                   "NVDA", 
                   "GFC.PA", 
                   su_tx)

my_buying_dates <- c(
  # BTC-EUR
  "2022-03-02", 
  "2022-05-30", 
  "2022-06-06", 
  "2022-06-13",
  "2022-06-20", 
  "2022-06-27", 
  "2022-07-11",
  "2022-07-29", 
  "2022-08-08", 
  # ETH-EUR
  "2022-02-02", 
  "2022-07-05", 
  "2022-07-25", 
  # MATIC-EUR
  "2022-02-03", 
  # MANA-EUR
  "2022-03-02", 
  # COIN
  "2022-03-29",
  # AMZN
  "2022-04-08", 
  "2022-06-16",
  # META
  "2022-05-17",
  "2022-08-01", 
  # NVDA
  "2022-05-30", 
  # GFC.PA
  "2022-07-01", 
  # SU.PA
  "2022-07-04", 
  "2022-08-05"
)
names(my_buying_dates) <- my_tickers_tx

my_num_shares <- c(
  # BTC-EUR
  0.00037241, 
  0.00041181, 
  0.00025110, 
  0.00031765,
  0.00041653, 
  0.00061189,
  0.00071752, 
  0.00041181, 
  0.00061388,
  # ETH-EUR
  0.00594658, 
  0.01296357,
  0.00982442,
  # MATIC-EUR
  10.7270573, 
  # MANA-EUR
  5.77722346, 
  # COIN
  0.10934065, 
  # AMZN
  0.15021685, 
  0.04869812, 
  # META
  0.20416538, 
  0.1906, 
  # NVDA
  0.22274457, 
  # GFC.PA
  0.334, 
  # SU.PA
  0.1601, 
  0.1269
)

names(my_num_shares) <- my_tickers_tx


# USD assets ------------------------------------------------------------------

usd_tickers <- c(
  setdiff(x = stock_tickers,
          y = c(cac40_assets$tickers, 
                stoxx600_assets$tickers)), 
  "^GSPC"
)

my_usd_assets <- intersect(x = usd_tickers, y = my_tickers)
