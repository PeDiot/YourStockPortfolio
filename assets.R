# Load data ------------------------------------------------------------------

load(file = "cac40_assets.RData")
load(file = "stoxx600_assets.RData")
load(file = "dowJ_assets.RData")
load(file = "sp100_assets.RData")

# Stocks ------------------------------------------------------------------

stock_tickers <- c(cac40_assets$tickers, 
                   stoxx600_assets$tickers, 
                   dowJ_assets$tickers, 
                   sp100_assets$tickers, 
                   "COIN") %>% unique()

stock_names <- c(rownames(cac40_assets), 
                 rownames(stoxx600_assets),
                 rownames(dowJ_assets), 
                 rownames(sp100_assets), 
                 "COINBASE") %>% unique()

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
                 "^GSPC") 

etf_names <- c("CAC40", 
               "S&P 500") 

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

my_tickers <- c("BTC-EUR", 
                "ETH-EUR", 
                "MATIC-EUR", 
                "MANA-EUR", 
                "COIN", 
                "AMZN", 
                "FB", 
                "NVDA")

my_tickers_ix <- lapply(
  X = 1:nrow(symbols), 
  FUN = function(ix){
    if (symbols[ix, "tickers"] %in% my_tickers){
      return(ix)
    }
  }
) %>% unlist()

names(my_tickers) <- symbols[my_tickers_ix, ] %>%
  names()

my_buying_dates <- c("2022-03-02", 
                     "2022-02-02", 
                     "2022-02-03", 
                     "2022-03-02", 
                     "2022-03-29",
                     "2022-04-08", 
                     "2022-05-17", 
                     "2022-05-30")
names(my_buying_dates) <- my_tickers

my_num_shares <- c(0.00037241, 
                   0.00594658, 
                   10.8270573, 
                   10.8270573, 
                   0.10934065, 
                   0.00790615, 
                   0.20416538, 
                   0.22274457)

names(my_num_shares) <- my_tickers


# USD assets ------------------------------------------------------------------

usd_tickers <- c(
  setdiff(x = stock_tickers,
          y = c(cac40_assets$tickers, 
                stoxx600_assets$tickers)), 
  "^GSPC"
)

my_usd_assets <- intersect(x = usd_tickers, y = my_tickers)
