# Stocks ------------------------------------------------------------------


stock_tickers <- c("1QZ.F", 
                   "UEN.F", 
                   "OVH.PA", 
                   "TFI.PA",
                   "APC.F", 
                   "MSF.F", 
                   "ABEC.F", 
                   "AMZ.F", 
                   "ALUVI.PA", 
                   "COX.PA", 
                   "NAVYA.PA", 
                   "BRK-B", 
                   "B1C.F", 
                   "NNND.F", 
                   "FB2A.F", 
                   "NVD.F", 
                   "TL0.F", 
                   "NFC.F",
                   "2PP.F", 
                   "MC.PA",
                   "OR.PA",
                   "RMS.PA",
                   "TTE.PA",
                   "SAN.PA", 
                   "SU.PA",
                   "AIR.PA",
                   "KER.PA",
                   "BNP.PA",
                   "EL.PA",
                   "AI.PA", 
                   "CS.PA",
                   "DSY.PA",
                   "STLA.PA",
                   "DG.PA",
                   "RI.PA",
                   "SAF.PA",
                   "ACA.PA",
                   "STM.PA",
                   "BN.PA",
                   "CAP.PA",
                   "SGO.PA",
                   "ENGI.PA",
                   "ML.PA",
                   "GLE.PA",
                   "LR.PA",
                   "ORA.PA",
                   "VIE.PA",
                   "TEP.PA",
                   "ERF.PA",
                   "HO.PA",
                   "PUB.PA",
                   "WLN.PA",
                   "CA.PA",
                   "ALO.PA",
                   "VIV.PA",
                   "EN.PA",
                   "RNO.PA") 

stock_names <- c("Coinbase", 
                 "Ubisoft", 
                 "OVH Groupe", 
                 "TF1", 
                 "Apple", 
                 "Microsoft", 
                 "Alphabet", 
                 "Amazon", 
                 "UV Germi", 
                 "Nicox", 
                 "Navya", 
                 "Berkshire Hathaway", 
                 "Baidu", 
                 "Tencent", 
                 "Meta Platforms", 
                 "NVIDIA", 
                 "Tesla", 
                 "Netflix", 
                 "PayPal", 
                 "LVMH",
                 "L'OREAL",
                 "HERMES INTL",
                 "TOTALENERGIES",
                 "SANOFI",           
                 "SCHNEIDER ELECTRIC SE",
                 "AIRBUS SE",            
                 "KERING",
                 "BNP PARIBAS ACT.A",
                 "ESSILORLUXOTTICA",
                 "AIR LIQUIDE",
                 "AXA",
                 "DASSAULT SYSTEMES",
                 "STELLANTIS NV",
                 "VINCI",
                 "PERNOD RICARD",
                 "SAFRAN",            
                 "CREDIT AGRICOLE",
                 "STMICROELECTRONICS",
                 "DANONE",
                 "CAPGEMINI",
                 "SAINT GOBAIN",
                 "ENGIE",
                 "MICHELIN",
                 "SOCIETE GENERALE",
                 "LEGRAND",
                 "ORANGE",
                 "VEOLIA ENVIRON.",
                 "TELEPERFORMANCE",
                 "EUROFINS SCIENT.",
                 "THALES",
                 "PUBLICIS GROUPE SA",
                 "WORLDLINE",
                 "CARREFOUR",
                 "ALSTOM",
                 "VIVENDI SE",
                 "BOUYGUES",           
                 "RENAULT") %>% str_to_upper()

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
                    "USDC-EUR")

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
                  "USDC") %>% str_to_upper()

names(crypto_tickers) <- crypto_names


# ETFs ------------------------------------------------------------------


etf_tickers <- c("^FCHI", 
                 "^GSPC", 
                 "E5T.F")

etf_names <- c("CAC40", 
               "S&P 500", 
               "EUROTECH") 

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
)


# My assets ------------------------------------------------------------------


my_tickers <- c("BTC-EUR", 
                "ETH-EUR", 
                "MATIC-EUR", 
                "MANA-EUR", 
                "1QZ.F", 
                "AMZ.F", 
                "FB2A.F", 
                "NVD.F")

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
