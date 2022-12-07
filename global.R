# Setup -------------------------------------------------------------------

source("Rpackages.R")
source("setup.R", encoding = "UTF-8")
source("assets.R", encoding = "UTF-8")

# Data --------------------------------------------------------------------

date_init <- today() - years(3)

yf_data <- get_tq_data(tickers = symbols$tickers, start_date = date_init)

yf_data[usd_tickers] <- lapply(
  
  X = yf_data[usd_tickers], 
  
  FUN = function(d){
    d %>%
      mutate_at(vars(open:close), usd_to_euros)
  }
  
)

yf_data[gbx_tickers] <- lapply(
  
  X = yf_data[gbx_tickers], 
  
  FUN = function(d){
    d %>%
      mutate_at(vars(open:close), gbx_to_euros)
  }
  
)
