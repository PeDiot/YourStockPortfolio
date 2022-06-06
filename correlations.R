# Setup -------------------------------------------------------------------

source("Rpackages.R")
source("setup.R", encoding = "UTF-8")

library(ggcorrplot)


# Data --------------------------------------------------------------------

date_init <- today() - years(3)
yf_data <- get_tq_data(tickers = symbols$tickers, 
                       start_date = date_init)

yf_data[usd_tickers] <- lapply(
  
  X = yf_data[usd_tickers], 
  
  FUN = function(d){
    d %>%
      mutate_at(vars(open:close), usd_to_euros)
  }
  
)

# Assets & portfolio value --------------------------------------------------------------------

assets_value_list_wid <- compute_assets_value(data = yf_data, 
                                              num_shares = my_num_shares) 

my_assets_value <- my_tickers %>%
  lapply(FUN = query_assets_since_buying_date, 
         assets_dat = assets_value_list_wid[my_tickers],
         buying_dates = my_buying_dates) 

my_assets_weights <- my_assets_value %>%
  bind_rows() %>%
  calculate_assets_weights() %>% 
  select(c(ticker, wts))

portfol_value <- get_portfolio_value(my_assets_value %>%
                                       bind_rows())

# Returns --------------------------------------------------------------------

my_assets_returns <- calculate_multiple_assets_returns(
  assets_value_list_wid,
  my_tickers
)

weighted_returns <- compute_weighted_returns(ret_data = my_assets_returns, 
                                             wts_dat = my_assets_weights)

weighted_returns %>%
  plot_ly(x = ~date, 
          y = ~ret, 
          color = ~ticker, 
          name = ~ticker, 
          mode = "lines", 
          line = list(width=.8)) %>%
  plotly_layout(title = "Asset weighted returns", title.y = "") 

port_weighted_ret <- weighted_returns %>%
  group_by(date) %>%
  summarise(ret = sum(wt_return))

port_weighted_ret %>%
  plot_ly() %>% 
  plot_daily_returns(trace_col = evolution, 
                     legend_group = "one") %>% 
  plotly_layout(title = "Portfolio daily returns", title.y = "")


# Sharpe Ratio --------------------------------------------------------------------

# SR = (Rp - Rf) / std.Dev(Rp)

portfol_ret_ts <- ts(data = port_weighted_ret %>% pull(ret))

sr <- SharpeRatio(portfol_ret_ts, Rf = .02) ; sr
# Sharpe ratio is negative when the investment return is lower than the risk-free rate.

# Correlations --------------------------------------------------------------------

## Matrix --------------------------------------------------------------------

my_assets_returns_6m <- calculate_multiple_assets_returns(
  assets_value_list_wid,
  my_tickers,
  start_date = today() - months(6)
) %>%
  pivot_wider(id_cols = date, 
              names_from = ticker, 
              values_from = ret)

colnames(my_assets_returns_6m) <- c("date", names(my_tickers))

cor_mat <- my_assets_returns_6m %>% 
  select(-date) %>%
  cor(use = "complete.obs")

p_mat <- my_assets_returns_6m %>% 
  select(-date) %>%
  cor_pmat(use = "complete.obs")

plot_cor_mat(cor_mat, p_mat)

## Average portfolio correlation --------------------------------------------------------------------
# http://www.nematrian.com/MeasuringAverageStockCorrelation

calculate_avg_cor(cor_mat, my_assets_weights$wts)
