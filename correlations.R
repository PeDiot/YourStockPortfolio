source("Rpackages.R")
source("setup.R", encoding = "UTF-8")

assets_value_list_wid <- compute_assets_value(data = yf_data, 
                                              num_shares = my_num_shares) 

my_assets_value <- my_tickers %>%
  lapply(FUN = query_assets_since_buying_date, 
         assets_dat = assets_value_list_wid[my_tickers],
         buying_dates = my_buying_dates) 

portfol_value <- get_portfolio_value(my_assets_value %>%
                                       bind_rows())

date_selection <- my_buying_dates[my_buying_dates != min(my_buying_dates)] %>%
  as.Date()

portfol_ret <- portfol_value %>%
  compute_daily_returns(asset_dat = NULL) %>%
  filter( !(date %in% date_selection) )

portfol_ret_ts <- ts(data = portfol_ret %>% pull(ret))

sr <- SharpeRatio(portfol_ret_ts, Rf = .0003) ; sr

# negative Sharpe ratio as the portfolio returns are < the risk free returns

start_date <- today() - months(6)

my_assets_returns <- lapply(
  X = assets_value_list_wid[my_tickers], 
  FUN = function(d){
    d %>%
      filter(date >= start_date) %>% 
      compute_daily_returns() 
  }
) %>% bind_rows() 

my_assets_returns %>%
  plot_ly(x = ~date, 
          y = ~ret, 
          color = ~ticker, 
          name = ~ticker, 
          mode = "lines", 
          line = list(width=.8)) %>%
  plotly_layout(title = "Asset returns", title.y = "") 

my_assets_returns_wide <- my_assets_returns %>%
  pivot_wider(id_cols = date, 
              names_from = ticker, 
              values_from = ret)

