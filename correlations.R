source("Rpackages.R")
source("setup.R", encoding = "UTF-8")

library(ggcorrplot)

date_init <- today() - years(5)
yf_data <- get_tq_data(tickers = symbols$tickers, 
                       start_date = date_init)

assets_value_list_wid <- compute_assets_value(data = yf_data, 
                                              num_shares = my_num_shares) 

my_assets_value <- my_tickers %>%
  lapply(FUN = query_assets_since_buying_date, 
         assets_dat = assets_value_list_wid[my_tickers],
         buying_dates = my_buying_dates) 

my_assets_weights <- my_assets_value %>%
  bind_rows() %>%
  calculate_assets_weights(pct = F) %>% 
  select(c(ticker, wts))

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
    buying_date <- my_buying_dates[d$ticker]
    d %>%
      filter(date >= buying_date) %>% 
      compute_daily_returns() 
  }
) %>% bind_rows() 

weighted_returns <- compute_weighted_returns(ret_data = my_assets_returns, 
                                             wts_dat = my_assets_weights)

compute_cumulative_returns(ret_data = weighted_returns, all = F, weighted = T) %>% 
  filter(date == max(date)) %>% 
  ungroup() %>% 
  filter(cr == max(cr, na.rm = T))

port_weighted_ret <- weighted_returns %>%
  group_by(date) %>%
  summarise(ret = sum(wt_return))

port_cumulative_ret <- port_weighted_ret %>%
  compute_cumulative_returns()

port_cumulative_ret %>% 
  plot_ly() %>% 
  plot_cumulative_returns(legend_group = "one")

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
colnames(my_assets_returns_wide) <- c("date", names(my_tickers))

cor_mat <- my_assets_returns_wide %>% 
  select(-date) %>%
  cor()
p_mat <- my_assets_returns_wide %>% 
  select(-date) %>%
  cor_pmat()

cor_plot <- ggcorrplot(cor_mat,
                       hc.order = TRUE, 
                       lab = T, 
                       lab_col = "#76787B",
                       p.mat = p_mat, 
                       type = "lower",
                       outline.col = "white",
                       ggtheme = ggplot2::theme_minimal(),
                       colors = c("#E88787", "white", "#758ADA")) +
  theme(legend.position = "none", 
        axis.title = element_text(color = "#76787B"))

ggplotly(cor_plot) %>%
  layout(xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE))

# http://www.nematrian.com/MeasuringAverageStockCorrelation

coefs <- NULL
for (tick1 in my_tickers){
  for (tick2 in my_tickers){
    if (tick2 != tick1){
      c <- cor_mat[tick1, tick2]
      name <- paste(tick1, tick2, sep = "/")
      if (!( c %in% coefs )){
        coefs <- c(coefs, c)
        names(coefs)[length(coefs)] <- name
      }
    }
  }
}
