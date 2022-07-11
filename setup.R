# Setup -------------------------------------------------------

options(warn = -1)

## Credentials -------------------------------------------------------
credentials <- data.frame(
  user = c("zeujahh"),
  password = c("1995poSSe*"),
  admin = TRUE,
  comment = "Simple and secure authentification mechanism for single ‘Shiny’ applications.",
  stringsAsFactors = FALSE
)

## Colors -------------------------------------------------------

evolution <- "#709ABE"
high <- "Forest Green"
low <- "Red"
short <- "#72C4D7"
medium <- "purple"
long <- "black"
macd <- "#C47D72"
macdHist <- "#B5CDD2"
rsi <- "#F3B0DE"
stoch_rsi <- "purple"
bbands <- "#CACED2"
pctBB <- "#5EB6B1"
pred <- "#A48BAB"
obv <- "#E4E39D"

## GGplot theme -------------------------------------------------------

theme_set(theme_minimal())

# Utils -------------------------------------------------------

save_data_list <- function(df_list){
  "Save list of dataframes."
  
  names <- names(df_list)
  n_dfs <- length(df_list)
  sapply(seq_along(1:n_dfs), 
         function(i){
           write_feather(
             x = df_list[[i]] %>%
               filter(date >= today() - months(12)), 
             path = paste0("./backup/assets/", names[i],".feather")
           )
         }
  )
  
}

save_num_shares <- function(num_shares){
  "Save number of shares to RData."
  
  dat <- data.frame(num_shares) 
  save(dat, 
       file = paste0(backup, "num_shares.RData"))
  
}

get_ticker <- function(company_name){
  "Return ticker given company_name."
  
  symbols[company_name, "tickers"]
}

get_company_name <- function(ticker){
  "Return company name given ticker."
  
  symbols %>%
    filter(tickers == ticker) %>%
    rownames()
}

extract_ticker <- function(ticker_name){
  "Extract ticker name in the case of multiple buying points."
  
  ticker <- str_split(string = ticker_name, pattern = "_") %>% unlist()
  
  if (length(ticker) > 1){
    ticker <- ticker[1]
  }
  
  return(ticker)
  
}

get_asset_last_value <- function(ticker, assets_value){
  "Return ticker's last value given its price and number of shares."
  assets_value[assets_value$ticker == ticker,]$value %>% 
    tail(1)
}


get_indicator_plot_title <- function(ticker, indicator_type){
  "Make title ."
  asset <- get_company_name(ticker)
  title <- paste0(asset, 
                  " (", 
                  ticker,
                  ") - ", 
                  indicator_type)
  return(title)
}

clean_tq_data <- function(df){
  "Remove duplicated rows and fill NA values."
  
  df %>%
    distinct(date, 
             .keep_all = T) %>%
    mutate(ticker = na.locf(ticker), 
           close = na.locf(close)) %>%
    complete(date = seq.Date(min(date), today(), by="day")) %>%
    fill(everything())
}

get_tq_data <- function(tickers, start_date){
  "Return price data for given asset and period."
  
  if (length(tickers) == 1){
    
    dat <- tq_get(x = tickers, 
                  get = "stock.prices", 
                  from = start_date,
                  to = today(),
                  complete_cases = T) %>%
      clean_tq_data()
    
    return(dat)
    
  }
  else {
    
    dat <- tq_get(x = tickers, 
                  get = "stock.prices", 
                  from = start_date,
                  to = today(),
                  complete_cases = T)
    colnames(dat)[1] <- "ticker"  
    df_list <- split(dat, dat$ticker) %>%
      lapply(clean_tq_data)
    
    return(df_list)
    
  }
  
}


clean_assets_value <- function(assets_value, portfolio_value){
  "Modify assets value df for vizualisation."
  
  tickers <- assets_value %>%
    pull(ticker) %>%
    unique()
  new_levels <- lapply(
    tickers,
    function(ticker){
      last_val <- get_asset_last_value(ticker, 
                                       assets_value) 
      contrib <- 100 * last_val / portfolio_value
      asset <- get_company_name(ticker)
      paste0(asset, " (", round(contrib, 1), "%)")
    }
  ) %>% unlist()
  names(new_levels) <- tickers
  
  assets_value %>%
    mutate(ticker = revalue(ticker, new_levels))
  
}

cumret_to_percent <- function(cr){
  "Convert floating cumulative returns to percent."
  
  if (cr >= 1){
    pct_cr <- 100*(cr - 1) %>%
      round(3)
  }
  else{
    pct_cr <- - 100*(1 - cr) %>%
      round(3)
  }
  return(pct_cr)
  
}

ret_to_percent <- function(tot_ret){
  "Convert returns to percent."
  
  pct_ret <- round(100 * tot_ret, 2)
  return(pct_ret)
  
}

format_number <- function(x){
  "Render number easier to read."
  
  x %>% 
    round(2) %>%
    prettyNum(big.mark = ",") 
  
}

format_table_numbers <- function(tab){
  "Convert numbers into more friendly format."
  
  tab %>% 
    mutate_if(is.numeric, 
              format_number) 
}

get_buying_price <- function(ticker){
  "Return asset buying price."
  from <- my_buying_dates[ticker] %>% as.Date()
  
  yf_data[[ extract_ticker(ticker) ]] %>%
    filter(date == from) %>% 
    pull(close)
  
}

get_current_price <- function(ticker){
  "Return asset current price."
  
  yf_data[[ extract_ticker(ticker) ]] %>%
    filter(date == today()) %>% 
    pull(close)
  
}

get_invested_amount <- function(ticker){
  "Return invested amount for one asset."
  
  price <- get_buying_price(ticker)
  return(price * my_num_shares[ticker])
  
}

get_current_value <- function(ticker){
  "Compute asset current value given number of shares held."
  
  price <- get_current_price(ticker)
  return(price * my_num_shares[ticker])
}

calculate_total_invested <- function(tickers){
  "Return total amount invested in €."
  
  val <- lapply(X = tickers, FUN = get_invested_amount) %>% 
    unlist() %>% 
    sum()
  return(val)
  
}

create_tx_table <- function(tickers, buying_dates, num_shares){
  "Create a table to display transactions."
  
  assets <- lapply(X = tickers, 
                   FUN = function(x) extract_ticker(x) %>% 
                     get_company_name()) %>% unlist()
  
  buying_prices <- lapply(X = tickers, FUN = get_buying_price) %>% unlist() 
  
  current_prices <- lapply(X = tickers, FUN = get_current_price) %>% unlist()
  
  invest_amounts <- lapply(X = tickers, FUN = get_invested_amount) %>% unlist()
  
  val_returns <- round( num_shares * (current_prices - buying_prices), 2 )
  
  pct_returns <- round( 100 * calculate_total_returns(buying_prices, current_prices), 2 ) 
  
  data.frame(buying_dates = buying_dates,
             assets = assets, 
             num_shares = num_shares, 
             buying_prices = buying_prices, 
             current_prices = current_prices,
             invest_amounts = invest_amounts, 
             val_returns = val_returns, 
             pct_returns = pct_returns) %>% 
    mutate_at( vars(buying_prices:invest_amounts), format_number ) %>% 
    rename(`Buying date` = buying_dates, 
            Asset = assets, 
           `Number of shares` = num_shares, 
           `Buying price (€)` = buying_prices, 
           `Current price (€)` = current_prices, 
           `Invested amount (€)` = invest_amounts, 
           `€ Returns` = val_returns, 
           `% Returns` = pct_returns) %>%
    as.data.frame(row.names = 1:nrow(.)) %>% 
    arrange(desc(`Buying date`))
  
}

txUSDEUR <- getQuote("USDEUR=X") %>% pull(Last)

usd_to_euros <- function(usd_val){
  "Convert USD value to €."
  
  eur_val <- usd_val * txUSDEUR
  return(eur_val)
}

# Value -------------------------------------------------------

compute_assets_value <- function(
  data,
  tickers = my_tickers_tx, 
  buying_dates = my_buying_dates, 
  num_shares = my_num_shares, 
  start_date = NULL
){
  "Return assets value given prices and number of shares from start date or buying date."
  
  res <- lapply(
    
    tickers, 
    
    function(ticker){
      
      df <- data[[extract_ticker(ticker)]]
      n_shares <- num_shares[ticker]
      from <- ifelse(is.null(start_date), 
                     buying_dates[ticker], 
                     start_date) 
      
      df %>%
        filter( date >= from ) %>% 
        mutate( n_shares = rep(n_shares, nrow(.)) ) %>%
        mutate( value = close * n_shares ) 
      
    }
  ) 
  
  names(res) <- tickers
  
  return(res) 
  
}

get_portfolio_value <- function(assets_value){
  "Return portfolio value given assets' prices and number of shares."
  
  assets_value %>%
    group_by(date) %>%
    summarise(value = sum(value))
  
}

calculate_assets_weights <- function(tickers){
  "Calculate each asset's weights in the portfolio based on amount invested."
  
  invests <- lapply(X = tickers, FUN = get_invested_amount) %>% unlist()
  total_invest <- invests %>% sum()
  current_vals <- lapply(X = tickers, FUN = get_current_value) %>% unlist()
  total_val <- current_vals %>% sum()
  
  d <- data.frame(
    ticker = lapply(X = tickers, FUN = extract_ticker) %>% unlist(), 
    invest = invests,
    val = current_vals, 
    row.names = 1:length(tickers)
  ) %>%
    group_by(ticker) %>% 
    summarise_at( vars(invest, val), sum ) %>% 
    mutate(wts_invest = invest / total_invest, 
           wts_val = val / total_val) %>%
    mutate(pct_invest = 100*wts_invest, 
           pct_val = 100*wts_val)
  
  return(d)
  
}

compute_tx_weight <- function(ticker){
  "Return transaction weight in the total number of shares held for a given asset."
  num_shares <- my_num_shares[ticker]
  mask <- str_detect( string = my_tickers_tx, pattern = extract_ticker(ticker) )
  S_weights <- my_num_shares[mask] %>% sum()
  
  return( num_shares / S_weights )
  
}

# Assets performance -------------------------------------------------------

calculate_total_returns <- function(p0, p1){
  r <- (p1 - p0) / p0
  return(r)
}

get_all_assets_total_returns <- function(tickers){
  "Calculate each asset total returns."
  
  d <- data.frame(
    
    ticker = tickers, 
    
    tot_ret = lapply(
      X = tickers, 
      FUN = function(ticker) calculate_total_returns( p0 = get_buying_price(ticker),
                                                      p1 = get_current_price(ticker) )
    ) %>% unlist(), 
    
    row.names = 1:length(tickers)
    
  ) 
  
  d %>% 
    mutate(ticker_ = lapply(ticker, extract_ticker) %>% unlist()) %>%
    group_by(ticker_) %>%
    mutate( tot_ret = weight_tx_returns(ticker, tot_ret) ) %>% 
    summarise(tot_ret = sum(tot_ret)) %>% 
    ungroup() %>%
    rename(ticker = ticker_) %>%
    relocate(ticker, .before = tot_ret)
  
}


compute_daily_returns <- function(asset_dat, portfolio_dat = NULL){
  "Calculate the daily returns and for our assets."
  
  if ( !(is.null(asset_dat)) ){
    n_tickers <- asset_dat %>% 
      pull(ticker) %>%
      unique() %>%
      length()
    if (n_tickers == 1){
      returns <- asset_dat %>%
        tq_mutate(select = close,
                  mutate_fun = periodReturn,
                  period = "daily",
                  col_rename = "ret") %>%
        select(c(ticker,
                 date, 
                 ret))
    }
    else{
      returns <- asset_dat %>%
        group_by(ticker) %>%
        tq_mutate(select = close,
                  mutate_fun = periodReturn,
                  period = "daily",
                  col_rename = "ret") %>%
        select(c(ticker,
                 date, 
                 ret))
    }
    
  }
  if ( !(is.null(portfolio_dat)) ){
    returns <- portfolio_dat %>%
      tq_transmute(select = value,
                   mutate_fun = periodReturn,
                   period = "daily",
                   col_rename = "ret")
  }
  
  return(returns)
  
}

weight_tx_returns <-  function(tx, tot_returns){
  "Weight transaction returns for a given asset."
  
  wt <- compute_tx_weight(ticker = tx)
  wt_tot_returns <- wt * tot_returns 
  return(wt_tot_returns)
  
}

weight_returns_per_asset <- function(returns_data){
  "Weight returns for asset with multiple buying points."
  
  returns_data %>%
    mutate(ticker_ = lapply(ticker, extract_ticker) %>% unlist()) %>%
    group_by(ticker) %>%
    mutate( ret = weight_tx_returns(ticker, ret) ) %>% 
    ungroup() %>%
    select(-ticker) %>% 
    rename(ticker = ticker_) %>%
    relocate(ticker, .before = date)
  
}

compute_weighted_returns <- function(ret_data, wts_dat){
  "Calculate the weighted average of our asset returns."
  ret_data <- left_join(x = ret_data,
                        y = wts_dat, 
                        by = "ticker")
  
  ret_data %>%
    mutate(wt_return = wts_invest * ret)
  
}

compute_cumulative_returns <- function(ret_data, all = T, weighted = F){
  "Calculate the cumulative returns for the entire portfolio or a specific ticker. "
  
  if (all == T){
    cum_returns <- ret_data %>%
      mutate(cr = cumprod(1 + ret)) 
  }
  else{
    if (weighted == F){
      cum_returns <- ret_data %>%
        group_by(ticker) %>%
        mutate(cr = cumprod(1 + ret)) 
    }
    else{
      cum_returns <- ret_data %>%
        group_by(ticker) %>%
        mutate(cr = cumprod(1 + wt_return)) 
    }
  }
  
  return(cum_returns)
  
}

calculate_multiple_assets_returns <- function(
  data_list, 
  tickers = my_tickers,
  start_date = NULL
){
  "Calculate daily returns for multiple assets since each asset buying date or start date."
  
  if ( is.null(start_date) ){
    
    f <- function(tick, d){
      buying_date <- my_buying_dates[tick]
      
      d %>%
        mutate( ticker = rep(tick, nrow(.)) ) %>%
        filter(date >= buying_date) %>% 
        compute_daily_returns() 
    }
    
  }
  
  else{
    
    f <- function(tick, d){
      
      d %>%
        mutate( ticker = rep(tick, nrow(.)) ) %>% 
        filter(date >= start_date) %>% 
        compute_daily_returns() 
      
    }
    
  }
  
  res <- mapply(FUN = f, 
                tick = tickers, d = data_list[tickers], 
                SIMPLIFY = F) %>% bind_rows()
  
  return(res)
  
}


get_current_cumret <- function(cumret_data){
  "Return cumulative returns at last date." 
  
  last_cr <- cumret_data %>%
    filter(date == max(date)) %>%
    pull(cr) %>%
    cumret_to_percent()
  return(last_cr)
  
}

get_best_asset <- function(assets_total_rets){
  "Return asset with best total returns as of today."
  
  res <- assets_total_rets %>% 
    filter(tot_ret == max(tot_ret)) %>% 
    head(n = 1)
  
  list(asset = get_company_name(res$ticker), 
       pct_tot_ret = ret_to_percent(res$tot_ret))
  
}

get_worst_asset <- function(assets_total_rets){
  "Return asset with worst total returns as of today."
  
  res <- assets_total_rets %>% 
    filter(tot_ret == min(tot_ret)) %>% 
    head(n = 1)
  
  list(asset = get_company_name(res$ticker), 
       pct_tot_ret = ret_to_percent(res$tot_ret))
  
}

# Financial indicators -------------------------------------------------------

add_moving_avg <- function(
  price_data,
  window, 
  align = "right"
){
  "Return price data with moving average for given period and prices."
  
  new_name <- paste0("MA", as.character(window))
  price_data %>%
    mutate(MA = rollmean(x = close,
                         k = window,
                         fill = NA,
                         align = align)) %>%
    as.data.table() %>%
    setnames(old = "MA", new = new_name) %>%
    as.data.frame()
  
}

get_last_ma <- function(price_data, ma){
  "Return last moving average."
  
  price_data %>%
    filter(date == max(date)) %>%
    pull(ma)
  
}

get_ma_signals <- function(
  price_data, 
  MAshort = "MA20",
  MAlong = "MA50"
){
  "Identify buying and selling signals from moving average."
  
  col_names <- colnames(price_data)
  if (MAshort %in% col_names & MAlong %in% col_names){
    price_data %>%
      mutate(
        MABuy = case_when(
          ((!!sym(MAshort)) > (!!sym(MAlong)) & lag((!!sym(MAshort))) < lag((!!sym(MAlong)))) ~ 1, 
          TRUE ~ 0
        ),
        MASell = case_when(
          ((!!sym(MAshort)) < (!!sym(MAlong)) & lag((!!sym(MAshort))) > lag((!!sym(MAlong)))) ~ 1, 
          TRUE ~ 0
        )
      )
  }
  else{
    stop(paste0("You need to calculate ", MAshort, " and ", MAlong, "."))
  }
  
}

calculate_bbands <- function(
  price_data,
  n_periods = 20,
  sd = 2
){
  "Compute Bollinger bands and %B."
  
  closes <- price_data %>% pull(close)
  dates <- price_data %>%
    pull(date)
  
  BBands(HLC = closes, 
         n = n_periods, 
         sd = sd) %>%
    data.frame() %>%
    mutate(date = dates, 
           close = closes) 
  
}

add_bbands <- function(price_data, bbands_data){
  "Add Bollinger bands to price data."
  
  new_dat <- merge( x = price_data, 
                    y = bbands_data %>%
                      select(c(date, 
                              up, 
                              dn)) )
  return(new_dat)
}

add_macd <- function(
  price_data, 
  ema_short = 12, 
  ema_long = 26, 
  signal = 9, 
  percent = T
){
  "Return MACD and signal values for given period and prices."
  
  days_diff <- today() - min(price_data %>%
                               pull(date))
  
  if (days_diff < ema_long){
    price_data <- price_data %>%
      mutate( MACD = rep(NA, nrow(.)) ) 
  }
  
  else{
    if (percent == T){
      macd_ <- MACD(x = price_data %>% pull(close), 
                    nFast = ema_short, 
                    nSlow = ema_long, 
                    nSig = signal, 
                    percent = T)
    }
    else{
      macd_ <- MACD(x = price_data %>% pull(close), 
                    nFast = ema_short, 
                    nSlow = ema_long, 
                    nSig = signal, 
                    percent = F)
    }
    price_data <- price_data %>%
      mutate(MACD = macd_[, 1], 
             MACDSignal = macd_[, 2], 
             MACDHist = MACD - MACDSignal)
    
  }
  
  return(price_data) 
  
}

get_macd_signals <- function(price_data){
  "Identify buying and selling signals from MACD."
  
  col_names <- colnames(price_data)
  if ("MACD" %in% col_names & "MACDSignal" %in% col_names){
    price_data %>%
      mutate(
        MACDBuy = case_when(
          MACDHist > 0 & lag(MACDHist) < 0 ~ 1, 
          TRUE ~ 0
        ), 
        MACDSell = case_when(
          MACDHist < 0 & lag(MACDHist) > 0 ~ 1, 
          TRUE ~ 0
        )
      )
  }
  else{
    stop("You need to calculate MACD and signal.")
  }
  
}


add_rsi <- function(
  price_data, 
  rsi_period = 14, 
  ma_type = "WMA"
){
  "Compute Relative Strength Index (RSI) for given period and prices."
  
  days_diff <- today() - min(price_data %>%
                               pull(date))
  
  if (days_diff < rsi_period){
    price_data <- price_data %>%
      mutate( RSI = rep(NA, nrow(.)) ) 
  }
  
  else{
    price_data <- price_data %>%
      mutate(RSI = RSI(price = close, 
                       n =   rsi_period,  
                       maType = ma_type))
  }

  return(price_data)
  
}

add_stochRsi <- function(data){
  "Compute and add stochastic RSI to existing data."
  
  minRSI <- data %>%
    pull(RSI) %>% 
    min(na.rm = T)
  maxRSI <- data %>% 
    pull(RSI) %>% 
    max(na.rm = T)
  
  data %>% 
    mutate(stochRSI = 100 * ( RSI - minRSI ) / ( maxRSI - minRSI ))
  
}

get_rsi_signals <- function(
  price_data,
  rsi_type = "RSI", 
  ma_window = 100
){
  "Identify buying and selling signals from RSI for days where closing price > MAlong."
  
  start_date <- price_data %>% 
    pull(date) %>%
    min()
  max_date <- price_data %>% 
    pull(date) %>% 
    max()
  
  if ( !( paste0("MA", ma_window) %in% colnames(price_data) ) ){
    
    if ( (max_date - start_date) > ma_window ) {
      res <- price_data %>%
        add_moving_avg(window = ma_window) %>% 
        filter( close >= !!sym(paste0("MA", ma_window)) ) 
    }
    else{ stop(paste("You need more than", ma_window, "days.")) }
    
  
  }
  else{ res <- price_data }
  
  
  if (rsi_type %in% colnames(price_data)){
    
    if (rsi_type == "RSI"){
      res <- res %>% 
        mutate(
          RSIBuy = case_when(
            RSI < 30 ~ 1, 
            TRUE ~ 0
          ), 
          RSISell = case_when(
            RSI > 70 ~ 1, 
            TRUE ~ 0
          )
        )
    }
    
    else{
      if (rsi_type == "stochRSI"){
        res <- res %>% 
          mutate(
            stochRSIBuy = case_when(
              stochRSI < 20 ~ 1, 
              TRUE ~ 0
            ), 
            stochRSISell = case_when(
              stochRSI > 80 ~ 1, 
              TRUE ~ 0
            )
          )
      }
      else{ stop("rsi_type takes the value 'RSI' or 'stochRSI'.") }
    }
  
  }
  else{ stop(paste("You need more than", ma_window, "days.")) }
  
  return(res)
  
}

add_obv <- function(fin_data){
  "Add on-balance-volume indicator."
  fin_data %>%
    mutate(OBV = OBV(price = close, volume = volume))
}


add_price_direction <- function(fin_data){
  "Stock direction based on opening and closing prices."
  fin_data %>%
    mutate(direction = if_else(
      condition = close > open, 
      true = "Up", 
      false = "Down"
    ) %>% as.factor())
}

calculate_sharpeRatio <- function(returns, rf = .02){
  "Calculate sharpe ratio based on returns and risk free rate."
  
  sr <- SharpeRatio(
    R = ts(data = returns),
    Rf = rf
  )
  return(sr[1, 1])
  
}

calculate_weighted_cor <- function(asset1, asset2, cor_mat, weights){
  "Compute weighted correlation btw two assets returns."
  
  if (asset1 != asset2){
  
    w1 <- weights[get_ticker(asset1)]
    w2 <- weights[get_ticker(asset2)]
    
    return( w1 * w2 * cor_mat[asset1, asset2] )
    
  }
  
}

calculate_avg_cor <- function(cor_mat, weights_data){
  "Calculate average correlation btw several assets."
  
  weights <- weights_data$wts_invest
  names(weights) <- weights_data$ticker
  
  assets <- colnames(cor_mat)
  denom <- 1 - sum(weights**2)
  
  num <- lapply(
    X = names(my_tickers), 
    FUN = function(asset1) lapply(X = assets, 
                                  FUN = calculate_weighted_cor, 
                                  asset2 = asset1, 
                                  cor_mat = cor_mat,
                                  weights = weights) %>% 
      unlist() %>% 
      sum()
    
  ) %>% 
    unlist() %>% 
    sum()
  
  return(num / denom)
  
}

# Data Viz -------------------------------------------------------

range_selector_period <- function(
  x_pos = .5, 
  y_pos
){
  "Plotly buttons to select period."
  
  list(visible = TRUE, x = x_pos, y = y_pos,
       xanchor = "center", yref = "paper",
       font = list(size = 9),
       buttons = list(
         list(count=1,
              label="ALL",
              step="all"),
         list(count=6,
              label="6 MO",
              step="month",
              stepmode="backward"),
         list(count=3,
              label="3 MO",
              step="month",
              stepmode="backward"),
         list(count=1,
              label="1 MO",
              step="month",
              stepmode="backward")
       ))
  
}

plotly_legend <- function(x.pos = .5, y.pos = -.15, size = 12){
  list(orientation = "h", x = x.pos, y = y.pos,
       xanchor = "center", yref = "paper",
       font = list(size = 12),
       bgcolor = "transparent", 
       borderwidth = .2)
}

plotly_layout <- function(
  p,
  title,
  title.y, 
  range_selector = T
){
  
  if (range_selector == T){
    p_layout <-p %>%
      layout(title = title,
             xaxis = list(rangeslider = list(visible = F), 
                          rangeselector = range_selector_period(y_pos = -.1),
                          title = ""),
             yaxis = list(fixedrange = FALSE, 
                          title = list(text = title.y,
                                       font = list(color = "#76787B"))),
             legend = plotly_legend()) 
  }
  else {
    p_layout <- p %>%
      layout(title = title,
             xaxis = list(rangeslider = list(visible = F), 
                          title = ""),
             yaxis = list(fixedrange = FALSE, 
                          title = list(text = title.y,
                                       font = list(color = "#76787B"))),
             legend = plotly_legend(y.pos = -.1)) 
  }
  return(p_layout)
  
}


plot_daily_returns <- function(
  plotly_obj, 
  trace_col,
  legend_group,
  yaxis = NULL
){
  "Plot daily returns evolution."
  
  if (is.null(yaxis)){
    p <- plotly_obj %>%
      add_trace(type = "scatter", 
                mode = "lines",
                marker = NULL,
                x = ~date,
                y = ~ret,
                name = "Daily returns", 
                yaxis = yaxis, 
                line = list(width = 1.7, 
                            color = trace_col,
                            dash = "dot"), 
                legendgroup = legend_group) %>%
      add_trace(type = "scatter", 
                mode = "lines",
                marker = NULL,
                x = c(~min(date), ~max(date)),
                y = c(0, 0), 
                yaxis = yaxis, 
                line = list(color = "black",
                            width = 2), 
                showlegend = F)
  }
  else{
    p <- plotly_obj %>%
      add_trace(type = "scatter", 
                mode = "lines",
                marker = NULL,
                x = ~date,
                y = ~ret,
                name = "Daily returns", 
                yaxis = yaxis, 
                line = list(width = 1.7, 
                            color = trace_col,
                            dash = "dot"), 
                legendgroup = legend_group) %>%
      add_trace(type = "scatter", 
                mode = "lines",
                marker = NULL,
                x = c(~min(date), ~max(date)),
                y = c(0, 0), 
                yaxis = yaxis, 
                line = list(color = "black",
                            width = 2), 
                showlegend = F)
  }
  
  return(p)
  
}

plot_cumulative_returns <- function(
  plotly_obj, 
  multiple = F, 
  title = "", 
  trace_col = evolution, 
  legend_group = NULL,
  yaxis = NULL, 
  with_legend = T
){
  "Plot cumulative returns evolution."

  if (multiple == F){
    p <- plotly_obj %>%
      add_trace(type = "scatter", 
                mode = "lines",
                marker = NULL,
                x = ~date,
                y = ~cr,
                name = "Cumulative returns", 
                yaxis = yaxis, 
                line = list(width = 1.7, 
                            color = trace_col,
                            dash = "dot"), 
                legendgroup = legend_group)
  }
  else{
    p <- plotly_obj %>%
      add_trace(type = "scatter", 
                mode = "lines",
                marker = NULL,
                x = ~date,
                y = ~cr,
                group = ~ticker,
                color = ~ticker, 
                name = ~ticker, 
                yaxis = yaxis, 
                line = list(width = 1.7,
                            dash = "dot"), 
                legendgroup = legend_group)
  }
   
  p <- p %>%
    add_trace(type = "scatter", 
              mode = "lines",
              marker = NULL,
              x = c(~min(date), ~max(date)),
              y = c(1, 1), 
              yaxis = yaxis, 
              line = list(color = "black",
                          width = 2), 
              showlegend = F) %>%
    layout(title = title,
           xaxis = list(title = ""),
           yaxis = list(title = ""), 
           legend = plotly_legend(), 
           showlegend = with_legend)
  
  return(p)
  
}

plot_price_evolution <- function(
  plotly_obj, 
  title, 
  legend_group = NULL, 
  ticker = NULL, 
  yaxis = NULL, 
  with_ma = F
){
  "Plot price evolution."
  
  if (is.null(ticker)){
    trace_name <- "Value (€)"
  }
  else{
    trace_name <- ticker
  }
  
  p <- plotly_obj %>%
    add_trace(type = "scatter", 
              mode = "lines",
              marker = NULL,
              x = ~date,
              y = ~value,
              name = trace_name,
              yaxis = yaxis, 
              line = list(color = evolution,
                          width = 1.7), 
              legendgroup = legend_group)
  
  if (with_ma == T){ p <- p %>% ma_chart(yaxis = yaxis) }
  
  return(p)
  
  
}

plot_evolution <- function(
  price_dat,
  cum_ret_dat = NULL, 
  daily_ret_dat = NULL, 
  ticker = NULL, 
  with_ma = F
){
  "Combine price evolution and cumulative/daily returns plots."
  
  if (is.null(daily_ret_dat)){
    
    last_ret <- cum_ret_dat %>%
      filter(date == max(date)) %>% 
      pull(cr)
    trace_col <- if_else(last_ret < 1, 
                        "red", 
                        "green")
    
    data <- merge(x = price_dat, 
                  y = cum_ret_dat, 
                  by = "date") 
    
    p <- plot_ly(data) %>%
      plot_price_evolution(title = "", 
                           legend_group = "one", 
                           ticker = ticker, 
                           with_ma = with_ma) %>%
      plot_cumulative_returns(trace_col = trace_col, 
                              title = "", 
                              yaxis = "y2", 
                              legend_group = "two")
    
  }
  if (is.null(cum_ret_dat)){
    
    last_ret <- daily_ret_dat %>%
      filter(date == max(date)) %>% 
      pull(ret)
    trace_col <- if_else(last_ret < 0,
                        "red", 
                        "green")
    
    data <- merge(x = price_dat, 
                  y = daily_ret_dat, 
                  by = "date") 
    
    p <- plot_ly(data) %>%
      plot_price_evolution(title = "", 
                           legend_group = "one",
                           ticker = ticker, 
                           with_ma = with_ma) %>%
      plot_daily_returns(trace_col = trace_col, 
                         title = "", 
                         yaxis = "y2", 
                         legend_group = "two")
    
  }
  
  p %>%
    layout(title = "",
           xaxis = list(rangeslider = list(visible = F), 
                        rangeselector = range_selector_period(y_pos = -0.1), 
                        title = ""),
           yaxis = list(domain = c(0.4, 1),
                        fixedrange = FALSE,
                        tickfont = list(color = evolution), 
                        title = list(text = "€",
                                     font = list(color = "#76787B"))), 
           yaxis2 = list(domain = c(0, 0.3),
                         fixedrange = FALSE, 
                         tickfont = list(color = macd), 
                         title = ""), 
           legend = plotly_legend())
  
}

plot_portfolio_composition <- function(assets_weights, mode = "val"){
  "Return a pie chart with the contribution of each asset to the portfolio
  based on their current value."
  
  if (mode == "invest"){
    gradient <- colorRampPalette(c("#D2F5E8", "#699B89"))
  }
  else{
    if (mode == "val"){
      gradient <- colorRampPalette(c("#E0D7E8", "#9A8AA9"))
    }
    else{ stop("mode takes the following values: 'val' or 'invest'.") }
  }
  
  numeric_cut <- cut(assets_weights %>% pull(all_of( paste("pct", mode, sep = "_") )), 
                     breaks = nrow(assets_weights)) %>%
    as.numeric()
  assets_weights <- assets_weights %>%
    mutate( colors = gradient(nrow(assets_weights))[numeric_cut] ) %>%
    mutate(asset = lapply(ticker, get_company_name))
  
  fig <- assets_weights %>%
    mutate(asset = lapply(ticker, get_company_name)) %>% 
    plot_ly(labels = ~ticker, 
            values = paste0("~", mode) %>% as.formula(), 
            type = "pie", 
            textposition = "inside",
            textinfo = "label+percent",
            insidetextfont = list(color = "black"),
            hoverinfo = "text",
            text = ~paste0("</br> Asset: ", asset, 
                           "</br> Invested: ", invest %>% format_number(), "€", 
                           "</br> Value: ", val %>% format_number(), "€"),
            marker = list(colors = ~ colors, 
                          line = list(color = "#FFFFFF", width = 1)),
            showlegend = FALSE)
  
  options <- list(showgrid = FALSE,
                  zeroline = FALSE, 
                  showticklabels = FALSE)
  fig %>%
    layout(title = "", 
           xaxis = options,
           yaxis = options)
}

# --- Indicators

plotly_layout <- function(plotly_obj, title.x = "", title.y, show_grid = T){
  "Customize plotly layout."
  
  p_layout <- plotly_obj %>%
    layout(
      
      xaxis = list( rangeslider = list(visible = F), 
                    title = list(text = title.x,
                                 font = list(color = "#76787B")), 
                    showgrid = show_grid ),
      
      yaxis = list( fixedrange = FALSE, 
                    title = list(text = title.y,
                                 font = list(color = "#76787B")), 
                    showgrid = show_grid )
      
    ) 
  
}

ma_chart <- function(plotly_obj, yaxis = NULL){
  "Add moving averages to an existing plotly chart."
  
  plotly_obj %>%
    add_trace(type = "scatter", 
              mode = "lines",
              marker = NULL,
              x = ~date,
              y = ~MA20,
              name = "MA20",
              line = list(color = short, 
                          width = 1.5),
              yaxis = yaxis, 
              hoverinfo = "none", 
              inherit = F) %>%
    add_trace(type = "scatter", 
              mode = "lines",
              marker = NULL,
              x = ~date,
              y = ~MA50,
              name = "MA50",
              line = list(color = medium,
                          width = 1.5), 
              yaxis = yaxis, 
              hoverinfo = "none", 
              inherit = F) %>%
    add_trace(type = "scatter", 
              mode = "lines",
              marker = NULL,
              x = ~date,
              y = ~MA100,
              name = "MA100",
              line = list(color = long, 
                          dash = "dot", 
                          width = 1.5), 
              yaxis = yaxis, 
              hoverinfo = "none", 
              inherit = F)
  
  
}

candlestick_chart <- function(plotly_obj, ticker){
  "Build plotly candlestick chart with moving averages for a given asset."
  
  p <- plotly_obj %>%
    add_trace(type = "candlestick",
              x = ~date,
              open = ~open, 
              high = ~high, 
              low = ~low, 
              close = ~close,
              name = ticker, 
              increasing = list(line = list(color = high,
                                            width = 1.5)), 
              decreasing = list(line = list(color = low,
                                            width = 1.5)), 
              inherit = F)
  
  return(p)
  
  
}

bbands_chart <- function(plotly_obj){
  "Build plotly chart whith prices, bollinger bands and %B."
  p <- plotly_obj %>%
    add_trace(type = "scatter", 
              mode = "lines",
              marker = NULL,
              x = ~date,
              y = ~dn, 
              name = "Bollinger Inf", 
              line = list(color = bbands,
                          width = .9), 
              inherit = F) %>%
    add_trace(type = "scatter", 
              mode = "lines",
              marker = NULL,
              x = ~date,
              y = ~up, 
              name = "Bollinger Sup", 
              line = list(color = bbands,
                          width = .9),
              inherit = F)
  
  return(p)
  
}

volume_chart <- function(data){
  "Visualize volume evolution given price direction."
  
  data %>% 
    plot_ly(x = ~date, 
            y = ~volume,
            type = "bar", 
            name = "Volume",
            color = ~direction,
            colors = c(low, high), 
            opacity = .6)
}

obv_chart <- function(plotly_obj){
  "Build plotly chart for OBV."
  
  p <- plotly_obj %>%
    add_trace(x = ~date, 
              y = ~OBV, 
              type = "scatter",
              mode = "lines", 
              marker = NULL, 
              name = "OBV", 
              line = list(color = obv,
                          width = 1.7)) 
  return(p)
  
}

macd_chart <- function(plotly_obj){
  "Build plotly chart for MACD and MACD signal."
  
  p <- plotly_obj %>%
    add_trace(type = "scatter", 
              mode = "lines",
              marker = NULL,
              x = ~date,
              y = ~MACD,
              name = "MACD",
              line = list(color = macd,
                          width = 1.3)) %>%
    add_trace(type = "scatter", 
              mode = "lines",
              marker = NULL,
              x = ~date,
              y = ~MACDSignal,
              name = "MACD Signal",
              line = list(color = long,
                          width = 1,
                          dash = "dot")) %>%
    add_trace(type = "bar", 
              x = ~date,
              y = ~MACDHist,
              name = "MACD Hist",
              marker = list(color = macdHist), 
              showlegend = F)
  
  return(p)
  
}

rsi_chart <- function(plotly_obj, trace_name = "RSI"){
  "Build plotly chart for RSI signal and bounds."
  
  lower <- if_else(trace_name == "RSI", 
                   30, 
                   20)
  upper <- if_else(trace_name == "RSI", 
                   70, 
                   80)
  bounds_trace_col <- if_else(trace_name == "RSI", 
                             "red", 
                             "black")
  
  lower_trace_name <- paste("Lower", trace_name)
  upper_trace_name <- paste("Upper", trace_name)
  
  p <- plotly_obj %>%
    add_trace(type = "scatter", 
              mode = "lines",
              marker = NULL,
              x = ~date,
              y = paste0("~", trace_name) %>% as.formula(),
              name = trace_name,
              line = list( color = if_else(trace_name == "RSI", 
                                           rsi,
                                           stoch_rsi), 
                           width = 1.2 )) %>%
    add_trace(type = "scatter", 
              mode = "lines",
              marker = NULL,
              x = c(~min(date), ~max(date)),
              y = c(upper, upper),
              name = upper_trace_name, 
              line = list(color = bounds_trace_col,
                          width = 0.5,
                          dash = "dot"), 
              showlegend = F) %>%
    add_trace(type = "scatter", 
              mode = "lines",
              marker = NULL,
              x = c(~min(date), ~max(date)),
              y = c(lower, lower),
              name = lower_trace_name, 
              line = list(color = bounds_trace_col,
                          width = 0.5,
                          dash = "dot"), 
              showlegend = F)
  return(p)
  
}

financialDataViz <- function(data, ticker, indicators = c("Volume", "MACD", "RSI")){
  "Combine multiple financial charts."
  
  plot_list <- list(
    "candlestick" = data %>% 
      plot_ly() %>%
      candlestick_chart(ticker = ticker) %>% 
      ma_chart() %>% 
      bbands_chart() %>%
      plotly_layout(title.y = "€")
  )
  
  if ("Volume" %in% indicators){
    plot_list[["volume"]] <- data %>%
      volume_chart() %>% 
      plotly_layout(title.y = "Volume")
  }
  
  if ("OBV" %in% indicators){
    plot_list[["OBV"]] <- data %>%
      plot_ly() %>% 
      obv_chart() %>% 
      plotly_layout(title.y = "") 
  }
  
  if ("MACD" %in% indicators){
    plot_list[["MACD"]] <- data %>%
      plot_ly() %>% 
      macd_chart() %>%
      plotly_layout(title.y = "", show_grid = T)
  }
  
  if ("RSI" %in% indicators){
    plot_list[["RSI"]] <- data %>%
      plot_ly() %>% 
      rsi_chart() %>%
      rsi_chart(trace_name = "stochRSI") %>% 
      plotly_layout(title.y = "", show_grid = T)
  }
  
  plot_heights <- list(
    "1" = c(.95), 
    "2" = c(.7, .25), 
    "3" = c(.55, .2, .2), 
    "4" = c(.5, .15, .15, .15), 
    "5" = c(.35, .15, .15, .15 , .15 )
  )
  n_plots <- length(plot_list)
  
  fig <- subplot(plot_list,
                 heights = plot_heights[[n_plots %>% as.character()]], 
                 nrows = n_plots,
                 shareX = TRUE, 
                 titleY = TRUE) %>% 
    layout(title = "", 
           legend = plotly_legend(y.pos = -.1))
  
  return(fig)
  
}

ic_alpha <- function(alpha, acf_res){
  "Confidence interval for ACF."
  
  ic <- qnorm((1 + (1 - alpha))/2)/sqrt(acf_res$n.used)
  return(ic)
  
}

plot_cor_mat <- function(cor_mat, p_mat = NULL){
  "Plotly visualization of correlation matrix."
  
  cor_plot <- ggcorrplot(cor_mat,
                         hc.order = TRUE, 
                         p.mat = p_mat, 
                         type = "lower",
                         outline.col = "white",
                         tl.cex = 10,
                         ggtheme = ggplot2::theme_minimal(),
                         colors = c("#E88787", "white", "#758ADA")) +
    theme(legend.position = "none", 
          axis.title = element_text(color = "#76787B"))
  
  ggplotly(cor_plot) %>%
    layout(xaxis = list(showgrid = FALSE),
           yaxis = list(showgrid = FALSE))
  
}

# Stock Recommender -------------------------------------------------------

## Compute all indicators --------------------------------------------------------------

add_indicators <- function(
  price_dat, 
  start_date, 
  indicators, 
  ema_short = 12, 
  ema_long = 26, 
  macd_signal = 9, 
  rsi_period = 14
){
  "Add MACD, RSI and stochRSI to price data."
  
  res <- price_dat %>% 
    filter(date >= start_date) 
  
  if ("MACD" %in% indicators){
    res <- res %>%
      add_macd(ema_short = ema_short, 
               ema_long = ema_long, 
               signal = macd_signal)
  }
  
  if ( "RSI" %in% indicators | "stochRSI" %in% indicators ){
    res <- res %>% 
      add_rsi(rsi_period = rsi_period)
  }
     
  if ("stochRSI" %in% indicators){
    res <- res %>%
      add_stochRsi()
  }
  
  return(res)
  
}

## Potential buying points ------------------------------------------------------------------

return_buying_points <- function(indicators_dat, indicators){
  "Return buying points based on multiple indicators (MACD, RSI, stochRSI)."
  
  buying_dat <- indicators_dat
  
  if ("MACD" %in% indicators){
    buying_dat <- buying_dat %>% get_macd_signals()
  }
  
  if ("RSI" %in% indicators){
    buying_dat <- buying_dat %>% get_rsi_signals()
  }
  
  if ("stochRSI" %in% indicators){
    buying_dat <- buying_dat %>% get_rsi_signals(rsi_type = "stochRSI")
  }
  
  buying_dat <- buying_dat %>%
    mutate( TotalBuy = select(., contains("Buy")) %>% rowSums() ) %>%
    filter(TotalBuy == length(indicators)) 
  
  return(buying_dat)
  
}

return_last_signal_point <- function(signal_points_dat){
  "Return the last buying point for a given stock."
  
  signal_points_dat %>% 
    filter( date == if_else(max(date) == today(), 
                            today(), 
                            today() - days(1)) )
}


## Potential selling points -----------------------------------------------------------------

return_selling_points <- function(indicators_dat, indicators){
  "Return selling points based on multiple indicators (MACD, RSI, both)."
  
  selling_dat <- indicators_dat
  
  if ("MACD" %in% indicators){
    selling_dat <- selling_dat %>% get_macd_signals()
  }
  
  if ("RSI" %in% indicators){
    selling_dat <- selling_dat %>% get_rsi_signals()
  }
  
  if ("stochRSI" %in% indicators){
    selling_dat <- selling_dat %>% get_rsi_signals(rsi_type = "stochRSI")
  }
  
  selling_dat <- selling_dat %>%
    mutate( TotalSell = select(., contains("Sell")) %>% rowSums() ) %>%
    filter(TotalSell == length(indicators)) 
  
  return(selling_dat)
  
}

add_returns <- function(selling_points_dat, assets_total_returns){
  "Add global returns for selling points."
  
  if(nrow(selling_points_dat) > 0){
    ticker <- selling_points_dat %>%
      pull(ticker) %>% 
      unique()
    
    tot_ret <- assets_total_returns[assets_total_returns$ticker == ticker, ]$tot_ret
    
    selling_points_dat %>% 
      mutate( returns = tot_ret )
  }

}

## Stock recommendation process -------------------------------------------------------

stock_recommender <- function(
  stock_data, 
  start_date, 
  action = "Buy", 
  ema_short = 12, 
  ema_long = 26, 
  macd_signal = 9, 
  rsi_period = 14, 
  indicators = c("MACD"),
  assets_total_returns
){
  "Build the recommendation system."
  process <- function(indicators_dat, ticker){
    
    if (action == "Buy"){
      
      dat <- indicators_dat %>%
        return_buying_points(indicators = indicators)
      dat <- dat %>% 
        return_last_signal_point()
    
      return(dat)
    }
    
    else{
      if (action == "Sell"){
        
        dat <- indicators_dat %>%
          return_selling_points(indicators = indicators) 
        
        if (nrow(dat) > 0){
          dat <- dat %>% 
            return_last_signal_point() %>% 
            add_returns(assets_total_returns = assets_total_returns)
          
          return(dat)
        }
        
      }
      
      else{
        stop("'action' must take one of the following values: 'Buy', 'Sell'.")
      }
      
    }
    
  }
  
  if (action == "Sell"){
    stock_data <- stock_data[my_tickers]
  }
  
  dat <- lapply(X = stock_data, 
                FUN = function(dat){
                  ticker <- dat %>% 
                    head(1) %>% 
                    pull(ticker)
                  dat %>%
                    add_indicators(start_date = start_date, 
                                   indicators = indicators,
                                   ema_short = ema_short, 
                                   ema_long = ema_long, 
                                   macd_signal = macd_signal, 
                                   rsi_period = rsi_period) %>%
                    process(ticker = ticker)
                }) %>% 
    bind_rows()
  
  if (ncol(dat) == 0){
    dat <- data.frame(date = as.Date(as.character()),
                      open = as.numeric(), 
                      high = as.numeric(), 
                      low = as.numeric(), 
                      close = as.numeric(),
                      volume = as.numeric(),
                      adjusted = as.numeric()) 
  }
  
  return(dat)
}

# UI -------------------------------------------------------

infoBox_dims <- function(
  box_height = "35px",
  icon_height = "45px", 
  icon_line_height = "35px"
){
  "Define dimensions for infoBox."
  
  dims <- paste0(".info-box {min-height: ",
                 box_height,
                 ";} .info-box-icon {height:",
                 icon_height,
                 "; line-height:", 
                 icon_line_height, 
                 ";} .info-box-content {padding-top: 0px; padding-bottom: 0px;}")
  return(dims)
} 


infoBox_last_price <- function(last_price){
  "Return infoBox for last price."
  
  infoBox(title = "Current price",
          value = last_price %>% format_number(),
          color = "light-blue", 
          icon = tags$i(class = "fas fa-euro-sign", 
                        style = "font-size: 20px"), 
          fill = F)
}

infoBox_last_value <- function(last_val){
  "Return infoBox for last value"
  
  infoBox(title = "Current value",
          value = paste(last_val %>% round(0), "€"), 
          color = "purple", 
          icon = tags$i(class = "fas fa-money-check", 
                        style = "font-size: 20px"), 
          fill = F)
}

infoBox_total_invest <- function(total_invest){
  "Return infoBox for total amount invested."
  
  infoBox(title = "Total invested",
          value = paste(total_invest %>% round(0), "€"), 
          color = "light-blue", 
          icon = tags$i(class = "fas fa-money-check", 
                        style = "font-size: 20px"), 
          fill = F)
  
}

infoBox_num_shares <- function(num_shares){
  "Return infoBox for number of shares."
  
  infoBox(title = "Number of shares",
          value = num_shares, 
          color = "aqua", 
          icon = tags$i(class = "fas fa-wallet", 
                        style = "font-size: 20px"), 
          fill = F)
}


infoBox_last_cumret <- function(last_cr){
  "Return infoBox for current cumulative returns."
  
  if (last_cr < 0){
    ib <- infoBox(title = "Cumulative returns", 
                  value = paste("-", abs(last_cr)), 
                  icon = tags$i(class = "fas fa-percent", 
                                style="font-size: 20px"), 
                  color = "red", 
                  fill = F)
  }
  else{
    ib <- infoBox(title = "Cumulative returns", 
                  value = paste("+", last_cr), 
                  icon = tags$i(class = "fas fa-percent", 
                                style="font-size: 20px"), 
                  color = "green", 
                  fill = F)
  }
  return(ib)
}

infoBox_asset_tot_ret <- function(asset, type = "best"){
  "Return infoBox to display best asset's cumulative returns."
  
  val <- ifelse(asset$pct_tot_ret > 0, 
                paste("+", asset$pct_tot_ret), 
                ifelse(asset$pct_tot_ret == 0, 
                       0, 
                       paste("-", abs(asset$pct_tot_ret))))
  
  if (type == "best"){
    icon <- tags$i(class = "fas fa-thumbs-up", 
                   style="font-size: 20px")
    color <- "green"
  }
  if (type == "worst"){
    icon <- tags$i(class = "fas fa-thumbs-down", 
                   style="font-size: 20px")
    color <- "red"
  }
  
  infoBox(title = asset$asset, 
          value = paste0(val, "%"),  
          icon = icon, 
          color = color, 
          fill = F)
  
}

infoBox_sharpe <- function(sr){
  "Return infoBox for Sharpe ratio."
  
  color <- if_else(sr < 0,
                   "red", 
                   "green")
  ib <- infoBox(title = "Sharpe ratio", 
                value = round(sr, 2), 
                icon = tags$i(class = "fas fa-calculator", 
                              style="font-size: 20px"), 
                color = color, 
                fill = F)
  return(ib)
}

infoBox_avg_cor <- function(avg_cor){
  "Return infoBox for average correlation."
  
  ib <- infoBox(title = "Average correlation", 
                value = round(avg_cor, 2), 
                icon = tags$i(class = "fas fa-chart-line", 
                              style="font-size: 20px"), 
                color = "light-blue", 
                fill = F)
  return(ib)
  
}

asset_inputs <- function(
  asset, 
  val = 1, 
  buying_date = today() - months(6)
){
  "Build shiny inputs for number of shares and buying date."
  
  numInputId <- paste("num_shares", get_ticker(asset), sep = "_")
  dateInputId <- paste("buying_date", get_ticker(asset), sep = "_")
  
  fluidRow(
    style = "height:80px;", 
    column(width = 6, 
           numericInput(numInputId,
                        label = h5("Number of shares"), 
                        min = 0, 
                        value = val)), 
    column(width = 6,
           airDatepickerInput(
             inputId = dateInputId,
             value = buying_date,
             minDate = date_init, 
             maxDate = today(),
             label = h5("Buying date"),
             placeholder = "",
             multiple = F, 
             clearButton = F))
  )
}

picker_inputs_font_weight <- function(){
  font_weights <- rep(x = "font-weight: plain;", nrow(symbols))
  font_weights[my_tickers_ix] <- "font-weight: bold;"
  return(font_weights)
}
