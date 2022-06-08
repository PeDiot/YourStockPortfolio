server <- function(input, output, session) {
  

# User Authentification ---------------------------------------------------
res_auth <- secure_server(
  check_credentials = check_credentials(credentials)
)
  
output$auth_output <- renderPrint({
  reactiveValuesToList(res_auth)
})
  
# home page --------------------------------------------------------------
  
## home image --------------------------------------------------------------
  output$home_img <- renderImage({
    list(src = "home_img.png", 
         width = 300,
         height = 300)
    
  }, deleteFile = F)
  
# portfolio --------------------------------------------------------------
      
    assets_value_list_wid <- compute_assets_value(data = yf_data, num_shares = my_num_shares) 
    
    my_assets_value <- my_tickers %>%
      lapply(FUN = query_assets_since_buying_date, 
             assets_dat = assets_value_list_wid[my_tickers],
             buying_dates = my_buying_dates) %>%
      bind_rows()
    
## portfolio value --------------------------------------------------------------
    port_value <- get_portfolio_value(my_assets_value)
    port_ma <- port_value %>%
      mutate(close = value) %>%
      add_moving_avg(window = 20) %>%
      add_moving_avg(window = 50) %>%
      add_moving_avg(window = 100)
    
    output$port_last_val <- renderInfoBox({
      val <- get_current_value(data = port_value) 
      infoBox_last_value(last_val = val)
      
    })
    
## portfolio returns --------------------------------------------------------------
    my_assets_weights <- my_assets_value %>%
      bind_rows() %>%
      calculate_assets_weights() %>% 
      select(c(ticker, value, wts, pct))
    
    my_assets_returns <- calculate_multiple_assets_returns(
      assets_value_list_wid,
      my_tickers
    )
    
    weighted_returns <- compute_weighted_returns(ret_data = my_assets_returns, 
                                                 wts_dat = my_assets_weights)
    
    port_weighted_ret <- weighted_returns %>%
      group_by(date) %>%
      summarise(ret = sum(wt_return))
    
    port_cumulative_ret <- port_weighted_ret %>%
      compute_cumulative_returns()
    
## data viz --------------------------------------------------------------
    output$portfolio_evolution <- renderPlotly({
      plot_evolution(price_dat = port_ma, 
                     cum_ret_dat = port_cumulative_ret, 
                     with_ma = T) 
    })
    
    max_date <- max(my_buying_dates)
    output$port_last_cumret <- renderInfoBox({
      last_cumret <- get_current_cumret(port_cumulative_ret)
      infoBox_last_cumret(last_cumret)
    })
    
## portfolio composition --------------------------------------------------------------
    
### best and worst assets --------------------------------------------------------------
    assets_cumret <- weighted_returns %>%
      compute_cumulative_returns(all = F, weighted = T)
    
    best_asset <- get_best_asset(assets_cumret) 
    worst_asset <- get_worst_asset(assets_cumret) 
    
### data viz --------------------------------------------------------------
    output$portfolio_composition <- renderPlotly({
      my_assets_weights %>%
        plot_portfolio_composition()
    })
    
    output$best_asset_cumret <- renderInfoBox({
      infoBox_asset_cumret(best_asset, type = "best")
    })
    
    output$worst_asset_cumret <- renderInfoBox({
      infoBox_asset_cumret(worst_asset, type = "worst")
    })
    
## correlations -------------------------------------------------------------- 
    
### calculate correlations btw assets returns --------------------------------------------------------------
    
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
    
### portfolio average correlation --------------------------------------------------------------
    avg_cor <- calculate_avg_cor(cor_mat, my_assets_weights$wts)
    
### portfolio sharpe ratio --------------------------------------------------------------
    sr <- calculate_sharpeRatio(port_weighted_ret$ret)
    
### data viz --------------------------------------------------------------
    
    output$cor_mat <- renderPlotly({
      plot_cor_mat(cor_mat, p_mat)
    })
    
    output$sharpeRatio <- renderInfoBox({
      infoBox_sharpe(sr)
    })
    
    output$avg_cor <- renderInfoBox({
      infoBox_avg_cor(avg_cor)
    })
    
## portfolio infos --------------------------------------------------------------
    
    port_info <- format_portfolio_info(my_tickers, my_buying_dates, my_num_shares)
    output$port_info <- renderDataTable( { port_info },
                                         options = list(pageLength = 10,
                                                        lengthMenu = c(10, 25, 50, 100)) )
    
    
## portfolio data table --------------------------------------------------------------
    port_dat <- merge(x = port_value, 
                      y = port_cumulative_ret, 
                      by = "date") %>%
      format_table_numbers() %>% 
      arrange(desc(date)) %>%
      rename(Date = date, 
             Value = value, 
             `Daily Weighted Returns` = ret, 
             `Cumulative Returns` = cr) %>%
      rename_at( vars( everything() ), str_to_title )
    output$port_data <- renderDataTable( {port_dat}, 
                                         options = list(pageLength = 10,
                                                        lengthMenu = c(10, 25, 50, 100)) )
      
# financial indicators per asset --------------------------------------------------------------
  
    assets_value_list <- compute_assets_value(data = yf_data, 
                                              num_shares = my_num_shares)        
  
## start date financial analysis --------------------------------------------------------------
  
    observeEvent(
      input$ticker_fin_analysis, 
      {
        output$start_date_fin_analysis_out <- renderUI({
          
          if (input$ticker_fin_analysis %in% my_tickers){
            date <- today() - months(3)
            if (my_buying_dates[input$ticker_fin_analysis] < date){
              date <- my_buying_dates[input$ticker_fin_analysis] 
            }
          }
          else{
            date <- today() %m-% months(6)
          }
          
          airDatepickerInput(
            inputId = "start_date_fin_analysis",
            label = h5("How far back do you want to go?", 
                       style = "color:#76787B;"),
            value = date,
            minDate = date_init, 
            maxDate = today() - days(35),
            width = "200px",
            placeholder = "",
            multiple = F, 
            clearButton = F)
          
        })
        
      }
    )
  
  observeEvent(
    
    c(input$ticker_fin_analysis, 
      input$start_date_fin_analysis),
    {
      
      req(input$start_date_fin_analysis)
      req(input$ticker_fin_analysis)
      
## asset data with indicators --------------------------------------------------------------
      prices <- assets_value_list[[input$ticker_fin_analysis]] %>%
        filter( date >= input$start_date_fin_analysis) %>%
        add_moving_avg(window = 20) %>%
        add_moving_avg(window = 50) %>%
        add_moving_avg(window = 100) %>%
        add_macd() %>%
        add_rsi()
      
## asset returns --------------------------------------------------------------
      daily_ret <- prices %>%
        compute_daily_returns() 
      asset_cumret <- daily_ret %>%
        compute_cumulative_returns(all = F)
      
## asset global evolution --------------------------------------------------------------
      output$asset_evolution <- renderPlotly({
        plot_evolution(price_dat = prices %>%
                         dplyr::select(c(date, close)) %>%
                         rename(value = close), 
                       cum_ret_dat = asset_cumret, 
                       ticker = input$ticker_fin_analysis)
      })
      
## asset last price --------------------------------------------------------------
      output$asset_last_price <- renderInfoBox({
        val <- get_current_price(data = prices)  
        infoBox_last_price(last_price = val)
        
      })
      
## asset number of shares --------------------------------------------------------------
      output$asset_num_shares <- renderInfoBox({
        if (input$ticker_fin_analysis %in% my_tickers){
          num_shares <- my_num_shares[input$ticker_fin_analysis]
        }
        else{
          num_shares <- 0
        }
        infoBox_num_shares(num_shares)
      })
      
      
## asset last cumulative returns --------------------------------------------------------------
      output$asset_last_cumret <- renderInfoBox({
        last_cumret <- get_current_cumret(asset_cumret)
        infoBox_last_cumret(last_cumret)
      })
      
## candlestick with MAs --------------------------------------------------------------
      output$candlestick_plot <- renderPlotly({
        prices %>%
          candlestick_chart(ticker = input$ticker_fin_analysis) 
      })
      
## bollinger bands --------------------------------------------------------------
      bbands_dat <- calculate_bbands(price_data = prices)
      output$bbands_plot <- renderPlotly({
        bbands_chart(bbands_dat, input$ticker_fin_analysis)
      })
      
## MACD --------------------------------------------------------------
      output$macd_plot <- renderPlotly({
        prices %>%
          macd_chart(ticker = input$ticker_fin_analysis) 
      })
      
## RSI --------------------------------------------------------------
      output$rsi_plot <- renderPlotly({
        prices %>%
          rsi_chart(ticker = input$ticker_fin_analysis) 
      })
      
## On-Balance Volume --------------------------------------------------------------
      output$obv_plot <- renderPlotly({
        prices %>%
          add_obv() %>%
          add_price_direction() %>%
          obv_chart(ticker = input$ticker_fin_analysis)
      })
      
      
    }
  )
          
# data --------------------------------------------------------------

## start date data --------------------------------------------------------------
  
    observeEvent(
      input$ticker_dat,
      {
        output$start_date_dat_out <- renderUI({
          
          if (input$ticker_dat %in% my_tickers){
            date <- today() - days(35)
            if (my_buying_dates[input$ticker_dat] < date){
              date <- my_buying_dates[input$ticker_dat] 
            }
          }
          else{
            date <- today() %m-% months(6)
          }
          
          airDatepickerInput(
            inputId = "start_date_dat",
            label = h5("How far back do you want to go?", 
                       style = "color:#76787B;"),
            value = date,
            minDate = date_init, 
            maxDate = today(),
            width = "200px", 
            placeholder = "",
            multiple = F, 
            clearButton = F)
          
        })
      }
    )
  
    observeEvent(
      
      c(input$ticker_dat, 
        input$start_date_dat),
      {
        
        req(input$start_date_dat)
        req(input$ticker_dat)
        
## yf data --------------------------------------------------------------
        dat <- assets_value_list[[input$ticker_dat]] %>%
          filter(date >= input$start_date_dat) %>%
          select(-c(n_shares, value)) 
        
## returns --------------------------------------------------------------
        ret_dat <- dat %>%
          compute_daily_returns() %>%
          compute_cumulative_returns(all = F) %>%
          select(c(ticker, 
                   date, 
                   ret,
                   cr)) %>%
          format_table_numbers() %>% 
          rename(`Daily Returns` = ret, 
                 `Cumulative Returns` = cr)
        
## indicators --------------------------------------------------------------      
        indicators_dat <- dat %>% 
          add_moving_avg(window = 20) %>%
          add_moving_avg(window = 50) %>%
          add_moving_avg(window = 100) %>%
          add_macd() %>%
          add_rsi() %>%
          select(c(ticker, 
                   date,
                   close, 
                   MA20:RSI)) %>%
          format_table_numbers() 
        
## data tables --------------------------------------------------------------
        output$data <- renderDataTable( { 
          dat %>%
            format_table_numbers() %>% 
            arrange(desc(date)) %>%
            rename_at( vars( everything() ), str_to_title )
          }, 
          options = list(pageLength = 10,
                         lengthMenu = c(10, 25, 50, 100)) 
          )
        
        output$ret_data <- renderDataTable( { 
          ret_dat %>% 
            arrange(desc(date)) %>%
            rename_at( vars( everything() ), str_to_title )
          }, 
          options = list(pageLength = 10,
                         lengthMenu = c(10, 25, 50, 100)) 
          )
        
        output$indicators_data <- renderDataTable( { 
          indicators_dat %>% 
            arrange(desc(date)) %>%
            rename_at( vars( ticker:close ), str_to_title )
          }, 
          options = list(pageLength = 10,
                         lengthMenu = c(10, 25, 50, 100)) 
          )
        
## download -------------------------------------------------------------- 
        output$download <- downloadHandler(
          filename <- function() {
            paste0(input$ticker_dat,
                   "_", 
                   input$start_date_dat, 
                   "_", 
                   today(), 
                   ".xlsx")
          },
          content <- function(file) {
            write_xlsx(x = list("Yahoo Finance Data" = dat, 
                                "Returns" = ret_dat, 
                                "Financial Indicators" = indicators_dat), 
                       path = file)
          }
        )
        
      }
      
    )
        
# stock recommender --------------------------------------------------------------
      
      observeEvent(
        c(input$indicator, 
          input$recommendation_start_date, 
          input$action), 
        
        {
          
          req(input$indicator)
          req(input$recommendation_start_date)
          req(input$action)
          
## compute recommendations --------------------------------------------------------------
          if (length(input$indicator) == 2){
            criterion <- "MACD+RSI"
          }
          else{
            criterion <- input$indicator
          }
          
          recommendation <- stock_recommender(stock_data = yf_data, 
                                              action = input$action,       
                                              start_date = input$recommendation_start_date, 
                                              criterion = criterion, 
                                              num_shares = my_num_shares, 
                                              buying_dates = my_buying_dates) 
          recommended_tickers <- recommendation$ticker 
          
## recommended stocks table --------------------------------------------------------------
          output$recommendation_data <- renderDataTable( { 
            new_tickers <- lapply(recommended_tickers, 
                                  FUN = function(x){
                                    sym <- symbols %>% filter(tickers == x) %>% rownames()
                                    paste0(sym, " (", x, ")")
                                  }) %>% unlist()
            recommendation %>% 
              mutate(ticker = new_tickers) %>% 
              format_table_numbers() %>% 
              select(-date) %>% 
              rename_at( vars( !contains("MACD") & !contains("RSI") ), str_to_title )
          }, 
          options = list(pageLength = 3,
                         lengthMenu = c(1, 3, 5)) 
          )
          
## recommendation table title --------------------------------------------------------------
          output$recommendation_tab_title <- renderUI({
            if (input$action == "Buy"){
              title <- "Interesting stocks to buy as of"
            }
            else{
              title <- "Stocks you could sell for a profit as of"
            }
            h4(strong(paste(title, format(today(), "%B %d, %Y"))), 
               align = "center", 
               style = "color:#76787B;")
          })
          
## cumulative returns chart --------------------------------------------------------------
          
          output$recommendation_chart <- renderPlotly({
            
            if (length(recommended_tickers) == 0){
              plotly_empty()
            }
            
            else{
              
              cumrets <- yf_data[recommended_tickers] %>%
                bind_rows() %>% 
                filter(date >= input$recommendation_start_date) %>% 
                compute_daily_returns() %>%
                compute_cumulative_returns(all = F) %>%
                mutate( ticker = lapply(X = ticker, FUN = get_company_name) %>%
                          unlist() )
              
              if (length(recommended_tickers) == 1){
                cumrets %>%
                  plot_ly() %>%
                  plot_cumulative_returns(legend_group = NULL) %>% 
                  layout(showlegend = F)
              }
              else{
                cumrets %>%
                  plot_ly() %>%
                  plot_cumulative_returns(multiple = T,
                                          legend_group = NULL) %>% 
                  layout(showlegend = F)
              }
            }
            
          })
          
          
        }
      )
    
}
      
      