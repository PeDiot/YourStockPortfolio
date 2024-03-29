server <- function(input, output, session) {

# User Authentification ---------------------------------------------------
  # res_auth <- secure_server(check_credentials = check_credentials(credentials))
    
  # output$auth_output <- renderPrint({ reactiveValuesToList(res_auth) })
  
# home page --------------------------------------------------------------
  
## home image --------------------------------------------------------------
  output$home_img <- renderImage({
    list(src = "home_img.png", 
         width = 300,
         height = 300)
    
  }, deleteFile = F)
  
# portfolio --------------------------------------------------------------
    my_assets_value_list <- compute_assets_value(data = yf_data[my_tickers])
    my_assets_value <- my_assets_value_list %>% bind_rows()
    
## portfolio value --------------------------------------------------------------
    port_value <- get_portfolio_value(my_assets_value)
    port_ma <- port_value %>%
      mutate(close = value) %>%
      add_moving_avg(window = 20) %>%
      add_moving_avg(window = 50) %>%
      add_moving_avg(window = 100)
    
## portfolio returns --------------------------------------------------------------
    my_assets_weights <- calculate_assets_weights(my_tickers_tx)
    
    my_assets_returns <- my_assets_value_list %>% 
      calculate_multiple_assets_returns(tickers = my_tickers_tx) %>%
      weight_returns_per_asset() 
    
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
    
    output$total_invest <- renderInfoBox({
      val <- calculate_total_invested(my_tickers_tx)
      infoBox_total_invest(val)
    })
    
    output$port_last_val <- renderInfoBox({
      val <- port_value %>%
        filter(date == today()) %>%
        pull(value)
      infoBox_last_value(last_val = val)
      
    })
    
    output$port_last_cumret <- renderInfoBox({
      last_cumret <- get_current_cumret(port_cumulative_ret)
      infoBox_last_cumret(last_cumret)
    })
    
## portfolio composition --------------------------------------------------------------
    
### best and worst assets --------------------------------------------------------------
    
    assets_total_returns <- get_all_assets_total_returns(my_tickers_tx) %>%
      filter(ticker %in% my_active_tickers)
    
    best_asset <- get_best_asset(assets_total_returns) 
    worst_asset <- get_worst_asset(assets_total_returns) 
    
### data viz --------------------------------------------------------------
    output$portfolio_composition_val <- renderPlotly({
      my_assets_weights %>%
        plot_portfolio_composition()
    })
    
    output$portfolio_composition_invest <- renderPlotly({
      my_assets_weights %>%
        plot_portfolio_composition(mode = "invest")
    })
    
    output$best_asset <- renderInfoBox({
      infoBox_asset_tot_ret(best_asset, type = "best")
    })
    
    output$worst_asset <- renderInfoBox({
      infoBox_asset_tot_ret(worst_asset, type = "worst")
    })
    
    
## cumulative returns -------------------------------------------------------------- 
    
    cumulative_returns <- weighted_returns %>% 
      compute_cumulative_returns()
    
    output$cumrets_plot <- renderPlotly({
      cumulative_returns %>% 
        plot_ly() %>% 
        plot_cumulative_returns(multiple = T)
    })
    
## correlations -------------------------------------------------------------- 
    
### calculate correlations btw assets returns --------------------------------------------------------------
    
    my_assets_returns_6m <- calculate_multiple_assets_returns(
      yf_data[my_active_tickers],
      my_active_tickers,
      start_date = today() - months(12)
    ) %>%
      pivot_wider(id_cols = date, 
                  names_from = ticker, 
                  values_from = ret)
    colnames(my_assets_returns_6m) <- c("date", names(my_active_tickers))
    
    cor_mat <- my_assets_returns_6m %>% 
      select(-date) %>%
      cor(use = "complete.obs")
    
### portfolio average correlation --------------------------------------------------------------
    avg_cor <- calculate_avg_cor( cor_mat = cor_mat, 
                                  weights_data = my_assets_weights %>%
                                    filter(ticker %in% my_active_tickers),
                                  tickers = my_active_tickers )
    
### portfolio sharpe ratio --------------------------------------------------------------
    sr <- calculate_sharpeRatio(port_weighted_ret$ret)
    
### data viz --------------------------------------------------------------
    
    output$cor_mat <- renderPlotly({
      plot_cor_mat(cor_mat)
    })
    
    output$sharpeRatio <- renderInfoBox({
      infoBox_sharpe(sr)
    })
    
    output$avg_cor <- renderInfoBox({
      infoBox_avg_cor(avg_cor)
    })
    
## transactions --------------------------------------------------------------
    
    tx_table <- create_tx_table(my_tickers_tx, my_buy_dates, my_sale_dates, my_num_shares)
    output$tx_table <- renderDataTable( { tx_table },
                                         options = list(pageLength = 10,
                                                        lengthMenu = c(10, 25, 50, 100)) )
      
# financial indicators per asset --------------------------------------------------------------
  
  observeEvent(
    
    c(input$ticker_fin_analysis, 
      input$start_date_fin_analysis,
      input$indicator),
    {
      
      req(input$start_date_fin_analysis)
      req(input$ticker_fin_analysis)
      req(input$indicator)
      
## asset data with indicators --------------------------------------------------------------
      prices <- yf_data[[input$ticker_fin_analysis]] %>%
        filter( date >= input$start_date_fin_analysis) %>%
        add_moving_avg(window = 20) %>%
        add_moving_avg(window = 50) %>%
        add_moving_avg(window = 100) %>%
        add_macd() %>%
        add_rsi() %>% 
        add_stochRsi() %>% 
        add_obv() %>% 
        add_price_direction()
      
      bbands_dat <- calculate_bbands(price_data = prices)
      prices <- prices %>%
        add_bbands(bbands_data = bbands_dat)
      
## asset returns --------------------------------------------------------------
      daily_ret <- prices %>%
        compute_daily_returns() 
      asset_cumret <- daily_ret %>%
        compute_cumulative_returns(all = F)
      
## asset global evolution --------------------------------------------------------------
      output$asset_evolution <- renderPlotly({
        plot_evolution(price_dat = prices %>% 
                         rename(value = close), 
                       cum_ret_dat = asset_cumret, 
                       ticker = input$ticker_fin_analysis, 
                       with_ma = T)
      })
      
## asset last price --------------------------------------------------------------
      output$asset_last_price <- renderInfoBox({
        val <- yf_data[[input$ticker_fin_analysis]] %>% 
          filter(date == today()) %>% 
          pull(close) 
        infoBox_last_price(last_price = val)
        
      })
      
## asset last cumulative returns --------------------------------------------------------------
      output$asset_last_cumret <- renderInfoBox({
        last_cumret <- get_current_cumret(asset_cumret)
        infoBox_last_cumret(last_cumret)
      })
      
## indicators data viz --------------------------------------------------------------
      output$financial_data_viz <- renderPlotly({
        financialDataViz(prices, input$ticker_fin_analysis, input$indicator) 
      })
      
    }
  )
          
# data --------------------------------------------------------------
    
    observeEvent(
      
      c(input$ticker_dat, 
        input$start_date_dat),
      {
        
        req(input$start_date_dat)
        req(input$ticker_dat)
        
## yf data --------------------------------------------------------------
        dat <- yf_data[[input$ticker_dat]] %>%
          filter(date >= input$start_date_dat)
        
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
       input$run, 
        
        {
         
## compute recommendations --------------------------------------------------------------
          recommendation <- stock_recommender(stock_data = yf_data, 
                                              action = input$action,       
                                              start_date = input$recommendation_start_date, 
                                              indicators = input$reco_indicators, 
                                              assets_total_returns = assets_total_returns) 
          
          recommended_tickers <- recommendation$ticker 
          
## recommended stocks table --------------------------------------------------------------
          output$recommendation_data <- renderDataTable( { 
            
            new_tickers <- lapply(recommended_tickers, 
                                  FUN = function(x){
                                    sym <- symbols %>% filter(tickers == x) %>% rownames()
                                    paste0(sym, " (", x, ")")
                                  }) %>% unlist()
            
            recommendation %>% 
              select( -c( contains("Buy") | contains("Sell") ) ) %>% 
              mutate(ticker = new_tickers) %>% 
              format_table_numbers() %>% 
              select(-date) %>% 
              rename_at( vars( !contains("MA") & !contains("RSI") ), str_to_title )
            
            }, 
          
            options = list( pageLength = 5, lengthMenu = c(5, 10, 20) ) 
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
          
          output$cumrets_title <- renderUI({
            h4( strong("Cumulative returns"), align = "center", style = "color:#76787B;" )
          })
          
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
                  plot_cumulative_returns(multiple = T, with_legend = F)  
              }
            }
            
          })
          
          
        }
      )
    
}