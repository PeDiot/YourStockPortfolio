loader_css <- "
#spinner {
display: inline-block;
border: 3px solid #f3f3f3;
border-top: 3px solid #3498db;
border-radius: 50%;
width: 40px;
height: 40px;
animation: spin 1s ease-in-out infinite;
}
@keyframes spin {
0% { transform: rotate(0deg); }
100% { transform: rotate(360deg); }
}"


StockRecoPromise <- worker$run_job(
  
  id = "StockRecoPromise", 
  
  fun = function(action, start_date, indicators) {
    stock_recommender(stock_data = yf_data, 
                      action = action,       
                      start_date = start_date, 
                      indicators = indicators, 
                      assets_total_returns = assets_total_returns) 
  },
  
  args_reactive = reactive({
    input$action
    input$recommendation_start_date
    input$reco_indicators
  })
  
)

output$loader <- renderUI({
  
  task <- StockRecoPromise()
  
  if (!task$resolved) {
    div(
      div(class = "loader-text", "Your recommendations are on the way..."),
      div(id = "spinner")
    )
  }
  
})

task <- StockRecoPromise()

if (task$resolved){
  
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
  options = list(pageLength = 5,
                 lengthMenu = c(5, 10, 20)) 
  )

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
          plot_cumulative_returns(multiple = T, with_legend = F)  
      }
    }
    
  })
  
}