ui <- fluidPage(
  
  useShinydashboard(), 
  html_dependency_awesome(),
  
  tags$head(
    tags$style(infoBox_dims())
  ),
 
  navbarPage("Your Stock Portfolio", 
             theme = shinytheme("lumen"), 
             

# home --------------------------------------------------------------------
             
              tabPanel(title = "", 
                      icon = icon("home"), 
                      h1("Welcolme To Your Stock Portfolio!", align = "center"),
                      br(), 
                      br(), 
                      br(), 
                      br(), 
                      br(), 
                      br(), 
                      column(width = 12, 
                        imageOutput("home_img"), 
                        align = "center"), 
                      br(),
                      br(), 
                      br(), 
                      br(), 
                      br(), 
                      br(), 
                      br(), 
                      br(), 
                      br(), 
                      br(),
                      br(), 
                      br(), 
                      hr(), 
                      br(), 
                      br(), 
                      br(), 
                      br(), 
                      br(), 
                      br(), 
                      hr(), 
                      h4(strong("About the app")),
                      p(style="text-align: justify; font-size = 25px",
                      "This stock portfolio monitoring tool will help you ", 
                      strong("boost your returns."),  
                      "The app provides multiple ", 
                      strong("data visualizations "),
                      "and ", 
                      strong("financial analysis."), 
                      " The input data comes from the ", 
                      a(href = "https://fr.finance.yahoo.com/",
                        target = "_blank",
                        "Yahoo Finance"),
                      " website. 
                                Feel free to modify the source code in order to select your own assets. 
                                Go to ",
                      a(href = "https://github.com/PeDiot/StockPortfolio",
                        target = "_blank",
                        icon("github")),
                      " to find more details on the project."), 
                      hr()), 


# portfolio value ---------------------------------------------------------

             
              tabPanel("Portfolio Value", 
                      fluid = TRUE, 
                      icon = icon("euro"), 
                      
                      sidebarLayout(
                        
## inputs ---------------------------------------------------------
                        sidebarPanel( 
                          
                          width = 3, 
                          
                          h4(strong("Your Portfolio"),
                             style = "color:#76787B;"), 
                          hr(),
                          
                          tags$head(tags$style("h5 {color:#76787B;}")),
                          h5(strong("Stocks")), 
                          HTML("<ul><li>Coinbase (Lydia)</li><li>Amazon (Lydia)</li><li>Meta Platforms (Lydia)</li><li>Nvidia (Lydia)</li></ul>"),
                          br(), 
                          h5(strong("Cryptocurrencies")), 
                          HTML("<ul><li>Bitcoin (Lydia & Bitstack)</li><li>Ethereum (Binance)</li><li>Polygon MATIC (Metamask)</li><li>Decentraland MANA (Lydia)</li></ul>"),
      
                        ), 
                        
                        mainPanel(
                          
                          tabsetPanel(
                            
## portfolio overview ---------------------------------------------------------
                            tabPanel("Overview",
                                     br(), 
                                     br(),
                                     fluidRow(column(width = 2), 
                                              infoBoxOutput("port_last_val"),
                                              column(width = 1), 
                                              infoBoxOutput("port_last_cumret")), 
                                     hr(), 
                                     div(plotlyOutput("portfolio_evolution", 
                                                      height = 620, 
                                                      width = 900),
                                         align = "center")),
                            
## portfolio composition ---------------------------------------------------------
                              tabPanel("Composition",
                                     br(), 
                                     h4(strong("Contribution of each asset to the portfolio value"), 
                                        align = "center", 
                                        style = "color:#76787B;"),
                                     div(plotlyOutput("portfolio_composition", 
                                                      height = 500, 
                                                      width = 800), 
                                         align = "center"), 
                                     br(),
                                     hr(), 
                                     h4(strong("Your best and worst assets"), 
                                        align = "center", 
                                        style = "color:#76787B;"),
                                     br(), 
                                     fluidRow(column(width = 2), 
                                              infoBoxOutput("best_asset_cumret"),
                                              column(width = 1), 
                                              infoBoxOutput("worst_asset_cumret"))), 

## correlations ---------------------------------------------------------

                              tabPanel("Correlations", 
                                       br(), 
                                       h4(strong("Correlations between assets' returns"), 
                                          align = "center", 
                                          style = "color:#76787B;"), 
                                       br(), 
                                       div(plotlyOutput("cor_mat", 
                                                        height = 500, 
                                                        width = 800), 
                                           align = "center"), 
                                       br(), 
                                       hr(), 
                                       fluidRow(column(width = 2), 
                                                infoBoxOutput("avg_cor"),
                                                column(width = 1), 
                                                infoBoxOutput("sharpeRatio")), 
                                       br(),
                                       p(icon(name = "book"), 
                                         " The average correlation coefficient is derived based on Tierens and Anadu's research, see ", 
                                         a(href = "http://www.nematrian.com/MeasuringAverageStockCorrelation",
                                           target = "_blank",
                                           "here"), 
                                         " for more details.")),

## portfolio infos ---------------------------------------------------------
                              tabPanel("Infos", 
                                       br(),
                                       div(dataTableOutput(outputId = "port_info"), 
                                           align = "center")), 

## portfolio data ---------------------------------------------------------
                              tabPanel("Data", 
                                       br(),
                                       div(dataTableOutput(outputId = "port_data"), 
                                           align = "center"))

                          )
                          
                        )
                        
                      )
                      
             ),
             
# financial analysis ---------------------------------------------------------
             
             tabPanel("Financial Analysis", 
                      fluid = TRUE, 
                      icon = icon("chart-line"), 
                      
                      sidebarLayout(
                        
                        ## inputs ---------------------------------------------------------
                        sidebarPanel(
                          width = 3, 
                          h4(strong("Your Financial Indicators"), 
                             style = "color:#76787B;"), 
                          hr(), 
                          br(), 
                          pickerInput("indicator", 
                                      label = h5("Which indicator(s)?", 
                                                 style = "color:#76787B;"), 
                                      width = "200px", 
                                      choices = c("Volume", "MACD", "RSI", "OBV"), 
                                      multiple = T, 
                                      selected = c("Volume", "RSI"), 
                                      options = list(`live-search` = TRUE)),
                          br(), 
                          br(), 
                          br(), 
                          pickerInput("ticker_fin_analysis", 
                                      label = h5("Which asset?", 
                                                 style = "color:#76787B;"), 
                                      width = "200px", 
                                      choices = list(Cryptocurrencies = crypto_tickers,
                                                     Stocks = stock_tickers,
                                                     ETFs = etf_tickers), 
                                      choicesOpt = list(style = picker_inputs_font_weight()),
                                      multiple = F, 
                                      options = list(`live-search` = TRUE)),
                          br(), 
                          br(), 
                          uiOutput(outputId = "start_date_fin_analysis_out")
                        ),
                        
                        ## financial data viz ---------------------------------------------------------
                        mainPanel( 
                          tabsetPanel(
                            tabPanel("Price & Returns", 
                                     br(), 
                                     br(), 
                                     fluidRow(infoBoxOutput("asset_num_shares", 
                                                            width = 4),
                                              infoBoxOutput("asset_last_price", 
                                                            width = 4),
                                              infoBoxOutput("asset_last_cumret", 
                                                            width = 4)), 
                                     hr(), 
                                     div(plotlyOutput("asset_evolution", 
                                                      height = 620, 
                                                      width = 900),
                                         align = "center")),
                            tabPanel("Indicators",
                                     div(plotlyOutput("financial_data_viz", 
                                                      height = 750, 
                                                      width = 1000), 
                                         align = "center")) 
                            
                          )
                        )
                        
                      )
                      
             ), 

# data ---------------------------------------------------------
              tabPanel(
               "Data", 
               fluid = TRUE, 
               icon = icon("database"), 
               
               sidebarLayout(
                 sidebarPanel(
                   width = 3, 
                   h4(strong("Your Financial Data", 
                             style = "color:#76787B;")),
                   hr(), 
                   br(), 
                   pickerInput("ticker_dat", 
                               label = h5("Which asset?", 
                                          style = "color:#76787B;"), 
                               width = "200px", 
                               choices = list(Cryptocurrencies = crypto_tickers,
                                              Stocks = stock_tickers,
                                              ETFs = etf_tickers), 
                               choicesOpt = list(style = picker_inputs_font_weight()),
                               multiple = F, 
                               options = list(`live-search` = TRUE)),
                   br(), 
                   br(), 
                   br(), 
                   br(), 
                   br(), 
                   br(), 
                   uiOutput(outputId = "start_date_dat_out"),
                   br(), 
                   br(), 
                   br(), 
                   br(), 
                   br(), 
                   br(),  
                   downloadButton("download", "Download the data"),
                   br(), 
                   p("Data is extracted from ",
                     a(href = "https://fr.finance.yahoo.com/",
                       target = "_blank",
                       "Yahoo Finance", .noWS = "outside"),
                     " with the ",
                     a(href = "https://www.rdocumentation.org/packages/tidyquant/versions/0.3.0",
                       target = "_blank",
                       "tidyquant", .noWS = "outside"),
                     " package.", 
                     .noWS = c("after-begin", "before-end")) 
                 ), 
                 
                 mainPanel(
                   tabsetPanel(
                     tabPanel("Yahoo Finance", 
                              br(),
                              div(dataTableOutput(outputId = "data"), 
                                  align = "center")), 
                     tabPanel("Returns", 
                              br(),
                              div(dataTableOutput(outputId = "ret_data"), 
                                  align = "center")), 
                     tabPanel("Indicators", 
                              br(),
                              div(dataTableOutput(outputId = "indicators_data"), 
                                  align = "center"))
                   ), 
                  
                 )
                 
               )
               
             ), 

# Stock recommender ---------------------------------------------------
              tabPanel(
                "Stock Recommendation", 
                fluid = TRUE, 
                icon = icon("check"), 
                
                sidebarLayout(
                  sidebarPanel(
                    width = 3, 
                    h4(strong("Stock Recommendation System", 
                              style = "color:#76787B;")),
                    p("Stock selection is based on the ",
                      a(href = "https://www.investopedia.com/terms/r/rsi.asp",
                        target = "_blank",
                        "RSI"), 
                      " and ", 
                      a(href = "https://www.investopedia.com/terms/m/macd.asp",
                        target = "_blank",
                        "MACD"), 
                      " indicators, as well as on ", 
                      a(href = "https://www.investopedia.com/terms/m/movingaverage.asp#:~:text=In%20finance%2C%20a%20moving%20average,a%20constantly%20updated%20average%20price.",
                        target = "_blank",
                        "moving average", ),
                      " computations.", 
                      style = "color:#76787B;"), 
                    hr(), 
                    pickerInput(inputId = "action", 
                                label = h5("Which action?", 
                                           style = "color:#76787B;"), 
                                width = "200px", 
                                choices = c("Buy", "Sell"),
                                selected = "Buy", 
                                multiple = F), 
                    br(), 
                    br(), 
                    br(), 
                    pickerInput("reco_indicators", 
                                label = h5("Which indicator(s)?", 
                                           style = "color:#76787B;"), 
                                width = "200px", 
                                choices = c("MACD", "RSI", "stochRSI"),
                                selected = "RSI", 
                                multiple = T), 
                    br(), 
                    br(), 
                    br(), 
                    airDatepickerInput(
                      inputId = "recommendation_start_date",
                      label = h5("How far back do you want to go?", 
                                 style = "color:#76787B;"),
                      value = today() %m-% months(6),
                      minDate = date_init, 
                      maxDate = today() - days(101),
                      width = "200px", 
                      placeholder = "",
                      multiple = F, 
                      clearButton = F), 
                    
                  ), 

                  mainPanel(
                    
                    tabsetPanel(
                      
                      tabPanel(title = "Table", 
                               br(),
                               uiOutput(outputId = "recommendation_tab_title"),
                               br(),  
                               div(dataTableOutput(outputId = "recommendation_data"), 
                                   align = "center")), 
                      
                      tabPanel(title = "Returns", 
                               br(),
                               h4(strong("Cumulative returns"), 
                                  align = "center", 
                                  style = "color:#76787B;"),
                               div(plotlyOutput("recommendation_chart", 
                                                height = 600, 
                                                width = 900), 
                                   align = "center"))
                      
                    )
                    
                  )
                  
                )
                
              ) 
                           
  )
  
)


# User Authentication ---------------------------------------------------

ui <- secure_app(ui, theme = shinytheme("lumen"))

