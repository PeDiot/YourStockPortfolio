BACKUP_DIR <- "./tickers/backup/"

format__excel_data <- function(excel_file_name, sheet_name){
  
  dat <- readxl::read_excel(path = paste0(BACKUP_DIR, excel_file_name), 
                            sheet = sheet_name)
  
  dat %>%
    distinct(shortName, .keep_all = T) %>% 
    mutate( name = if_else(is.na(shortName), 
                           longName, 
                           shortName) %>% str_to_upper() ) %>% 
    select( c(symbol, name) ) %>% 
    rename(tickers = symbol) %>% 
    column_to_rownames(var = "name") 
  
  
}

sp500IT_assets <- format__excel_data("tickers_america.xlsx", "S&P 500 Information Technology")

save(sp500IT_assets, file = "sp500IT_assets.RData")

