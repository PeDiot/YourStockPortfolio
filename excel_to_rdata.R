BACKUP_DIR <- "./tickers/backup/"

format__excel_data <- function(excel_file_name, sheet_name){
  
  dat <- readxl::read_excel(path = paste0(BACKUP_DIR, excel_file_name), 
                            sheet = sheet_name)
  
  dat %>%
    distinct(shortName, .keep_all = T) %>% 
    mutate( name = longName %>% 
               gsub(pattern = "[[:punct:] ]+", replacement = " ") %>% 
               str_to_upper() ) %>% 
    select( c(symbol, name) ) %>% 
    rename(tickers = symbol) %>% 
    column_to_rownames(var = "name") 
  
  
}

stoxx600_assets <- format__excel_data("tickers_france.xlsx", "STOXX 600")

save(stoxx600_assets, file = "stoxx600_assets.RData")

