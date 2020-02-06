# Script is used to get data from selected stock index ----

get_stock_data_function <- function(stockSymbols,
                                    startDate, 
                                    endDate){
    
    prices <- getSymbols(
        symbols, src = "yahoo",
        from= startDate,
        to = endDate,
        auto.assign = TRUE,
        warnings = FALSE
    ) %>% 
        map(~Ad(get(.))) %>% # Gets adjusted price for each stock, but returns an xts for each stock symbol
        reduce(merge) %>%    # merges all xts into a single one, based on date column
        `colnames<-` (symbols) # change the column names to the symbol names
    
    return(prices)
}

# 
# # Testing----
# library(quantmod)
# library(tidyverse)
# 
# symbols <- c("TSLA","AAPL")
# 
# get_stock_data_function(stockSymbols = symbols, startDate = "2019-01-01", endDate = "2019-12-31")
