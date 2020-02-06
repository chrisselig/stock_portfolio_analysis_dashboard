# Script is used to load functions related to getting and cleaning stock data ----


# Function is used to get data from selected stock index ----
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


calculate_returns_function <- function(data){
    
    prices_tidy <- data %>% 
        data.frame(date = index(.)) %>% 
        remove_rownames() %>% 
        pivot_longer(
            cols = 1:(ncol(.)-1)
        ) %>% 
        group_by(name) %>% 
        mutate(returns = (log(value) - log(lag(value)))) %>% 
        select(-value) %>% 
        na.omit() %>% 
        rename(stock = name)
    
    return(prices_tidy)
    
}

# Function is used to switch between daily, monthly, and annual data processing ----
stock_data_tidy_function <- function(stockPrices,
                                     timePeriod = "monthly"){
    
    
    
    if(timePeriod == "monthly"){
        prices_tidy <- prices %>% 
            to.monthly(indexAt = "lastof", OHLC = FALSE) %>% 
            calculate_returns_function()
    } else if(timePeriod == "daily") {
        prices_tidy <- prices %>% 
            to.daily(indexAt = "lastof", OHLC = FALSE) %>% 
            calculate_returns_function()
    } else if(timePeriod == "weekly"){
        prices_tidy <- prices %>% 
            to.weekly(indexAt = "lastof", OHLC = FALSE) %>% 
            calculate_returns_function()
    } else {
        prices_tidy <- prices %>% 
            to.yearly(indexAt = "lastof", OHLC = FALSE) %>% 
            calculate_returns_function()
    }
    
    return(prices_tidy)
}



 
# # Testing----
# library(quantmod)
# library(tidyverse)
# library(tibbletime)
# 
# symbols <- c("TSLA","AAPL")
# 
# prices <- get_stock_data_function(stockSymbols = symbols, startDate = "2015-01-01", endDate = "2019-12-31")
# 
# stock_data_tidy_function(stockPrices = prices, timePeriod = "monthly")
# stock_data_tidy_function(stockPrices = prices, timePeriod = "yearly")
