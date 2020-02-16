# Scripts are used within the Shiny App ----

# 1.0 Getting and Cleaning Stock Data ----
# 1.1 Function is used to get data from selected stock index ----
get_stock_data_function <- function(stockSymbols,
                                    startDate, 
                                    endDate){
    
    getSymbols(stockSymbols,
               src = "yahoo",
               from= startDate,
               to = endDate,
               auto.assign = TRUE,
               warnings = FALSE
    ) %>% 
        map(~Ad(get(.))) %>% # Gets adjusted price for each stock, but returns an xts for each stock symbol
        reduce(merge) #%>%    # merges all xts into a single one, based on date column
        #`colnames<-`(stockSymbols()) # change the column names to the symbol names
}


# 1.2 Function is used to switch between daily, monthly, and annual data processing ----
stock_data_tidy_function <- function(stockPrices,
                                     timePeriod = "monthly"){
    
    
    
    if(timePeriod == "monthly"){
        prices_tidy <- stockPrices %>% 
            to.monthly(indexAt = "lastof", OHLC = FALSE) %>% 
            calculate_returns_function()
    } else if(timePeriod == "daily") {
        prices_tidy <- stockPrices %>% 
            to.daily(indexAt = "lastof", OHLC = FALSE) %>% 
            calculate_returns_function()
    } else if(timePeriod == "weekly"){
        prices_tidy <- stockPrices %>% 
            to.weekly(indexAt = "lastof", OHLC = FALSE) %>% 
            calculate_returns_function()
    } else {
        prices_tidy <- stockPrices %>% 
            to.yearly(indexAt = "lastof", OHLC = FALSE) %>% 
            calculate_returns_function()
    }
    
    return(prices_tidy)
}


# 1.3 Calculate Returns ----
calculate_returns_function <- function(data){
    
    data %>% 
        data.frame(date = index(.), stringsAsFactors = FALSE) %>% 
        
        remove_rownames() %>% 
        pivot_longer(
            cols = 1:(ncol(.)-1)
        ) %>% 
        group_by(name) %>% 
        mutate(returns = (log(value) - log(lag(value)))) %>% 
        select(-value) %>% 
        na.omit() %>% 
        rename(stock = name) #%>% 
        # mutate(date2 = as.character(date),
        #        date3 = as.Date(date2, format = "%Y-%m-%d"))
}

# 2.0 Simulation Functions ----


 
# 3.0 Testing----

#  library(quantmod)
#  library(tidyverse)
#  library(tibbletime)
#  
# 
# stockSymbols <- c("TSLA","AAPL",'QLD','BTC-USD')
# weight <- c(0,20,NA,NA)
# df <- data.frame(symbols, weight)
# df <- na.omit(df)
# 
# stockSymbols <- df
# 
# startDate = "2015-01-01"
# endDate = "2019-12-31"
# # # 
# prices <- get_stock_data_function(stockSymbols = stockSymbols, startDate = "2015-01-01", endDate = "2019-12-31")
# # 
# stock_data_tidy_function(stockPrices = prices, timePeriod = "monthly")
# stock_data_tidy_function(stockPrices = prices, timePeriod = "yearly")
