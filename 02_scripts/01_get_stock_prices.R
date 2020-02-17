# Scripts are used within the Shiny App ----

# 1.0 Getting and Cleaning Stock Data ----
# 1.1 Function is used to get data from selected stock index ----
get_stock_data_function <- function(data,
                                    startDate, 
                                    endDate){
    
    stocks <- data$symbols %>% 
        na.omit() %>%  
        as.character() 
    
    getSymbols(stocks,
               src = "yahoo",
               from= startDate,
               to = endDate,
               auto.assign = TRUE,
               warnings = FALSE
    ) %>% 
        map(~Ad(get(.))) %>% # Gets adjusted price for each stock, but returns an xts for each stock symbol
        reduce(merge) %>%    # merges all xts into a single one, based on date column
        `colnames<-`(stocks) # change the column names to the symbol names
}


# 1.2 Function is used to switch between daily, monthly, and annual data processing ----
# stock_data_tidy_function <- function(stockPrices,
#                                      timePeriod = "monthly"){
#     
#     
#     
#     if(timePeriod == "monthly"){
#         prices_tidy <- stockPrices %>% 
#             to.monthly(indexAt = "lastof", OHLC = FALSE) #%>% 
#             #calculate_returns_function()
#     } else if(timePeriod == "daily") {
#         prices_tidy <- stockPrices %>% 
#             to.daily(indexAt = "lastof", OHLC = FALSE) #%>% 
#             #calculate_returns_function()
#     } else if(timePeriod == "weekly"){
#         prices_tidy <- stockPrices %>% 
#             to.weekly(indexAt = "lastof", OHLC = FALSE) #%>% 
#             #calculate_returns_function()
#     } else {
#         prices_tidy <- stockPrices %>% 
#             to.yearly(indexAt = "lastof", OHLC = FALSE) #%>% 
#             #calculate_returns_function()
#     }
#     
#     return(prices_tidy)
# }


# 1.2 Calculate Returns ----
calculate_returns_function <- function(data,weights_tbl,timePeriod){
    
    data <- data
    
    if(timePeriod == "monthly"){
        data <- data %>% 
            to.monthly(indexAt = "lastof", OHLC = FALSE)
    } else if(timePeriod == "daily") {
        data <- data %>% 
            to.daily(indexAt = "lastof", OHLC = FALSE)
    } else if(timePeriod == "weekly"){
        data <- data %>% 
            to.weekly(indexAt = "lastof", OHLC = FALSE)
    } else {
        data <- data %>% 
            to.yearly(indexAt = "lastof", OHLC = FALSE)
    }
    
    data <- data %>% 
        data.frame(date = index(.)) %>% 
        remove_rownames() %>% 
        pivot_longer(
            cols = 1:(ncol(.)-1)
        ) %>% 
        group_by(name) %>% 
        mutate(returns = (log(value)-log(lag(value)))) %>% 
        select(-value) %>% 
        ungroup() %>% 
        na.omit() %>% 
        rename(asset = name) %>% 
        left_join(weights_tbl, by = c('asset' = 'symbols')) %>% 
        mutate(weighted_returns = returns * weights)
        
    return(data)
}

# 2.0 Simulation Functions ----


 
# 3.0 Testing----

#  library(quantmod)
#  library(tidyverse)
#  library(tibbletime)
# # # #  
# # # # 
# symbols <- c("TSLA","AAPL",'QLD','BTC-USD')
# weights <- c(35,20,25,20)
# data <- data.frame(symbols, weights)
# data <- na.omit(data)
# # # 
# #  stockSymbols <- df
# # # 
# # startDate = "2015-01-01"
# # endDate = "2019-12-31"
# # # 
# prices <- get_stock_data_function(data, startDate = "2015-01-01", endDate = "2019-12-31")
# prices_month <- stock_data_tidy_function(prices, timePeriod = "monthly")
#calculate_returns_function(prices_month)
# stock_data_tidy_function(stockPrices = prices, timePeriod = "yearly")

