# Plotting Functions ----

# 1.0 Line Charts ----
line_chart_function <- function(data,
                                y_axis,
                                title,
                                y_axis_label){
    g <- data %>% 
        ggplot(aes(x = date,y_axis, group = asset, label = label_text)) +
        # Geoms
        geom_line(aes(color = asset)) +
        theme_tufte() +
        labs(
            x = '',
            y = y_axis_label,
            title = title
        ) +
        scale_y_continuous(
            labels = scales::number_format(accuracy = 0.01),
            breaks = scales::pretty_breaks(12)
        ) +
        scale_x_date(breaks = scales::pretty_breaks(12)) +
        theme(
            #plot.title = element_text(hjust = 0.5),
            legend.title = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1)
        )
    
    ggplotly(g, tooltip = c("label"))
    
}


# 
# # Testing ----
# library(quantmod)
# library(tidyverse)
# library(tibbletime)
# library(scales)
# library(ggthemes)
# 
# symbols <- c("TSLA","AAPL",'QLD')
# weights <- c(35,20,25)
# data <- data.frame(symbols, weights)
# data <- na.omit(data)
# 
# startDate = "2015-01-01"
# endDate = "2019-12-31"
# 
# prices <- get_stock_data_function(data, startDate = "2015-01-01", endDate = "2019-12-31")
# 
# weighted_returns_tbl <- calculate_returns_function(prices,data,timePeriod = "monthly")
# 
# data <- weighted_returns_tbl
# y_axis <- weighted_returns_tbl$weighted_returns
# y_axis_label <- 'Log Returns'
# title = 'Log Returns for Selected Assets'
