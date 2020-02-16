#
# Created by Chris Selig of BIDAMIA INC. ----
# https://www.linkedin.com/in/chris-selig/
# bidamia.ca
# App for portfolio and stock analysis ----
# Feb 2020 v1.0
#

library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinyjs)
library(shinydashboard)

# Data manipulation functions
library(quantmod)
library(tidyverse)
library(tibbletime)
library(lubridate)

# Plotting functions
library(plotly)


# Source Scripts ----
source("../02_scripts/01_get_stock_prices.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # column(
    #     width = 6
    #     #title = h2("Portfolio Analysis")
    # ),
    # column(
    #     id = "hide_button",
    #     width = 2,
    div(
        fluidRow(
        id = "title-bar",
        actionButton(inputId = "filter_selector","Show/Hide Filters")
    )  
    ),
    # ),
    
    inverse= TRUE,
    collapsible = TRUE,
    
    theme = shinytheme("flatly"),
    
    # * CSS ----
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    
    # * JS ----
    shinyjs::useShinyjs(),
    

    
    # 1.0 Filter Panel ----
    div(
        #id = "filter-panel",
        sidebarLayout(
            
            position = "right",
            sidebarPanel(
                id = "filter-panel",
                # 1.1 Filters ----
                wellPanel(
                    
                    inline = FALSE,
                    
                    # 1.1.1 Stock 1 ----
                    fluidRow(
                        column(
                            width = 6,
                            textInput(
                                inputId = "input_stock1",
                                label = 'Stock 1'
                            )
                        ),
                        column(
                            width = 6,
                            numericInput(
                                inputId = 'input_stock1_weight',
                                label = 'Weight %',
                                value = 0,
                                min = 0,
                                max = 100
                            )
                        )
                    ),
                    
                    # 1.1.2 Stock 2 ----
                    fluidRow(
                        column(
                            width = 6,
                            textInput(
                                inputId = "input_stock2",
                                label = 'Stock 2'
                            )
                        ),
                        column(
                            width = 6,
                            numericInput(
                                inputId = 'input_stock2_weight',
                                label = 'Weight %',
                                value = 0,
                                min = 0,
                                max = 100
                            )
                        )
                    ),
                    # 1.1.3 Stock 3 ----
                    fluidRow(
                        column(
                            width = 6,
                            textInput(
                                inputId = "input_stock3",
                                label = 'Stock 3'
                            )
                        ),
                        column(
                            width = 6,
                            numericInput(
                                inputId = 'input_stock3_weight',
                                label = 'Weight %',
                                value = 0,
                                min = 0,
                                max = 100
                            )
                        )
                    ),
                    # 1.1.4 Stock 4 ----
                    fluidRow(
                        column(
                            width = 6,
                            textInput(
                                inputId = "input_stock4",
                                label = 'Stock 4'
                            )
                        ),
                        column(
                            width = 6,
                            numericInput(
                                inputId = 'input_stock4_weight',
                                label = 'Weight %',
                                value = 0,
                                min = 0,
                                max = 100
                            )
                        )
                    ),
                    # 1.1.5 Stock 5 ----
                    fluidRow(
                        column(
                            width = 6,
                            textInput(
                                inputId = "input_stock5",
                                label = 'Stock 5'
                            )
                        ),
                        column(
                            width = 6,
                            numericInput(
                                inputId = 'input_stock5_weight',
                                label = 'Weight %',
                                value = 0,
                                min = 0,
                                max = 100
                            )
                        )
                    ),
                    
                    # 1.1.6 Start & End Dates ----
                    fluidRow(
                        column(
                            width = 6,
                            dateInput(
                                inputId = "input_start_date",
                                label = "Start Date",
                                value = "2015-01-01"
                            )
                        ),
                        column(
                            width = 6,
                            dateInput(
                                inputId = "input_end_date",
                                label = "End Date",
                                value = today()
                            )
                        )
                    ),
                    
                    hr(),
                    
                    # 1.1.7 Window & Calculate Button ----
                    fluidRow(
                        column(
                            width = 6,
                            numericInput(
                                inputId = "input_window",
                                label = "Rolling Period (Window)",
                                value = 12L,
                                min = 2L
                            )
                        ),
                        column(
                            width = 6,
                            actionButton(
                                inputId = "btn_calculate",
                                label = "Analyze"
                            )
                        )
                    ),
                    
                    
                    # 1.1.8 Simulation Inputs ----
                    # 1.1.8.1 Period & Number of Sims ----
                    fluidRow(
                        column(
                            width = 4,
                            numericInput(
                                inputId = "input_num_periods",
                                label = "Periods to Simulate",
                                value = 12L,
                                min = 1L
                            )
                        ),
                        column(
                            width = 4,
                            numericInput(
                                inputId = "input_num_sims",
                                label = "# of Sims",
                                value = 100L,
                                min =1L,
                                max = 100000L,
                                step = 50L
                            )
                        ),
                        column(
                            width = 3,
                            numericInput(
                                inputId = "input_portfolio_value",
                                label = "Portfolio Value",
                                value = 1000,
                                min = 0
                            )
                        )
                    ),
                    
                    # 1.1.8.2 Simulate Button ----
                    fluidRow(
                        column(
                            width = 6,
                            actionButton(
                                inputId = "btn_simualate",
                                label = "Simulate"
                            )
                        )
                    ),
                    
                    hr(),
                    
                    # 1.1.10 Reset Defaults ----
                    fluidRow(
                        column(
                            width = 6,
                            actionButton(
                                inputId = "btn_reset_defaults",
                                label = "Reset to Defaults"
                            )
                        )
                    )
                )
            ),
            
            div(
                mainPanel(
                    # column(width = 8),
                    
                    tabsetPanel(type = "tabs",
                                
                                # 2.0 Portfolio Analysis Tab ----
                                tabPanel(
                                    class = "tabPanel",
                                    "Portfolio Analysis"
                                ),
                                
                                # 3.0 Portfolio Simulation Tab ----
                                tabPanel(
                                    class = "tabPanel",
                                    "Portfolio Simulation"),
                                
                                # 4.0 Disclaimer / Information Tab ----
                                tabPanel(
                                    class = "tabPanel",
                                    "Disclaimer / Information")
                    )
                    
                )                
            )
    )

    )
)

# Server Logic ----
server <- function(input, output,session) {
    
    # Get list of stocks ----
    prices <- eventReactive(input$btn_simualate,{
        
        symbols <- c(input$input_stock1, input$input_stock2, input$input_stock3, input$input_stock4, input$input_stock5)
        # symbols <- c("QLD","PNQI","AAPL")
        
        # Get stock prices & Tidy Data ----
        prices <- get_stock_data_function(stockSymbols = symbols(), startDate = "2015-01-01", endDate = "2019-12-31") %>%
            stock_data_tidy_function(timePeriod = "monthly")
        
        prices <- stock_data_tidy_function(stockPrices = prices, timePeriod = "monthly")   
        
    })
    
    # Show/Hide Filter Bar ----
    observeEvent(input$filter_selector, {
        shinyjs::toggle(id = "filter-panel", anim = TRUE, animType = "slide")
    })
    # Simulation plot ----
    # sims <- eventReactive(input$btn_simulate, {input$input_num_sims})
    # 
    # monte_carlo_sims <- eventReactive(input$btn_simulate, { 
    #     
    #     sims <- sims()
    #     
    #     starts <-  
    #         rep(1, sims) %>%
    #         set_names(paste("sim", 1:sims, sep = ""))
    #     
    #     map_dfc(starts, simulation_accum_1,
    #             N = input$sim_months, mean = mean_port_return(), 
    #             stdev = stddev_port_return()) %>% 
    #         mutate(month = seq(1:nrow(.))) %>% 
    #         select(month, everything()) %>% 
    #         `colnames<-`(c("month", names(starts))) %>% 
    #         gather(sim, growth, -month) %>% 
    #         group_by(sim) %>% 
    #         mutate_all(funs(round(., 2)))
    #     
    # })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
