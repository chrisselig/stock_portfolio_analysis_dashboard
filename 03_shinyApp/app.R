#
# BIDAMIA INC. ----
# App for portfolio and stock analysis ----
# Feb 2020 v1
#

library(shiny)
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinyjs)

# Data manipulation functions
library(quantmod)
library(tidyverse)
library(tibbletime)

# Plotting functions
library(plotly)


# Source Scripts ----
source("../02_scripts/01_get_stock_prices.R")

# Define UI for application that draws a histogram
ui <- navbarPage(
    
    title = "Portfolio Analysis",
    
    inverse= TRUE,
    collapsible = TRUE,
    
    theme = shinytheme("flatly"),
    
    # 1.0 Portfolio Simulation Page ----
    tabPanel(
        
        
        # ** CSS ----
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
        ),
        
        # ** JS ----
        shinyjs::useShinyjs(),
        
        title = "Portfolio Simulation",
        
        div(
            class = "title_bar",
            # 1.3 Portfolio Simulation ----
            column(
                width = 8,
                div(
                    plotlyOutput(
                        outputId = "simulation_line_plot"
                    )
                )
            ),
            
            # 1.4 Filters ----
            column(
                width = 4,
                wellPanel(
                    class = "my-filters",
                    inline = FALSE,
                        
                    # 1.4.1 Stock 1 ----
                    fluidRow(
                        column(
                            width = 5,
                            textInput(
                                inputId = "input_stock1",
                                label = 'Stock 1'
                            )
                        ),
                        column(
                            width = 5,
                            numericInput(
                                inputId = 'input_stock1_weight',
                                label = 'Weight',
                                value = 0,
                                min = 0,
                                max = 1,
                                step = .1
                            )
                        )
                    ),
                        
                    # 1.4.2 Stock 2 ----
                    fluidRow(
                        column(
                            width = 5,
                            textInput(
                                inputId = "input_stock2",
                                label = 'Stock 2'
                            )
                        ),
                        column(
                            width = 5,
                            numericInput(
                                inputId = 'input_stock2_weight',
                                label = 'Weight',
                                value = 0,
                                min = 0,
                                max = 1,
                                step = .1
                            )
                        )
                    ),    
                    # 1.4.3 Stock 3 ----
                    fluidRow(
                        column(
                            width = 5,
                            textInput(
                                inputId = "input_stock3",
                                label = 'Stock 3'
                            )
                        ),
                        column(
                            width = 5,
                            numericInput(
                                inputId = 'input_stock3_weight',
                                label = 'Weight',
                                value = 0,
                                min = 0,
                                max = 1,
                                step = .1
                            )
                        )
                    ),    
                    # 1.4.4 Stock 4 ----
                    fluidRow(
                        column(
                            width = 5,
                            textInput(
                                inputId = "input_stock4",
                                label = 'Stock 4'
                            )
                        ),
                        column(
                            width = 5,
                            numericInput(
                                inputId = 'input_stock4_weight',
                                label = 'Weight',
                                value = 0,
                                min = 0,
                                max = 1,
                                step = .1
                            )
                        )
                    ),    
                    # 1.4.5 Stock 5 ----
                    fluidRow(
                        column(
                            width = 5,
                            textInput(
                                inputId = "input_stock5",
                                label = 'Stock 5'
                            )
                        ),
                        column(
                            width = 5,
                            numericInput(
                                inputId = 'input_stock5_weight',
                                label = 'Weight',
                                value = 0,
                                min = 0,
                                max = 1,
                                step = .1
                            )
                        )
                    ),
                    
                    # 1.4.6 Start Date ----
                    fluidRow(
                        column(
                            width = 5,
                            dateInput(
                                inputId = "input_start_date",
                                label = "Start Date",
                                value = "2015-01-01"
                            )
                        ),
                        column(
                            width = 5,
                            numericInput(
                                inputId = "input_portfolio_value",
                                label = "Portfolio Value",
                                value = 1000,
                                min = 0
                            )
                        )
                    ),
                    
                    # 1.4.7 Period & Number of Sims ----
                    fluidRow(
                        column(
                            width = 5,
                            numericInput(
                                inputId = "input_num_periods",
                                label = "# of Periods to Simulate",
                                value = 12,
                                min = 1
                            )
                        ),
                        column(
                            width = 5,
                            numericInput(
                                inputId = "input_num_sims",
                                label = "# of Sims",
                                value = 100,
                                min =1,
                                max = 1000,
                                step = 50
                            )
                        )
                    ),
                    
                    hr(),
                    
                    # 1.4.8 Submit & Reset Buttons ----
                    fluidRow(
                        column(
                            width = 5,
                            actionButton(
                                inputId = "btn_simualate",
                                label = "Simulate"
                            )
                        ),
                        column(
                            width = 5,
                            actionButton(
                                inputId = "btn_reset_defaults",
                                label = "Reset to Defaults"
                            )
                        )
                    )
                )
            )
        )
    ),
    
    # 2.0 Portfolio Analysis Page ----
    tabPanel(
        title = "Portfolio Analysis"
    ),
    
    # 3.0 Information / Disclaimer Page ----
    tabPanel(
        title = "Information / Disclaimer"
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

    
    # Simulation plot ----
    sims <- eventReactive(input$btn_simulate, {input$input_num_sims})
    
    monte_carlo_sims <- eventReactive(input$btn_simulate, { 
        
        sims <- sims()
        
        starts <-  
            rep(1, sims) %>%
            set_names(paste("sim", 1:sims, sep = ""))
        
        map_dfc(starts, simulation_accum_1,
                N = input$sim_months, mean = mean_port_return(), 
                stdev = stddev_port_return()) %>% 
            mutate(month = seq(1:nrow(.))) %>% 
            select(month, everything()) %>% 
            `colnames<-`(c("month", names(starts))) %>% 
            gather(sim, growth, -month) %>% 
            group_by(sim) %>% 
            mutate_all(funs(round(., 2)))
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
