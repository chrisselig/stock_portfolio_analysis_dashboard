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
#library(shinydashboard)

# Data manipulation functions
library(quantmod)
library(tidyverse)
# library(tidyquant)
library(tibbletime)
library(lubridate)
library(scales)

# Plotting functions
library(plotly)


# Source Scripts ----
source("../02_scripts/01_get_stock_prices.R")
source("../02_scripts/02_plotting_functions.R")

# Define UI for application that draws a histogram
ui <- navbarPage(
  
        #position = "fixed-bottom",
        title = "",
    
    # inverse= TRUE,
    collapsible = TRUE,
    
    theme = shinytheme("flatly"),
        
    # 1.0 General Inputs ----
        tabPanel(
          class = "tabPanel",
          "GENERAL INPUTS",
          # 1.0 Filter Panel ----
          column(
            width = 9,
            # 1.1.1 Stock 1 ----
            fluidRow(
              column(
                width = 6,
                textInput(
                  inputId = "input_stock1",
                  label = 'Stock 1',
                  placeholder = "AAPL",
                  value = "AAPL"
                )
              ),
              column(
                width = 6,
                numericInput(
                  inputId = 'input_stock1_weight',
                  label = 'Weight %',
                  value = 25,
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
                  label = 'Stock 2',
                  placeholder = "TSLA",
                  value = "TSLA"
                )
              ),
              column(
                width = 6,
                numericInput(
                  inputId = 'input_stock2_weight',
                  label = 'Weight %',
                  value = 25,
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
                  label = 'Stock 3',
                  placeholder = "QLD",
                  value = "QLD"
                )
              ),
              column(
                width = 6,
                numericInput(
                  inputId = 'input_stock3_weight',
                  label = 'Weight %',
                  value = 50,
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
                  value = NA_real_,
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
                  value = NA_real_,
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
                  value = today()-1
                )
              )
            ),
            
            hr(),
            
            # 1.1.7 Window & Calculate Button ----
            fluidRow(
              column(
                width = 4,
                numericInput(
                  inputId = "input_window",
                  label = "Rolling Period (Window)",
                  value = 12L,
                  min = 2L
                )
              ),
              column(
                width = 4,
                selectInput(
                  inputId = "input_timePeriod",
                  label = "Returns Time Period",
                  choices = c('daily','weekly','monthly','yearly'),
                  selected = "monthly"
                )
              ),
              div(
                column(
                  width = 4,
                  actionButton(
                    inputId = "btn_calculate",
                    label = "Analyze"
                  )
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
                  #)
                  #)
                )
              )
            )
            
          ),
          column(
            width = 3,
            h2("Disclaimer"),
            br(),
            p("Seek real financial advice"),
            br(),
            h2("Information"),
            p("Page is used to input the stocks for your portfolio")
          )
        ),
        # 2.0 Portfolio Analysis Tab ----
        tabPanel(
            class = "tabPanel",
            "PORTFOLIO ANALYSIS",
            # actionButton(inputId = "filter_selector","Show/Hide Filters"),
            # * CSS ----
            tags$head(
                tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
            ),
            
            # * JS ----
            shinyjs::useShinyjs(),
            
            div(
              fluidRow(
                column(width = 3),
                column(width = 3),
                column(width = 3),
                column(width = 3)
              ),
              fluidRow(
                column(width = 6),
                # Visualize Individual Asset Returns
                column(
                  width = 6,
                  plotlyOutput("returnsPlot")
                )
              ),
              fluidRow(
                column(width = 6),
                column(width = 6)
              )
            )
        ),
        
        # 3.0 Portfolio Simulation Tab ----
        tabPanel(
            class = "tabPanel",
            "PORTFOLIO SIMULATION",
            
            fluidRow(
              tableOutput(outputId = 'returns_tbl')
            ),
            hr(),
            fluidRow(
              tableOutput(outputId = 'stocksymbol')
            )
        )
     )

# Server Logic ----
server <- function(input, output,session) {
    
    # Get List of Stocks and Weights ----
    stock_symbols <- eventReactive(input$btn_calculate,{
        symbols <- c(input$input_stock1, input$input_stock2, input$input_stock3, input$input_stock4, input$input_stock5)
        weights <- c(input$input_stock1_weight/100,input$input_stock2_weight/100,input$input_stock3_weight/100,input$input_stock4_weight/100,input$input_stock5_weight/100)
        df <- data.frame(symbols, weights) %>% 
          na.omit()
    },ignoreNULL = FALSE)
  
    # Get Stock Prices ----
    stock_prices <- reactive({
        get_stock_data_function(stock_symbols(), startDate = input$input_start_date, endDate = input$input_end_date)
    })

    # Tidy Stock Prices & Calculate Log Returns ----
    returns_tbl <- reactive({
        # timePeriod <- input$input_timePeriod
        # weights <- c(input$input_stock1_weight,input$input_stock2_weight,input$input_stock3_weight,input$input_stock4_weight,input$input_stock5_weight)
      stock_weights_tbl <- stock_symbols()  
      calculate_returns_function(stock_prices(), weights_tbl = stock_weights_tbl, timePeriod = input$input_timePeriod)
    })
    
    
    output$returns_tbl <- renderTable(returns_tbl())
    output$stocksymbol <- renderTable(stock_prices())
    
    # 2.0 Portfolio Analysis Tab ----
    # 2.1 KPI's ----
    # 2.1.1 Portfolio Standard Deviation ----
    # portfolio_sd <- reactive({
    #   returns_tbl() %>% 
    #     round(sd(returns,na.rm = TRUE),4)
    # })
    # 
    # output$portfolio_sd <- portfolio_sd()
    # 
    # Sum up weights to ensure it is not greater than 100% ----
    
    # 2.1.2 Render Returns Plot ----
    returns_chart <- reactive({
      line_chart_function(returns_tbl(),
                          y_axis = returns_tbl()$weighted_returns,
                          title = "Log Returns for Selected Assets",
                          y_axis_label = "Log Returns"
                          )
    })
    
    output$returnsPlot <- renderPlotly(returns_chart())
    
    # output$value <- renderText({input$input_stock1})
    # # Show/Hide Filter Bar ----
    # observeEvent(input$filter_selector, {
    #     shinyjs::toggle(id = "filter-panel", anim = TRUE, animType = "slide")
    # })
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
