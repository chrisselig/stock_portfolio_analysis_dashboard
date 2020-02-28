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
library(e1071)

# Plotting functions
library(ggthemes)
library(plotly)


# Source Scripts ----
source("../02_scripts/01_data_transformation_functions.R")
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
          h2("Portfolio Inputs"),
          column(
            width = 6,
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

            #hr(),
            
            # 1.1.8 Simulation Inputs ----
            # 1.1.8.1 Period & Number of Sims ----
            # h2("Simulation Inputs"),
            # fluidRow(
            #   column(
            #     width = 4,
            #     numericInput(
            #       inputId = "input_num_periods",
            #       label = "Periods to Simulate",
            #       value = 12L,
            #       min = 1L
            #     )
            #   ),
            #   column(
            #     width = 4,
            #     numericInput(
            #       inputId = "input_num_sims",
            #       label = "# of Sims",
            #       value = 100L,
            #       min =1L,
            #       max = 100000L,
            #       step = 50L
            #     )
            #   ),
            #   column(
            #     width = 3,
            #     numericInput(
            #       inputId = "input_portfolio_value",
            #       label = "Portfolio Value",
            #       value = 1000,
            #       min = 0
            #     )
            #   )
            # ),
            # 
            # # 1.1.8.2 Simulate Button ----
            # fluidRow(
            #   column(
            #     width = 6,
            #     actionButton(
            #       inputId = "btn_simualate",
            #       label = "Simulate"
            #     )
            #   )
            # ),
            
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
          # 1.1.11 Market or Comparision Asset ----
          column(
            width = 3,
            textInput(
              inputId = "input_market",
              label = 'Market Comparision Asset',
              placeholder = 'SPY',
              value = 'SPY'
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
                ),
              )
            ),
            hr(),
            column(
              width = 4,
              actionButton(
                inputId = "btn_calculate",
                label = "Analyze"
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
            column(
              width = 10,
              div(
                fluidRow(
                  column(width = 3),
                  column(width = 3),
                  column(width = 3),
                  column(width = 3)
                ),
                fluidRow(
                  column(
                    width = 6,
                    # 2.2 Rolling Calculations ----
                    tabsetPanel(
                      type = "tabs",
                      # 2.2.1 Visualize Sd ----
                      tabPanel("Standard Deviation",plotlyOutput("rollingStd")),
                      # 2.2.2 Visualize Kurtosis ----
                      tabPanel("Kurtosis",plotlyOutput("rollingkurt")),
                      # 2.2.3 Visualize Skewness ----
                      tabPanel("Skewness",plotlyOutput("rollingskew"))
                    )
                  ),
                  # 2.3 Visualize Individual Asset Returns & Portfolio ----
                  column(
                    width = 6,
                    tabsetPanel(
                      type = "tabs",
                      tabPanel("Returns",plotlyOutput("returnsPlot"))
                    )
                  )
                ),
                fluidRow(
                  # 2.4 Component Contribution to Standard Deviation ---- 
                  column(
                    width = 6,
                    tabsetPanel(
                      type = "tabs",
                      tabPanel("Component Contribution",plotlyOutput('compContBar'))
                    )
                    ),
                  # 2.5 Visualize Covariance ----
                  column(
                    width = 6,
                    # 2.5.2 Visualize Covariance ----
                    tabsetPanel(
                      type = "tabs",
                      tabPanel("Covariance",plotlyOutput("covar_plot"))
                    ),
                    tabsetPanel(
                      type = "tabs",
                      tabPanel("Correlation",plotlyOutput("cor_tot"))
                    )
                  )
                )
              )
            ),
            # 2.6 Links to yahoo quote page ----
            column(
              width = 2,
              fluidRow(
                h2('Further Information for Selected Assets')
              ),
              fluidRow(
                # htmlOutput()
                tableOutput('links')
              )
            )
        )
        
        # 3.0 Portfolio Simulation Tab ----
        # tabPanel(
        #     class = "tabPanel",
        #     "PORTFOLIO SIMULATION",
        #     
        #     fluidRow(
        #       tableOutput(outputId = 'returns_tbl')
        #     ),
        #     hr(),
        #     fluidRow(
        #       tableOutput(outputId = 'stocksymbol')
        #     )
        # )
     )

# Server Logic ----
server <- function(input, output,session) {
    
    # Get List of Stocks and Weights ----
    stock_symbols <- eventReactive(input$btn_calculate,{
        symbols <- c(input$input_stock1, input$input_stock2, input$input_stock3, input$input_stock4, input$input_stock5,input$input_market)
        weights <- c(input$input_stock1_weight/100,input$input_stock2_weight/100,input$input_stock3_weight/100,input$input_stock4_weight/100,input$input_stock5_weight/100,100/100)
        df <- data.frame(symbols, weights) %>% 
          na.omit()
    },ignoreNULL = FALSE)
  
    # Get Weights ----
    stock_weights_tbl <- reactive({
      stock_symbols()
    })
    
    # Get Stock Prices ----
    stock_prices <- reactive({
        get_stock_data_function(stock_symbols(), startDate = input$input_start_date, endDate = input$input_end_date)
    })

    # Tidy Stock Prices & Calculate Log Returns ----
    returns_tbl <- reactive({
        # timePeriod <- input$input_timePeriod
        # weights <- c(input$input_stock1_weight,input$input_stock2_weight,input$input_stock3_weight,input$input_stock4_weight,input$input_stock5_weight)
      #stock_weights_tbl <- stock_symbols()  
      calculate_returns_function(stock_prices(), weights_tbl = stock_weights_tbl(), timePeriod = input$input_timePeriod)
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
    
    # 2.2 Rolling Calculations ----
    # 2.2.1 Rolling Standard Deviation ----
    rolling_sd <- reactive({
      rolling_calculation_function(returns_tbl(), window = input$input_window,.func = sd,func_label = "Standard Dev.")
    })
    
    rolling_sd_chart <- reactive({
      line_chart_function(rolling_sd(),
                          y_axis = rolling_sd()$value,
                          title = paste0("Rolling ",input$input_window," Period Standard Deviation"),
                          y_axis_label = "Standard Deviation")
    })
    
    output$rollingStd <- renderPlotly(rolling_sd_chart())
    
    # 2.2.2 Rolling Kurtosis ----
    rolling_kurt <- reactive({
      rolling_calculation_function(returns_tbl(), window = input$input_window,.func = kurtosis,func_label = "Kurtosis")
    })
    
    rolling_kurtosis_chart <- reactive({
      line_chart_function(rolling_kurt(),
                          y_axis = rolling_kurt()$value,
                          title = paste0("Rolling ",input$input_window," Period Kurtosis"),
                          y_axis_label = "Kurtosis")
    })
    
    output$rollingkurt <- renderPlotly(rolling_kurtosis_chart())
    
    # 2.2.3 Rolling Skewness ----
    rolling_skew <- reactive({
      rolling_calculation_function(returns_tbl(), window = input$input_window,.func = skewness,func_label = "Skewness")
    })
    
    rolling_skewness_chart <- reactive({
      line_chart_function(rolling_skew(),
                          y_axis = rolling_skew()$value,
                          title = paste0("Rolling ",input$input_window," Period Skewness"),
                          y_axis_label = "Skewness")
    })
    
    output$rollingskew <- renderPlotly(rolling_skewness_chart())
    
    
    # 2.3 Returns ----
    # 2.3.1 Render Returns Plot ----
    returns_chart <- reactive({
      line_chart_function(returns_tbl(),
                          y_axis = returns_tbl()$returns,
                          title = "Log Returns for Selected Assets",
                          y_axis_label = "Log Returns"
                          )
    })
    
    output$returnsPlot <- renderPlotly(returns_chart())
    
    # 2.4 Component Contribution to Standard Deviation ----
    component_contribution <- reactive({
      returns_tbl() %>% 
        tidy_data_for_covar_cor_function(marketAsset = input$input_market) %>% 
        covariance_function() %>% 
        component_contribution_function(stock_weights_tbl()) %>% 
        bar_chart_function(title = "Component Contribution to Standard Deviation")
    })
    
    # component_contribution <- reactive({
    #   bar_chart_function(contribution_tbl(),title = "Component Contribution to Standard Deviation")
    # })
    
    output$compContBar <- renderPlotly(component_contribution())
    

    # 2.5 Covariance ----
    
    # 2.5.1 Total Covariance ----
    covar_tot <- reactive({
      tidy_data_for_covar_cor_function(data = returns_tbl(),marketAsset = input$input_market) %>% 
        covariance_function() %>% 
        matrix_to_df_function() %>% 
        heat_map_function(title = "Covariance Matrix")
    })
    
    output$covar_plot <- renderPlotly(covar_tot())
    
    # 2.5.2 Total Correlation ----
    cor_tot <- reactive({
      tidy_data_for_covar_cor_function(data = returns_tbl(),marketAsset = input$input_market) %>% 
        correlation_function() %>% 
        matrix_to_df_function() %>% 
        heat_map_function(title = "Correlation Matrix")
    })
    
    output$cor_plot <- renderPlotly(cor_tot())
    
    # 2.5.3 Rolling Covariance ----
    rolling_covar <- reactive({
      rolling_calculation_function(returns_tbl(), window = input$input_window,.func = cov,func_label = "Covariance")
    })
    
    rolling_covar_chart <- reactive({
      line_chart_function(rolling_covar(),
                          y_axis = rolling_covar()$value,
                          title = paste0("Rolling ",input$input_window," Period Covariance"),
                          y_axis_label = "Covariance")
    })
    
    output$rollingcovar <- renderPlotly(rolling_covar_chart())
    
    # 2.6 Links to Quote Pages at Yahoo Finance ----
    links_tbl <- reactive({
      stock_symbols()$symbols %>% 
        mutate(symbols2 = as.character(symbols2)) %>% 
        filter(symbols2 == 'Portfolio') %>% 
        mutate(website_link = paste0('https://finance.yahoo.com/quote/',symbols2))
    })
    
    output$links <- renderTable(links_tbl())
    # output$value <- renderText({input$input_stock1})
    # # Show/Hide Filter Bar ----
    # observeEvent(input$filter_selector, {
    #     shinyjs::toggle(id = "filter-panel", anim = TRUE, animType = "slide")
    # })

    
}

# Run the application 
shinyApp(ui = ui, server = server)
