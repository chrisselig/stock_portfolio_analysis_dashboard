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
# library(RColorBrewer)
library(ggsci)
library(plotly)


# Source Scripts ----
source("../02_scripts/01_data_transformation_functions.R")
source("../02_scripts/02_plotting_functions.R")

# Define UI for application that draws a histogram
ui <- 
  tagList(
    # * CSS ----
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      tags$link(href="https://fonts.googleapis.com/css?family=Old+Standard+TT:400,700&display=swap", rel="stylesheet")
    ),
    
    # * JS ----
    shinyjs::useShinyjs(),
    
    navbarPage(
      
      #position = "fixed-bottom",
      title = "Portfolio Analysis by Chris Selig",
      
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
          
          hr(),
          
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
          fluidRow(
            h2("Disclaimer"),
            p(
              'All information and data on this site is for information purposes only. I make no representations as to accuracy, completeness, suitability or validity, of any information. I will not be liable for any errors, omissions, or any losses, injuries, or damages arising from its display or use. All information is provided on an AS IS with no warranties, and confers no rights.'
            ),
            
            p(
              'Because information on this site are based on personal opinion and experience, it should not be considered professional financial investment advice. The ideas and strategies should never be used without first assessing your own personal situation, or without consulting a financial professional. My thoughts and opinions will also change from time to time as I learn and accumulate more knowledge. It is very important to do your own analysis before making any investment based on your own personal circumstances.'         
            )
          ),        
          
          
          
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
          
          # 1.1.7 Window & Calculate Button & Reset Buttons ----
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
                choices = c('weekly','monthly','yearly'),
                selected = "monthly"
              ),
            )
          ),
          hr(),
          fluidRow(
            div(
              column(
                width = 4,
                actionButton(
                  inputId = "btn_calculate",
                  label = "   Analyze",
                  icon = icon("calculator")
                )
              )
            ),
            div(column(
              width = 4,
              # 1.1.10 Reset Defaults ----
              column(
                width = 6,
                actionButton(
                  inputId = "btn_reset_defaults",
                  label = "   Reset to Defaults",
                  icon = icon("sync")
                )
              )
            )
            )
          )
          
        ),
      ),
      
      # 2.0 Portfolio Analysis Tab ----
      tabPanel(
        class = "tabPanel",
        "PORTFOLIO ANALYSIS",
        # actionButton(inputId = "filter_selector","Show/Hide Filters"),
        
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
                
                tabsetPanel(
                  type = "tabs",
                  # 2.5.2 Visualize Covariance ----
                  tabPanel("Covariance",plotlyOutput("covar_plot")),
                  # 2.5.3 Visualize Correlation ----
                  tabPanel("Correlation",plotlyOutput("cor_plot"))
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
            class = "infoLinks",
            uiOutput('links1')
          ),
          br(),
          fluidRow(
            class = "infoLinks",
            uiOutput('links2')
          ),
          br(),
          fluidRow(
            class = "infoLinks",
            uiOutput('links3')
          ),
          br(),
          fluidRow(
            class = "infoLinks",
            uiOutput('links4')
          ),
          br(),
          fluidRow(
            class = "infoLinks",
            uiOutput('links5')
          )
        )
        
        
      ),
      # 3.0 Information Tab ----
      tabPanel(
        class = "tabPanel",
        "INFORMATION",
        div(
          fluidRow(
            # 3.1 Further Information ----
            column(
              width = 8,
              h3("Further Information"),
              br(),
              h4("Standard Deviation"),
              p("Measures how much the investment returns differ from the probability distribution of 
                investments. So, standard deviation is a measure of risk and a higher standard deviation
                means higher volatility of returns."),
              br(),
              h4("Kurtosis"),
              p("Describes the distribution of returns by measuring extreme values in either tail.
                A high kurtosis means investors will occasionally experience extreme 
                high or low returns."),
              br(),
              h4("Skewness"),
              p("Skewness is all helps describe the returns distribution, in particular it's asymmetry in
                a symmetrical bell curve. If portfolio returns are skewed right, it implies numerous small negative 
                returns and a few large positive returns"),
              br(),
              h4("Log Returns"),
              p("Since each investment asset can have very different prices and returns, the log returns
                help compare different assets"),
              br(),
              h4("Component Contribution"),
              p("Measures the amount of risk that each asset contributes to overall portfolio volatility"),
              br(),
              h4("Covariance"),
              p("Convariance tells if assets move in the same direction to each other (positive move together, negative move opposite)"),
              br(),
              h4("Correlation"),
              p("Correlation measures how strong the relationshiop is between two assets. Close to 1 is a strong positive relationship.")
              ),
            
            # 3.2 Links to Concepts ----
            column(
              width = 4,
              h3("Further Information on Concepts"),
              br(),
              fluidRow(
              tags$ul(
                tags$li(a(href = "https://www.investopedia.com/ask/answers/042815/what-difference-between-expected-return-and-standard-deviation-portfolio.asp","Standard Deviation", target = "_blank")),
                tags$li(a(href = "https://www.investopedia.com/terms/k/kurtosis.asp","Kurtosis", target = "_blank")),
                tags$li(a(href = "https://www.investopedia.com/terms/s/skewness.asp","Skewness", target = "_blank")),
                tags$li(a(href = "https://quantivity.wordpress.com/2011/02/21/why-log-returns/","Log Returns", target = "_blank")),
                tags$li(a(href = "https://rviews.rstudio.com/2017/09/13/asset-contribution-to-portfolio-volatility/","Component Contribution", target = "_blank")),
                tags$li(a(href = "https://www.investopedia.com/terms/c/covariance.asp","Covariance", target = "_blank")),
                tags$li(a(href = "https://www.investopedia.com/terms/c/correlation.asp","Correlation", target = "_blank"))
              )
                ),
              fluidRow(
                h3("Code"),
                br(),
                div(
                  p("To see the code for this Shiny app, please visit my ",a(href = "https://github.com/chrisselig/stock_portfolio_analysis_dashboard","Github page!")),
                  
                )
                )
              
              )
          )
        )
      )
    ),
  )

# Server Logic ----
server <- function(input, output,session) {
  
  # 1.0 Get List of Stocks and Weights ----
  stock_symbols <- eventReactive(input$btn_calculate,{
    symbols <- c(input$input_stock1, input$input_stock2, input$input_stock3, input$input_stock4, input$input_stock5,input$input_market)
    weights <- c(input$input_stock1_weight/100,input$input_stock2_weight/100,input$input_stock3_weight/100,input$input_stock4_weight/100,input$input_stock5_weight/100,100/100)
    df <- data.frame(symbols, weights) %>% 
      na.omit()
  },ignoreNULL = FALSE)
  
  # 1.1 Get Weights ----
  stock_weights_tbl <- reactive({
    stock_symbols()
  })
  
  # 1.2 Get Stock Prices ----
  stock_prices <- reactive({
    get_stock_data_function(stock_symbols(), startDate = input$input_start_date, endDate = input$input_end_date)
  })
  
  # 1.3 Tidy Stock Prices & Calculate Log Returns ----
  returns_tbl <- reactive({
    calculate_returns_function(stock_prices(), weights_tbl = stock_weights_tbl(), timePeriod = input$input_timePeriod)
  })
  
  # 1.4 Reset to Defaults ----
  observeEvent(eventExpr = input$btn_reset_defaults, handlerExpr = {
    
    # Stock 1
    updateTextInput(session = session, inputId = "input_stock1", value = "AAPL")
    updateNumericInput(session = session, inputId = "input_stock1_weight", value = 25)
    
    # Stock 2
    updateTextInput(session = session, inputId = "input_stock2", value = "TSLA")
    updateNumericInput(session = session, inputId = "input_stock2_weight", value = 25)
    
    # Stock 3
    updateTextInput(session = session, inputId = "input_stock3", value = "QLD")
    updateNumericInput(session = session, inputId = "input_stock3_weight", value = 50)
    
    # Stock 4
    updateTextInput(session = session, inputId = "input_stock4", value = NA_character_)
    updateNumericInput(session = session, inputId = "input_stock4_weight", value = NA_integer_)
    
    # Stock 5
    updateTextInput(session = session, inputId = "input_stock5", value = NA_character_)
    updateNumericInput(session = session, inputId = "input_stock5_weight", value = NA_integer_)
    
    delay(ms = 300, expr = {
      click(id = "apply")
    })
    
  })
  
  # 2.0 Portfolio Analysis Tab ----
  # 2.1 KPI's ----
  
  
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
      component_contribution_function(stock_weights_tbl(), marketAsset = input$input_market) %>% 
      bar_chart_function(title = "Component Contribution to Standard Deviation")
  })
  
  
  # output$stock_weights <- renderTable(stock_weights_tbl())
  output$compContBar <- renderPlotly(component_contribution())
  # output$compContbar <- renderTable(component_contribution())
  
  
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
  url1 <- reactive({
    a(input$input_stock1, href=paste0("https://finance.yahoo.com/quote/",input$input_stock1), target = '_blank')
    
  })
  
  output$links1 <- renderUI({tagList(url1())})
  
  url2 <- reactive({
    a(input$input_stock2, href=paste0("https://finance.yahoo.com/quote/",input$input_stock2), target = '_blank')
    
  })
  
  output$links2 <- renderUI({tagList(url2())})
  
  url3 <- reactive({
    a(input$input_stock3, href=paste0("https://finance.yahoo.com/quote/",input$input_stock3), target = '_blank')
    
  })
  
  output$links3 <- renderUI({tagList(url3())})
  
  url4 <- reactive({
    a(input$input_stock4, href=paste0("https://finance.yahoo.com/quote/",input$input_stock4), target = '_blank')
    
  })
  
  output$links4 <- renderUI({tagList(url4())})
  
  url5 <- reactive({
    a(input$input_stock5, href=paste0("https://finance.yahoo.com/quote/",input$input_stock5), target = '_blank')
    
  })
  
  output$links5 <- renderUI({tagList(url5())})
  # output$value <- renderText({input$input_stock1})
  # # Show/Hide Filter Bar ----
  # observeEvent(input$filter_selector, {
  #     shinyjs::toggle(id = "filter-panel", anim = TRUE, animType = "slide")
  # })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
