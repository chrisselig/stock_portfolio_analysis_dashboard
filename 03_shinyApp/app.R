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
            
            # 1.3 Portfolio Simulation ----
            column(
                width = 8
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
                                inputId = "input_stock1_symbol",
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
                                inputId = "input_stock2_symbol",
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
                                inputId = "input_stock3_symbol",
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
                                inputId = "input_stock4_symbol",
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
                                inputId = "input_stock5_symbol",
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

# Define server logic required to draw a histogram
server <- function(input, output) {


    
}

# Run the application 
shinyApp(ui = ui, server = server)
