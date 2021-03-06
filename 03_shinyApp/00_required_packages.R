# Script is used to install missing packages ----

required_packages <- c(
    "checkpoint"
)

# install missing packages

new.packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

if (length(new.packages)) {
    install.packages(new.packages)
}

rm(new.packages)

library(checkpoint)
checkpoint(snapshotDate ='2020-02-09')
# App and UI packages
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinyjs)
# Data manipulation packages
library(quantmod)
library(tidyverse)
library(tibbletime)
library(lubridate)
library(scales)
library(e1071)
# Plotting packages
library(ggthemes)
library(ggsci)
library(plotly)