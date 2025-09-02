# Simple startup script - Completely consistent with app_previous.R

# Check if data file exists
data_file <- "ADHD National Online Research Survey (Responses) - Rangiwai (R).xlsx"
if (!file.exists(data_file)) {
  stop("Error: Cannot find data file '", data_file, "'. Please ensure the file is in the current directory.")
}

# Load necessary packages
library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(plotly)
library(DT)
library(leaflet)
library(sf)
library(ggplot2)
library(tidyr)

cat("Starting ADHD New Zealand Research Data Dashboard...\n")
cat("Data file:", data_file, "\n")
cat("Application will open in browser.\n")

# Directly run application (completely consistent with app_previous.R)
shiny::runApp()
