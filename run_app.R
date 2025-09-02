# ADHD New Zealand Research Data Dashboard - Startup Script

# Check if data file exists
data_file <- "ADHD National Online Research Survey (Responses) - Rangiwai (R).xlsx"
if (!file.exists(data_file)) {
  stop("Error: Cannot find data file '", data_file, "'. Please ensure the file is in the current directory.")
}

# Check required packages
required_packages <- c("shiny", "shinydashboard", "readxl", "dplyr", "plotly", "DT", "leaflet", "sf", "ggplot2", "tidyr")
missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

if (length(missing_packages) > 0) {
  cat("Missing packages:", paste(missing_packages, collapse = ", "), "\n")
  cat("Please run install_packages.R first to install dependencies.\n")
  stop("Missing dependencies")
}

# Load packages
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

# Start application (using default configuration, consistent with app_previous.R)
shiny::runApp()
