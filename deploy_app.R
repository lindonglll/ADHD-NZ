# ADHD New Zealand Research Data Dashboard - Deployment Script

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

# Get local IP address
get_local_ip <- function() {
  if (Sys.info()["sysname"] == "Windows") {
    ip <- system("ipconfig", intern = TRUE)
    ip <- grep("IPv4", ip, value = TRUE)
    ip <- gsub(".*: ", "", ip)
    ip <- ip[1]
  } else {
    ip <- system("hostname -I", intern = TRUE)
    ip <- strsplit(ip, " ")[[1]][1]
  }
  return(ip)
}

# Deployment options
cat("Select deployment method:\n")
cat("1. Local access (localhost)\n")
cat("2. LAN access (local IP)\n")
cat("3. Custom port and host\n")
cat("4. Production deployment\n")

choice <- readline("Enter your choice (1-4): ")

if (choice == "1") {
  # Local access
  cat("Application will run locally: http://localhost:3838\n")
  shiny::runApp(host = "127.0.0.1", port = 3838)
  
} else if (choice == "2") {
  # LAN access
  local_ip <- get_local_ip()
  cat("Application will run on LAN: http://", local_ip, ":3838\n", sep = "")
  cat("Other devices can access the application via this address\n")
  shiny::runApp(host = "0.0.0.0", port = 3838)
  
} else if (choice == "3") {
  # Custom port and host
  custom_host <- readline("Enter host address (default: 0.0.0.0): ")
  if (custom_host == "") custom_host <- "0.0.0.0"
  
  custom_port <- readline("Enter port number (default: 3838): ")
  if (custom_port == "") custom_port <- 3838 else custom_port <- as.numeric(custom_port)
  
  cat("Application will run at: http://", custom_host, ":", custom_port, "\n", sep = "")
  shiny::runApp(host = custom_host, port = custom_port)
  
} else if (choice == "4") {
  # Production deployment
  cat("Production deployment options:\n")
  cat("1. Use shiny-server\n")
  cat("2. Use Docker\n")
  cat("3. Deploy to shinyapps.io\n")
  
  prod_choice <- readline("Select deployment method (1-3): ")
  
  if (prod_choice == "1") {
    cat("Please copy application files to shiny-server directory\n")
    cat("Typical path: /srv/shiny-server/adhd-dashboard/\n")
    cat("Then restart shiny-server service\n")
  } else if (prod_choice == "2") {
    cat("Creating Dockerfile...\n")
    # Dockerfile generation can be added here
    cat("Docker deployment requires additional configuration files\n")
  } else if (prod_choice == "3") {
    cat("Deploying to shinyapps.io requires:\n")
    cat("1. Register shinyapps.io account\n")
    cat("2. Install rsconnect package\n")
    cat("3. Configure account information\n")
    cat("4. Run deployApp()\n")
  }
  
} else {
  cat("Invalid choice, using default local deployment\n")
  shiny::runApp(host = "127.0.0.1", port = 3838)
}
