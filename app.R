library(shiny)
library(leaflet)
library(readxl)
library(dplyr)
library(sf)
library(shinydashboard)
library(plotly)
library(DT)
library(ggplot2)
library(tidyr)

# Read ADHD dataset
adhd_data <- read_excel("ADHD National Online Research Survey (Responses) - Rangiwai (R).xlsx")

# Data cleaning and preprocessing
# Assuming data contains the following columns (adjust based on actual data)
# 1. Demographic information (age, gender, region, etc.)
# 2. ADHD-related symptom scores
# 3. Quality of life indicators
# 4. Treatment-related information

# Data cleaning function
clean_adhd_data <- function(data) {
  # Remove completely empty rows
  data <- data[!apply(data, 1, function(x) all(is.na(x))), ]
  
  # Handle missing values
  data <- data %>%
    mutate(across(everything(), ~ifelse(. == "", NA, .)))
  
  return(data)
}

# Clean data
adhd_clean <- clean_adhd_data(adhd_data)

# Get numeric columns (for statistical analysis)
numeric_cols <- names(adhd_clean)[sapply(adhd_clean, is.numeric)]
categorical_cols <- names(adhd_clean)[sapply(adhd_clean, function(x) is.character(x) || is.factor(x))]

# Safe numeric conversion function
safe_numeric <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return(0)
  as.numeric(x)
}

# UI definition
ui <- dashboardPage(
  dashboardHeader(title = "ADHD New Zealand Research Data Dashboard"),
  dashboardSidebar(
    tags$head(tags$style(HTML(
      ".main-sidebar { background-color: #ffffff !important; }"
    ))),
    sidebarMenu(
      menuItem("Home", tabName = "tab-home", icon = icon("home")),
      menuItem("Data Overview", tabName = "tab-overview", icon = icon("table")),
      menuItem("Statistical Analysis", tabName = "tab-analysis", icon = icon("chart-bar")),
      menuItem("Geographic Distribution", tabName = "tab-map", icon = icon("globe"))
    ),
    br(),
    div(style = "position: absolute; bottom: 200px; left: 10px; right: 10px; font-size: 16px; border: 1px solid #ccc; padding: 10px; background-color: #ffffff; border-radius: 5px;",
        HTML("<span style='color: black;'>ADHD New Zealand Online Research Survey<br><br>
              Last Updated:<br>2025<br><br>
              Contact Email:<br><span style='font-size: 13px; color: black;'>Research Team</span></span>")
    )
  ),
  dashboardBody(
    tabItems(
      # Home page
      tabItem(tabName = "tab-home",
              fluidPage(
                div(style = "border: 2px solid #ccc; border-radius: 10px; padding: 30px; background-color: #fefefe; box-shadow: 2px 2px 10px rgba(0,0,0,0.1);",
                    
                    h2("ADHD New Zealand Online Research Survey Data Dashboard"),
                    
                    h4("📘 Data Source"),
                    p("This dashboard is built based on ADHD New Zealand Online Research Survey data.",
                      "The survey aims to understand the current situation, needs, and challenges of ADHD patients in New Zealand."),
                    
                    br(),
                    
                    h4("👥 Target Population"),
                    p("Survey participants include:"),
                    tags$ul(
                      tags$li("ADHD patients"),
                      tags$li("Family members of ADHD patients"),
                      tags$li("Healthcare providers"),
                      tags$li("Educators")
                    ),
                    
                    br(),
                    
                    h4("🎯 Research Objectives"),
                    tags$ul(
                      tags$li(em("Understand the distribution and characteristics of ADHD patients in New Zealand")),
                      tags$li(em("Assess the impact of ADHD on patients' quality of life")),
                      tags$li(em("Analyze accessibility of treatment and services")),
                      tags$li(em("Identify opportunities to improve ADHD care"))
                    ),
                    
                    br(),
                    
                    h4("📊 Data Overview"),
                    fluidRow(
                      column(4,
                             div(style = "text-align: center; padding: 20px; background-color: #e8f4fd; border-radius: 10px;",
                                 h3(textOutput("total_respondents")),
                                 p("Total Respondents")
                             )
                      ),
                      column(4,
                             div(style = "text-align: center; padding: 20px; background-color: #f0f8e8; border-radius: 10px;",
                                 h3(textOutput("avg_age")),
                                 p("Average Age")
                             )
                      ),
                      column(4,
                             div(style = "text-align: center; padding: 20px; background-color: #fff8e8; border-radius: 10px;",
                                 h3(textOutput("completion_rate")),
                                 p("Survey Completion Rate")
                             )
                      )
                    )
                )
              )
      ),
      
      # Data overview page
      tabItem(tabName = "tab-overview",
              fluidPage(
                tabsetPanel(id = "overview_mode", type = "tabs",
                            
                            tabPanel("Data Table",
                                     div(style = "text-align: center;",
                                         h2("ADHD Research Data Overview")
                                     ),
                                     fluidRow(
                                       column(4,
                                              selectInput("filter_column", "Select Filter Column:", 
                                                         choices = c("All", categorical_cols))
                                       ),
                                       column(4,
                                              uiOutput("filter_value_selector")
                                       ),
                                       column(4,
                                              downloadButton("download_data", "Download Data")
                                       )
                                     ),
                                     fluidRow(
                                       column(12,
                                              dataTableOutput("data_table")
                                       )
                                     )
                            ),
                            
                            tabPanel("Data Quality",
                                     fluidRow(
                                       column(6,
                                              h3("Missing Value Analysis"),
                                              plotlyOutput("missing_plot")
                                       ),
                                       column(6,
                                              h3("Data Type Distribution"),
                                              plotlyOutput("data_type_plot")
                                       )
                                     ),
                                     fluidRow(
                                       column(12,
                                              h3("Data Quality Report"),
                                              verbatimTextOutput("quality_report")
                                       )
                                     )
                            )
                )
              )
      ),
      
      # Statistical analysis page
      tabItem(tabName = "tab-analysis",
              fluidPage(
                tabsetPanel(id = "analysis_mode", type = "tabs",
                            
                            tabPanel("Descriptive Statistics",
                                     sidebarLayout(
                                       sidebarPanel(width = 3,
                                                    selectInput("analysis_variable", "Select Analysis Variable:", 
                                                               choices = numeric_cols),
                                                    selectInput("group_by_var", "Group Variable (Optional):", 
                                                               choices = c("None", categorical_cols)),
                                                    radioButtons("plot_type", "Chart Type:",
                                                                 choices = c("Histogram" = "histogram", 
                                                                            "Box Plot" = "boxplot",
                                                                            "Density Plot" = "density"))
                                       ),
                                       mainPanel(
                                         div(style = "height: calc(100vh - 100px);",
                                             plotlyOutput("analysis_plot", height = "100%")
                                         )
                                       )
                                     )
                            ),
                            
                            tabPanel("Correlation Analysis",
                                     fluidRow(
                                       column(4,
                                              selectInput("corr_var1", "Variable 1:", choices = numeric_cols),
                                              selectInput("corr_var2", "Variable 2:", choices = numeric_cols)
                                       ),
                                       column(8,
                                              h3("Correlation Analysis Results"),
                                              verbatimTextOutput("correlation_result"),
                                              plotlyOutput("correlation_plot")
                                       )
                                     )
                            ),
                            
                            tabPanel("Group Comparison",
                                     sidebarLayout(
                                       sidebarPanel(width = 3,
                                                    selectInput("compare_var", "Comparison Variable:", choices = numeric_cols),
                                                    selectInput("group_var", "Group Variable:", choices = categorical_cols),
                                                    radioButtons("test_type", "Statistical Test:",
                                                                 choices = c("T-Test" = "t_test", 
                                                                            "ANOVA" = "anova",
                                                                            "Non-parametric Test" = "wilcox"))
                                       ),
                                       mainPanel(
                                         div(style = "height: calc(100vh - 100px);",
                                             plotlyOutput("comparison_plot", height = "100%")
                                         )
                                       )
                                     )
                            )
                )
              )
      ),
      
      # Geographic distribution page
      tabItem(tabName = "tab-map",
              fluidPage(
                tags$head(tags$style(HTML("
                  .floating-box {
                    position: absolute;
                    top: 17vh;
                    left: 15vw;
                    width: 40vw;
                    height: 80vh;
                    z-index: 999;
                    background: white;
                    border: 1px solid #aaa;
                    padding: 10px;
                    box-shadow: 2px 2px 5px rgba(0,0,0,0.3);
                    overflow-y: auto;
                    border-radius: 8px;
                  }
                "))),
                div(style = "height: calc(100vh - 50px);",
                    leafletOutput("nz_map", height = "100%")
                ),
                uiOutput("map_info_box")
              )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Home page statistics
  output$total_respondents <- renderText({
    nrow(adhd_clean)
  })
  
  output$avg_age <- renderText({
    # Assuming there's an age column, adjust based on actual data
    age_col <- names(adhd_clean)[grepl("age", names(adhd_clean), ignore.case = TRUE)]
    if (length(age_col) > 0) {
      avg_age <- mean(as.numeric(adhd_clean[[age_col[1]]]), na.rm = TRUE)
      round(avg_age, 1)
    } else {
      "N/A"
    }
  })
  
  output$completion_rate <- renderText({
    # Calculate survey completion rate
    total_questions <- ncol(adhd_clean)
    completion_rates <- apply(adhd_clean, 1, function(row) {
      sum(!is.na(row)) / total_questions
    })
    paste0(round(mean(completion_rates, na.rm = TRUE) * 100, 1), "%")
  })
  
  # Data table
  output$filter_value_selector <- renderUI({
    if (input$filter_column != "All") {
      unique_values <- unique(adhd_clean[[input$filter_column]])
      selectInput("filter_value", "Select Value:", choices = c("All", unique_values))
    }
  })
  
  filtered_data <- reactive({
    data <- adhd_clean
    if (input$filter_column != "All" && !is.null(input$filter_value) && input$filter_value != "All") {
      data <- data[data[[input$filter_column]] == input$filter_value, ]
    }
    data
  })
  
  output$data_table <- renderDataTable({
    datatable(filtered_data(), 
              options = list(scrollX = TRUE, pageLength = 10),
              filter = "top")
  })
  
  # Download data
  output$download_data <- downloadHandler(
    filename = function() {
      paste("adhd_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  # Missing value analysis
  output$missing_plot <- renderPlotly({
    missing_counts <- colSums(is.na(adhd_clean))
    missing_percent <- (missing_counts / nrow(adhd_clean)) * 100
    
    plot_ly(x = names(missing_percent), y = missing_percent, type = 'bar',
            marker = list(color = '#ff7f0e')) %>%
      layout(title = "Missing Value Percentage by Column",
             xaxis = list(title = "Variable Name", tickangle = 45),
             yaxis = list(title = "Missing Value Percentage (%)"))
  })
  
  # Data type distribution
  output$data_type_plot <- renderPlotly({
    data_types <- sapply(adhd_clean, class)
    type_counts <- table(data_types)
    
    plot_ly(labels = names(type_counts), values = type_counts, type = 'pie') %>%
      layout(title = "Data Type Distribution")
  })
  
  # Data quality report
  output$quality_report <- renderPrint({
    cat("Data Quality Report\n")
    cat("==================\n\n")
    cat("Total Rows:", nrow(adhd_clean), "\n")
    cat("Total Columns:", ncol(adhd_clean), "\n")
    cat("Numeric Variables:", length(numeric_cols), "\n")
    cat("Categorical Variables:", length(categorical_cols), "\n")
    cat("Total Missing Values:", sum(is.na(adhd_clean)), "\n")
    cat("Missing Value Percentage:", round(sum(is.na(adhd_clean)) / (nrow(adhd_clean) * ncol(adhd_clean)) * 100, 2), "%\n")
  })
  
  # Descriptive statistical analysis
  output$analysis_plot <- renderPlotly({
    req(input$analysis_variable)
    
    var_data <- adhd_clean[[input$analysis_variable]]
    var_data <- as.numeric(var_data)
    var_data <- var_data[!is.na(var_data)]
    
    if (input$plot_type == "histogram") {
      plot_ly(x = var_data, type = 'histogram', nbinsx = 30) %>%
        layout(title = paste("Distribution Plot:", input$analysis_variable),
               xaxis = list(title = input$analysis_variable),
               yaxis = list(title = "Frequency"))
    } else if (input$plot_type == "boxplot") {
      plot_ly(y = var_data, type = 'box') %>%
        layout(title = paste("Box Plot:", input$analysis_variable),
               yaxis = list(title = input$analysis_variable))
    } else if (input$plot_type == "density") {
      density_data <- density(var_data)
      plot_ly(x = density_data$x, y = density_data$y, type = 'scatter', mode = 'lines') %>%
        layout(title = paste("Density Plot:", input$analysis_variable),
               xaxis = list(title = input$analysis_variable),
               yaxis = list(title = "Density"))
    }
  })
  
  # Correlation analysis
  output$correlation_result <- renderPrint({
    req(input$corr_var1, input$corr_var2)
    
    var1 <- as.numeric(adhd_clean[[input$corr_var1]])
    var2 <- as.numeric(adhd_clean[[input$corr_var2]])
    
    # Remove missing values
    complete_cases <- complete.cases(var1, var2)
    var1 <- var1[complete_cases]
    var2 <- var2[complete_cases]
    
    if (length(var1) > 0) {
      cor_result <- cor.test(var1, var2)
      cat("Pearson Correlation Coefficient:", round(cor_result$estimate, 3), "\n")
      cat("P-value:", round(cor_result$p.value, 4), "\n")
      cat("95% Confidence Interval:", round(cor_result$conf.int, 3), "\n")
    } else {
      cat("Insufficient data to calculate correlation")
    }
  })
  
  output$correlation_plot <- renderPlotly({
    req(input$corr_var1, input$corr_var2)
    
    var1 <- as.numeric(adhd_clean[[input$corr_var1]])
    var2 <- as.numeric(adhd_clean[[input$corr_var2]])
    
    complete_cases <- complete.cases(var1, var2)
    var1 <- var1[complete_cases]
    var2 <- var2[complete_cases]
    
    if (length(var1) > 0) {
      plot_ly(x = var1, y = var2, type = 'scatter', mode = 'markers') %>%
        layout(title = paste("Correlation Scatter Plot:", input$corr_var1, "vs", input$corr_var2),
               xaxis = list(title = input$corr_var1),
               yaxis = list(title = input$corr_var2))
    }
  })
  
  # Group comparison
  output$comparison_plot <- renderPlotly({
    req(input$compare_var, input$group_var)
    
    compare_data <- adhd_clean %>%
      select(!!sym(input$compare_var), !!sym(input$group_var)) %>%
      filter(!is.na(!!sym(input$compare_var)), !is.na(!!sym(input$group_var)))
    
    if (input$plot_type == "boxplot") {
      plot_ly(compare_data, x = ~get(input$group_var), y = ~get(input$compare_var), 
              type = 'box', color = ~get(input$group_var)) %>%
        layout(title = paste("Group Comparison:", input$compare_var, "by", input$group_var),
               xaxis = list(title = input$group_var),
               yaxis = list(title = input$compare_var))
    } else {
      # Default display box plot
      plot_ly(compare_data, x = ~get(input$group_var), y = ~get(input$compare_var), 
              type = 'box', color = ~get(input$group_var)) %>%
        layout(title = paste("Group Comparison:", input$compare_var, "by", input$group_var),
               xaxis = list(title = input$group_var),
               yaxis = list(title = input$compare_var))
    }
  })
  
  # Map related (simplified version)
  output$nz_map <- renderLeaflet({
    leaflet() %>%
      setView(lng = 174.7645, lat = -40.9006, zoom = 5) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addMarkers(lng = 174.7645, lat = -40.9006, 
                 popup = "New Zealand ADHD Research Data Collection Point")
  })
  
  output$map_info_box <- renderUI({
    absolutePanel(class = "floating-box", draggable = TRUE,
                  h4("Geographic Distribution Information"),
                  p("Geographic distribution analysis of ADHD research data will be displayed here."),
                  p("Click on map markers to view detailed information.")
    )
  })
}

# Run application (using default configuration, consistent with app_previous.R)
shinyApp(ui, server)
