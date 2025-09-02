# ADHD New Zealand Research Data Dashboard

This is an interactive data dashboard built with Shiny for analyzing and visualizing ADHD New Zealand Online Research Survey data.

## Features

### 📊 Data Overview
- **Home Statistics**: Display key indicators such as total respondents, average age, survey completion rate
- **Data Table**: Interactive data table with filtering and search capabilities
- **Data Quality Analysis**: Missing value analysis, data type distribution, etc.

### 📈 Statistical Analysis
- **Descriptive Statistics**: Multiple visualization methods including histograms, box plots, density plots
- **Correlation Analysis**: Pearson correlation coefficient calculation and scatter plots
- **Group Comparison**: Statistical methods including t-tests, ANOVA, non-parametric tests

### 🗺️ Geographic Distribution
- **Interactive Map**: New Zealand map based on Leaflet
- **Geographic Markers**: Display data collection points and distribution

## Installation and Running

### System Requirements
- R (version 3.6.0 or higher)
- RStudio (recommended)

### Package Installation
```r
# Install necessary R packages
install.packages(c("shiny", "shinydashboard", "readxl", "dplyr", "plotly", 
                   "DT", "leaflet", "sf", "ggplot2", "tidyr"))
```

### Running the Application

#### Method 1: Local Access
1. Ensure the `ADHD National Online Research Survey (Responses) - Rangiwai (R).xlsx` file is in the same directory
2. Run in R:
```r
source("run_app.R")
```

#### Method 2: Network Access (Recommended)
```r
source("run_network.R")
```
This will display local and LAN access addresses.

#### Method 3: Custom Deployment
```r
source("deploy_app.R")
```
Provides multiple deployment options.

#### Method 4: Direct Run (Recommended, consistent with app_previous.R)
```r
source("run_simple.R")
```
Or:
```r
shiny::runApp()
```

## Data Format Requirements

The application expects an Excel file containing the following types of columns:

### Demographic Information
- Age
- Gender
- Region
- Education Level

### ADHD-related Indicators
- Symptom Scores
- Diagnosis Information
- Treatment History

### Quality of Life Indicators
- Quality of Life Scores
- Functional Assessment
- Social Support

## Usage Instructions

### 1. Home Page
- View data overview and key statistical information
- Understand research background and objectives

### 2. Data Overview
- **Data Table**: Browse and filter raw data
- **Data Quality**: Check data completeness and quality

### 3. Statistical Analysis
- **Descriptive Statistics**: Select variables and chart types for analysis
- **Correlation Analysis**: Explore relationships between variables
- **Group Comparison**: Perform statistical tests and comparative analysis

### 4. Geographic Distribution
- View geographic distribution of data
- Click on map markers to get detailed information

## Customization and Extension

### Adding New Analysis Features
1. Add new `renderPlotly` or `renderDataTable` in the `server` function
2. Add corresponding input controls in the `ui`
3. Update the sidebar menu

### Modifying Data Source
1. Update the filename in the `read_excel()` function
2. Adjust column name mapping based on actual data structure
3. Modify the data cleaning function

### Adding New Visualizations
```r
# Example: Adding a new chart
output$new_plot <- renderPlotly({
  # Your plotting code
  plot_ly(data, x = ~x_var, y = ~y_var, type = 'scatter')
})
```

## Network Access Instructions

### Default Access Address
When using `shiny::runApp()`, Shiny will automatically:
- Select an available port (usually 3838, 3839, 3840, etc.)
- Display the access address in the console
- Automatically open the application in the browser

### Access Address Examples
- **Local Access**: `http://127.0.0.1:3838` or `http://localhost:3838`
- **LAN Access**: Requires manual specification of `host = "0.0.0.0"`

### Firewall Settings
If unable to access from other devices, please check:
1. Whether Windows firewall allows port 3838
2. Whether router settings block this port
3. Ensure devices are on the same network

## Troubleshooting

### Common Issues

1. **Data Reading Errors**
   - Check Excel file path and name
   - Ensure correct file format

2. **Package Dependency Issues**
   - Run `install.packages()` to install missing packages
   - Check R version compatibility

3. **Insufficient Memory**
   - Reduce data volume or optimize data processing
   - Increase R memory limits

4. **Network Access Issues**
   - Check firewall settings
   - Confirm port 3838 is not occupied
   - Try using different port numbers

### Debug Mode
```r
# Enable debug mode
options(shiny.trace = TRUE)
shiny::runApp()
```

## Contact Information

For questions or suggestions, please contact the research team.

## License

This project is for research use only.
