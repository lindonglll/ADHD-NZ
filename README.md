# ADHD New Zealand Research Survey Dashboard

This is an interactive Shiny application for analyzing and visualizing ADHD New Zealand Online Research Survey data. The application provides a streamlined interface for exploring survey responses with faceted visualizations and supports both Likert scales and many-to-many relationships.

## Features

### 📊 Interactive Data Visualization
- **Single Figure with Facets**: Clean, focused visualization with demographic faceting
- **Multiple Variable Groups**: Support for treatment, challenges, support systems, and diagnosis data
- **Flexible Display Modes**: Toggle between percentage and count displays
- **Dynamic Faceting**: Analyze data by age group, gender, ethnicity, or region

### 📈 Data Analysis Capabilities
- **Binary Group Analysis**: Handle 0/1 indicator variables for ADHD-related factors
- **Likert Scale Support**: Process both numeric (1-5) and text-based five-level scales
- **Single Column Analysis**: Support for categorical, text, and numeric variables
- **Cross-tabulation**: Calculate proportions and counts by demographic facets

### 🎯 Specialized ADHD Research Features
- **Treatment Analysis**: Medication and therapy usage patterns
- **Challenge Assessment**: Time management, focus, impulsivity, organization, memory
- **Support System Evaluation**: Family, friends, healthcare providers, community groups
- **Diagnosis Tracking**: Formal diagnosis, wait times, diagnostic systems

## Installation and Running

### System Requirements
- R (version 3.6.0 or higher)
- RStudio (recommended)

### Package Installation
```r
# Install necessary R packages
install.packages(c("shiny", "shinydashboard", "readxl", "dplyr", "ggplot2", 
                   "tidyr", "scales", "forcats", "stringr", "tidyselect", "rlang"))
```

### Running the Application

#### Method 1: Direct Run (Recommended)
1. Ensure the `ADHD National Online Research Survey (Responses) - Rangiwai (R).xlsx` file is in the same directory
2. Run in R:
```r
source("app.R")
```

#### Method 2: Using Shiny RunApp
```r
shiny::runApp("app.R")
```

#### Method 3: Network Access
To enable network access from other devices:
```r
shiny::runApp("app.R", host = "0.0.0.0", port = 3838)
```
This will display both local and LAN access addresses.

## Data Format Requirements

The application expects an Excel file with a "Main" sheet containing the following types of columns:

### Demographic Information
- **Age**: Standardized age ranges (18-24, 25-34, 35-44, 45-54, 55+)
- **Gender**: Gender categories
- **Region**: Geographic regions
- **Ethnicity**: Māori, European, Asian, MELAA, Pacific Peoples, Other Ethnicity

### ADHD-related Variable Groups (Binary 0/1 indicators)
- **Treatment**: medicator, Medication, therapy, Therapy
- **Challenges**: Time_mgmt, Time_mgnt, Time management, Focus, Impulsivity, Organisation, Memory
- **Support Systems**: Family, Friends, Healthcare providers, Community groups, Online resources
- **Diagnosis**: Formal_Diagnosis, Wait_List, Diagnosis_System

### Impact Variables (Likert scales)
- **Education_Effect**: Impact on education (1-5 scale or text)
- **Occupation_Effect**: Impact on occupation (1-5 scale or text)
- **Social_Effect**: Impact on social life (1-5 scale or text)
- **Matauranga**: Māori knowledge/education impact (1-5 scale or text)
- **Support_effect**: Support system effectiveness (1-5 scale or text)

### Additional Single Columns
- **Formal_Diagnosis**: Formal diagnosis status
- **Diagnosis_Age**: Age at diagnosis
- **Diagnosis_Who**: Who provided the diagnosis
- **Diagnosis_System**: Diagnostic system used
- **Wait_List**: Wait list information
- **Treatment_Effect**: Treatment effectiveness (Likert scale)

## Usage Instructions

### 1. Variable Selection
- **ADHD-related group**: Choose from treatment, challenges, support systems, diagnosis, or impact variables
- **Impact column**: When selecting impact variables, choose specific columns like Education_Effect, Occupation_Effect, etc.
- **Display mode**: Toggle between percentage and count displays

### 2. Faceting Options
- **None**: View overall data without demographic breakdown
- **Age group**: Analyze by age ranges (18-24, 25-34, 35-44, 45-54, 55+)
- **Gender**: Compare across gender categories
- **Ethnicity**: Analyze by ethnic groups (Māori, European, Asian, MELAA, Pacific Peoples, Other)
- **Region**: Compare across geographic regions

### 3. Data Visualization
- **Horizontal bar charts**: Clean, readable visualization with percentage/count labels
- **Faceted displays**: When faceting is enabled, charts are split by demographic groups
- **Dynamic titles**: Chart titles automatically update based on selected variables and faceting

### 4. Data Processing Features
- **Automatic data cleaning**: Handles missing values, standardizes text responses
- **Binary normalization**: Converts various formats (yes/no, 1/0, true/false) to standardized 0/1
- **Likert scale processing**: Handles both numeric (1-5) and text-based five-level scales
- **Many-to-many relationships**: Supports complex survey responses where participants can select multiple options

## Customization and Extension

### Adding New Variable Groups
1. Add new column groups in the "Column Groups" section:
```r
new_group_cols <- intersect(c("Column1", "Column2", "Column3"), all_cols)
```

2. Add corresponding options in the sidebar input:
```r
"New Group" = "__NEW_GROUP__"
```

3. Update the server logic to handle the new group:
```r
"__NEW_GROUP__" = new_group_cols
```

### Modifying Data Source
1. Update the Excel file path in the `xlsx_path` variable
2. Adjust column name mapping in the variable group definitions
3. Modify the data cleaning functions as needed

### Adding New Faceting Options
1. Add new demographic variables to the `demo_long()` function
2. Update the sidebar faceting options
3. Add corresponding server logic for the new facet

### Customizing Visualizations
The application uses a unified plotting function `plot_cross_group()`. To modify:
1. Update the `plot_cross_group()` function for styling changes
2. Modify the `ggplot2` theme and aesthetics as needed
3. Adjust text wrapping and label formatting

## Application Architecture

### Code Structure
The application follows a modular design with clear separation of concerns:

1. **Data Loading and Preprocessing**: Handles Excel file reading and data cleaning
2. **Variable Grouping**: Organizes survey variables into logical groups
3. **Utility Functions**: Provides data transformation and analysis functions
4. **User Interface**: Clean, focused interface with sidebar controls
5. **Server Logic**: Handles user interactions and generates visualizations

### Key Functions
- `norm01()`: Binary normalization for 0/1 indicators
- `coerce_likert_1to5()`: Likert scale standardization
- `adhd_long()`: Converts binary groups to long format
- `demo_long()`: Handles demographic faceting
- `build_cross_prop_*()`: Calculate proportions and counts
- `plot_cross_group()`: Unified visualization function

### Data Processing Pipeline
1. **Load**: Read Excel file from "Main" sheet
2. **Clean**: Standardize column names, handle missing values
3. **Transform**: Convert variables to appropriate formats
4. **Group**: Organize variables into analysis groups
5. **Analyze**: Calculate proportions and counts by facets
6. **Visualize**: Generate interactive charts

## Troubleshooting

### Common Issues

1. **Data Reading Errors**
   - Ensure the Excel file `ADHD National Online Research Survey (Responses) - Rangiwai (R).xlsx` is in the same directory
   - Check that the file has a "Main" sheet
   - Verify file is not corrupted or password-protected

2. **Package Dependency Issues**
   - Install missing packages: `install.packages(c("shiny", "shinydashboard", "readxl", "dplyr", "ggplot2", "tidyr", "scales", "forcats", "stringr", "tidyselect", "rlang"))`
   - Check R version compatibility (3.6.0 or higher)

3. **Variable Selection Issues**
   - If no variables appear in dropdowns, check that column names match expected patterns
   - Verify that data contains the expected variable groups (treatment, challenges, support, etc.)

4. **Visualization Issues**
   - If charts don't display, check for missing data in selected variables
   - Ensure faceting variables (age, gender, ethnicity) contain valid values

5. **Performance Issues**
   - For large datasets, consider filtering data before analysis
   - Check available system memory

### Debug Mode
```r
# Enable debug mode for troubleshooting
options(shiny.trace = TRUE)
shiny::runApp("app.R")
```

## Contact Information

For questions or suggestions, please contact the research team.

## License

This project is for research use only.
