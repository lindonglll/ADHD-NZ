library(shiny)
library(leaflet)
library(readxl)
library(dplyr)
library(sf)
library(shinydashboard)
library(plotly)
library(DT)




# Define file paths for easier data replacement(map data and the research data are in the document file 'Data')
map_data <- "Data/Map/statsnz-2023-census-population-change-by-regional-council-SHP/2023-census-population-change-by-regional-council.shp" ## The Map Data
data <- "Data/NZ_data_regional_council.xlsx" ## The Dataset for Page 3: Geographical Display, Using Regional Council Data
data_health <- "Data/NZ_data_health_region.xlsx" ## The Dataset for Page 2: Plot, Using Health Region Data
data_health_age <- "Data/NZ_data_health_region_age.xlsx" ## The Dataset for Page 3: Table, Using Health Region Data with young and all age

# Map label and id(For the new map data, you need to put the region name to replace REGC2023_1)
map_label = ~REGC2023_1
map_layerId = ~REGC2023_1

### Data Cleaning #####################

## Data cleaning for regional_council ##
region_data <- read_excel(data, skip = 6)
region_data <- region_data[-1, ] 

# Delete unnecessary columns and rows
region_data <- region_data[, c(-2,-83)]
region_data <- region_data[c(-19, -20), ]

# Rename columns
colnames(region_data)[1] <- "Area"
colnames(region_data)[2:81] <- c("allage_household_total", "allage_household_male", "allage_household_female", "allage_household_another",
                                 "household_total", "household_male", "household_female", "household_another",
                                 "Euro_allage_household_total", "Euro_allage_household_male", "Euro_allage_household_female", "Euro_allage_household_another",
                                 "Euro_household_total", "Euro_household_male", "Euro_household_female", "Euro_household_another",
                                 "Maori_allage_household_total", "Maori_allage_household_male", "Maori_allage_household_female", "Maori_allage_household_another",
                                 "Maori_household_total", "Maori_household_male", "Maori_household_female", "Maori_household_another",
                                 "Paci_allage_household_total", "Paci_allage_household_male", "Paci_allage_household_female", "Paci_allage_household_another",
                                 "Paci_household_total", "Paci_household_male", "Paci_household_female", "Paci_household_another",
                                 "Asia_allage_household_total", "Asia_allage_household_male", "Asia_allage_household_female", "Asia_allage_household_another",
                                 "Asia_household_total", "Asia_household_male", "Asia_household_female", "Asia_household_another",
                                 
                                 "allage_not_household_total", "allage_not_household_male", "allage_not_household_female", "allage_not_household_another",
                                 "not_household_total", "not_household_male", "not_household_female", "not_household_another",
                                 "Euro_allage_not_household_total", "Euro_allage_not_household_male", "Euro_allage_not_household_female", "Euro_allage_not_household_another",
                                 "Euro_not_household_total", "Euro_not_household_male", "Euro_not_household_female", "Euro_not_household_another",
                                 "Maori_allage_not_household_total", "Maori_allage_not_household_male", "Maori_allage_not_household_female", "Maori_allage_not_household_another",
                                 "Maori_not_household_total", "Maori_not_household_male", "Maori_not_household_female", "Maori_not_household_another",
                                 "Paci_allage_not_household_total", "Paci_allage_not_household_male", "Paci_allage_not_household_female", "Paci_allage_not_household_another",
                                 "Paci_not_household_total", "Paci_not_household_male", "Paci_not_household_female", "Paci_not_household_another",
                                 "Asia_allage_not_household_total", "Asia_allage_not_household_male", "Asia_allage_not_household_female", "Asia_allage_not_household_another",
                                 "Asia_not_household_total", "Asia_not_household_male", "Asia_not_household_female", "Asia_not_household_another"
)
## Data cleaning for regional_council ##


## Data cleaning for health_region ##
health_region_data <- read_excel(data_health, skip = 6)
health_region_data <- health_region_data[-1, ] 

# Delete unnecessary columns and rows
health_region_data <- health_region_data[, c(-2,-83)]
health_region_data <- health_region_data[c(-25, -26, -27), ]


# Rename columns
colnames(health_region_data)[1] <- "Area"
colnames(health_region_data)[2:81] <- c("allage_household_total", "allage_household_male", "allage_household_female", "allage_household_another",
                                        "household_total", "household_male", "household_female", "household_another",
                                        "Euro_allage_household_total", "Euro_allage_household_male", "Euro_allage_household_female", "Euro_allage_household_another",
                                        "Euro_household_total", "Euro_household_male", "Euro_household_female", "Euro_household_another",
                                        "Maori_allage_household_total", "Maori_allage_household_male", "Maori_allage_household_female", "Maori_allage_household_another",
                                        "Maori_household_total", "Maori_household_male", "Maori_household_female", "Maori_household_another",
                                        "Paci_allage_household_total", "Paci_allage_household_male", "Paci_allage_household_female", "Paci_allage_household_another",
                                        "Paci_household_total", "Paci_household_male", "Paci_household_female", "Paci_household_another",
                                        "Asia_allage_household_total", "Asia_allage_household_male", "Asia_allage_household_female", "Asia_allage_household_another",
                                        "Asia_household_total", "Asia_household_male", "Asia_household_female", "Asia_household_another",
                                        
                                        "allage_not_household_total", "allage_not_household_male", "allage_not_household_female", "allage_not_household_another",
                                        "not_household_total", "not_household_male", "not_household_female", "not_household_another",
                                        "Euro_allage_not_household_total", "Euro_allage_not_household_male", "Euro_allage_not_household_female", "Euro_allage_not_household_another",
                                        "Euro_not_household_total", "Euro_not_household_male", "Euro_not_household_female", "Euro_not_household_another",
                                        "Maori_allage_not_household_total", "Maori_allage_not_household_male", "Maori_allage_not_household_female", "Maori_allage_not_household_another",
                                        "Maori_not_household_total", "Maori_not_household_male", "Maori_not_household_female", "Maori_not_household_another",
                                        "Paci_allage_not_household_total", "Paci_allage_not_household_male", "Paci_allage_not_household_female", "Paci_allage_not_household_another",
                                        "Paci_not_household_total", "Paci_not_household_male", "Paci_not_household_female", "Paci_not_household_another",
                                        "Asia_allage_not_household_total", "Asia_allage_not_household_male", "Asia_allage_not_household_female", "Asia_allage_not_household_another",
                                        "Asia_not_household_total", "Asia_not_household_male", "Asia_not_household_female", "Asia_not_household_another"
)

## Data cleaning for health_region ##
health_region_age_data <- read_excel(data_health_age, skip = 6)
health_region_age_data <- health_region_age_data[-1, ] 

# Delete unnecessary columns and rows
health_region_age_data <- health_region_age_data[, c(-2,-83)]
health_region_age_data <- health_region_age_data[c(-25, -26, -27), ]


# Rename columns
colnames(health_region_age_data)[1] <- "Area"
colnames(health_region_age_data)[2:81] <- c("allage_household_total", "allage_household_male", "allage_household_female", "allage_household_another",
                                        "household_total", "household_male", "household_female", "household_another",
                                        "Euro_allage_household_total", "Euro_allage_household_male", "Euro_allage_household_female", "Euro_allage_household_another",
                                        "Euro_household_total", "Euro_household_male", "Euro_household_female", "Euro_household_another",
                                        "Maori_allage_household_total", "Maori_allage_household_male", "Maori_allage_household_female", "Maori_allage_household_another",
                                        "Maori_household_total", "Maori_household_male", "Maori_household_female", "Maori_household_another",
                                        "Paci_allage_household_total", "Paci_allage_household_male", "Paci_allage_household_female", "Paci_allage_household_another",
                                        "Paci_household_total", "Paci_household_male", "Paci_household_female", "Paci_household_another",
                                        "Asia_allage_household_total", "Asia_allage_household_male", "Asia_allage_household_female", "Asia_allage_household_another",
                                        "Asia_household_total", "Asia_household_male", "Asia_household_female", "Asia_household_another",
                                        
                                        "allage_not_household_total", "allage_not_household_male", "allage_not_household_female", "allage_not_household_another",
                                        "not_household_total", "not_household_male", "not_household_female", "not_household_another",
                                        "Euro_allage_not_household_total", "Euro_allage_not_household_male", "Euro_allage_not_household_female", "Euro_allage_not_household_another",
                                        "Euro_not_household_total", "Euro_not_household_male", "Euro_not_household_female", "Euro_not_household_another",
                                        "Maori_allage_not_household_total", "Maori_allage_not_household_male", "Maori_allage_not_household_female", "Maori_allage_not_household_another",
                                        "Maori_not_household_total", "Maori_not_household_male", "Maori_not_household_female", "Maori_not_household_another",
                                        "Paci_allage_not_household_total", "Paci_allage_not_household_male", "Paci_allage_not_household_female", "Paci_allage_not_household_another",
                                        "Paci_not_household_total", "Paci_not_household_male", "Paci_not_household_female", "Paci_not_household_another",
                                        "Asia_allage_not_household_total", "Asia_allage_not_household_male", "Asia_allage_not_household_female", "Asia_allage_not_household_another",
                                        "Asia_not_household_total", "Asia_not_household_male", "Asia_not_household_female", "Asia_not_household_another"
)

## Data cleaning for health_region ##


### Data Cleaning #####################

# Keep main regions only
health_region_main <- health_region_data %>%
  filter(!grepl("^\\·", Area))
# Create region list with "Main Region" as the first item, followed by all region names
health_region_list <- c("All New Zealand Main Health Region", unique(health_region_main$Area))

# Load shapefile
nz_shape <- st_read(map_data)
nz_shape <- st_transform(nz_shape, crs = 4326)


# safe function for missing numeric 
safe_numeric <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return(0)
  as.numeric(x)
}


ui <- dashboardPage(
  dashboardHeader(title = "Young carers in NZ"),
  dashboardSidebar(
    tags$head(tags$style(HTML(
      ".main-sidebar { background-color: #ffffff !important; }"
    ))),
    sidebarMenu(
      menuItem("First Page", tabName = "tab-home", icon = icon("home")),
      menuItem("Health Region Data View", tabName = "tab-2023", icon = icon("table")),
      menuItem("Geographical Region Data View", tabName = "tab-map", icon = icon("globe"))
    ),
    br(),
    div(style = "position: absolute; bottom: 200px; left: 10px; right: 10px; font-size: 16px; border: 1px solid #ccc; padding: 10px; background-color: #ffffff; border-radius: 5px;",
        HTML("<span style='color: black;'><a href='https://explore.data.stats.govt.nz/vis?fs[0]=2023%20Census%2C0%7CWork%23CAT_WORK%23&fs[1]=Unpaid%20activities%2C1%7CTotal%20-%20unpaid%20activities%2399%23%7CLooking%20after%20a%20member%20of%20own%20household%20who%20is%20ill%20or%20has%20a%20disability%233%23&pg=0&fc=Unpaid%20activities&snb=4&vw=tb&df[ds]=ds-nsiws-disseminate&df[id]=CEN23_WRK_013&df[ag]=STATSNZ&df[vs]=1.0&dq=2023.304%2B303%2B302%2B301%2B300%2B204%2B203%2B202%2B201%2B200%2B103%2B102%2B101%2B100%2B15%2B99%2B14%2B13%2B12%2B18%2B17%2B16%2B09%2B08%2B90%2B40%2B30%2B20%2B10%2B07%2B06%2B05%2B03%2B04%2B02%2B01%2B9999%2B99999.5%2B3.4%2B3%2B2%2B1%2B999.2%2B99.3%2B2%2B1%2B99&ly[rw]=CEN23_GEO_002&ly[cl]=CEN23_UNP_003%2CCEN23_ETH_002%2CCEN23_AGE_008%2CCEN23_GEN_002&to[TIME]=false' target='_blank' style='color: blue;'>Access the dataset</a><br><br>
              Last update:<br>2-7-2025<br><br>
              Contact Email:<br><span style='font-size: 13px; color: black;'><a href='mailto:fwu255@aucklanduni.ac.nz' style='color: blue;'>fwu255@aucklanduni.ac.nz</a></span></span>")
    ),
    tags$a(href = "https://www.compass.auckland.ac.nz", target = "_blank",
           tags$img(src = "img/COMPASS_logo.png", style = "position: absolute; bottom: 20px; left: 20px; width: 160px;"))
  ),
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "tab-home",
              fluidPage(
                div(style = "border: 2px solid #ccc; border-radius: 10px; padding: 30px; background-color: #fefefe; box-shadow: 2px 2px 10px rgba(0,0,0,0.1);",
                    
                    h2("Introduction to the Dataset: Young carers' unpaid activities - 2023 Census of Population and Dwellings"),
                    
                    h4("📘 Data Source"),
                    p("This dashboard is built upon data from the 2023 New Zealand Census of Population and Dwellings. ",
                      "It provides a snapshot of society and supports planning and decision-making in health, education, housing, and transport. ",
                      a("[View official dataset description]", href = "https://datainfoplus.stats.govt.nz/item/nz.govt.stats/7c1335e0-c2c7-4217-ac48-bfc7a68aea48", target = "_blank")),
                    
                    br(),
                    
                    p("The unpaid care-related statistics used here are part of the broader theme ",
                      strong("\"Unpaid activities\""),
                      " from the 2023 Census."),
                    p(a("Learn more about this data classification here",
                        href = "https://datainfoplus.stats.govt.nz/item/nz.govt.stats/bcb42609-9cde-4422-96ae-cff7a5b340ce/16",
                        target = "_blank")),
                    
                    br(),
                    
                    h4("👥 Target Population"),
                    p("The unpaid activities data applies to the usually resident population aged ",
                      strong("15 to 29 years"), ", we called them ", strong("\"Young Carers\""), "."),
                    
                    br(),
                    
                    h4("🧩 Unpaid Activities Classification"),
                    p("In the census, unpaid activities are classified into several categories. ",
                      "This dashboard focuses on two categories related to care work for sick or disabled individuals:"),
                    tableOutput("unpaid_category_table"),
                    p(em("Note: Respondents may report multiple unpaid activities, so total counts may exceed the population count.")),
                    
                    br(),
                    
                    h4("🎯 Research Focus"),
                    tags$ul(
                      tags$li(em("To understand the level of unpaid care work provided by gender groups")),
                      tags$li(em("To assess how unpaid activities support social and economic systems")),
                      tags$li(em("To analyse the demographic characteristics (e.g., gender, ethnicity) of people involved in unpaid activities")),
                      tags$li(em("To analyse the importance of young carers in the society unpaid activities"))
                    ),
                    
                    br(),
                    
                    h4("🔬 Research Result"),
                    p("The research analysis is currently in progress. Findings will be updated and published in future versions of this dashboard."),
                    
                    # # Example result image
                    # # Caption for the image
                    # tags$img(
                    #   src = "img/results_image1.png",   # Path to the image (relative to 'www' folder or app directory)
                    #   width = "80%",                    # Set image width to 80% of the parent container (responsive scaling)
                    # 
                    #   style = "
                    #     display:block;                             /* Make the image behave as a block element (ensures it respects margin settings) */
                    #     margin-left:auto; margin-right:auto;       /* Horizontally center the image using automatic left/right margins */
                    #     margin-top:20px; margin-bottom:10px;       /* Add spacing above (20px) and below (10px) the image */
                    #     box-shadow: 2px 2px 10px rgba(0,0,0,0.1);  /* Add a subtle shadow effect for better visual separation */
                    #   "
                    # ),
                    # # Caption for the image, centered below the image
                    # p(
                    #   em("Figure 1: The result images"),  # The caption text in italic style (using 'em' tag)
                    # 
                    #   style = "
                    #     text-align: center;                        /* Center-align the caption text below the image */
                    #     font-style: italic;                        /* Make the text italic (redundant here since 'em' is already italic, but ensures compatibility) */
                    #     margin-top: 5px; margin-bottom: 20px;      /* Add spacing: small gap (5px) above caption, larger space (20px) below caption */
                    #   "
                    # ),
                    # 
                    # # Result summary paragraph
                    # p(
                    #   span("Replace the text with the result finding - ",
                    #        style = "color: #2E86C1; font-size: 18px; font-family: Arial, sans-serif;"),
                    # 
                    #   span("this part is bold", style = "font-weight: bold; color: #27AE60;"),
                    # 
                    #   span(", this part is italic", style = "font-style: italic; color: #AF7AC5;"),
                    # 
                    #   span(", this part is bold & italic", style = "font-weight: bold; font-style: italic; color: #E74C3C; font-size: 16px;"),
                    # 
                    #   span(", this part is larger font", style = "font-size: 22px; color: #F39C12;")
                    # )
                    
                )
              )
      ),
      
      tabItem(tabName = "tab-2023",
              fluidPage(
                tabsetPanel(id = "data_view_mode", type = "tabs",
                            
                            tabPanel("Plot Form",
                                     sidebarLayout(
                                       sidebarPanel(width = 3,
                                                    selectInput("plot_main_region", "Select Main Region:", choices = unique(health_region_main$Area)),
                                                    uiOutput("subregion_selector"),
                                                    radioButtons("compare_mode", "Comparison Mode:",
                                                                 choices = c("Overall" = "total", "By Gender" = "gender", "By Ethnicity" = "ethnicity")),
                                                    uiOutput("plot_option_selector"),
                                       ),
                                       mainPanel(
                                         div(style = "height: calc(100vh - 100px);",
                                             plotlyOutput("bar_plot", height = "100%")
                                         )
                                       )
                                     )
                            ),
                            tabPanel("Table Form",
                                     div(style = "text-align: center;",
                                         h2("2023 All New Zealand health region Data")
                                     ),
                                     fluidRow(
                                       column(4,
                                              selectInput("region_select", "Select Region:", choices = health_region_list)
                                       )
                                     ),
                                     fluidRow(
                                       column(12,
                                              dataTableOutput("region_table")
                                       )
                                     )
                            )
                )
              )
      ),
      
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


# Define server logic to handle map rendering and user interaction, and show full table on data tab
server <- function(input, output, session) {
  
  # Render table in First page
  output$unpaid_category_table <- renderTable({
    data.frame(
      Code = c(3, 5),
      Category = c(
        "Looking after a member of own household who is ill or has a disability",
        "Helping someone who is ill or has a disability who does not live in own household"
      )
    )
  })
  
  
  # Render full region table in the 2023 Data tab (not filtered)
  # Render table based on selected region
  output$region_table <- renderDataTable({
    if (input$region_select == "All New Zealand Main Health Region") {
      datatable(health_region_age_data, options = list(scrollX = TRUE))
    } else {
      selected <- input$region_select
      index <- which(health_region_age_data$Area == selected)
      sub_region_rows <- index
      for (i in (index + 1):nrow(health_region_age_data)) {
        if (startsWith(health_region_age_data$Area[i], "·")) {
          sub_region_rows <- c(sub_region_rows, i)
        } else {
          break
        }
      }
      datatable(health_region_age_data[sub_region_rows, ], options = list(scrollX = TRUE))
    }
  })
  
  # select the subregion
  output$subregion_selector <- renderUI({
    req(input$plot_main_region)
    index <- which(health_region_data$Area == input$plot_main_region)
    sub_names <- c(input$plot_main_region)
    for (i in (index + 1):nrow(health_region_data)) {
      if (startsWith(health_region_data$Area[i], "·")) {
        sub_names <- c(sub_names, health_region_data$Area[i])
      } else {
        break
      }
    }
    selectInput("plot_subregion", "Select Area (Main or Sub):", choices = sub_names)
  })
  
  # option selection after Select the comparison mode
  output$plot_option_selector <- renderUI({
    if (input$compare_mode == "gender") {
      selectInput("ethnicity_filter", "Select Ethnicity:",
                  choices = c("All" = "all", "European" = "euro", "Māori" = "maori", "Pacific Peoples" = "paci", "Asian" = "asia"))
    } else if (input$compare_mode == "ethnicity") {
      selectInput("gender_filter", "Select Gender:",
                  choices = c("Total" = "total", "Male" = "male", "Female" = "female", "Other gender" = "another"))
    } else {
      NULL
    }
  })
  
  
  output$bar_plot <- renderPlotly({
    req(input$plot_subregion)
    df <- health_region_data %>% filter(Area == input$plot_subregion)
    
    if (input$compare_mode == "total") {
      Count <- c(as.numeric(df$household_total), as.numeric(df$not_household_total))
      AllCount <- c(as.numeric(df$allage_household_total), as.numeric(df$allage_not_household_total))
      
      Text <- paste0("Young carers in this group: ", Count, "<br>",
                     "Total aged 15 to 29 in this group: ", AllCount, "<br>(", 
                     ifelse(AllCount > 0, round(Count / AllCount * 100, 1), NA), "% of young people in this group are carers for selected household)")
      
      data <- data.frame(
        Category = c("Care for household member", "Care for non-household person"),
        Count = c(as.numeric(df$household_total), as.numeric(df$not_household_total))
      )
      
      plot_ly(data, x = ~Category, y = ~Count, type = 'bar',
              text = ~Count, textposition = 'outside',
              hovertext = ~Text, hoverinfo = 'text',
              marker = list(color = c("#FDBE85", "#FD8D3C"))) %>%
        layout(title = paste("New Zealand Health Region Young Carers Unpaid activities -", input$plot_subregion),
               margin = list(t = 100), yaxis = list(title = "Count"), xaxis = list(title = ""))
      
    } else if (input$compare_mode == "gender") {
      ethnicity_prefix <- switch(input$ethnicity_filter,
                                 "euro" = "Euro_",
                                 "maori" = "Maori_",
                                 "paci" = "Paci_",
                                 "asia" = "Asia_",
                                 "all" = "")
      
      Count <- c(
        as.numeric(df[[paste0(ethnicity_prefix, "household_male")]]),
        as.numeric(df[[paste0(ethnicity_prefix, "not_household_male")]]),
        as.numeric(df[[paste0(ethnicity_prefix, "household_female")]]),
        as.numeric(df[[paste0(ethnicity_prefix, "not_household_female")]]),
        as.numeric(df[[paste0(ethnicity_prefix, "household_another")]]),
        as.numeric(df[[paste0(ethnicity_prefix, "not_household_another")]])
      )
      
      AllCount <- c(
        as.numeric(df[[paste0(ethnicity_prefix, "allage_household_male")]]),
        as.numeric(df[[paste0(ethnicity_prefix, "allage_not_household_male")]]),
        as.numeric(df[[paste0(ethnicity_prefix, "allage_household_female")]]),
        as.numeric(df[[paste0(ethnicity_prefix, "allage_not_household_female")]]),
        as.numeric(df[[paste0(ethnicity_prefix, "allage_household_another")]]),
        as.numeric(df[[paste0(ethnicity_prefix, "allage_not_household_another")]])
      )
      
      Text <- paste0("Young carers in this group: ", Count, "<br>",
                     "Total aged 15 to 29 in this group: ", AllCount, "<br>(", 
                     ifelse(AllCount > 0, round(Count / AllCount * 100, 1), NA), "% of young people in this group are carers for selected household)")
      
      data <- data.frame(
        Gender = rep(c("Male", "Female", "Other Gender"), each = 2),
        Category = rep(c("Household", "Non-Household"), times = 3),
        Count = Count,
        Text = Text
      )
      
      plot_ly(data, x = ~Gender, y = ~Count, color = ~Category, type = 'bar',
              text = ~Count, textposition = 'outside',
              textfont = list(color = "black", size = 12),
              colors = c("#FDBE85", "#FD8D3C"),
              hovertext = ~Text, hoverinfo = 'text', barmode = 'group') %>%
        layout(title = paste("Young Carers by Gender -", input$plot_subregion),
               margin = list(t = 100), yaxis = list(title = "Count"), xaxis = list(title = ""))
      
    } else if (input$compare_mode == "ethnicity") {
      gender_suffix <- switch(input$gender_filter,
                              "total" = "_total",
                              "male" = "_male",
                              "female" = "_female",
                              "another" = "_another")
      
      Count <- c(
        as.numeric(df[[paste0("Euro_household", gender_suffix)]]),
        as.numeric(df[[paste0("Euro_not_household", gender_suffix)]]),
        as.numeric(df[[paste0("Maori_household", gender_suffix)]]),
        as.numeric(df[[paste0("Maori_not_household", gender_suffix)]]),
        as.numeric(df[[paste0("Paci_household", gender_suffix)]]),
        as.numeric(df[[paste0("Paci_not_household", gender_suffix)]]),
        as.numeric(df[[paste0("Asia_household", gender_suffix)]]),
        as.numeric(df[[paste0("Asia_not_household", gender_suffix)]])
      )
      
      AllCount <- c(
        as.numeric(df[[paste0("Euro_allage_household", gender_suffix)]]),
        as.numeric(df[[paste0("Euro_allage_not_household", gender_suffix)]]),
        as.numeric(df[[paste0("Maori_allage_household", gender_suffix)]]),
        as.numeric(df[[paste0("Maori_allage_not_household", gender_suffix)]]),
        as.numeric(df[[paste0("Paci_allage_household", gender_suffix)]]),
        as.numeric(df[[paste0("Paci_allage_not_household", gender_suffix)]]),
        as.numeric(df[[paste0("Asia_allage_household", gender_suffix)]]),
        as.numeric(df[[paste0("Asia_allage_not_household", gender_suffix)]])
      )
      
      Text <- paste0("Young carers in this group: ", Count, "<br>",
                     "Total aged 15 to 29 in this group: ", AllCount, "<br>(", 
                     ifelse(AllCount > 0, round(Count / AllCount * 100, 1), NA), "% of young people in this group are carers for selected household)")
      
      data <- data.frame(
        Ethnicity = rep(c("European", "Māori", "Pacific Peoples", "Asian"), each = 2),
        Category = rep(c("Household", "Non-Household"), times = 4),
        Count = Count,
        Text = Text
      )
      
      plot_ly(data, x = ~Ethnicity, y = ~Count, color = ~Category, type = 'bar',
              text = ~Count, textposition = 'outside',
              textfont = list(color = "black", size = 12),
              colors = c("#FDBE85", "#FD8D3C"),
              hovertext = ~Text, hoverinfo = 'text', barmode = 'group') %>%
        layout(title = paste("Young Carers by Ethnicity -", input$plot_subregion),
               margin = list(t = 100), yaxis = list(title = "Count"), xaxis = list(title = ""))
    } 
    
  })
  
  # Store the ID of the region clicked on the map
  clicked_region <- reactiveVal(NULL)
  # Control whether the floating information panel is visible
  show_info <- reactiveVal(TRUE)
  
  # Render the base Leaflet map with New Zealand region polygons
  output$nz_map <- renderLeaflet({
    leaflet(data = nz_shape, options = leafletOptions(minZoom = 5, maxZoom = 18)) %>%
      setMaxBounds(lng1 = 155, lat1 = -55, lng2 = 190, lat2 = -25) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = "#FFFACD",
        fillOpacity = 0.2,
        color = "black",
        weight = 1,
        label = map_label,
        layerId = map_layerId,
        highlightOptions = highlightOptions(color = "blue", weight = 2, bringToFront = TRUE)
      )
  })
  
  # Handle map shape click: store clicked region and show info panel
  observeEvent(input$nz_map_shape_click, {
    clicked_region(input$nz_map_shape_click$id)
    show_info(TRUE)
  })
  
  # Handle close button on the floating box: hide info panel
  observeEvent(input$hide_box, show_info(FALSE))
  
  
  # Dynamically render the floating info box showing clicked region name
  output$map_info_box <- renderUI({
    req(clicked_region(), show_info())
    
    absolutePanel(class = "floating-box", draggable = TRUE,
                  actionButton("hide_box", "✖", style = "float: right; margin-bottom: 5px;"),
                  h4(strong("Region Information")),
                  p(HTML(paste0("<strong>Selected Region:</strong> <span style='color: red; font-weight: bold;'>", clicked_region(), "</span>"))),
                  
                  radioButtons("map_mode", "View Mode:",
                               choices = c(
                                 "Compare the young carers by unpaid activity type" = "type",
                                 "Compare the young carers by gender" = "gender",
                                 "Compare the young carers by ethnicity" = "ethnicity"),
                               selected = "type"),
                  uiOutput("map_sub_selector"),
                  
                  actionButton("compare_region_btn", "Compare with another region", style = "margin-top: 10px;"),
                  conditionalPanel(
                    condition = "input.compare_region_btn % 2 == 1",
                    selectInput("second_region", "Select region to compare:",
                                choices = region_data$Area[grepl("Region$", region_data$Area)],
                                selected = clicked_region())
                  ),
                  
                  br(),
                  div(style = "height: calc(100vh - 300px);",
                      plotlyOutput("map_pie", height = "100%"))
    )
  })
  
  
  # selection in infobox
  output$map_sub_selector <- renderUI({
    if (input$map_mode == "type") {
      tagList(
        selectInput("map_gender", "Select the gender:",
                    choices = c("Overall" = "overall", "Male" = "male", "Female" = "female", "Other gender" = "another"),
                    selected = "overall"),
        selectInput("map_ethnicity", "Select the ethnicity:",
                    choices = c("Overall" = "overall", "European" = "euro", "Māori" = "maori", "Pacific Peoples" = "paci", "Asian" = "asia"),
                    selected = "overall")
      )
    } else if (input$map_mode == "gender") {
      tagList(
        selectInput("map_activity", "Select the unpaid activities type:",
                    choices = c("Overall" = "overall",
                                "Care for household member" = "household",
                                "Care for non-household person" = "nonhousehold"),
                    selected = "Overall"),
        selectInput("map_ethnicity", "Select the ethnicity:",
                    choices = c("Overall" = "overall", "European" = "euro", "Māori" = "maori", "Pacific Peoples" = "paci", "Asian" = "asia"),
                    selected = "overall")
      )
    } else if (input$map_mode == "ethnicity") {
      tagList(
        selectInput("map_activity", "Select the unpaid activities type:",
                    choices = c("Overall" = "overall",
                                "Care for household member" = "household",
                                "Care for non-household person" = "nonhousehold"),
                    selected = "Overall"),
        selectInput("map_gender", "Select the gender:",
                    choices = c("Overall" = "overall", "Male" = "male", "Female" = "female", "Other gender" = "another"),
                    selected = "overall")
      )
    }
  })
  
  
  
  
  
  # pie plot in the infobox of Map
  output$map_pie <- renderPlotly({
    `%_%` <- function(a, b) paste0(a, b)
    req(clicked_region())
    df1 <- region_data %>% filter(Area == clicked_region())
    if (nrow(df1) == 0) return(NULL)
    
    compare_mode <- input$compare_region_btn %% 2 == 1
    has_second <- !is.null(input$second_region) && input$second_region != ""
    df2 <- if (has_second) region_data %>% filter(Area == input$second_region) else NULL
    
    if (!compare_mode) {
      # Pie plot
      if (input$map_mode == "type") {
        gender <- input$map_gender
        ethnicity <- input$map_ethnicity
        if (is.null(ethnicity) || length(ethnicity) != 1) return(NULL)
        prefix <- switch(ethnicity,
                         "euro" = "Euro_",
                         "maori" = "Maori_",
                         "paci" = "Paci_",
                         "asia" = "Asia_",
                         "overall" = "")
        
        values <- switch(gender,
                         "overall" = c("Care for household member" = safe_numeric(df1[[paste0(prefix, "household_total")]]),
                                       "Care for non-household person" = safe_numeric(df1[[paste0(prefix, "not_household_total")]])),
                         "male" = c("Care for household member" = safe_numeric(df1[[paste0(prefix, "household_male")]]),
                                    "Care for non-household person" = safe_numeric(df1[[paste0(prefix, "not_household_male")]])),
                         "female" = c("Care for household member" = safe_numeric(df1[[paste0(prefix, "household_female")]]),
                                      "Care for non-household person" = safe_numeric(df1[[paste0(prefix, "not_household_female")]])),
                         "another" = c("Care for household member" = safe_numeric(df1[[paste0(prefix, "household_another")]]),
                                     "Care for non-household person" = safe_numeric(df1[[paste0(prefix, "not_household_another")]]))
        )
        labels <- names(values)
      } else if (input$map_mode == "gender") {
        activity <- input$map_activity
        ethnicity <- input$map_ethnicity
        prefix <- switch(ethnicity,
                         "euro" = "Euro_",
                         "maori" = "Maori_",
                         "paci" = "Paci_",
                         "asia" = "Asia_",
                         "overall" = "")
        
        values <- switch(activity,
                         "overall" = c("Male" = safe_numeric(df1[[paste0(prefix, "household_male")]]) + safe_numeric(df1[[paste0(prefix, "not_household_male")]]),
                                       "Female" = safe_numeric(df1[[paste0(prefix, "household_female")]]) + safe_numeric(df1[[paste0(prefix, "not_household_female")]]),
                                       "Other gender" = safe_numeric(df1[[paste0(prefix, "household_another")]]) + safe_numeric(df1[[paste0(prefix, "not_household_another")]])),
                         "household" = c("Male" = safe_numeric(df1[[paste0(prefix, "household_male")]]),
                                         "Female" = safe_numeric(df1[[paste0(prefix, "household_female")]]),
                                         "Other gender" = safe_numeric(df1[[paste0(prefix, "household_another")]])),
                         "nonhousehold" = c("Male" = safe_numeric(df1[[paste0(prefix, "not_household_male")]]),
                                            "Female" = safe_numeric(df1[[paste0(prefix, "not_household_female")]]),
                                            "Other gender" = safe_numeric(df1[[paste0(prefix, "not_household_another")]]))
        )
        labels <- names(values)
      } else if (input$map_mode == "ethnicity") {
        activity <- input$map_activity
        gender <- input$map_gender
        suffix <- switch(gender,
                         "overall" = "_total",
                         "total" = "_total",
                         "male" = "_male",
                         "female" = "_female",
                         "another" = "_another")
        
        
        values <- switch(activity,
                         "overall" = c("European" = safe_numeric(df1[["Euro_household" %_% suffix]]) + safe_numeric(df1[["Euro_not_household" %_% suffix]]),
                                       "Māori" = safe_numeric(df1[["Maori_household" %_% suffix]]) + safe_numeric(df1[["Maori_not_household" %_% suffix]]),
                                       "Pacific Peoples" = safe_numeric(df1[["Paci_household" %_% suffix]]) + safe_numeric(df1[["Paci_not_household" %_% suffix]]),
                                       "Asian" = safe_numeric(df1[["Asia_household" %_% suffix]]) + safe_numeric(df1[["Asia_not_household" %_% suffix]])),
                         "household" = c("European" = safe_numeric(df1[[paste0("Euro_household", suffix)]]),
                                         "Māori" = safe_numeric(df1[[paste0("Maori_household", suffix)]]),
                                         "Pacific Peoples" = safe_numeric(df1[[paste0("Paci_household", suffix)]]),
                                         "Asian" = safe_numeric(df1[[paste0("Asia_household", suffix)]])),
                         "nonhousehold" = c("European" = safe_numeric(df1[[paste0("Euro_not_household", suffix)]]),
                                            "Māori" = safe_numeric(df1[[paste0("Maori_not_household", suffix)]]),
                                            "Pacific Peoples" = safe_numeric(df1[[paste0("Paci_not_household", suffix)]]),
                                            "Asian" = safe_numeric(df1[[paste0("Asia_not_household", suffix)]]))
        )
        labels <- names(values)
        
      }
      

      
      AllValues <- switch(input$map_mode,
                          "type" = {
                            switch(gender,
                                   "overall" = c("Care for household member" = safe_numeric(df1[[paste0(prefix, "allage_household_total")]]),
                                                 "Care for non-household person" = safe_numeric(df1[[paste0(prefix, "allage_not_household_total")]])),
                                   "male" = c("Care for household member" = safe_numeric(df1[[paste0(prefix, "allage_household_male")]]),
                                              "Care for non-household person" = safe_numeric(df1[[paste0(prefix, "allage_not_household_male")]])),
                                   "female" = c("Care for household member" = safe_numeric(df1[[paste0(prefix, "allage_household_female")]]),
                                                "Care for non-household person" = safe_numeric(df1[[paste0(prefix, "allage_not_household_female")]])),
                                   "other" = c("Care for household member" = safe_numeric(df1[[paste0(prefix, "allage_household_another")]]),
                                               "Care for non-household person" = safe_numeric(df1[[paste0(prefix, "allage_not_household_another")]]))
                            )
                          },
                          "gender" = {
                            switch(activity,
                                   "overall" = c("Male" = safe_numeric(df1[[paste0(prefix, "allage_household_male")]]) + safe_numeric(df1[[paste0(prefix, "allage_not_household_male")]]),
                                                 "Female" = safe_numeric(df1[[paste0(prefix, "allage_household_female")]]) + safe_numeric(df1[[paste0(prefix, "allage_not_household_female")]]),
                                                 "Other gender" = safe_numeric(df1[[paste0(prefix, "allage_household_another")]]) + safe_numeric(df1[[paste0(prefix, "allage_not_household_another")]])),
                                   "household" = c("Male" = safe_numeric(df1[[paste0(prefix, "allage_household_male")]]),
                                                   "Female" = safe_numeric(df1[[paste0(prefix, "allage_household_female")]]),
                                                   "Other gender" = safe_numeric(df1[[paste0(prefix, "allage_household_another")]])),
                                   "nonhousehold" = c("Male" = safe_numeric(df1[[paste0(prefix, "allage_not_household_male")]]),
                                                      "Female" = safe_numeric(df1[[paste0(prefix, "allage_not_household_female")]]),
                                                      "Other gender" = safe_numeric(df1[[paste0(prefix, "allage_not_household_another")]]))
                            )
                          },
                          "ethnicity" = {
                            switch(activity,
                                   "overall" = c("European" = safe_numeric(df1[["Euro_allage_household" %_% suffix]]) + safe_numeric(df1[["Euro_allage_not_household" %_% suffix]]),
                                                 "Māori" = safe_numeric(df1[["Maori_allage_household" %_% suffix]]) + safe_numeric(df1[["Maori_allage_not_household" %_% suffix]]),
                                                 "Pacific Peoples" = safe_numeric(df1[["Paci_allage_household" %_% suffix]]) + safe_numeric(df1[["Paci_allage_not_household" %_% suffix]]),
                                                 "Asian" = safe_numeric(df1[["Asia_allage_household" %_% suffix]]) + safe_numeric(df1[["Asia_allage_not_household" %_% suffix]])),
                                   "household" = c("European" = safe_numeric(df1[["Euro_allage_household" %_% suffix]]),
                                                   "Māori" = safe_numeric(df1[["Maori_allage_household" %_% suffix]]),
                                                   "Pacific Peoples" = safe_numeric(df1[["Paci_allage_household" %_% suffix]]),
                                                   "Asian" = safe_numeric(df1[["Asia_allage_household" %_% suffix]])),
                                   "nonhousehold" = c("European" = safe_numeric(df1[["Euro_allage_not_household" %_% suffix]]),
                                                      "Māori" = safe_numeric(df1[["Maori_allage_not_household" %_% suffix]]),
                                                      "Pacific Peoples" = safe_numeric(df1[["Paci_allage_not_household" %_% suffix]]),
                                                      "Asian" = safe_numeric(df1[["Asia_allage_not_household" %_% suffix]]))
                            )
                          }
      )  
      values <- as.numeric(values)
      hover_text <- paste0("Young carers in this group: ", values,
                           "<br> Share of this pie plot: ", round(values / sum(values, na.rm = TRUE) * 100, 1), "%",
                           "<br> Total aged 15 to 29 in this group: ", AllValues, 
                           " <br> (", ifelse(AllValues > 0, round(values / AllValues * 100, 1), NA), "% of young people in this group are carers for selected household)")
      
      plot_ly(
        labels = labels,
        values = values,
        type = "pie",
        text = hover_text,
        textinfo = "value+percent",
        textposition = "auto",
        hoverinfo = "text"
      ) %>%
        layout(showlegend = TRUE, legend = list(x = 1, y = 0.9))
      
    } else {
      
      
      # Bar plot for compare
      if (input$map_mode == "type") {
        gender <- input$map_gender
        ethnicity <- input$map_ethnicity
        prefix <- switch(ethnicity,
                         "euro" = "Euro_",
                         "maori" = "Maori_",
                         "paci" = "Paci_",
                         "asia" = "Asia_",
                         "overall" = "")
        
        get_values <- function(df) {
          switch(gender,
                 "overall" = c("Care for household member" = safe_numeric(df[[paste0(prefix, "household_total")]]),
                               "Care for non-household person" = safe_numeric(df[[paste0(prefix, "not_household_total")]])),
                 "male" = c("Care for household member" = safe_numeric(df[[paste0(prefix, "household_male")]]),
                            "Care for non-household person" = safe_numeric(df[[paste0(prefix, "not_household_male")]])),
                 "female" = c("Care for household member" = safe_numeric(df[[paste0(prefix, "household_female")]]),
                              "Care for non-household person" = safe_numeric(df[[paste0(prefix, "not_household_female")]])),
                 "another" = c("Care for household member" = safe_numeric(df[[paste0(prefix, "household_another")]]),
                             "Care for non-household person" = safe_numeric(df[[paste0(prefix, "not_household_another")]]))
          )
        }
        
      } else if (input$map_mode == "gender") {
        activity <- input$map_activity
        ethnicity <- input$map_ethnicity
        prefix <- switch(ethnicity,
                         "euro" = "Euro_",
                         "maori" = "Maori_",
                         "paci" = "Paci_",
                         "asia" = "Asia_",
                         "overall" = "")
        
        get_values <- function(df) {
          switch(activity,
                 "overall" = c("Male" = safe_numeric(df[[paste0(prefix, "household_male")]]) + safe_numeric(df[[paste0(prefix, "not_household_male")]]),
                               "Female" = safe_numeric(df[[paste0(prefix, "household_female")]]) + safe_numeric(df[[paste0(prefix, "not_household_female")]]),
                               "Other gender" = safe_numeric(df[[paste0(prefix, "household_another")]]) + safe_numeric(df[[paste0(prefix, "not_household_another")]])),
                 "household" = c("Male" = safe_numeric(df[[paste0(prefix, "household_male")]]),
                                 "Female" = safe_numeric(df[[paste0(prefix, "household_female")]]),
                                 "Other gender" = safe_numeric(df[[paste0(prefix, "household_another")]])),
                 "nonhousehold" = c("Male" = safe_numeric(df[[paste0(prefix, "not_household_male")]]),
                                    "Female" = safe_numeric(df[[paste0(prefix, "not_household_female")]]),
                                    "Other gender" = safe_numeric(df[[paste0(prefix, "not_household_another")]]))
          )
        }
        
      } else if (input$map_mode == "ethnicity") {
        activity <- input$map_activity
        gender <- input$map_gender
        suffix <- switch(gender,
                         "total" = "_total",
                         "overall" = "_total",
                         "male" = "_male",
                         "female" = "_female",
                         "another" = "_another")
        
        get_values <- function(df) {
          switch(activity,
                 "overall" = c("European" = safe_numeric(df[["Euro_household" %_% suffix]]) + safe_numeric(df[["Euro_not_household" %_% suffix]]),
                               "Māori" = safe_numeric(df[["Maori_household" %_% suffix]]) + safe_numeric(df[["Maori_not_household" %_% suffix]]),
                               "Pacific Peoples" = safe_numeric(df[["Paci_household" %_% suffix]]) + safe_numeric(df[["Paci_not_household" %_% suffix]]),
                               "Asian" = safe_numeric(df[["Asia_household" %_% suffix]]) + safe_numeric(df[["Asia_not_household" %_% suffix]])),
                 "household" = c("European" = safe_numeric(df[[paste0("Euro_household", suffix)]]),
                                 "Māori" = safe_numeric(df[[paste0("Maori_household", suffix)]]),
                                 "Pacific Peoples" = safe_numeric(df[[paste0("Paci_household", suffix)]]),
                                 "Asian" = safe_numeric(df[[paste0("Asia_household", suffix)]])),
                 "nonhousehold" = c("European" = safe_numeric(df[[paste0("Euro_not_household", suffix)]]),
                                    "Māori" = safe_numeric(df[[paste0("Maori_not_household", suffix)]]),
                                    "Pacific Peoples" = safe_numeric(df[[paste0("Paci_not_household", suffix)]]),
                                    "Asian" = safe_numeric(df[[paste0("Asia_not_household", suffix)]]))
          )
        }
      }
      
      
      # Retrieve young carers values for the clicked and second regions
      values1 <- get_values(df1)
      labels <- names(values1)
      values2 <- if (has_second) get_values(df2) else NULL
      

      
      # Function to retrieve all-age carers values by mode and input
      get_all_values <- function(df) {
        if (input$map_mode == "type") {
          gender <- input$map_gender
          switch(gender,
                 "overall" = c("Care for household member" = safe_numeric(df[[paste0(prefix, "allage_household_total")]]),
                               "Care for non-household person" = safe_numeric(df[[paste0(prefix, "allage_not_household_total")]])),
                 "male" = c("Care for household member" = safe_numeric(df[[paste0(prefix, "allage_household_male")]]),
                            "Care for non-household person" = safe_numeric(df[[paste0(prefix, "allage_not_household_male")]])),
                 "female" = c("Care for household member" = safe_numeric(df[[paste0(prefix, "allage_household_female")]]),
                              "Care for non-household person" = safe_numeric(df[[paste0(prefix, "allage_not_household_female")]])),
                 "another" = c("Care for household member" = safe_numeric(df[[paste0(prefix, "allage_household_another")]]),
                             "Care for non-household person" = safe_numeric(df[[paste0(prefix, "allage_not_household_another")]]))
          )
        } else if (input$map_mode == "gender") {
          switch(activity,
                 "overall" = c("Male" = safe_numeric(df[[paste0(prefix, "allage_household_male")]]) + safe_numeric(df[[paste0(prefix, "allage_not_household_male")]]),
                               "Female" = safe_numeric(df[[paste0(prefix, "allage_household_female")]]) + safe_numeric(df[[paste0(prefix, "allage_not_household_female")]]),
                               "Other gender" = safe_numeric(df[[paste0(prefix, "allage_household_another")]]) + safe_numeric(df[[paste0(prefix, "allage_not_household_another")]])),
                 "household" = c("Male" = safe_numeric(df[[paste0(prefix, "allage_household_male")]]),
                                 "Female" = safe_numeric(df[[paste0(prefix, "allage_household_female")]]),
                                 "Other gender" = safe_numeric(df[[paste0(prefix, "allage_household_another")]])),
                 "nonhousehold" = c("Male" = safe_numeric(df[[paste0(prefix, "allage_not_household_male")]]),
                                    "Female" = safe_numeric(df[[paste0(prefix, "allage_not_household_female")]]),
                                    "Other gender" = safe_numeric(df[[paste0(prefix, "allage_not_household_another")]]))
          )
        } else {
          switch(activity,
                 "overall" = c("European" = safe_numeric(df[["Euro_allage_household" %_% suffix]]) + safe_numeric(df[["Euro_allage_not_household" %_% suffix]]),
                               "Māori" = safe_numeric(df[["Maori_allage_household" %_% suffix]]) + safe_numeric(df[["Maori_allage_not_household" %_% suffix]]),
                               "Pacific Peoples" = safe_numeric(df[["Paci_allage_household" %_% suffix]]) + safe_numeric(df[["Paci_allage_not_household" %_% suffix]]),
                               "Asian" = safe_numeric(df[["Asia_allage_household" %_% suffix]])) + safe_numeric(df[["Asia_allage_not_household" %_% suffix]]),
                 "household" = c("European" = safe_numeric(df[["Euro_allage_household" %_% suffix]]),
                                 "Māori" = safe_numeric(df[["Maori_allage_household" %_% suffix]]),
                                 "Pacific Peoples" = safe_numeric(df[["Paci_allage_household" %_% suffix]]),
                                 "Asian" = safe_numeric(df[["Asia_allage_household" %_% suffix]])),
                 "nonhousehold" = c("European" = safe_numeric(df[["Euro_allage_not_household" %_% suffix]]),
                                    "Māori" = safe_numeric(df[["Maori_allage_not_household" %_% suffix]]),
                                    "Pacific Peoples" = safe_numeric(df[["Paci_allage_not_household" %_% suffix]]),
                                    "Asian" = safe_numeric(df[["Asia_allage_not_household" %_% suffix]]))
          )
        }
      }
      
      # Retrieve all-age carers values for each region
      all1 <- get_all_values(df1)
      all2 <- if (has_second) get_all_values(df2) else NULL
      
      # Compose custom text with count + percentage
      Text <- c(
        paste0("Young carers in this group: ", as.numeric(values1), "<br>",
               "Total aged 15 to 29 in this group: ", all1, "<br>(", 
               ifelse(all1 > 0, round(as.numeric(values1) / all1 * 100, 1), NA), "% of young people in this group are carers for selected household)"),
        if (has_second)
          paste0("Young carers in this group: ", as.numeric(values2), "<br>",
                 "Total aged 15 to 29 in this group: ", all2, "<br>(", 
                 ifelse(all2 > 0, round(as.numeric(values2) / all2 * 100, 1), NA), "% of young people in this group are carers for selected household)")
      )
      

      # Assemble plot data with region, category, value and text
      # Defensive check before plotting
      if (length(values1) == 0 || length(labels) == 0 ||
          (has_second && (is.null(values2) || length(values2) != length(labels)))) {
        return(NULL)
      }
      # Then safely assemble plot_data
      plot_data <- data.frame(
        Category = rep(labels, if (has_second) 2 else 1),
        Value = c(as.numeric(values1), if (has_second) as.numeric(values2) else NULL),
        Region = rep(c(clicked_region(), if (has_second) input$second_region), each = length(labels)),
        Text = Text
      )
      
      
      # Generate grouped bar chart with custom labels
      plot_ly(
        data = plot_data,
        x = ~Category, y = ~Value,
        type = "bar",
        color = ~Region,
        colors = setNames(
          c("#FD8D3C", "#FDBE85"),
          c(clicked_region(), input$second_region)
        ),
        text = ~Value, textposition = 'outside',
        textfont = list(color = "black", size = 12),
        hovertext = ~Text,
        hoverinfo = 'text',
        barmode = "group"
      ) %>%
        layout(
          title = list(text = "Comparison of Unpaid Activities", pad = list(t = 40)),
          margin = list(t = 80),
          xaxis = list(title = ""),
          yaxis = list(title = "Count")
        )
      
      
    }
  })
}

shinyApp(ui, server)
