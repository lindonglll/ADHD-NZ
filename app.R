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

# 读取 ADHD 数据集
adhd_data <- read_excel("ADHD National Online Research Survey (Responses) - Rangiwai (R).xlsx")

# 数据清理和预处理
# 假设数据包含以下列（根据实际数据调整）
# 1. 人口统计学信息（年龄、性别、地区等）
# 2. ADHD 相关症状评分
# 3. 生活质量指标
# 4. 治疗相关信息

# 数据清理函数
clean_adhd_data <- function(data) {
  # 移除完全为空的行
  data <- data[!apply(data, 1, function(x) all(is.na(x))), ]
  
  # 处理缺失值
  data <- data %>%
    mutate(across(everything(), ~ifelse(. == "", NA, .)))
  
  return(data)
}

# 清理数据
adhd_clean <- clean_adhd_data(adhd_data)

# 获取数值型列（用于统计分析）
numeric_cols <- names(adhd_clean)[sapply(adhd_clean, is.numeric)]
categorical_cols <- names(adhd_clean)[sapply(adhd_clean, function(x) is.character(x) || is.factor(x))]

# 安全数值转换函数
safe_numeric <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return(0)
  as.numeric(x)
}

# UI 定义
ui <- dashboardPage(
  dashboardHeader(title = "ADHD 新西兰研究数据仪表板"),
  dashboardSidebar(
    tags$head(tags$style(HTML(
      ".main-sidebar { background-color: #ffffff !important; }"
    ))),
    sidebarMenu(
      menuItem("首页", tabName = "tab-home", icon = icon("home")),
      menuItem("数据概览", tabName = "tab-overview", icon = icon("table")),
      menuItem("统计分析", tabName = "tab-analysis", icon = icon("chart-bar")),
      menuItem("地理分布", tabName = "tab-map", icon = icon("globe"))
    ),
    br(),
    div(style = "position: absolute; bottom: 200px; left: 10px; right: 10px; font-size: 16px; border: 1px solid #ccc; padding: 10px; background-color: #ffffff; border-radius: 5px;",
        HTML("<span style='color: black;'>ADHD 新西兰在线研究调查<br><br>
              最后更新:<br>2025年<br><br>
              联系邮箱:<br><span style='font-size: 13px; color: black;'>研究团队</span></span>")
    )
  ),
  dashboardBody(
    tabItems(
      # 首页
      tabItem(tabName = "tab-home",
              fluidPage(
                div(style = "border: 2px solid #ccc; border-radius: 10px; padding: 30px; background-color: #fefefe; box-shadow: 2px 2px 10px rgba(0,0,0,0.1);",
                    
                    h2("ADHD 新西兰在线研究调查数据仪表板"),
                    
                    h4("📘 数据来源"),
                    p("本仪表板基于 ADHD 新西兰在线研究调查数据构建。",
                      "该调查旨在了解新西兰 ADHD 患者的现状、需求和挑战。"),
                    
                    br(),
                    
                    h4("👥 目标人群"),
                    p("调查对象包括："),
                    tags$ul(
                      tags$li("ADHD 患者"),
                      tags$li("ADHD 患者的家庭成员"),
                      tags$li("医疗保健提供者"),
                      tags$li("教育工作者")
                    ),
                    
                    br(),
                    
                    h4("🎯 研究目标"),
                    tags$ul(
                      tags$li(em("了解新西兰 ADHD 患者的分布和特征")),
                      tags$li(em("评估 ADHD 对患者生活质量的影响")),
                      tags$li(em("分析治疗和服务的可及性")),
                      tags$li(em("识别改善 ADHD 护理的机遇"))
                    ),
                    
                    br(),
                    
                    h4("📊 数据概览"),
                    fluidRow(
                      column(4,
                             div(style = "text-align: center; padding: 20px; background-color: #e8f4fd; border-radius: 10px;",
                                 h3(textOutput("total_respondents")),
                                 p("总受访者数")
                             )
                      ),
                      column(4,
                             div(style = "text-align: center; padding: 20px; background-color: #f0f8e8; border-radius: 10px;",
                                 h3(textOutput("avg_age")),
                                 p("平均年龄")
                             )
                      ),
                      column(4,
                             div(style = "text-align: center; padding: 20px; background-color: #fff8e8; border-radius: 10px;",
                                 h3(textOutput("completion_rate")),
                                 p("问卷完成率")
                             )
                      )
                    )
                )
              )
      ),
      
      # 数据概览页
      tabItem(tabName = "tab-overview",
              fluidPage(
                tabsetPanel(id = "overview_mode", type = "tabs",
                            
                            tabPanel("数据表格",
                                     div(style = "text-align: center;",
                                         h2("ADHD 研究数据概览")
                                     ),
                                     fluidRow(
                                       column(4,
                                              selectInput("filter_column", "选择筛选列:", 
                                                         choices = c("全部", categorical_cols))
                                       ),
                                       column(4,
                                              uiOutput("filter_value_selector")
                                       ),
                                       column(4,
                                              downloadButton("download_data", "下载数据")
                                       )
                                     ),
                                     fluidRow(
                                       column(12,
                                              dataTableOutput("data_table")
                                       )
                                     )
                            ),
                            
                            tabPanel("数据质量",
                                     fluidRow(
                                       column(6,
                                              h3("缺失值分析"),
                                              plotlyOutput("missing_plot")
                                       ),
                                       column(6,
                                              h3("数据类型分布"),
                                              plotlyOutput("data_type_plot")
                                       )
                                     ),
                                     fluidRow(
                                       column(12,
                                              h3("数据质量报告"),
                                              verbatimTextOutput("quality_report")
                                       )
                                     )
                            )
                )
              )
      ),
      
      # 统计分析页
      tabItem(tabName = "tab-analysis",
              fluidPage(
                tabsetPanel(id = "analysis_mode", type = "tabs",
                            
                            tabPanel("描述性统计",
                                     sidebarLayout(
                                       sidebarPanel(width = 3,
                                                    selectInput("analysis_variable", "选择分析变量:", 
                                                               choices = numeric_cols),
                                                    selectInput("group_by_var", "分组变量 (可选):", 
                                                               choices = c("无", categorical_cols)),
                                                    radioButtons("plot_type", "图表类型:",
                                                                 choices = c("直方图" = "histogram", 
                                                                            "箱线图" = "boxplot",
                                                                            "密度图" = "density"))
                                       ),
                                       mainPanel(
                                         div(style = "height: calc(100vh - 100px);",
                                             plotlyOutput("analysis_plot", height = "100%")
                                         )
                                       )
                                     )
                            ),
                            
                            tabPanel("相关性分析",
                                     fluidRow(
                                       column(4,
                                              selectInput("corr_var1", "变量 1:", choices = numeric_cols),
                                              selectInput("corr_var2", "变量 2:", choices = numeric_cols)
                                       ),
                                       column(8,
                                              h3("相关性分析结果"),
                                              verbatimTextOutput("correlation_result"),
                                              plotlyOutput("correlation_plot")
                                       )
                                     )
                            ),
                            
                            tabPanel("分组比较",
                                     sidebarLayout(
                                       sidebarPanel(width = 3,
                                                    selectInput("compare_var", "比较变量:", choices = numeric_cols),
                                                    selectInput("group_var", "分组变量:", choices = categorical_cols),
                                                    radioButtons("test_type", "统计检验:",
                                                                 choices = c("t检验" = "t_test", 
                                                                            "方差分析" = "anova",
                                                                            "非参数检验" = "wilcox"))
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
      
      # 地理分布页
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

# 服务器逻辑
server <- function(input, output, session) {
  
  # 首页统计信息
  output$total_respondents <- renderText({
    nrow(adhd_clean)
  })
  
  output$avg_age <- renderText({
    # 假设有年龄列，根据实际数据调整
    age_col <- names(adhd_clean)[grepl("age|年龄", names(adhd_clean), ignore.case = TRUE)]
    if (length(age_col) > 0) {
      avg_age <- mean(as.numeric(adhd_clean[[age_col[1]]]), na.rm = TRUE)
      round(avg_age, 1)
    } else {
      "N/A"
    }
  })
  
  output$completion_rate <- renderText({
    # 计算问卷完成率
    total_questions <- ncol(adhd_clean)
    completion_rates <- apply(adhd_clean, 1, function(row) {
      sum(!is.na(row)) / total_questions
    })
    paste0(round(mean(completion_rates, na.rm = TRUE) * 100, 1), "%")
  })
  
  # 数据表格
  output$filter_value_selector <- renderUI({
    if (input$filter_column != "全部") {
      unique_values <- unique(adhd_clean[[input$filter_column]])
      selectInput("filter_value", "选择值:", choices = c("全部", unique_values))
    }
  })
  
  filtered_data <- reactive({
    data <- adhd_clean
    if (input$filter_column != "全部" && !is.null(input$filter_value) && input$filter_value != "全部") {
      data <- data[data[[input$filter_column]] == input$filter_value, ]
    }
    data
  })
  
  output$data_table <- renderDataTable({
    datatable(filtered_data(), 
              options = list(scrollX = TRUE, pageLength = 10),
              filter = "top")
  })
  
  # 下载数据
  output$download_data <- downloadHandler(
    filename = function() {
      paste("adhd_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  # 缺失值分析
  output$missing_plot <- renderPlotly({
    missing_counts <- colSums(is.na(adhd_clean))
    missing_percent <- (missing_counts / nrow(adhd_clean)) * 100
    
    plot_ly(x = names(missing_percent), y = missing_percent, type = 'bar',
            marker = list(color = '#ff7f0e')) %>%
      layout(title = "各列缺失值百分比",
             xaxis = list(title = "变量名", tickangle = 45),
             yaxis = list(title = "缺失值百分比 (%)"))
  })
  
  # 数据类型分布
  output$data_type_plot <- renderPlotly({
    data_types <- sapply(adhd_clean, class)
    type_counts <- table(data_types)
    
    plot_ly(labels = names(type_counts), values = type_counts, type = 'pie') %>%
      layout(title = "数据类型分布")
  })
  
  # 数据质量报告
  output$quality_report <- renderPrint({
    cat("数据质量报告\n")
    cat("============\n\n")
    cat("总行数:", nrow(adhd_clean), "\n")
    cat("总列数:", ncol(adhd_clean), "\n")
    cat("数值型变量数:", length(numeric_cols), "\n")
    cat("分类型变量数:", length(categorical_cols), "\n")
    cat("总缺失值数:", sum(is.na(adhd_clean)), "\n")
    cat("缺失值比例:", round(sum(is.na(adhd_clean)) / (nrow(adhd_clean) * ncol(adhd_clean)) * 100, 2), "%\n")
  })
  
  # 描述性统计分析
  output$analysis_plot <- renderPlotly({
    req(input$analysis_variable)
    
    var_data <- adhd_clean[[input$analysis_variable]]
    var_data <- as.numeric(var_data)
    var_data <- var_data[!is.na(var_data)]
    
    if (input$plot_type == "histogram") {
      plot_ly(x = var_data, type = 'histogram', nbinsx = 30) %>%
        layout(title = paste("分布图:", input$analysis_variable),
               xaxis = list(title = input$analysis_variable),
               yaxis = list(title = "频数"))
    } else if (input$plot_type == "boxplot") {
      plot_ly(y = var_data, type = 'box') %>%
        layout(title = paste("箱线图:", input$analysis_variable),
               yaxis = list(title = input$analysis_variable))
    } else if (input$plot_type == "density") {
      density_data <- density(var_data)
      plot_ly(x = density_data$x, y = density_data$y, type = 'scatter', mode = 'lines') %>%
        layout(title = paste("密度图:", input$analysis_variable),
               xaxis = list(title = input$analysis_variable),
               yaxis = list(title = "密度"))
    }
  })
  
  # 相关性分析
  output$correlation_result <- renderPrint({
    req(input$corr_var1, input$corr_var2)
    
    var1 <- as.numeric(adhd_clean[[input$corr_var1]])
    var2 <- as.numeric(adhd_clean[[input$corr_var2]])
    
    # 移除缺失值
    complete_cases <- complete.cases(var1, var2)
    var1 <- var1[complete_cases]
    var2 <- var2[complete_cases]
    
    if (length(var1) > 0) {
      cor_result <- cor.test(var1, var2)
      cat("皮尔逊相关系数:", round(cor_result$estimate, 3), "\n")
      cat("p值:", round(cor_result$p.value, 4), "\n")
      cat("95%置信区间:", round(cor_result$conf.int, 3), "\n")
    } else {
      cat("数据不足，无法计算相关性")
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
        layout(title = paste("相关性散点图:", input$corr_var1, "vs", input$corr_var2),
               xaxis = list(title = input$corr_var1),
               yaxis = list(title = input$corr_var2))
    }
  })
  
  # 分组比较
  output$comparison_plot <- renderPlotly({
    req(input$compare_var, input$group_var)
    
    compare_data <- adhd_clean %>%
      select(!!sym(input$compare_var), !!sym(input$group_var)) %>%
      filter(!is.na(!!sym(input$compare_var)), !is.na(!!sym(input$group_var)))
    
    if (input$plot_type == "boxplot") {
      plot_ly(compare_data, x = ~get(input$group_var), y = ~get(input$compare_var), 
              type = 'box', color = ~get(input$group_var)) %>%
        layout(title = paste("分组比较:", input$compare_var, "by", input$group_var),
               xaxis = list(title = input$group_var),
               yaxis = list(title = input$compare_var))
    } else {
      # 默认显示箱线图
      plot_ly(compare_data, x = ~get(input$group_var), y = ~get(input$compare_var), 
              type = 'box', color = ~get(input$group_var)) %>%
        layout(title = paste("分组比较:", input$compare_var, "by", input$group_var),
               xaxis = list(title = input$group_var),
               yaxis = list(title = input$compare_var))
    }
  })
  
  # 地图相关（简化版）
  output$nz_map <- renderLeaflet({
    leaflet() %>%
      setView(lng = 174.7645, lat = -40.9006, zoom = 5) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addMarkers(lng = 174.7645, lat = -40.9006, 
                 popup = "新西兰 ADHD 研究数据收集点")
  })
  
  output$map_info_box <- renderUI({
    absolutePanel(class = "floating-box", draggable = TRUE,
                  h4("地理分布信息"),
                  p("ADHD 研究数据的地理分布分析将在此显示。"),
                  p("点击地图上的标记查看详细信息。")
    )
  })
}

# 运行应用
shinyApp(ui, server)
