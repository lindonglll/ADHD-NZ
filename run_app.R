# ADHD 新西兰研究数据仪表板 - 启动脚本

# 检查数据文件是否存在
data_file <- "ADHD National Online Research Survey (Responses) - Rangiwai (R).xlsx"
if (!file.exists(data_file)) {
  stop("错误：找不到数据文件 '", data_file, "'。请确保文件在当前目录中。")
}

# 检查必需的包
required_packages <- c("shiny", "shinydashboard", "readxl", "dplyr", "plotly", "DT", "leaflet", "sf", "ggplot2", "tidyr")
missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

if (length(missing_packages) > 0) {
  cat("缺少以下包：", paste(missing_packages, collapse = ", "), "\n")
  cat("请先运行 install_packages.R 安装依赖包。\n")
  stop("依赖包缺失")
}

# 加载包
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

cat("正在启动 ADHD 新西兰研究数据仪表板...\n")
cat("数据文件：", data_file, "\n")
cat("应用将在浏览器中打开。\n")

# 启动应用
shiny::runApp()
