# ADHD 新西兰研究数据仪表板 - 依赖包安装脚本

# 检查并安装所需的 R 包
required_packages <- c(
  "shiny",
  "shinydashboard", 
  "readxl",
  "dplyr",
  "plotly",
  "DT",
  "leaflet",
  "sf",
  "ggplot2",
  "tidyr"
)

# 安装缺失的包
missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

if (length(missing_packages) > 0) {
  cat("正在安装缺失的包...\n")
  install.packages(missing_packages, dependencies = TRUE)
  cat("包安装完成！\n")
} else {
  cat("所有必需的包都已安装。\n")
}

# 验证安装
cat("\n验证包安装状态:\n")
for (pkg in required_packages) {
  if (require(pkg, character.only = TRUE)) {
    cat(paste("✓", pkg, "已安装\n"))
  } else {
    cat(paste("✗", pkg, "安装失败\n"))
  }
}

cat("\n安装完成！现在可以运行 shiny::runApp() 启动应用。\n")
