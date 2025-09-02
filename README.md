# ADHD 新西兰研究数据仪表板

这是一个基于 Shiny 构建的交互式数据仪表板，用于分析和可视化 ADHD 新西兰在线研究调查数据。

## 功能特点

### 📊 数据概览
- **首页统计**: 显示总受访者数、平均年龄、问卷完成率等关键指标
- **数据表格**: 交互式数据表格，支持筛选和搜索
- **数据质量分析**: 缺失值分析、数据类型分布等

### 📈 统计分析
- **描述性统计**: 直方图、箱线图、密度图等多种可视化方式
- **相关性分析**: 皮尔逊相关系数计算和散点图
- **分组比较**: t检验、方差分析、非参数检验等统计方法

### 🗺️ 地理分布
- **交互式地图**: 基于 Leaflet 的新西兰地图
- **地理标记**: 显示数据收集点和分布情况

## 安装和运行

### 系统要求
- R (版本 3.6.0 或更高)
- RStudio (推荐)

### 依赖包安装
```r
# 安装必要的 R 包
install.packages(c("shiny", "shinydashboard", "readxl", "dplyr", "plotly", 
                   "DT", "leaflet", "sf", "ggplot2", "tidyr"))
```

### 运行应用
1. 确保 `ADHD National Online Research Survey (Responses) - Rangiwai (R).xlsx` 文件在同一目录下
2. 在 R 中运行：
```r
shiny::runApp()
```

## 数据格式要求

应用期望的 Excel 文件应包含以下类型的列：

### 人口统计学信息
- 年龄 (Age)
- 性别 (Gender)
- 地区 (Region)
- 教育水平 (Education)

### ADHD 相关指标
- 症状评分 (Symptom Scores)
- 诊断信息 (Diagnosis Information)
- 治疗历史 (Treatment History)

### 生活质量指标
- 生活质量评分 (Quality of Life Scores)
- 功能评估 (Functional Assessment)
- 社会支持 (Social Support)

## 使用说明

### 1. 首页
- 查看数据概览和关键统计信息
- 了解研究背景和目标

### 2. 数据概览
- **数据表格**: 浏览和筛选原始数据
- **数据质量**: 检查数据完整性和质量

### 3. 统计分析
- **描述性统计**: 选择变量和图表类型进行分析
- **相关性分析**: 探索变量间的关系
- **分组比较**: 进行统计检验和比较分析

### 4. 地理分布
- 查看数据的地理分布情况
- 点击地图标记获取详细信息

## 自定义和扩展

### 添加新的分析功能
1. 在 `server` 函数中添加新的 `renderPlotly` 或 `renderDataTable`
2. 在 `ui` 中添加相应的输入控件
3. 更新侧边栏菜单

### 修改数据源
1. 更新 `read_excel()` 函数中的文件名
2. 根据实际数据结构调整列名映射
3. 修改数据清理函数

### 添加新的可视化
```r
# 示例：添加新的图表
output$new_plot <- renderPlotly({
  # 你的绘图代码
  plot_ly(data, x = ~x_var, y = ~y_var, type = 'scatter')
})
```

## 故障排除

### 常见问题

1. **数据读取错误**
   - 检查 Excel 文件路径和名称
   - 确保文件格式正确

2. **包依赖问题**
   - 运行 `install.packages()` 安装缺失的包
   - 检查 R 版本兼容性

3. **内存不足**
   - 减少数据量或优化数据处理
   - 增加 R 内存限制

### 调试模式
```r
# 启用调试模式
options(shiny.trace = TRUE)
shiny::runApp()
```

## 联系信息

如有问题或建议，请联系研究团队。

## 许可证

本项目仅供研究使用。
