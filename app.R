# ===============================================
# ADHD NZ Survey Shiny — single figure with facets (fixed Likert + many-to-many)
# ===============================================

library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(forcats)
library(stringr)
library(tidyselect)
library(rlang)

# ---------- 1) Load & Clean ----------
xlsx_path <- "ADHD National Online Research Survey (Responses) - Rangiwai (R).xlsx"
dat_raw <- read_excel(xlsx_path, sheet = "Main")

clean_names <- function(nms) gsub("^\\s+|\\s+$", "", nms)
names(dat_raw) <- clean_names(names(dat_raw))

trim_char_cols <- function(df){
  df %>% mutate(across(where(is.character), \(x){
    y <- str_trim(x)
    y[y==""] <- NA
    y[tolower(y)=="missing"] <- NA
    y
  }))
}
dat <- trim_char_cols(dat_raw)

# Gender
if ("Gender" %in% names(dat)) {
  dat <- dat %>% mutate(Gender = ifelse(is.na(Gender) | Gender=="", NA, str_trim(Gender)))
}

# Age -> ordered factor
if ("Age" %in% names(dat)) {
  dat <- dat %>%
    mutate(
      Age = str_trim(Age),
      Age = str_replace_all(Age, "[–—−]", "-"),
      Age = str_replace_all(Age, regex("\\s+and\\s+above$", ignore_case = TRUE), "+"),
      Age = ifelse(str_detect(Age, "^55\\s*\\+\\s*$"), "55+", Age),
      Age = case_when(
        str_detect(Age, "^18\\s*-\\s*24$") ~ "18–24",
        str_detect(Age, "^25\\s*-\\s*34$") ~ "25–34",
        str_detect(Age, "^35\\s*-\\s*44$") ~ "35–44",
        str_detect(Age, "^45\\s*-\\s*54$") ~ "45–54",
        str_detect(Age, "^55\\+\\s*$")     ~ "55+",
        TRUE ~ NA_character_
      ),
      Age = factor(Age, levels = c("18–24","25–34","35–44","45–54","55+"), ordered = TRUE)
    )
}

# Region
if ("Region" %in% names(dat)) {
  dat <- dat %>% mutate(Region = ifelse(is.na(Region) | tolower(Region)=="missing", NA, str_trim(Region)))
}

# Drop fully blank rows
is_blank_or_na <- function(x){
  if (is.character(x)) { xx <- str_trim(x); is.na(xx) | xx=="" } else { is.na(x) }
}
row_all_blank <- apply(dat, 1, function(r) all(is_blank_or_na(r)))
dat <- dat[!row_all_blank, , drop = FALSE]
dat$.rid <- seq_len(nrow(dat))

# ---------- 2) Column Groups ----------
all_cols <- names(dat)

# Ethnicity (for facet)
ethnicity_cols <- intersect(c("Māori","European","Asian","MELAA","Pacific Peoples","Other Ethnicity"), all_cols)

# ADHD grouped (0/1)
treatment_cols  <- intersect(c("medicator","Medication","therapy","Therapy"), all_cols)
challenges_cols <- intersect(c("Time_mgmt","Time_mgnt","Time management","Focus","Impulsivity","Organisation","Memory"), all_cols)
support_cols    <- intersect(c("Family","Friends","Healthcare providers","Community groups","Online resources"), all_cols)
diagnosis_cols  <- intersect(c("Formal_Diagnosis","Wait_List","Diagnosis_System"), all_cols)

# Impacts (single column Likert)
impact_cols     <- intersect(c("Education_Effect","Occupation_Effect","Social_Effect","Matauranga","Support_effect"), all_cols)

# ---------- 3) Helpers ----------
# 0/1 normalization
norm01 <- function(x){
  if (is.numeric(x)) return(x)
  if (is.logical(x)) return(as.numeric(x))
  if (is.character(x)) {
    xx <- tolower(trimws(x))
    return(case_when(
      xx %in% c("1","yes","true","y") ~ 1,
      xx %in% c("0","no","false","n") ~ 0,
      TRUE ~ NA_real_
    ))
  }
  rep(NA_real_, length(x))
}

# Convert text/numeric Likert to ordered factor
coerce_likert_1to5 <- function(v){
  # Try numeric first
  v_num <- suppressWarnings(as.numeric(v))
  if (any(!is.na(v_num))) {
    v_num[!(v_num %in% 1:5)] <- NA
    return(factor(v_num, levels = 1:5, labels = c("1","2","3","4","5"), ordered = TRUE))
  }
  # Text five-level
  vv <- tolower(trimws(as.character(v)))
  map <- c(
    "not important at all" = "Not important at all",
    "not very important"   = "Not very important",
    "neutral"              = "Neutral",
    "somewhat important"   = "Somewhat important",
    "very important"       = "Very important"
  )
  lbl <- unname(map[vv])
  # Others (blank, all selected, unknown) set to NA
  fac <- factor(lbl, levels = unname(map), ordered = TRUE)
  fac
}

# ADHD 0/1 group -> (.rid, Option) keep only value==1
adhd_long <- function(dat, cols){
  cols <- intersect(cols, names(dat))
  validate(need(length(cols)>=2, "Selected ADHD group has <2 valid columns."))
  df <- dat %>% select(.rid, all_of(cols)) %>%
    pivot_longer(all_of(cols), names_to="Option", values_to="value")
  df$value <- norm01(df$value)
  df <- df %>% filter(!is.na(value), value==1) %>% select(.rid, Option)
  df$Option <- factor(df$Option, levels = cols)
  df
}

# Demographics facet -> (.rid, Facet)
demo_long <- function(dat, facet_key){
  if (facet_key == "Age")      return(dat %>% select(.rid, Facet = Age)    %>% filter(!is.na(Facet)))
  if (facet_key == "Gender")   return(dat %>% select(.rid, Facet = Gender) %>% filter(!is.na(Facet)))
  if (facet_key == "Region")   return(dat %>% select(.rid, Facet = Region) %>% filter(!is.na(Facet)))
  if (facet_key == "Ethnicity"){
    validate(need(length(ethnicity_cols)>=2, "Ethnicity columns not found."))
    el <- dat %>% select(.rid, all_of(ethnicity_cols)) %>%
      pivot_longer(all_of(ethnicity_cols), names_to="Facet", values_to="value")
    el$value <- norm01(el$value)
    el <- el %>% filter(!is.na(value), value==1) %>% select(.rid, Facet)
    el$Facet <- factor(el$Facet, levels = ethnicity_cols)
    return(el)
  }
  dat %>% transmute(.rid, Facet = factor("(All)"))
}

# 0/1 group: normalize by facet
build_cross_prop_grouped <- function(dat, adhd_cols, facet_key){
  a <- adhd_long(dat, adhd_cols)
  f <- demo_long(dat, facet_key)
  j <- inner_join(a, f, by = ".rid", relationship = "many-to-many")
  validate(need(nrow(j)>0, "No rows after combining ADHD selections and facet."))
  j %>% count(Facet, Option, name="n") %>%
    group_by(Facet) %>% mutate(prop = n/sum(n)) %>% ungroup()
}

# Likert single column (numeric 1-5 or text five-level): normalize by facet
build_cross_prop_likert <- function(dat, col, facet_key){
  validate(need(col %in% names(dat), paste0("Impact column not found: ", col)))
  L <- dat %>% transmute(.rid, Option = coerce_likert_1to5(.data[[col]])) %>%
    filter(!is.na(Option))
  validate(need(nrow(L)>0, paste0("No valid Likert (1–5 or five-level text) in ", col)))
  f <- demo_long(dat, facet_key)
  j <- inner_join(L, f, by = ".rid", relationship = "many-to-many")
  validate(need(nrow(j)>0, "No rows after combining impact values and facet."))
  j %>% count(Facet, Option, name="n") %>%
    group_by(Facet) %>% mutate(prop = n/sum(n)) %>% ungroup()
}

# Single column [categorical/text/numeric] → statistics by facet
build_cross_prop_singlecat <- function(dat, col, facet_key){
  validate(need(col %in% names(dat), paste0("Column not found: ", col)))
  L <- dat %>% transmute(.rid, Option = as.character(.data[[col]])) %>%
    filter(!is.na(Option), trimws(Option)!="")          # Remove blanks
  validate(need(nrow(L) > 0, paste0("No non-missing values in ", col)))
  f <- demo_long(dat, facet_key)
  j <- inner_join(L, f, by = ".rid", relationship = "many-to-many")
  validate(need(nrow(j) > 0, "No rows after combining column and facet."))
  j %>% count(Facet, Option, name = "n") %>%
    group_by(Facet) %>% mutate(prop = n / sum(n)) %>% ungroup()
}

# Unified plotting (Count or Percent)
plot_cross_group <- function(df, title_txt, mode=c("pct","cnt")){
  mode <- match.arg(mode)
  df <- df %>% mutate(Option = fct_reorder(Option, prop, .fun = mean))
  p <- ggplot(df, aes(x = Option, y = if (mode=="pct") prop else n, fill = Option)) +
    geom_col() +
    geom_text(aes(label = if (mode=="pct") scales::percent(prop, accuracy = 0.1) else n),
              vjust = 0.5, size = 6, fontface = "bold") +
    labs(y = if (mode=="pct") "Percent" else "Count", x = NULL, title = title_txt) +
    scale_y_continuous(labels = if (mode=="pct") scales::percent else waiver(), limits = c(0, NA)) +
    scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 30)) +
    theme_bw(base_size = 16) + guides(fill = "none") + coord_flip()
  if (!all(as.character(df$Facet) == "(All)")) {
    p <- p + facet_grid(~ Facet, scales = "free_x")
  }
  p
}

# ---------- 4) Sidebar ----------
sidebarInput <- function(id, date = "01-01-2025") {
  ns <- NS(id)
  defaultDate <- as.Date("01-01-2025", "%d-%m-%Y")
  date <- as.Date(date, "%d-%m-%Y")
  date <- ifelse(date > defaultDate, format(date, "%d-%m-%Y"), format(defaultDate, "%d-%m-%Y"))
  
  tagList(
    selectInput(
      ns("adhd_var"),
      label = HTML('<font size="4">ADHD-related group</font>'),
      choices = list(
        "Treatment (grouped)"         = "__TREATMENT__",
        "Challenges (grouped)"        = "__CHALLENGES__",
        "Support (grouped)"           = "__SUPPORT__",
        "Diagnosis (grouped)"         = "__DIAGNOSIS__",
        "Impacts (pick one 1–5/text)" = "__IMPACT_SINGLE__",
        "Formal_Diagnosis (single)"   = "__FORMAL_DIAGNOSIS__",
        "Diagnosis_Age (single)"      = "__DIAGNOSIS_AGE__",
        "Diagnosis_Who (single)"      = "__DIAGNOSIS_WHO__",
        "Diagnosis_System (single)"   = "__DIAGNOSIS_SYSTEM__",
        "Wait_List (single)"          = "__WAIT_LIST__",
        "Treatment_Effect (single)"   = "__TREATMENT_EFFECT__",
        "Matauranga (single)"         = "__MATAURANGA__",
        "Support_effect (single)"     = "__SUPPORT_EFFECT__"
      ),
      selected = "__CHALLENGES__"
    ),
    conditionalPanel(
      condition = sprintf("input['%s'] == '__IMPACT_SINGLE__'", ns("adhd_var")),
      selectInput(
        ns("impact_col"),
        label = HTML('<font size="4">Impact column</font>'),
        choices = as.list(impact_cols),
        selected = if (length(impact_cols)) impact_cols[1] else ""
      )
    ),
    radioButtons(
      ns("show_mode"),
      label = HTML('<font size="3">Show</font>'),
      choices = c("Percent" = "pct", "Count" = "cnt"),
      selected = "pct",
      inline = TRUE
    ),
    selectInput(
      ns("facet_by"),
      label = HTML('<font size="4">Facet by</font>'),
      choices = list(
        "None"                 = "__NONE__",
        "Age group"            = "__AGE__",
        "Gender"               = "__GENDER__",
        "Ethnicity (grouped)"  = "__ETHNICITY__",
        "Region"               = "__REGION__"
      ),
      selected = "__ETHNICITY__"
    ),
    box(
      h4("ADHD New Zealand Online Research Survey"),
      h4("Latest Update:"),
      h4(date),
      width = 12,
      background = "black"
    )
  )
}

# ---------- 5) UI ----------
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "ADHD New Zealand Research Survey", titleWidth = 420),
  dashboardSidebar(sidebarInput("side", date = "1-1-2025")),
  dashboardBody(
    box(
      h2(textOutput("title")),
      plotOutput("mainPlot", height = "720px"),
      width = 12
    )
  )
)

# ---------- 6) Server ----------
server <- function(input, output) {
  
  output$title <- renderText({
    adhd_lab <- switch(req(input[["side-adhd_var"]]),
                       "__TREATMENT__"        = "Treatment",
                       "__CHALLENGES__"       = "Challenges",
                       "__SUPPORT__"          = "Support systems",
                       "__DIAGNOSIS__"        = "Diagnosis",
                       "__IMPACT_SINGLE__"    = paste0("Impact: ", req(input[["side-impact_col"]])),
                       "__FORMAL_DIAGNOSIS__" = "Formal_Diagnosis",
                       "__DIAGNOSIS_AGE__"    = "Diagnosis_Age",
                       "__DIAGNOSIS_WHO__"    = "Diagnosis_Who",
                       "__DIAGNOSIS_SYSTEM__" = "Diagnosis_System",
                       "__WAIT_LIST__"        = "Wait_List",
                       "__TREATMENT_EFFECT__" = "Treatment_Effect",
                       "__MATAURANGA__"       = "Matauranga",
                       "__SUPPORT_EFFECT__"   = "Support_effect"
    )
    facet_lab <- switch(req(input[["side-facet_by"]]),
                        "__NONE__" = "Overall",
                        "__AGE__"  = "by Age",
                        "__GENDER__" = "by Gender",
                        "__ETHNICITY__" = "by Ethnicity",
                        "__REGION__" = "by Region"
    )
    paste("ADHD New Zealand Research Survey –", adhd_lab, facet_lab)
  })
  
  output$mainPlot <- renderPlot({
    adhd_key <- req(input[["side-adhd_var"]])
    facet_key <- switch(req(input[["side-facet_by"]]),
                        "__NONE__" = "None",
                        "__AGE__"  = "Age",
                        "__GENDER__" = "Gender",
                        "__ETHNICITY__" = "Ethnicity",
                        "__REGION__" = "Region")
    mode <- req(input[["side-show_mode"]])  # "pct" or "cnt"
    
    # Impacts: specified column (supports numeric 1-5 or text five-level)
    if (adhd_key == "__IMPACT_SINGLE__") {
      col <- req(input[["side-impact_col"]])
      cross_df <- build_cross_prop_likert(dat, col, facet_key)
      title_txt <- paste0(col, if (facet_key!="None") paste0(" – by ", facet_key) else "")
      return(plot_cross_group(cross_df, title_txt, mode))
    }
    
    # New single columns (text/category/numeric; *_Effect/Matauranga/Support_effect use likert)
    if (adhd_key %in% c(
      "__FORMAL_DIAGNOSIS__","__DIAGNOSIS_AGE__","__DIAGNOSIS_WHO__",
      "__DIAGNOSIS_SYSTEM__","__WAIT_LIST__","__TREATMENT_EFFECT__",
      "__MATAURANGA__","__SUPPORT_EFFECT__"
    )) {
      colname <- switch(adhd_key,
                        "__FORMAL_DIAGNOSIS__" = "Formal_Diagnosis",
                        "__DIAGNOSIS_AGE__"    = "Diagnosis_Age",
                        "__DIAGNOSIS_WHO__"    = "Diagnosis_Who",
                        "__DIAGNOSIS_SYSTEM__" = "Diagnosis_System",
                        "__WAIT_LIST__"        = "Wait_List",
                        "__TREATMENT_EFFECT__" = "Treatment_Effect",
                        "__MATAURANGA__"       = "Matauranga",
                        "__SUPPORT_EFFECT__"   = "Support_effect"
      )
      
      likert_cols <- c("Treatment_Effect","Matauranga","Support_effect")
      cross_df <- if (colname %in% likert_cols) {
        build_cross_prop_likert(dat, colname, facet_key)
      } else {
        build_cross_prop_singlecat(dat, colname, facet_key)
      }
      
      title_txt <- paste0(colname, if (facet_key!="None") paste0(" – by ", facet_key) else "")
      return(plot_cross_group(cross_df, title_txt, mode))
    }
    
    # Multi-column 0/1 groups
    adhd_cols <- switch(adhd_key,
                        "__TREATMENT__"  = treatment_cols,
                        "__CHALLENGES__" = challenges_cols,
                        "__SUPPORT__"    = support_cols,
                        "__DIAGNOSIS__"  = diagnosis_cols,
                        character(0)
    )
    validate(need(length(adhd_cols)>=2, "Selected group has <2 valid columns."))
    cross_df <- build_cross_prop_grouped(dat, adhd_cols, facet_key)
    title_txt <- paste0(
      switch(adhd_key,
             "__TREATMENT__"  = "Treatment (grouped)",
             "__CHALLENGES__" = "Challenges (grouped)",
             "__SUPPORT__"    = "Support systems (grouped)",
             "__DIAGNOSIS__"  = "Diagnosis (grouped)"
      ),
      if (facet_key!="None") paste0(" – by ", facet_key) else ""
    )
    plot_cross_group(cross_df, title_txt, mode)
  })
}

# ---------- 7) Run ----------
shinyApp(ui, server)




