library(shiny)
library(shinydashboard)

source("getdata2023.R")



library(tidyverse)
library(questionr)

# ggplots for the survey

plotOutWeighted <-
  function(input, output, session, dat, datRaw) {
    
    
    reactive({
      dat$X <- datRaw %>%
        pull(names(fullName[fullName == req(input$varname)]))
      
      #Remove the missing
      if(is.numeric(dat$X)){
        if (any(attr(dat$X, "labels") %in% 99)) {
          dat <- dat %>% filter(as.numeric(X) != 99)
        } else {
          dat <- dat %>% filter(as.numeric(X) != 9)
        }
      } else {
        if (any(names(attr(dat$X, "labels")) == "Can't choose")) {
          
          dat<- 
            dat %>% filter(as.character(X) != 
                             attr(dat$X, "labels")[
                               names(attr(dat$X, "labels")) == 
                                 "Can't choose"])
        }
        
        dat<- 
          dat %>% filter(as.character(X) != "")
      }
      
      
      dat$XX <- as_factor(dat$X)
      
      # Initialise ggplot2 object
      g <-
        if (input$stratified == "None") {
          ggplot(dat, aes(
            x = XX,
            weight = wgt,
            group = 1
          ))
        } else if (input$stratified == "Gender") {
          ggplot(dat %>% filter(!is.na(Gender)), 
                 aes(x = XX,
                     weight = wgt,
                     group = Gender
                 ))
        } else if (input$stratified == "Age group") {
          ggplot(dat %>% filter(!is.na(Age)), aes(
            x = XX,
            weight = wgt,
            group = Age
          ))
        } else if (input$stratified == "Gender by Age") {
          ggplot(dat %>% filter(!is.na(Gender), !is.na(Age)), aes(
            x = XX,
            weight = wgt,
            group = interaction(Age, Gender)
          ))
        }
      
      yLimits <-
        if (input$stratified == "None") {
          max(prop.table(wtd.table(dat$XX,
                                   weights = dat$wgt)), na.rm = TRUE) + .01
        } else if (input$stratified == "Gender") {
          max(prop.table(wtd.table(dat$XX, dat$Gender,
                                   weights = dat$wgt), 2), na.rm = TRUE) + .05
        } else if (input$stratified == "Age group") {
          max(prop.table(wtd.table(dat$XX, dat$Age,
                                   weights = dat$wgt), 2), na.rm = TRUE) + .15
        } else if (input$stratified == "Gender by Age") {
          max(prop.table(wtd.table(
            dat$XX, interaction(dat$Age, dat$Gender),
            weights = dat$wgt
          ), 2), na.rm = TRUE) + .2
        }
      
      # Adding plot here
      g <- g +
        geom_bar(aes(y = after_stat(prop), fill = factor(after_stat(x))),
                 stat = "count") +
        geom_text(
          aes(label = scales::percent(after_stat(prop), accuracy = 0.1),
              y = after_stat(prop)),
          stat = "count",
          hjust = 0,
          size = 7,
          fontface = "bold"
        ) +
        labs(y = "Percent",  x = "") +
        expand_limits(y = yLimits) +
        scale_y_continuous(labels = scales::percent) +
        theme_bw() +
        theme(text = element_text(size = 30)) +
        guides(fill = "none")  +
        scale_x_discrete(
          labels = function(x)
            str_wrap(x, width = 30)
        ) +
        coord_flip()
      
      #Facet here
      if (input$stratified == "None") {
        g
      } else if (input$stratified == "Gender") {
        g + facet_grid( ~ Gender) +
          theme(text = element_text(size = 25))
      } else if (input$stratified == "Age group") {
        g + facet_grid( ~ Age) +
          theme(text = element_text(size = 20))
      } else if (input$stratified == "Gender by Age") {
        g + facet_grid(Gender ~ Age) +
          theme(text = element_text(size = 20))
      }
    })
  }



plotOutMain <-
  function(input, output, session, datList, datRawList, fullNameList) {
    
    reactive({
      
      dat <- datList[[input$year]]
      datRaw <- datRawList[[input$year]]
      fullName <- fullNameList[[input$year]]
      
      dat$X <- datRaw %>%
        pull(names(fullName[fullName == req(input$varname)]))
      
      
      
      #Remove the missing
      if(is.numeric(dat$X)){
        if (any(attr(dat$X, "labels") %in% 99)) {
          dat <- dat %>% filter(as.numeric(X) != 99)
        } else {
          dat <- dat %>% filter(as.numeric(X) != 9)
        }
      } 
      
      if (any(names(attr(dat$X, "labels")) %in% 
              c("Can't choose", "Cant choose"))) {
        
        dat<- 
          dat %>% filter(as.character(X) != 
                           attr(dat$X, "labels")[
                             names(attr(dat$X, "labels")) %in% 
                               c("Can't choose", "Cant choose")])
      }
      
      dat<- 
        dat %>% filter(as.character(X) != "" | !is.na(X) )
      
      
      
      
      dat$XX <- as_factor(dat$X)
      
      # Initialise ggplot2 object
      
      if( any(names(dat) == "wgt")){
        
        g <-
          if (input$stratified == "None") {
            ggplot(dat, aes(
              x = XX,
              weight = wgt,
              group = 1
            ))
          } else if (input$stratified == "Gender") {
            ggplot(dat %>% filter(!is.na(Gender)), 
                   aes(x = XX,
                       weight = wgt,
                       group = Gender
                   ))
          } else if (input$stratified == "Age group") {
            ggplot(dat %>% filter(!is.na(Age)), aes(
              x = XX,
              weight = wgt,
              group = Age
            ))
          } else if (input$stratified == "Gender by Age") {
            ggplot(dat %>% filter(!is.na(Gender), !is.na(Age)), aes(
              x = XX,
              weight = wgt,
              group = interaction(Age, Gender)
            ))
          }
        
        yLimits <-
          if (input$stratified == "None") {
            max(prop.table(wtd.table(dat$XX,
                                     weights = dat$wgt)), na.rm = TRUE) + .01
          } else if (input$stratified == "Gender") {
            max(prop.table(wtd.table(dat$XX, dat$Gender,
                                     weights = dat$wgt), 2), na.rm = TRUE) + .05
          } else if (input$stratified == "Age group") {
            max(prop.table(wtd.table(dat$XX, dat$Age,
                                     weights = dat$wgt), 2), na.rm = TRUE) + .15
          } else if (input$stratified == "Gender by Age") {
            max(prop.table(wtd.table(
              dat$XX, interaction(dat$Age, dat$Gender),
              weights = dat$wgt
            ), 2), na.rm = TRUE) + .2
          }
        
      } else {
        g <-
          if (input$stratified == "None") {
            ggplot(dat, aes(
              x = XX,
              group = 1
            ))
          } else if (input$stratified == "Gender") {
            ggplot(dat %>% filter(!is.na(Gender)), aes(
              x = XX,
              group = Gender
            ))
          } else if (input$stratified == "Age group") {
            ggplot(dat %>% filter(!is.na(Age)), aes(
              x = XX,
              group = Age
            ))
          } else if (input$stratified == "Gender by Age") {
            ggplot(dat %>% filter(!is.na(Gender), !is.na(Age)), aes(
              x = XX,
              group = interaction(Age, Gender)
            ))
          }
        
        
        yLimits <-
          if (input$stratified == "None") {
            max(prop.table(table(dat$XX)), na.rm = TRUE) + .01
          } else if (input$stratified == "Gender") {
            max(prop.table(table(dat$XX, dat$Gender), 2), na.rm = TRUE) + .05
          } else if (input$stratified == "Age group") {
            max(prop.table(table(dat$XX, dat$Age), 2), na.rm = TRUE) + .15
          } else if (input$stratified == "Gender by Age") {
            max(prop.table(table(
              dat$XX, interaction(dat$Age, dat$Gender)
            ), 2), na.rm = TRUE) + .2
          }
        
      }
      
      
      # Adding plot here
      g <- g +
        geom_bar(aes(y = after_stat(prop), fill = factor(after_stat(x))),
                 stat = "count") +
        geom_text(
          aes(label = scales::percent(after_stat(prop)),
              y = after_stat(prop)),
          stat = "count",
          hjust = 0,
          size = 7,
          fontface = "bold"
        ) +
        labs(y = "Percent",  x = "") +
        expand_limits(y = yLimits) +
        scale_y_continuous(labels = scales::percent) +
        theme_bw() +
        theme(text = element_text(size = 30)) +
        guides(fill = "none")  +
        scale_x_discrete(
          labels = function(x)
            str_wrap(x, width = 30)
        ) +
        coord_flip()
      
      #Facet here
      if (input$stratified == "None") {
        g
      } else if (input$stratified == "Gender") {
        g + facet_grid( ~ Gender) +
          theme(text = element_text(size = 25))
      } else if (input$stratified == "Age group") {
        g + facet_grid( ~ Age) +
          theme(text = element_text(size = 20))
      } else if (input$stratified == "Gender by Age") {
        g + facet_grid(Gender ~ Age) +
          theme(text = element_text(size = 20))
      }
      
    })
  }


plotOutWeighted2015 <-
  function(input, output, session, dat, datRaw,
           qmoveTempOverall, qmoveTempByAge,
           qmoveTempBySex, qmoveTempBySexAge) {
    
    reactive({
      if (req(input$varname) == "F6. Reasons for moving to another country") {
        g <-
          if (input$stratified == "None") {
            ggplot(qmoveTempOverall, aes(
              x = Group,
              y = Prop,
              fill = Group
            ))
          } else if (input$stratified == "Gender") {
            ggplot(qmoveTempBySex, aes(
              x = Group,
              y = Prop,
              fill = Group
            ))
          } else if (input$stratified == "Age group") {
            ggplot(qmoveTempByAge, aes(
              x = Group,
              y = Prop,
              fill = Group
            ))
          } else if (input$stratified == "Gender by Age") {
            ggplot(qmoveTempBySexAge, aes(
              x = Group,
              y = Prop,
              fill = Group
            ))
          }
        
        yLimits <-
          if (input$stratified == "None") {
            max(qmoveTempOverall$Prop, na.rm = TRUE) + .01
          } else if (input$stratified == "Gender") {
            max(qmoveTempBySex$Prop, na.rm = TRUE) + .05
          } else if (input$stratified == "Age group") {
            max(qmoveTempByAge$Prop, na.rm = TRUE) + .15
          } else if (input$stratified == "Gender by Age") {
            max(qmoveTempBySexAge$Prop, na.rm = TRUE) + .2
          }
        
        
        g <- g +
          geom_col() +
          geom_text(
            aes(label = scales::percent(Prop)),
            hjust = 0,
            size = 7,
            fontface = "bold"
          ) +
          expand_limits(y = yLimits)
        
      } else {
        dat$X <- datRaw %>%
          pull(names(fullName[fullName == req(input$varname)]))
        
        #Remove the missing
        if(is.numeric(dat$X)){
          if (any(attr(dat$X, "labels") %in% 99)) {
            dat <- dat %>% filter(as.numeric(X) != 99)
          } else {
            dat <- dat %>% filter(as.numeric(X) != 9)
          }
        } else {
          if (any(names(attr(dat$X, "labels")) == "Can't choose")) {
            
            dat<- 
              dat %>% filter(as.character(X) != 
                               attr(dat$X, "labels")[
                                 names(attr(dat$X, "labels")) == 
                                   "Can't choose"])
          }
          
          dat<- 
            dat %>% filter(as.character(X) != "")
        }
        
        
        dat$XX <- as_factor(dat$X)
        
        
        # Initialise ggplot2 object
        g <-
          if (input$stratified == "None") {
            ggplot(dat, aes(
              x = XX,
              weight = wgt,
              group = 1
            ))
          } else if (input$stratified == "Gender") {
            ggplot(dat, aes(
              x = XX,
              weight = wgt,
              group = Gender
            ))
          } else if (input$stratified == "Age group") {
            ggplot(dat, aes(
              x = XX,
              weight = wgt,
              group = Age
            ))
          } else if (input$stratified == "Gender by Age") {
            ggplot(dat, aes(
              x = XX,
              weight = wgt,
              group = interaction(Age, Gender)
            ))
          }
        
        
        yLimits <-
          if (input$stratified == "None") {
            max(prop.table(wtd.table(dat$XX,
                                     weights = dat$wgt)), na.rm = TRUE) + .01
          } else if (input$stratified == "Gender") {
            max(prop.table(wtd.table(dat$XX, dat$Gender,
                                     weights = dat$wgt), 2), na.rm = TRUE) + .05
          } else if (input$stratified == "Age group") {
            max(prop.table(wtd.table(dat$XX, dat$Age,
                                     weights = dat$wgt), 2), na.rm = TRUE) + .15
          } else if (input$stratified == "Gender by Age") {
            max(prop.table(wtd.table(
              dat$XX,
              interaction(dat$Age, dat$Gender),
              weights = dat$wgt
            ), 2), na.rm = TRUE) + .2
          }
        
        
        # Adding plot here
        g <- g +
          geom_bar(aes(y = after_stat(prop), fill = factor(after_stat(x))),
                   stat = "count") +
          geom_text(
            aes(label = scales::percent(after_stat(prop)),
                y = after_stat(prop)),
            stat = "count",
            hjust = 0,
            size = 7,
            fontface = "bold"
          ) +
          expand_limits(y = yLimits)
      }
      
      
      g <- g +
        labs(y = "Percent",  x = "") +
        scale_y_continuous(labels = scales::percent) +
        theme_bw() +
        theme(text = element_text(size = 30)) +
        guides(fill = "none")  +
        scale_x_discrete(
          labels = function(x)
            str_wrap(x, width = 30)
        ) +
        coord_flip()
      
      #Facet here
      if (input$stratified == "None") {
        g
      } else if (input$stratified == "Gender") {
        g + facet_grid(~ Gender) +
          theme(text = element_text(size = 25))
      } else if (input$stratified == "Age group") {
        g + facet_grid(~ Age) +
          theme(text = element_text(size = 20))
      } else if (input$stratified == "Gender by Age") {
        g + facet_grid(Gender ~ Age) +
          theme(text = element_text(size = 20))
      }
      
    })
  }




# Module UI function
sidebarInput <- function(id, date = "01-08-2018") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  dafaultDate  <- as.Date("01-08-2018", "%d-%m-%Y")
  
  
  date <- as.Date(date, "%d-%m-%Y")
  
  date <- ifelse(date >  dafaultDate, 
                 format(date, "%d-%m-%Y") ,
                 format(dafaultDate, "%d-%m-%Y"))
  
  tagList(
    selectInput(
      ns("varname"),
      label = HTML('<font size=\"4\"> Select an item to examine </font>'),
      choices = c(Choose = '', fullNameForSelect),
      selectize = TRUE
    ),
    selectInput(
      ns("stratified"),
      label = HTML('<font size=\"4\"> stratified by </font>'),
      choices = c("None", "Gender", "Age group", "Gender by Age"),
      selected = "None"
    ),
    box(
      h4("To access the datasets and questionnaires, go ",  
         a("here.", href = "https://doi.org/10.17608/k6.auckland.c.2174592")),
      h4("Latest Update:"),
      h4(date),
      h4("Contact email:"),
      h5(a("Barry Milne", href = "mailto:b.milne@auckland.ac.nz")),
      #h5(a("Kevin Chang", href = "mailto:k.chang@auckland.ac.nz")),
      h5(a("Martin von Randow", href = "mailto:m.vonrandow@auckland.ac.nz")),
      #h5(a("Eileen Li", href = "mailto:y.li@auckland.ac.nz")),
      width = 12,
      background = "black"
    ), 
    HTML("<a href=\"http://www.arts.auckland.ac.nz/en/about/our-research/research-centres-and-archives/compass.html\" target=\"_blank\"> 
         <img src=\"compass.png\" width=\"200\" height=\"95\" /></a>")
  )
}




# Module UI function
sidebarInputYear <-
  function(id) {
    # Create a namespace function using the provided id
    ns <- NS(id)
    
    tagList(
      selectInput(
        ns("year"),
        label = HTML('<font size=\"4\"> Select a year of survey </font>'),
        choices = c(Choose = '', 2010:1991),
        selectize = TRUE, 
        selected = "2010"
      )
    )
  }


topicsOut <-
  function(input, output, session, topics) 
    reactive( paste("Topic:", topics$Topic[topics$Year == input$year]))





# Module UI function
sidebarInputMultiYears <-
  function(input, output, session, 
           fullNameForSelectList = fullNameForSelectList,
           date = "01-08-2018") {
    # Create a namespace function using the provided id
    
    dafaultDate  <- as.Date("01-08-2018", "%d-%m-%Y")
    
    
    date <- as.Date(date, "%d-%m-%Y")
    
    date <- ifelse(date >  dafaultDate, 
                   format(date, "%d-%m-%Y") ,
                   format(dafaultDate, "%d-%m-%Y"))
    
    
    reactive({
      
      ns <- session$ns
      
      tagList(
        selectInput(
          ns("varname"),
          label = HTML('<font size=\"4\"> Select an item to examine </font>'),
          choices = c(Choose = '', sort(fullNameForSelectList[[ input$year]])),
          selectize = TRUE
        ),
        selectInput(
          ns("stratified"),
          label = HTML('<font size=\"4\"> stratified by </font>'),
          choices = c("None", "Gender", "Age group", "Gender by Age"),
          selected = "None"
        ),
        box(
          h4("To access the datasets and questionnaires, go to ",  
             a("here.", href = "https://doi.org/10.17608/k6.auckland.c.2174592.v9")),
          h4("Latest Update:"),
          h4(date),
          h4("Contact email:"),
          h5(a("Barry Milne", href = "mailto:b.milne@auckland.ac.nz")),
          h5(a("Kevin Chang", href = "mailto:k.chang@auckland.ac.nz")),
          h5(a("Martin von Randow", href = "mailto:m.vonrandow@auckland.ac.nz")),
          width = 12,
          background = "black"
        ), 
        HTML("<a href=\"http://www.arts.auckland.ac.nz/en/about/our-research/research-centres-and-archives/compass.html\" target=\"_blank\"> 
         <img src=\"compass.png\" width=\"200\" height=\"95\" /></a>")
      )
      
    })
    
  }




#source("https://raw.githubusercontent.com/kcha193/isspshiny/master/Rcode/plotsOutput_2024.R")
#source("https://raw.githubusercontent.com/kcha193/isspshiny/master/Rcode/sidebarInput.R")
source("https://raw.githubusercontent.com/kcha193/isspshiny/master/Rcode/titleOutput.R")

# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin = "blue",
  
  # Application title
  dashboardHeader(title = "ISSP 2023 - National Identity & Citizenship", titleWidth = 420),
  
  # Sidebar with a slider input for number of bins
  dashboardSidebar(sidebarInput("side",
          date = "9-10-2024")), # <- change this for every update 
  
  # Show a plot 
  dashboardBody(box(
    h2(textOutput("title")), 
    plotOutput("barPlot",  height = "800px"),
    width = 12,
    height = 850
  ))
)


# Define server logic required to draw a plot
server <- function(input, output) {
  
  plotOut <- callModule(plotOutWeighted, "side", dat, datRaw)
  titleOut <- callModule(titleOut, "side")
  
  output$title <- renderText({ titleOut()})
  
  output$barPlot <- renderPlot({plotOut()})
  
}

# Run the application
shinyApp(ui = ui, server = server)
