library(shiny)
library(ggplot2)

onspse <- read.csv('./PSE_change_long.csv') 

shinyUI(
  navbarPage("Whitehall",
    tabPanel('Staff numbers',
      fluidRow(
        headerPanel("Civil Servants in Departments, 2010-2013"),
        tabsetPanel(
          tabPanel('Chart',
            sidebarPanel(width=2,
              tags$style(type='text/css', ".well { background-color: #ffffff;}"),
              tags$style(type='text/css', "h1 { font-size: 3vh;}"),
              selectInput('dept', 'Department', unique(onspse$Dept),multiple=F,
                          selected='CO'),    
              checkboxInput('showAll', 'Show all',value=T),
              checkboxInput('showAvg', 'Show Civil Service total',value=T)
            ),
            mainPanel(plotOutput('plot')) 
          ),
          tabPanel('Table',dataTableOutput("table")),
          position='below'
        )
    )),
    tabPanel('[Another theme]',fluidRow(
      headerPanel("Under construction"),
      responsive=T))
  )
)