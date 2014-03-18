library(shiny)
library(ggplot2)

onspse <- read.csv('./PSE_change_long.csv') 

shinyUI(pageWithSidebar(
  
  headerPanel("Civil Servants in Departments, 2010-2013"),
  
  sidebarPanel(width=2,
    tags$style(type='text/css', ".well { background-color: #ffffff;}"),
    tags$style(type='text/css', "h1 { font-size: 3vh;}"),
    selectInput('dept', 'Department', unique(onspse$Dept),multiple=T),    
    checkboxInput('showAll', 'Show all')
  ),
  
  mainPanel(
    plotOutput('plot')
  )
))