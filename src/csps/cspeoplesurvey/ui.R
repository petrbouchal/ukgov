library(shiny)
library(shinyIncubator)
library(ggvis)

shinyUI(fluidPage(
  fluidRow(
    titlePanel('Civil Service People Survey'),
    sidebarPanel(width=2,
      uiOutput('dimensionsList'),
      uiOutput('measuresList')
  #     uiOutput('ggvis_ui'),
      ),
    mainPanel(
      tabsetPanel(
        tabPanel('Visualisation',
          ggvisOutput('ggvis')
          ),
        tabPanel('Table',
          dataTableOutput('datat')
          )
        )
      )
    )
  )
)