library(shiny)
library(ggvis)


shinyServer(function(input, output) {
  csps <- read.csv('csps.csv')
  csps$value <- csps$value*100
  csps$split <- relevel(csps$split, 'SCS')
  
  output$dimensionsList <- renderUI({
    dimensions <- unique(as.character(csps$dimension))
    selectInput("dimension2", "Choose dimension", as.list(dimensions),
                selected='Age')
  })
  
  output$measuresList <- renderUI({
    measures <- unique(as.character(csps$measure))
    selectInput("measure2", "Choose measure", as.list(measures),multiple = TRUE,
                selected='Employee engagement index')
  })
  
  cspsshort <- csps[csps$measure=='Employee engagement index' &
                      csps$dimension=='Age',]
  cspsshort$measure <- droplevels(cspsshort$measure)
  cspsshort$dimension <- droplevels(cspsshort$dimension)
  cspsshort$split <- droplevels(cspsshort$split)
  
  csps3 <- reactive({
#     if (is.null(input$dimension2)) {
#       return(csps)
#     }
    meas <- input$measure2
    dimen <- input$dimension2
    csps2 <- csps[csps$dimension==dimen & csps$measure %in% meas,]
    csps2$dimension <- droplevels(csps2$dimension) 
    csps2$split <- droplevels(csps2$split)
    csps2$measure <- droplevels(csps2$measure)
    if(nrow(csps2) == 0){
      return(cspsshort)
    }
    csps2
    }) 
  
  tooltip <- function(data) {
    paste0('<i>',data$split,'</i>', '<br />', data$measure, ': ', '<b>',data$value,'</b>')
  }

  colourscale <- c("#00ccff", "#37424a", "#d40072", "#83389b", 
                   "#7a9393", "#457e81", "#be8b5e",
                   "#cdcfd1", "#bff2ff", "#f4bfdb", "#e0cde6",
                   "#dde4e4", "#d0dedf", "#eee2d6") 

  csps3 %>%
    ggvis(~split, ~value, stroke = ~measure, fillOpacity.hover := 1,
          fillOpacity := .7, strokeOpacity := .7, strokeOpacity.hover :=1) %>%
    layer_lines(strokeWidth:=3) %>%
    layer_points(fill = ~measure, size := 50, size.hover := 90) %>%
#     layer_bar(width=.9) %>%
    add_axis('x',tick_size_major = 0, grid=FALSE, title='') %>%  
    add_axis('y',tick_size_major = 0, grid=TRUE, values=c(0,25,50,75,100),
             title = '',
             properties=axis_props(axis=c('stroke'='white'))) %>% 
    scale_numeric('y',domain=c(0,100),zero = TRUE) %>%
    scale_ordinal('fill', range=colourscale, label = '') %>%
    scale_ordinal('stroke', range=colourscale, label = '') %>%
    add_tooltip(tooltip) %>%
    scale_nominal(property = 'x') %>%
    scale_numeric('y') %>%
    add_legend(c('stroke','fill')) %>%
    bind_shiny('ggvis')
  
  output$datat <- renderDataTable({
    head(csps3(), 20)
  })
})