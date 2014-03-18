library(shiny)
library(ggplot2)
source('./lib/lib_acses.R')

shinyServer(function(input, output) {
  onspse <- read.csv('./PSE_change_long.csv') 
  dataset <- reactive(
    onspse[onspse$Whitehall=='Departmental Group' & onspse$Dept==input$dept,]
  )
  datasetall <- onspse[onspse$Whitehall=='Departmental Group',]
  
  labelsx <- c('2010Q3','Q4',
               '2011Q1','Q2','Q3','Q4',
               '2012Q1','Q2','Q3','Q4',
               '2013Q1','Q2','Q3')
  output$plot <- renderPlot({
    p <- ggplot(dataset(), aes(x=Period, y=value)) +
      geom_line(size=2,aes(group=group),colour=IfGcols[2,1]) +
      scale_color_manual(values=c(IfGcols[3,1], IfGcols[2,1], IfGcols[5,1])) +
      scale_y_continuous(labels=percent) +
      scale_x_discrete(labels=labelsx) +
      labs(y='% change since Spending Review 2010 (2010 Q3)') +
      guides(colour = guide_legend(ncol = 3,keywidth=unit(1,'cm'))) +
      theme(axis.text.x = element_text(angle = 0, hjust = 1),plot.title=element_blank(),
            axis.line.x=element_line(size=unit(1,'mm'),colour=IfGcols[1,2]),
            panel.border=element_rect(fill=NA,color=IfGcols[1,2]),
            panel.margin=unit(c(1,1,1,1),'mm'),
            axis.ticks=element_line(colour=IfGcols[1,2]),axis.ticks.x=element_blank(),
            axis.ticks.y=element_blank(),
            panel.grid=element_line(colour=IfGcols[1,3]),panel.grid.minor=element_blank(),
            panel.grid.major.x=element_blank(),
            axis.text.x=element_text(size=14))
    
    q <- ggplot(dataset(), aes(x=Period, y=value)) +
      geom_line(data=datasetall,aes(group=group),colour=IfGcols[1,3],size=1) +
      geom_line(size=2,aes(group=group),colour=IfGcols[2,1]) +
      scale_color_manual(values=c(IfGcols[3,1], IfGcols[2,1], IfGcols[5,1])) +
      scale_y_continuous(labels=percent) +
      scale_x_discrete(labels=labelsx) +
      labs(y='% change since Spending Review 2010 (2010 Q3)') +
      guides(colour = guide_legend(ncol = 3,keywidth=unit(1,'cm'))) +
      theme(axis.text.x = element_text(angle = 0, hjust = 1),plot.title=element_blank(),
            axis.line.x=element_line(size=unit(1,'mm'),colour=IfGcols[1,2]),
            panel.border=element_rect(fill=NA,color=IfGcols[1,2]),
            panel.margin=unit(c(1,1,1,1),'mm'),
            axis.ticks=element_line(colour=IfGcols[1,2]),axis.ticks.x=element_blank(),
            panel.grid=element_line(colour=IfGcols[1,3]),panel.grid.minor=element_blank(),
            panel.grid.major.x=element_blank(),
            axis.text.x=element_text(size=14))
    if(input$showAll) {
      z <- q
    } else z <- p
    print(z)
  })
})