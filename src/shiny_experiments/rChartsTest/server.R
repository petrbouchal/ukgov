# options(error=browser)
library(shiny)
library(ggplot2)
source('./lib/lib_acses.R')

shinyServer(function(input, output) {
  onspse <- read.csv('./PSE_change_long.csv') 
  onspse <- onspse[onspse$measure=='Cumulative_Perc_net_change',]
  dataset <- reactive(
    onspse[onspse$Whitehall=='Departmental Group' & onspse$Dept==input$dept,]
  )
  output$downloadData <- downloadHandler(
    filename = 'headcount.csv',
    content = function(file) {
    write.csv(onspse, file) })
  
  labelsx <- c('2010Q3','Q4',
               '2011Q1','Q2','Q3','Q4',
               '2012Q1','Q2','Q3','Q4',
               '2013Q1','Q2','Q3','2013Q4')
  output$table = renderDataTable({
    onspse
  }, options = list(bSortClasses = TRUE))
  output$plot <- renderPlot({
    p_base <- ggplot(dataset(),aes(group=group,x=Period,y=value)) +
      scale_color_manual(values=c(IfGcols[3,1], IfGcols[2,1], IfGcols[5,1])) +
      scale_y_continuous(labels=percent) +
      scale_x_discrete(labels=labelsx) +
      labs(y='% change since Spending Review 2010 (2010 Q3)') +
      guides(colour = guide_legend(ncol = 3,keywidth=unit(1,'cm'))) +
      theme(axis.text.x = element_text(angle = 0, hjust = 1),plot.title=element_blank(),
            axis.line.x=element_line(size=unit(1,'mm'),colour=IfGcols[1,2]),
            panel.border=element_rect(fill=NA,color=NA),
            panel.margin=unit(c(1,1,1,1),'mm'),
            axis.ticks=element_line(colour=IfGcols[1,2]),axis.ticks.x=element_blank(),
            axis.ticks.y=element_blank(),
            panel.grid=element_line(colour=IfGcols[1,3]),panel.grid.minor=element_blank(),
            panel.grid.major.x=element_blank(),
            axis.text.x=element_text(size=14))
    p_dept <- geom_line(size=2,colour=IfGcols[2,1])
    
    z <- p_base
    if(input$showAll & !input$showAvg) {
      datasetall <- onspse[onspse$Whitehall=='Departmental Group' &
                             onspse$Dept!='NIO',]
      p_all <- geom_line(data=datasetall,aes(group=group,x=Period, y=value),
                  colour=IfGcols[1,3],size=1)
      z <- p_base + p_all + p_dept
    } else if(input$showAll & input$showAvg) {
      datasetall <- onspse[onspse$Whitehall=='Departmental Group' &
                             onspse$Dept!='NIO',]
      datasetavg <- onspse[onspse$Whitehall=='Total Employment ONS total',]
      p_all <- geom_line(data=datasetall,aes(group=group,x=Period, y=value),
                  colour=IfGcols[1,3],size=1)
      p_avg <- geom_line(data=datasetavg,aes(group=group,x=Period, y=value),
                  colour=IfGcols[3,1],size=1)
      z <- p_base + p_all + p_avg + p_dept
    } else if(!input$showAll & input$showAvg) {
      datasetavg <- onspse[onspse$Whitehall=='Total Employment ONS total',]
      p_avg <- geom_line(data=datasetavg,aes(group=group,x=Period, y=value),
                  colour=IfGcols[3,1],size=1)
      z <- p_base + p_avg + p_dept
    } else {z <- p_base + p_dept}
    print(z)
  })
})