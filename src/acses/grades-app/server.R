library(ggvis)
library(dplyr)
server = shinyServer(function(input, output) {
   load('./gradedata.rda')
   gd2 <- gradedata[gradedata$left==FALSE,]
   gd2 <- arrange(gd2, Date, Civil.Service.grad)
   visdata1y <- reactive({
     gd2 <- gd2[gd2$Group==input$select_dept,]
     gd3 <- gd2[gd2$Date==input$slider_date,]
     if(nrow(gd3)==0) {
       return(gd2[gd2$Date==2010])
     } else {gd3}
     })
   scalemin <- min(gd2$share2, na.rm=T)
   visdata1y %>%
     ggvis(x=~Civil.Service.grad, fill:='red') %>%
#      layer_bars(fill=~updown, stack=F, stroke:='none', width=1) %>%
     layer_ribbons(y=~share/2, y2=~share2) %>%
     scale_numeric('y', domain=c(scalemin, scalemin*-1)) %>%
     add_axis('x',tick_size_major = 0, grid=FALSE, title='') %>%
     add_axis('y',tick_size_major = 0, grid=FALSE, title='') %>%
     bind_shiny('ggvis')           
})