library(ggvis)
library(dplyr)
server = shinyServer(function(input, output) {
   load('./gradedata.rda')
   gd2 <- gradedata[gradedata$left==FALSE,]
   gd2 <- arrange(gd2, Date, Civil.Service.grad)
   visdata1y <- reactive({
#      gd2 <- gd2[gd2$Group==input$select_dept,]
     gd3 <- gd2[gd2$Date==input$slider_date,]
     if(nrow(gd3)==0) {
       return(gd2[gd2$Date==2010])
     } else {gd3}
     })
   scalemin <- min(gd2$share2, na.rm=T)
   data1 <- reactive(visdata1y()[visdata1y()$Group=='HMT',])
   data2 <- reactive(visdata1y()[visdata1y()$Group=='DCMS',])
   data2 <- reactive(visdata1y()[visdata1y()$Group=='BIS',])
   data3 <- reactive(visdata1y()[visdata1y()$Group=='DWP',])
   data4 <- reactive(visdata1y()[visdata1y()$Group=='DECC',])
   data5 <- reactive(visdata1y()[visdata1y()$Group=='DfID',])
   data7 <- reactive(visdata1y()[visdata1y()$Group=='DCLG',])
   data6 <- reactive(visdata1y()[visdata1y()$Group=='CO',])
   data1 %>%
     ggvis(x=~Civil.Service.grad, fill:="#00ccff") %>%
     layer_ribbons(y=~share/2, y2=~share2) %>%
     scale_numeric('y', domain=c(scalemin, scalemin*-1)) %>%
     add_axis('x',tick_size_major = 0, grid=FALSE, title='') %>%
     add_axis('y',tick_size_major = 0, grid=FALSE, title='') %>%
     bind_shiny('ggvis1') %>%
     set_options(width=200, height = 200, padding=padding(20,20,20,20))
   data2 %>%
     ggvis(x=~Civil.Service.grad, fill:="#00ccff") %>%
     layer_ribbons(y=~share/2, y2=~share2) %>%
     scale_numeric('y', domain=c(scalemin, scalemin*-1)) %>%
     add_axis('x',tick_size_major = 0, grid=FALSE, title='') %>%
     add_axis('y',tick_size_major = 0, grid=FALSE, title='') %>%
     bind_shiny('ggvis2')  %>% set_options(width=200)           
   data3 %>%
     ggvis(x=~Civil.Service.grad, fill:="#00ccff") %>%
     layer_ribbons(y=~share/2, y2=~share2) %>%
     scale_numeric('y', domain=c(scalemin, scalemin*-1)) %>%
     add_axis('x',tick_size_major = 0, grid=FALSE, title='') %>%
     add_axis('y',tick_size_major = 0, grid=FALSE, title='') %>%
     bind_shiny('ggvis3')  %>% set_options(width=200)           
   data4 %>%
     ggvis(x=~Civil.Service.grad, fill:="#00ccff") %>%
     layer_ribbons(y=~share/2, y2=~share2) %>%
     scale_numeric('y', domain=c(scalemin, scalemin*-1)) %>%
     add_axis('x',tick_size_major = 0, grid=FALSE, title='') %>%
     add_axis('y',tick_size_major = 0, grid=FALSE, title='') %>%
     bind_shiny('ggvis4')  %>% set_options(width=200)           
   data5 %>%
     ggvis(x=~Civil.Service.grad, fill:="#00ccff") %>%
     layer_ribbons(y=~share/2, y2=~share2) %>%
     scale_numeric('y', domain=c(scalemin, scalemin*-1)) %>%
     add_axis('x',tick_size_major = 0, grid=FALSE, title='') %>%
     add_axis('y',tick_size_major = 0, grid=FALSE, title='') %>%
     bind_shiny('ggvis5')  %>% set_options(width=100)          
   data6 %>%
     ggvis(x=~Civil.Service.grad, fill:="#00ccff") %>%
     layer_ribbons(y=~share/2, y2=~share2) %>%
     scale_numeric('y', domain=c(scalemin, scalemin*-1)) %>%
     add_axis('x',tick_size_major = 0, grid=FALSE, title='') %>%
     add_axis('y',tick_size_major = 0, grid=FALSE, title='') %>%
     bind_shiny('ggvis6')  %>% set_options(width=200)                  
   data7 %>%
     ggvis(x=~Civil.Service.grad, fill:="#00ccff") %>%
     layer_ribbons(y=~share/2, y2=~share2) %>%
     scale_numeric('y', domain=c(scalemin, scalemin*-1)) %>%
     add_axis('x',tick_size_major = 0, grid=FALSE, title='') %>%
     add_axis('y',tick_size_major = 0, grid=FALSE, title='') %>%
     bind_shiny('ggvis6')  %>% set_options(width=200)                  
})