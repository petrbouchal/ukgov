library(ggvis)
library(dplyr)
server = shinyServer(function(input, output) {
  load('./gradedata2.rda')
  gd2 <- arrange(gradedata2, Date, Civil.Service.grad)
  visdata1y <- reactive({
    #      gd2 <- gd2[gd2$Group==input$select_dept,]
    gd3 <- gd2[gd2$Date==input$slider_date,]
    if(nrow(gd3)==0) {
      return(gd2[gd2$Date==2010])
    } else {gd3}
  })
  scalemin <- min(gd2$share2, na.rm=T)
  
  data_hmt <- reactive(visdata1y()[visdata1y()$Group=='HMT',])
  data_dcms <- reactive(visdata1y()[visdata1y()$Group=='DCMS',])
  data_bis <- reactive(visdata1y()[visdata1y()$Group=='BIS',])
  data_dwp <- reactive(visdata1y()[visdata1y()$Group=='DWP',])
  data_decc <- reactive(visdata1y()[visdata1y()$Group=='DECC',])
  data_dfid <- reactive(visdata1y()[visdata1y()$Group=='DfID',])
  data_dclg <- reactive(visdata1y()[visdata1y()$Group=='DCLG',])
  data_co <- reactive(visdata1y()[visdata1y()$Group=='CO',])
  data_mod <- reactive(visdata1y()[visdata1y()$Group=='MoD',])
  data_moj <- reactive(visdata1y()[visdata1y()$Group=='MoJ',])
  data_defra <- reactive(visdata1y()[visdata1y()$Group=='Defra',])
  data_dft <- reactive(visdata1y()[visdata1y()$Group=='DfT',])
  data_dfe <- reactive(visdata1y()[visdata1y()$Group=='DfE',])
  data_hmrc <- reactive(visdata1y()[visdata1y()$Group=='HMRC',])
  data_fco <- reactive(visdata1y()[visdata1y()$Group=='FCO',])
  data_dh <- reactive(visdata1y()[visdata1y()$Group=='DH',])
  data_ho <- reactive(visdata1y()[visdata1y()$Group=='HO',])
  
  data_decc %>%
    ggvis(x=~Civil.Service.grad) %>%
    layer_ribbons(y=~share_2010/2, y2=~share2_2010, fill:='gray', opacity:=.3) %>%
    layer_ribbons(y=~share/2, y2=~share2, fill:="#00ccff", opacity:=.4) %>%
    scale_numeric('y', domain=c(scalemin, scalemin*-1)) %>%
    add_axis('x',tick_size_major = 0, grid=FALSE,
             title='DECC', title_offset = -100, layer = 'front',
             properties=axis_props(axis = list(stroke = "red", strokeWidth = 0),
                                   title=list(fontSize=17, fill='white'))) %>%
    hide_axis('y') %>%
    set_options(width=200, height = 200, padding=padding(0,0,20,10), resizable=FALSE) %>%
    bind_shiny('ggvis_decc')
  
  data_dcms %>%
    ggvis(x=~Civil.Service.grad, fill:="#00ccff") %>%
    layer_ribbons(y=~share_2010/2, y2=~share2_2010, fill:='gray', opacity:=.3) %>%
    layer_ribbons(y=~share/2, y2=~share2, fill:="#00ccff", opacity:=.4) %>%
    scale_numeric('y', domain=c(scalemin, scalemin*-1)) %>%
    add_axis('x',tick_size_major = 0, grid=FALSE,
             title='DCMS', title_offset = -100, layer = 'front',
             properties=axis_props(axis = list(stroke = "red", strokeWidth = 0),
                                   title=list(fontSize=17, fill='white'))) %>%
    hide_axis('y') %>%
    set_options(width=200, height = 200, padding=padding(0,0,20,10), resizable=FALSE) %>%
    bind_shiny('ggvis_dcms')        
  
  data_bis %>%
    ggvis(x=~Civil.Service.grad, fill:="#00ccff") %>%
    layer_ribbons(y=~share_2010/2, y2=~share2_2010, fill:='gray', opacity:=.3) %>%
    layer_ribbons(y=~share/2, y2=~share2, fill:="#00ccff", opacity:=.4) %>%    scale_numeric('y', domain=c(scalemin, scalemin*-1)) %>%
    add_axis('x',tick_size_major = 0, grid=FALSE,
             title='BIS', title_offset = -100, layer = 'front',
             properties=axis_props(axis = list(stroke = "red", strokeWidth = 0),
                                   title=list(fontSize=17, fill='white'))) %>%
    hide_axis('y') %>%
    set_options(width=200, height = 200, padding=padding(0,0,20,10), resizable=FALSE) %>%
    bind_shiny('ggvis_bis')
  
  data_dwp %>%
    ggvis(x=~Civil.Service.grad, fill:="#00ccff") %>%
    layer_ribbons(y=~share_2010/2, y2=~share2_2010, fill:='gray', opacity:=.3) %>%
    layer_ribbons(y=~share/2, y2=~share2, fill:="#00ccff", opacity:=.4) %>%    scale_numeric('y', domain=c(scalemin, scalemin*-1)) %>%
    add_axis('x',tick_size_major = 0, grid=FALSE,
             title='DWP', title_offset = -100, layer = 'front',
             properties=axis_props(axis = list(stroke = "red", strokeWidth = 0),
                                   title=list(fontSize=17, fill='white'))) %>%
    hide_axis('y') %>%
    set_options(width=200, height = 200, padding=padding(0,0,20,10), resizable=FALSE) %>%
    bind_shiny('ggvis_dwp')
  
  data_hmt %>%
    ggvis(x=~Civil.Service.grad, fill:="#00ccff") %>%
    layer_ribbons(y=~share_2010/2, y2=~share2_2010, fill:='gray', opacity:=.3) %>%
    layer_ribbons(y=~share/2, y2=~share2, fill:="#00ccff", opacity:=.4) %>%    scale_numeric('y', domain=c(scalemin, scalemin*-1)) %>%
    add_axis('x',tick_size_major = 0, grid=FALSE,
             title='HMT', title_offset = -100, layer = 'front',
             properties=axis_props(axis = list(stroke = "red", strokeWidth = 0),
                                   title=list(fontSize=17, fill='white'))) %>%
    hide_axis('y') %>%
    set_options(width=200, height = 200, padding=padding(0,0,20,10), resizable=FALSE) %>%
    bind_shiny('ggvis_hmt')          
  
  data_dfid %>%
    ggvis(x=~Civil.Service.grad, fill:="#00ccff") %>%
    layer_ribbons(y=~share_2010/2, y2=~share2_2010, fill:='gray', opacity:=.3) %>%
    layer_ribbons(y=~share/2, y2=~share2, fill:="#00ccff", opacity:=.4) %>%    scale_numeric('y', domain=c(scalemin, scalemin*-1)) %>%
    add_axis('x',tick_size_major = 0, grid=FALSE,
             title='DfID', title_offset = -100, layer = 'front',
             properties=axis_props(axis = list(stroke = "red", strokeWidth = 0),
                                   title=list(fontSize=17, fill='white'))) %>%
    hide_axis('y') %>%
    set_options(width=200, height = 200, padding=padding(0,0,20,10), resizable=FALSE) %>%
    bind_shiny('ggvis_dfid')                 

  data_fco %>%
    ggvis(x=~Civil.Service.grad, fill:="#00ccff") %>%
    layer_ribbons(y=~share_2010/2, y2=~share2_2010, fill:='gray', opacity:=.3) %>%
    layer_ribbons(y=~share/2, y2=~share2, fill:="#00ccff", opacity:=.4) %>%    scale_numeric('y', domain=c(scalemin, scalemin*-1)) %>%
    add_axis('x',tick_size_major = 0, grid=FALSE,
             title='FCO', title_offset = -100, layer = 'front',
             properties=axis_props(axis = list(stroke = "red", strokeWidth = 0),
                                   title=list(fontSize=17, fill='white'))) %>%
    hide_axis('y') %>%
    set_options(width=200, height = 200, padding=padding(0,0,20,10), resizable=FALSE) %>%
    bind_shiny('ggvis_fco')

  data_dft %>%
    ggvis(x=~Civil.Service.grad, fill:="#00ccff") %>%
    layer_ribbons(y=~share_2010/2, y2=~share2_2010, fill:='gray', opacity:=.3) %>%
    layer_ribbons(y=~share/2, y2=~share2, fill:="#00ccff", opacity:=.4) %>%    scale_numeric('y', domain=c(scalemin, scalemin*-1)) %>%
    add_axis('x',tick_size_major = 0, grid=FALSE,
             title='DfT', title_offset = -100, layer = 'front',
             properties=axis_props(axis = list(stroke = "red", strokeWidth = 0),
                                   title=list(fontSize=17, fill='white'))) %>%
    hide_axis('y') %>%
    set_options(width=200, height = 200, padding=padding(0,0,20,10), resizable=FALSE) %>%
    bind_shiny('ggvis_dft')

  data_dfe %>%
    ggvis(x=~Civil.Service.grad, fill:="#00ccff") %>%
    layer_ribbons(y=~share_2010/2, y2=~share2_2010, fill:='gray', opacity:=.3) %>%
    layer_ribbons(y=~share/2, y2=~share2, fill:="#00ccff", opacity:=.4) %>%    scale_numeric('y', domain=c(scalemin, scalemin*-1)) %>%
    add_axis('x',tick_size_major = 0, grid=FALSE,
             title='DfE', title_offset = -100, layer = 'front',
             properties=axis_props(axis = list(stroke = "red", strokeWidth = 0),
                                   title=list(fontSize=17, fill='white'))) %>%
    hide_axis('y') %>%
    set_options(width=200, height = 200, padding=padding(0,0,20,10), resizable=FALSE) %>%
    bind_shiny('ggvis_dfe')

  data_ho %>%
    ggvis(x=~Civil.Service.grad, fill:="#00ccff") %>%
    layer_ribbons(y=~share_2010/2, y2=~share2_2010, fill:='gray', opacity:=.3) %>%
    layer_ribbons(y=~share/2, y2=~share2, fill:="#00ccff", opacity:=.4) %>%    scale_numeric('y', domain=c(scalemin, scalemin*-1)) %>%
    add_axis('x',tick_size_major = 0, grid=FALSE,
             title='HO', title_offset = -100, layer = 'front',
             properties=axis_props(axis = list(stroke = "red", strokeWidth = 0),
                                   title=list(fontSize=17, fill='white'))) %>%
    hide_axis('y') %>%
    set_options(width=200, height = 200, padding=padding(0,0,20,10), resizable=FALSE) %>%
    bind_shiny('ggvis_ho')

  data_hmrc %>%
    ggvis(x=~Civil.Service.grad, fill:="#00ccff") %>%
    layer_ribbons(y=~share_2010/2, y2=~share2_2010, fill:='gray', opacity:=.3) %>%
    layer_ribbons(y=~share/2, y2=~share2, fill:="#00ccff", opacity:=.4) %>%    scale_numeric('y', domain=c(scalemin, scalemin*-1)) %>%
    add_axis('x',tick_size_major = 0, grid=FALSE,
             title='HMRC', title_offset = -100, layer = 'front',
             properties=axis_props(axis = list(stroke = "red", strokeWidth = 0),
                                   title=list(fontSize=17, fill='white'))) %>%
    hide_axis('y') %>%
    set_options(width=200, height = 200, padding=padding(0,0,20,10), resizable=FALSE) %>%
    bind_shiny('ggvis_hmrc')

  data_defra %>%
    ggvis(x=~Civil.Service.grad, fill:="#00ccff") %>%
    layer_ribbons(y=~share_2010/2, y2=~share2_2010, fill:='gray', opacity:=.3) %>%
    layer_ribbons(y=~share/2, y2=~share2, fill:="#00ccff", opacity:=.4) %>%    scale_numeric('y', domain=c(scalemin, scalemin*-1)) %>%
    add_axis('x',tick_size_major = 0, grid=FALSE,
             title='Defra', title_offset = -100, layer = 'front',
             properties=axis_props(axis = list(stroke = "red", strokeWidth = 0),
                                   title=list(fontSize=17, fill='white'))) %>%
    hide_axis('y') %>%
    set_options(width=200, height = 200, padding=padding(0,0,20,10), resizable=FALSE) %>%
    bind_shiny('ggvis_defra')

  data_moj %>%
    ggvis(x=~Civil.Service.grad, fill:="#00ccff") %>%
    layer_ribbons(y=~share_2010/2, y2=~share2_2010, fill:='gray', opacity:=.3) %>%
    layer_ribbons(y=~share/2, y2=~share2, fill:="#00ccff", opacity:=.4) %>%    scale_numeric('y', domain=c(scalemin, scalemin*-1)) %>%
    add_axis('x',tick_size_major = 0, grid=FALSE,
             title='MoJ', title_offset = -100, layer = 'front',
             properties=axis_props(axis = list(stroke = "red", strokeWidth = 0),
                                   title=list(fontSize=17, fill='white'))) %>%
    hide_axis('y') %>%
    set_options(width=200, height = 200, padding=padding(0,0,20,10), resizable=FALSE) %>%
    bind_shiny('ggvis_moj')

  data_mod %>%
    ggvis(x=~Civil.Service.grad, fill:="#00ccff") %>%
    layer_ribbons(y=~share_2010/2, y2=~share2_2010, fill:='gray', opacity:=.3) %>%
    layer_ribbons(y=~share/2, y2=~share2, fill:="#00ccff", opacity:=.4) %>%    scale_numeric('y', domain=c(scalemin, scalemin*-1)) %>%
    add_axis('x',tick_size_major = 0, grid=FALSE,
             title='MoD', title_offset = -100, layer = 'front',
             properties=axis_props(axis = list(stroke = "red", strokeWidth = 0),
                                   title=list(fontSize=17, fill='white'))) %>%
    hide_axis('y') %>%
    set_options(width=200, height = 200, padding=padding(0,0,20,10), resizable=FALSE) %>%
    bind_shiny('ggvis_mod')

  data_co %>%
    ggvis(x=~Civil.Service.grad, fill:="#00ccff") %>%
    layer_ribbons(y=~share_2010/2, y2=~share2_2010, fill:='gray', opacity:=.3) %>%
    layer_ribbons(y=~share/2, y2=~share2, fill:="#00ccff", opacity:=.4) %>%    scale_numeric('y', domain=c(scalemin, scalemin*-1)) %>%
    add_axis('x',tick_size_major = 0, grid=FALSE,
             title='CO', title_offset = -100, layer = 'front',
             properties=axis_props(axis = list(stroke = "red", strokeWidth = 0),
                                   title=list(fontSize=17, fill='white'))) %>%
    hide_axis('y') %>%
    set_options(width=200, height = 200, padding=padding(0,0,20,10), resizable=FALSE) %>%
    bind_shiny('ggvis_co')

  data_dh %>%
    ggvis(x=~Civil.Service.grad, fill:="#00ccff") %>%
    layer_ribbons(y=~share_2010/2, y2=~share2_2010, fill:='gray', opacity:=.3) %>%
    layer_ribbons(y=~share/2, y2=~share2, fill:="#00ccff", opacity:=.4) %>%    scale_numeric('y', domain=c(scalemin, scalemin*-1)) %>%
    add_axis('x',tick_size_major = 0, grid=FALSE,
             title='DH', title_offset = -100, layer = 'front',
             properties=axis_props(axis = list(stroke = "red", strokeWidth = 0),
                                   title=list(fontSize=17, fill='white'))) %>%
    hide_axis('y') %>%
    set_options(width=200, height = 200, padding=padding(0,0,20,10), resizable=FALSE) %>%
    bind_shiny('ggvis_dh')

  data_dclg %>%
    ggvis(x=~Civil.Service.grad, fill:="#00ccff") %>%
    layer_ribbons(y=~share_2010/2, y2=~share2_2010, fill:='gray', opacity:=.3) %>%
    layer_ribbons(y=~share/2, y2=~share2, fill:="#00ccff", opacity:=.4) %>%    scale_numeric('y', domain=c(scalemin, scalemin*-1)) %>%
    add_axis('x',tick_size_major = 0, grid=FALSE,
             title='DCLG', title_offset = -100, layer = 'front',
             properties=axis_props(axis = list(stroke = "red", strokeWidth = 0),
                                   title=list(fontSize=17, fill='white'))) %>%
    hide_axis('y') %>%
    set_options(width=200, height = 200, padding=padding(0,0,20,10), resizable=FALSE) %>%
    bind_shiny('ggvis_dclg')

})