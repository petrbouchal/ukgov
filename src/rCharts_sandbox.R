require('rCharts')
uu2 <- uu[uu$measure=='Cumulative_Perc_net_change',]
# rr <- xPlot(value ~ Period | Dept, color='Whitehall', data=uu2, type='line-dotted',group=c('Dept','Whitehall')
# rr <- xPlot(value ~ Period, color='Whitehall', data=uu2, type='line-dotted',group=c('Dept','Whitehall'))
rr <- nPlot(value ~ Period | Dept, color='Whitehall', data=uu2, type='multiBarChart',group=c('Dept','Whitehall'))

rr$params$height <- 800
rr
