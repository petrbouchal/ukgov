# Setup -------------------------------------------------------------------
# require(devtools)
# install_github('rCharts', 'ramnathv')

require('rCharts')
source('./src/lib/lib_acses.R')
source('./src/pse/PSE_Reshape.R') # to prepare data if needed
uu <- change

uu2 <- uu[uu$measure=='Cumulative_Perc_net_change',]
uu2 <- uu2[uu$Whitehall!='Total Employment ONS Total',]
uu2 <- uu2[uu$Period=='2013Q3',]
uu2 <- uu2[complete.cases(uu2),]
rr <- xPlot(value ~ Dept | Whitehall, color='Whitehall', data=uu2, type='line-dotted',group=c('Dept','Whitehall'))
# rr <- xPlot(value ~ Period, color='Whitehall', data=uu2, type='line-dotted',group=c('Dept','Whitehall'))
rr <- rPlot(value ~ Dept, color='Whitehall', data=uu2, type='multiBarChart', group='group')
rr <- nPlot(value ~ Dept, data=uu2, type='multiBarChart',group='Whitehall')

rr$params$height <- 600
rr$params$width <- 800
rr
