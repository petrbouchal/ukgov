# Setup -------------------------------------------------------------------
source('./src/acses_lib.R')
source('./src/PSE_Reshape.R') # to prepare data if needed
PSE <- change

# Plain chart in ggplot2 --------------------------------------------------

PSE$chart <- TRUE
PSE$chart[PSE$Dept == 'Total']  <- FALSE
PSE$chart[PSE$Dept == 'AGO']  <- FALSE
PSE$chart[PSE$Dept == 'NIO']  <- FALSE
PSE$chart[PSE$Dept == 'GEO']  <- FALSE
PSE$chart[PSE$Dept == 'Welsh Gov']  <- FALSE
PSE$chart[PSE$Dept == 'Scot Gov']  <- FALSE
PSE$chart[PSE$Dept == 'FCO' & PSE$Whitehall!='Total']  <- FALSE

# change order of facets
totals <- PSE[PSE$measure=='Cumulative_Perc_net_change' & PSE$Whitehall=='Total' & PSE$Period=='2013Q1',
                  c(1,3)]
totals$sorter <- totals$value
totals$value <- NULL
PSE <- merge(PSE, totals,all.x=TRUE)
PSE$sorter[PSE$Dept =='Total excl. WH FCO'] <- max(PSE$sorter)*1.1
PSE$Dept <- reorder(PSE$Dept,PSE$sorter,mean)
PSE$Whitehall <- factor(PSE$Whitehall,levels(PSE$Whitehall)[c(3,1,2)])

labelsx <- c('2010Q3','Q4','2011Q1','Q2','Q3','Q4','2012Q1','Q2','Q3','Q4','2013Q4')

plotPSE <- ggplot(data=PSE[PSE$measure=='Cumulative_Perc_net_change' &
                                 PSE$chart,],
                  aes(x=Period,y=value, group=group, colour=Whitehall)) + 
  geom_hline(yintercept=0,colour=IfGcols[1,2]) +
  geom_line(size=1) +
  facet_wrap(~Dept, scales='fixed',nrow=3) +
  scale_color_manual(values=c(IfGcols[3,1], IfGcols[2,1], IfGcols[5,1])) +
  scale_y_continuous(labels=percent) +
  scale_x_discrete(labels=labelsx) +
  labs(title='Change in civil service staff by department, SR 2010 to present',
       y='% change since SR 2010', x = '') +
  guides(colour = guide_legend(ncol = 3,keywidth=unit(1,'cm'))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.line.x=element_line(size=unit(1,'mm'),colour=IfGcols[1,2]),
        panel.border=element_rect(fill=NA,color=IfGcols[1,3]),
        panel.margin=unit(c(1,1,1,1),'mm'))
plotPSE

# Save ggplot -------------------------------------------------------------

SavePlot(plotname='PSE change',plotformat='wmf',ffamily=fontfamily,
         ploth=15.3,plotw=24.5)

# GoogleVis aka Gapminder chart -------------------------------------------

# Manual for google charts:
# 
# http://code.google.com/p/google-motion-charts-with-r/wiki/SettingOptions
# http://code.google.com/p/google-motion-charts-with-r/
# https://developers.google.com/chart/interactive/docs/

# reshape to wide(r) for googlevis
PSE2 <- PSE
PSE2$group <- NULL
PSE2 <- data.frame(cast(PSE, ... ~ measure + Whitehall))

# dates for googlevis - seems not to be needed
# PSE2$Period <- gsub("Q1","-03-31",PSE2$Period)
# PSE2$Period <- gsub("Q2","-06-30",PSE2$Period)
# PSE2$Period <- gsub("Q3","-09-30",PSE2$Period)
# PSE2$Period <- gsub("Q4","-12-31",PSE2$Period)
# 
# PSE2$Period <- as.Date(PSE2$Period, tz = "GMT", format='%Y-%m-%d')

# Google Plot
# TODO: rename variables
# TODO: get percent scales
# TODO: subset data for different charts (excl. totals, excl. abs OR relative vars)
#       and develop separate charts
# TODO: get the quarterly scale working
# TODO: get default variable choices working
suppressPackageStartupMessages(library(googleVis))
Motion=gvisMotionChart(PSE2, idvar="Dept", timevar="Period",
                       options=list(
                         height=600, 
                         width=1000, 
                         state="{\"playDuration\":15000,
                         \"xZoomedDataMin\":0,
                         \"xZoomedIn\":false,
                         \"time\":\"2010-09-30\",
                         \"yZoomedIn\":false,
                         \"orderedByY\":false,
                         \"sizeOption\":\"4\",
                         \"xLambda\":1,
                         \"colorOption\":\"2\",
                         \"yZoomedDataMax\":130000,
                         \"nonSelectedAlpha\":0.1,
                         \"iconType\":\"BUBBLE\",
                         \"dimensions\":{\"iconDimensions\":[\"dim0\"]},
                         \"uniColorForNonSelected\":true,
                         \"yZoomedDataMin\":0,
                         \"xZoomedDataMax\":21,
                         \"duration\":{\"multiplier\":1,\"timeUnit\":\"Y\"},
                         \"xAxisOption\":\"10\",
                         \"orderedByX\":false,
                         \"showTrails\":true,
                         \"yLambda\":1,
                         \"yAxisOption\":\"11\",
                         \"iconKeySettings\":[]};",
                         showSelectListComponent = 1,
                         showHeader=1,
                         showAdvancedPanel=1,
                         showChartButtons=1,
                         showXScalePicker=1,
                         showYScalePicker=1,
                         showXMetricPicker=1,
                         showYMetricPicker=1,
                         showSidePanel=1,
                         vAxis = "{\"format\" : \"#%\"}"
                       ))

plot(Motion)
#print(Motion, "chart")

# reorder levels - keeping code for record
# DeptsFTEwithtotals$Dept <- reorder(DeptsFTEwithtotals$Dept,
#                                   -DeptsFTEwithtotals$sFTE2012Q2) 

