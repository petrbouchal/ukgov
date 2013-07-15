source('./src/acses_lib.R')

source('./src/PSE_Reshape.R') # to prepare data if needed

# Manual for google charts:
# 
# http://code.google.com/p/google-motion-charts-with-r/wiki/SettingOptions
# http://code.google.com/p/google-motion-charts-with-r/
# https://developers.google.com/chart/interactive/docs/

# plain chart in ggplot2
changel$chart <- TRUE
changel$chart[changel$Dept == 'Total']  <- FALSE
changel$chart[changel$Dept == 'AGO']  <- FALSE
changel$chart[changel$Dept == 'NIO']  <- FALSE
changel$chart[changel$Dept == 'GEO']  <- FALSE
changel$chart[changel$Dept == 'Welsh Gov']  <- FALSE
changel$chart[changel$Dept == 'Scot Gov']  <- FALSE
changel$chart[changel$Dept == 'FCO' & changel$Whitehall!='Total']  <- FALSE
plotPSE <- ggplot(data=changel[changel$measure=='Cumulative_Perc_net_change' &
                                 changel$chart==TRUE,],
                  aes(x=Period,y=value, group=group, colour=Whitehall)) + 
  geom_line(size=1) +
  geom_point(aes(colour=Whitehall), size=1) +
  geom_point(colour='white', size=.8) +
  scale_color_manual(values=c(IfGcols[2,1], IfGcols[5,1], IfGcols[3,1])) +
  scale_y_continuous(labels=percent) +
  facet_wrap(~Dept, scales='fixed',nrow=3) +
  labs(title='Change in civil service staff by department, SR 2010 to present',
       y='% change since SR 2010', x = 'Quarter') +
  guides(colour = guide_legend(ncol = 1)) +
  guides(col=guide_legend(ncol=3)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border=element_rect(fill=NA,color=IfGcols[1,1]),
        panel.margin=unit(c(1,1,1,1),'mm'))
plotPSE

# reshape to wide(r) for googlevis
changel$group <- NULL
changel2 <- data.frame(cast(changel, ... ~ measure + Whitehall))

# dates for googlevis - seems not to be needed
# changel2$Period <- gsub("Q1","-03-31",changel2$Period)
# changel2$Period <- gsub("Q2","-06-30",changel2$Period)
# changel2$Period <- gsub("Q3","-09-30",changel2$Period)
# changel2$Period <- gsub("Q4","-12-31",changel2$Period)
# 
# changel2$Period <- as.Date(changel2$Period, tz = "GMT", format='%Y-%m-%d')

# Google Plot
# TODO: rename variables
# TODO: get percent scales
# TODO: subset data for different charts (excl. totals, excl. abs OR relative vars)
#       and develop separate charts
# TODO: get the quarterly scale working
# TODO: get default variable choices working
suppressPackageStartupMessages(library(googleVis))
Motion=gvisMotionChart(changel2, idvar="Dept", timevar="Period",
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

