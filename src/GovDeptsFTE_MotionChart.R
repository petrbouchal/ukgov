library(plyr)
source('./src/PSE_Reshape.R') # to prepare data if needed

# Manual for google charts:
# 
# http://code.google.com/p/google-motion-charts-with-r/wiki/SettingOptions
# http://code.google.com/p/google-motion-charts-with-r/
# https://developers.google.com/chart/interactive/docs/

# plain chart in ggplot2
plotPSE <- ggplot(data=subset(changel,changel$measure=='Cumulative_perc_endog_change'),
                  aes(x=Period,y=value, group=group, colour=Whitehall)) + 
  geom_line(size=1) +
  geom_point(aes(colour=Whitehall), size=1) +
  geom_point(colour='white', size=.8) +
  facet_wrap(~Dept) +
  theme_few()
plotPSE

# reshape to wide(r) for googlevis
changel2 <- data.frame(cast(changel, ... ~ measure))

# dates for googlevis
changel2$Period <- gsub("Q1","-03-31",changel2$Period)
changel2$Period <- gsub("Q2","-06-30",changel2$Period)
changel2$Period <- gsub("Q3","-09-30",changel2$Period)
changel2$Period <- gsub("Q4","-12-31",changel2$Period)

changel2$Period <- as.Date(changel2$Period, tz = "GMT", format='%Y-%m-%d')

# Google Plot
suppressPackageStartupMessages(library(googleVis))
Motion=gvisMotionChart(changel2, idvar="group", timevar="Period",
                       options=list(
                         height=400, 
                         width=1000, 
                         state="{\"playDuration\":10000,
                         \"xZoomedDataMin\":0,
                         \"xZoomedIn\":false,
                         \"time\":\"2010-07-01\",
                         \"yZoomedIn\":false,
                         \"orderedByY\":false,
                         \"sizeOption\":\"4\",
                         \"xLambda\":1,
                         \"colorOption\":\"2\",
                         \"yZoomedDataMax\":130000,
                         \"nonSelectedAlpha\":0.4,
                         \"iconType\":\"BUBBLE\",
                         \"dimensions\":{\"iconDimensions\":[\"dim0\"]},
                         \"uniColorForNonSelected\":false,
                       \"yZoomedDataMin\":0,
                       \"xZoomedDataMax\":21,
                       \"duration\":{\"multiplier\":1,\"timeUnit\":\"Q\"},
                       \"xAxisOption\":\"3\",
                       \"orderedByX\":true,
                       \"showTrails\":false,
                       \"yLambda\":1,
                       \"yAxisOption\":\"2\",
                       \"iconKeySettings\":[]};",
                         showSelectListComponent = 1,
                         showHeader=1,
                         showAdvancedPanel=1,
                         showChartButtons=1,
                         showXScalePicker=1,
                         showYScalePicker=1,
                         showXMetricPicker=1,
                         showYMetricPicker=1,
                         showSidePanel=1
                       ))

print(Motion, "chart")

# order levels

DeptsFTEwithtotals$Dept <- reorder(DeptsFTEwithtotals$Dept,
                                   -DeptsFTEwithtotals$sFTE2012Q2) 

