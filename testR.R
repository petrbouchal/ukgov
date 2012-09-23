# Manual here: https://google-developers.appspot.com/chart/interactive/docs/gallery/motionchart
# and here: http://code.google.com/p/google-motion-charts-with-r/wiki/GadgetExamples#Motion_Chart

#earners2012 <- read.table("/Users/petrbouchal/Downloads/High Earners/150k-Master-2012-20-9-12-CSV.csv", header=TRUE, sep=",", quote="\"", fileEncoding="UCS-2LE")
setwd("~/github/local/R test project")
load(file="./.Rdata")
library(googleVis)
# read.csv(file.choose())
Motion=gvisMotionChart(df, idvar="country", timevar="year",
                       options=list(
                       height=400, 
                       width=800, 
                       state="{\"iconKeySettings\":[],\"stateVersion\":3,\"time\":\"notime\",\"xAxisOption\":\"_NOTHING\",
                              \"playDuration\":1,\"iconType\":\"HBAR\",\"sizeOption\":\"_UNISIZE\",\"xZoomedDataMin\":null,
                              \"xZoomedIn\":false,\"duration\":{\"multiplier\":1,\"timeUnit\":\"none\"},\"yZoomedDataMin\":null,
                              \"xLambda\":1,\"colorOption\":\"_NOTHING\",\"nonSelectedAlpha\":0.4,
                              \"dimensions\":{\"iconDimensions\":[]},
                              \"yZoomedIn\":false,\"yAxisOption\":\"_NOTHING\",\"yLambda\":1,\"yZoomedDataMax\":null,
                              \"showTrails\":true,\"xZoomedDataMax\":null};",
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
#sink("myfile.txt") # redirect console output to a file
print(Motion, "chart")