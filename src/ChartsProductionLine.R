batchproduce <- TRUE

plotformat <- 'pdf'
ph <- 16
pw <- 24
WHswitch <- c(TRUE,FALSE)
for(i in WHswitch) {
  whitehallonly <- i
  source('./src/acses_chart_AgeDeGe.R')
  source('./src/acses_chart_DeDisabGrYr.R')
  source('./src/acses_chart_DeGeGrYr.R')
  source('./src/acses_chart_DeGeGrYr_lines.R')
  source('./src/acses_chart_DeGrMinYr.R')
}
source('./src/WMI_chart.R')
source('./src/PSE_Charts.R')

source('./src/acses_chart_AgeYr.R')
source('./src/acses_chart_GeGrChange.R')
source('./src/acses_chart_DisabGrYr.R')
source('./src/acses_chart_GrMinYr.R')

batchproduce <- FALSE