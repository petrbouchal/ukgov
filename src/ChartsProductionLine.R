batchproduce <- TRUE

plotformat <- 'eps'
ph <- 16
pw <- 24
WHswitch <- c(TRUE,FALSE)
for(i in WHswitch) {
  whitehallonly <- i
  source('./src/acses_chart_DeDisabGrYr.R')
  source('./src/acses_chart_DeGeGrYr.R')
  source('./src/acses_chart_DeGrMinYr.R')
  source('./src/acses_chart_AgeDeGe.R')
}
source('./src/WMI_chart.R')
source('./src/PSE_Charts.R')

batchproduce <- FALSE