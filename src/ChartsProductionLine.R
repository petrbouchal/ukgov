source('./src/lib/lib_acses.R')

batchproduce <- TRUE

plotformat <- 'pdf'
ph <- 15.5
pw <- 24
WHswitch <- c(TRUE,FALSE)
for(i in WHswitch) {
  whitehallonly <- i
  source('./src/acses/acses_chart_AgeDeGe.R')
  source('./src/acses/acses_chart_DeDisabGrYr.R')
  source('./src/acses/acses_chart_DeGeGrYr.R')
  source('./src/acses/acses_chart_DeGeGrYr_lines.R')
  source('./src/acses/acses_chart_DeGrMinYr.R')
}
source('./src/wmi/WMI_chart.R')
source('./src/pse/PSE_Charts.R')

#source('./src/acses/acses_chart_AgeYr.R')
#source('./src/acses/acses_chart_GeGrChange.R')
#source('./src/acses/acses_chart_DisabGrYr.R')
#source('./src/acses/acses_chart_GrMinYr.R')

batchproduce <- FALSE