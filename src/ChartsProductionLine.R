source('./src/lib/lib_acses.R')

batchproduce <- TRUE

plotformat <- 'png'
ph <- 6
pw <- 9
WHswitch <- c(TRUE,FALSE)
for(i in WHswitch) {
  whitehallonly <- i
  source('./src/acses/acses_chart_AgeDeGe.R')
  source('./src/acses/acses_chart_DeDisabGrYr.R')
  #source('./src/acses/acses_chart_DeGeGrYr.R')
  source('./src/acses/acses_chart_DeGeGrYr_lines.R')
  source('./src/acses/acses_chart_DeGrMinYr.R')
}
# source('./src/wmi/WMI_chart.R')
# source('./src/pse/PSE_Charts.R')
# source('./src/acses/acses_chart_GeGrChange.R')

source('./src/acses/acses_chart_DisabGrYr_line.R')
source('./src/acses/acses_chart_GrMinYr_line.R')
ph <- 8
pw <- 8
source('./src/acses/acses_chart_AgeYr.R')

batchproduce <- FALSE
