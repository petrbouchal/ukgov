#source('./src/acses_lib.R')

ph=24.5/2/2.54
pw=15.3/2.54
fontfamily='Calibri'
plotname='DiversityDashboard'
plotformat='wmf'

source('./src/acses_chart_AgeYr.R')
source('./src/acses_chart_GrMinYr.R')
source('./src/acses_chart_GeGrYr.R')
source('./src/acses_chart_DisabGrYr.R')

plot1 <- plot_AgeYr
plot2 <- plot_GrMinYr
plot3 <- plot_DisabGrYr
plot4 <- plot_GeGrYr

cairo_ps(file=paste0('./charts/ACSES charts/',plotname,'.ps'),
         family='Calibri',
         width=pw, height=ph)
grid.arrange(plot1,plot2,plot3,plot4)
dev.off()

cairo_pdf(file=paste0('./charts/ACSES charts/',plotname,'.pdf'),
         family='Calibri',
         width=pw, height=ph)
grid.arrange(plot1,plot2,plot3,plot4)
dev.off()

win.metafile(filename=paste0('./charts/ACSES charts/',plotname,'.wmf'),
         family='Calibri',
         width=pw, height=ph)
grid.arrange(plot1,plot2,plot3,plot4)
dev.off()