#source('./src/acses_lib.R')

ph=24.5/2/2.54
pw=15.3/2.54
fontfamily='Calibri'
plotname='DiversityDashboard'
plotformat='eps'

plot1 <- plot_AgeYr
plot2 <- plot_GrMinYr
plot3 <- plot_DisabGrYr
plot4 <- plot_GeGrYr
cairo_ps(file=paste0('./charts/ACSES charts/',plotname,'.',plotformat),
         family='Calibri',
         #paper='special',
         width=pw, height=ph)
grid.arrange(plot1,plot2,plot3,plot4)
dev.off()