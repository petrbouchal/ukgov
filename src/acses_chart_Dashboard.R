library(gridExtra)

ph=8000
pw=ph/6.3*9.7
fontfamily='Calibri'

plot1 <- plot_AgeYr
plot2 <- plot_GrMinYr
plot3 <- plot_AgeYr
plot4 <- plot_GrMinYr
jpeg('./charts/ACSES charts/dashboard.jpg',width=pw,height=ph,res=600)
grid.arrange(plot1,plot2,plot3,plot4)
dev.off()