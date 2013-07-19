source('./src/acses_lib.R')

origdata <- read.csv('./data-input/WMI.csv')

uu <- origdata
uu$Paybill_Payroll <- uu$Paybill_Payroll/10E3

uu <- melt(uu)
uu$grp <- paste0(uu$variable,uu$Dept)
uu$Month <- as.Date(uu$Month,format='%d/%m/%Y')
uu$Month <- as.POSIXct(uu$Month)

wmibase <- data.frame('Dept'=uu$Dept[uu$Month==min(uu$Month)],
                      'base'=uu$value[uu$Month==min(uu$Month)],
                      'variable'=uu$variable[uu$Month==min(uu$Month)])
uu <- merge(uu,wmibase)
uu$valuepc <- uu$value/uu$base-1

pw <- 24.5
ph <- 15.3

uu$yvar <- uu$valuepc
plottitle <- ''
plotname <- 'WMI_depts'
plotformat <- 'emf'
ylabel <- 'Pay bill (£000\'s)/Staff count (FTE)'
xlabel <- NULL

wmiplot <- ggplot(uu,aes(x=Month,y=yvar)) +
  geom_line(aes(colour=variable,group=grp),size=1) +
  scale_x_datetime(labels=date_format('%Y-%m')) +
  scale_colour_manual(values=c(IfGcols[3,1],IfGcols[2,1]),
                      labels=c('FTE (Payroll)','Pay bill (Payroll)'))+
  scale_y_continuous(labels=percent) +
  facet_wrap(~Dept,scales='free_y',ncol=4) +
  theme(panel.border=element_rect(fill=NA,colour=IfGcols[1,2]),
        plot.title=element_blank()) +
  labs(x=xlabel,y=ylabel)
wmiplot

SavePlot(plotname=plotname,plotformat=plotformat,ploth=ph,plotw=pw,ffamily=fontfamily)