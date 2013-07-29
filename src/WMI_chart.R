source('./src/acses_lib.R')


# Load data ---------------------------------------------------------------

origdata <- read.csv('./data-input/WMI.csv')
uu <- origdata

# Process data ------------------------------------------------------------

uu <- melt(uu)
uu$grp <- paste0(uu$variable,uu$Dept)
uu$Month <- as.Date(uu$Month,format='%d/%m/%Y')
uu$Month <- as.POSIXct(uu$Month)

wmibase <- data.frame('Dept'=uu$Dept[uu$Month==min(uu$Month)],
                      'base'=uu$value[uu$Month==min(uu$Month)],
                      'variable'=uu$variable[uu$Month==min(uu$Month)])
uu <- merge(uu,wmibase)
uu$valuepc <- uu$value/uu$base-1


# Filter
uu <- uu[uu$Dept!='W Office',]
uu <- uu[uu$Dept!='S Office',]
uu <- uu[uu$Dept!='AG Depts',]

# Setup plot --------------------------------------------------------------

pw <- 21
ph <- 14-2.5

uu$yvar <- uu$valuepc
plottitle <- ''
plotname <- 'WMI_depts'
plotformat <- 'eps'
ylabel <- 'Pay bill (£000\'s)/Staff count (FTE)'
xlabel <- NULL

ylimits <- c(min(uu$yvar,na.rm=TRUE),max(uu$yvar,na.rm=TRUE))

# Build plot --------------------------------------------------------------

wmiplot <- ggplot(uu,aes(x=Month,y=yvar)) +
  geom_line(aes(colour=variable,group=grp),size=1) +
  geom_hline(y=0, colour=IfGcols[1,2]) +
  scale_x_datetime(labels=date_format('%Y-%m')) +
  scale_colour_manual(values=c(IfGcols[3,1],IfGcols[2,1]),
                      labels=c('FTE (Payroll)','Pay bill (Payroll)')) +
  scale_y_continuous(labels=percent,limits=ylimits) +
  facet_wrap(~Dept) +
  theme(panel.border=element_rect(fill=NA,colour=IfGcols[1,3]),
        plot.title=element_blank(), axis.ticks=element_line(colour=IfGcols[1,3]),
        axis.ticks.y=element_blank()) +
  labs(x=xlabel,y=ylabel)
wmiplot

SavePlot(plotname=plotname,plotformat=plotformat,ploth=ph,plotw=pw,ffamily=fontfamily)