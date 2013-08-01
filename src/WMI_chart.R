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
uu$valuepc <- (uu$value-uu$base)/uu$base


# Filter
uu <- uu[uu$Dept!='W Office',]
uu <- uu[uu$Dept!='S Office',]
uu <- uu[uu$Dept!='AG Depts',]

# Setup plot --------------------------------------------------------------

uu$yvar <- uu$valuepc
plottitle <- ''
plotname <- 'plot_WMI'
ylabel <- 'Change from March 2012 (%)'
xlabel <- NULL

ylimits <- c(min(uu$yvar,na.rm=TRUE),max(uu$yvar,na.rm=TRUE))

# Build plot --------------------------------------------------------------

plot_WMI <- ggplot(uu,aes(x=Month,y=yvar)) +
  geom_hline(y=0, colour=IfGcols[1,2]) +
  geom_line(aes(colour=variable,group=grp),size=.6) +
  #scale_x_datetime(labels=date_format('%Y-%m'),breaks=date_breaks(width = "3 months"))+
  scale_x_datetime(labels=c('Apr\n2012','Jul\n2012','Oct\n2012','Jan\n2013'))+
  scale_colour_manual(values=c(IfGcols[3,1],IfGcols[2,1]),
                      labels=c('FTE (Payroll)','Pay bill (Payroll)')) +
  scale_y_continuous(labels=percent,limits=ylimits) +
  facet_wrap(~Dept,nrow=3,scales='free_y') +
  guides(colour=guide_legend(keywidth=unit(1,'cm'))) +
  labs(x=xlabel,y=ylabel) +
  theme(panel.border=element_rect(fill=NA,colour=IfGcols[1,2]),
        plot.title=element_blank(),
        axis.ticks=element_line(colour=IfGcols[1,2]),
        panel.grid=element_line(colour=IfGcols[1,3]),panel.grid.minor=element_blank(),
        panel.grid.major.x=element_blank(),plot.title=element_blank(),
        axis.text.x=element_text(size=8))
plot_WMI

SavePlot(plotname=plotname,plotformat=plotformat,ploth=ph,plotw=pw,ffamily=fontfamily)