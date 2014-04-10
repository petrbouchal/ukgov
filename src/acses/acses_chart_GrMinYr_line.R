source('./src/lib/lib_acses.R')

# Load data ---------------------------------------------------------------

filename <- 'ACSES_Gender_Dept_Ethn_Grade_Pay_data.tsv'
origdata <- LoadAcsesData(filename,location)

# Process data ------------------------------------------------------------
uu <- origdata

# FILTER OUT WAGE BAND LINES
uu <- uu[uu$Wage.band=='Total',]
uu <- uu[uu$Gender=='Total',]
uu <- uu[uu$Organisation=='Total (All Departments)',]

# MERGE FILTER/GROUP DATA INTO MAIN DATA
uu <- uu[uu$Ethnic.grou!='Total',]
uu <- uu[uu$Ethnic.grou!='Not reported / Not declared',]

# CREATE TOTALS PER GROUP
#uu <- uu[uu$Civil.Service.grad!='Total',]
uu <- ddply(uu, .(Ethnic.grou, Date, Civil.Service.grad),
               summarise, count=sum(count, na.rm=TRUE))

totals <- ddply(uu, .(Date, Civil.Service.grad), summarise,
                  total=sum(count, na.rm=TRUE))

# MERGE TOTALS INTO MAIN FILE
uu <- merge(uu, totals)
uu$share <- uu$count/uu$total

# ADJUST FACTOR LABELS
uu <- RelabelGrades(uu)

# Filter out unneeded things
uu <- uu[uu$Civil.Service.grad=='SCS' | uu$Civil.Service.grad=='All grades',]
uu <- uu[uu$Ethnic.grou!='White',]

# Build plot --------------------------------------------------------------

plotname <- 'plot_GrMinYr'
plottitle <- 'Civil Servants identifying as ethnic minority'
ylabel <- 'Ethnic minority as % of disclosed'
xlabel <- ''
pw=12
ph=9

uu$yvar <- uu$share
maxY <- max(abs(.16),na.rm=TRUE)
ylimits <- c(0, .16)
ybreaks <- c(0,0.05,.1,.15)
ylabels <- paste0(abs(ybreaks*100),'%')

uu$minpop <- 0.14

plot_GrMinYr <- ggplot(uu,aes(as.factor(Date), yvar)) +
  geom_segment(data=uu[uu$Civil.Service.grad=='SCS' & uu$Date=='2012',],
               aes(x='2010',xend='2012', y=minpop, yend=minpop,linetype='UK population (Census 2011)'),
               colour=IfGcols[1,1],show_guide=T,stat='identity', size=.5) +
  geom_line(aes(colour=Civil.Service.grad,group=Civil.Service.grad),size=1) +
  geom_point(aes(colour=Civil.Service.grad,group=Civil.Service.grad),size=4) +
  scale_colour_manual(values=c('All grades'=IfGcols[5,1],'SCS'=IfGcols[4,1]),
                    labels=c('All grades','Senior Civil Service')) +
  scale_linetype_manual(values=c('UK population (Census 2011)'='dashed')) +
  scale_x_discrete(labels=yearlabels)+
  guides(colour=guide_legend(order=1),
         colour=guide_legend(order=2, override.aes=list(colour=NA)),
         linetype=guide_legend(order=3, keywidth=unit(1,'cm'))) +
  scale_y_continuous(breaks=ybreaks,limits=ylimits,labels=ylabels,expand=c(0,0)) +
  labs(x=NULL,y=ylabel) +
  theme(axis.line=element_line(colour=IfGcols[1,2]),axis.line.y=element_blank(),
        text=element_text(family=fontfamily,size=10),plot.title=element_blank(),
        legend.position='bottom',plot.title=element_text(size=12),
        panel.grid=element_line(colour=IfGcols[1,3]),panel.grid.minor=element_blank(),
        panel.grid.major.x=element_blank(),axis.ticks=element_blank(),
        axis.ticks.x=element_blank(),legend.key.width=unit(1,'cm'))
plot_GrMinYr

# Save plot ---------------------------------------------------------------

SavePlot(ffamily=fontfamily,plotformat=plotformat,plotname=plotname,ploth=ph,plotw=pw)
