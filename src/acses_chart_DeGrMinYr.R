source('./src/acses_lib.R')

# Load data ---------------------------------------------------------------

filename <- 'ACSES_Gender_Dept_Ethn_Grade_Pay_data.tsv'
origdata <- LoadAcsesData(filename,location)

# Process data ------------------------------------------------------------
uu <- origdata

# FILTER OUT WAGE BAND LINES
uu <- uu[uu$Wage.band=='Total',]
uu <- uu[uu$Gender=='Total',]
uu <- AddOrgData(uu)

# MERGE FILTER/GROUP DATA INTO MAIN DATA
uu <- uu[uu$Ethnic.grou!='Total',]
uu <- uu[uu$Ethnic.grou!='Not reported / Not declared',]

# CREATE TOTALS PER GROUP
#uu <- uu[uu$Civil.Service.grad!='Total',]
uu <- ddply(uu, .(Ethnic.grou, Date, Civil.Service.grad,Group),
            summarise, count=sum(count, na.rm=TRUE))

totals <- ddply(uu, .(Date, Civil.Service.grad,Group), summarise,
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

grp <- paste(uu$Group,uu$Civil.Service.grad)

plotformat='eps'
plotname <- 'plot_DeGrMinYr'
plottitle <- 'Civil Servants identifying as ethnic minority'
ylabel <- 'Staff as % of disclosed'
xlabel <- ''
pw=15.3
ph=24.5

uu$yvar <- uu$share
maxY <- max(uu$yvar,na.rm=TRUE)
ylimits <- c(0, maxY*1.04)
ybreaks <- c(0,0.05,.1,.15)
ylabels <- paste0(abs(ybreaks*100),'%')

uu$minpop <- 0.14

plot_DeGrMinYr <- ggplot(uu,aes(as.factor(Date), yvar,group=grp)) +
  geom_bar(aes(fill=Civil.Service.grad,group=Civil.Service.grad),
           width=.6, stat='identity',position='dodge') +
  geom_line(aes(y=minpop,group=Civil.Service.grad,linetype='UK Population (2011)'),
            colour='grey',show_guide=T,
            stat='identity', size=1) +
  scale_colour_manual(values=c('All grades'=IfGcols[2,1],
                               'SCS'=IfGcols[3,1])) +
  scale_fill_manual(values=c('All grades'=IfGcols[2,1],
                             'SCS'=IfGcols[3,1])) +
  scale_linetype_manual(values=c('UK Population (2011)'='dotted')) +
  guides(colour=guide_legend(order=1),
         fill=guide_legend(order=2, override.aes=list(colour=NA)),
         linetype=guide_legend(order=3, keywidth=unit(1,'cm'))) +
  scale_y_continuous(breaks=ybreaks, limits=ylimits,labels=ylabels,expand=c(0,0)) +
  labs(x=xlabel,y=ylabel,title=plottitle) +
  facet_wrap(~Group,nrow=3) +
  theme(axis.line=element_line(colour=IfGcols[1,1]),
        panel.border=element_rect(fill=NA, colour=IfGcols[1,1]),
        text=element_text(family=fontfamily,size=8),
        plot.title=element_text(size=10))
plot_DeGrMinYr

# Save plot ---------------------------------------------------------------

SavePlot(ffamily=fontfamily,plotformat=plotformat,plotname=plotname,ploth=ph,plotw=pw)