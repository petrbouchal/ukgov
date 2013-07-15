source('./src/acses_lib.R')
origdata <- LoadAcsesData('ACSES_Gender_Dept_Age_Grade_data.tsv',location)

# Process data ------------------------------------------------------------
uu <- origdata

# FILTER OUT LINES
uu <- uu[uu$Gender!='Total' & uu$Civil.Service.grad=='Total',]
uu <- uu[uu$Organisation=='Total (All Departments)',]

uu <- RelabelAgebands(uu)

# SUMMARISE BY GROUP & CATEGORY
uu <- ddply(uu, .(Date, Age.band, Gender),
               summarise, count=sum(count, na.rm=TRUE))

# CREATE TOTALS PER GROUP
totals <- uu[uu$Age.band!='Total',]
totals <- ddply(totals, .(Date), summarise,
                total = sum(count))

# MERGE TOTALS INTO MAIN FILE
uu <- merge(uu, totals)
uu$share <- uu$count/uu$total

# Filter out unneeded things
uu <- uu[uu$Age.band!='Total',]
uu <- uu[uu$Age.band!='Unknown age',]

# Select years and flip one year's value into negative
uu <- uu[uu$Date=='2012' | uu$Date=='2010',]
uu$share[uu$Gender=='Female'] <- -uu$share[uu$Gender=='Female']
uu$count[uu$Gender=='Female'] <- -uu$count[uu$Gender=='Female']

# reshape to create year-on-year change figure
# uu <- melt(uu, id=c('Date','Age.band','Gender'))
# uu <- dcast(uu, ... ~ variable + Date, drop=TRUE)
# uu$sharediff <- (uu$share_2012 - uu$share_2010)

# Build plot --------------------------------------------------------------

plotformat='pdf'
plotname <- 'plot_AgeYr'
plottitle <- 'Civil Servants by gender and age'
ylabel <- 'Staff in age group as % of whole Civil Service'
xlabel <- ''
pw=15.3/2
ph=24.5/4

uu$yvar <- uu$share

maxY <- max(abs(uu$yvar),na.rm=TRUE)
ylimits <- c(-maxY*1.04, maxY*1.04)
ybreaks <- signif(seq(ylimits[1],ylimits[2],length.out=5),1)
ylabels <- paste0(abs(ybreaks*100),'%')

uu$grp <- paste0(uu$Gender,' ',uu$Date)

plot_AgeYr <- ggplot(uu, aes(x=Age.band, y=yvar,group=grp)) +
  geom_bar(position='dodge', width=.9,data=uu[uu$Gender=='Male',],
           aes(fill=as.factor(grp)),stat='identity') +
  geom_bar(position='dodge', width=.9,data=uu[uu$Gender=='Female',],
           aes(fill=as.factor(grp)),stat='identity') +
  scale_fill_manual(values=c('Female 2010'=IfGcols[3,2],
                             'Female 2012'=IfGcols[3,1],
                             'Male 2010'=IfGcols[2,2],
                             'Male 2012'=IfGcols[2,1])) +
  scale_y_continuous(limits=ylimits,
                      labels=ylabels,
                      breaks=ybreaks) +
  guides(fill=guide_legend(override.aes=list(shape=NA,size=1.2),
                           label.vjust=.5,order=2,nrow=2),
         shape=guide_legend(override.aes=list(size=4),
         label.vjust=.5,order=2,hjust=0)) +
  labs(x=xlabel,y=ylabel,title=plottitle) +
  coord_flip() +
  theme(axis.line=element_line(colour=IfGcols[1,1]),
        legend.direction='vertical',
        legend.position=c(.25,.92),
        text=element_text(family=fontfamily,size=8),
        plot.title=element_text(family=fontfamily,size=10))

# Draw plot ---------------------------------------------------------------

plot_AgeYr

# Save plot ---------------------------------------------------------------

SavePlot(plotname=plotname,ffamily='Calibri',ploth=ph,plotw=pw)
plot_AgeYr
