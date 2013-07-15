source('./src/acses_lib.R')

# Load data ---------------------------------------------------------------

filename <- 'ACSES_Gender_Dept_Grade_Pay_data.tsv'
origdata <- LoadAcsesData(filename, location)

# Process data ------------------------------------------------------------
uu <- origdata

# FILTER OUT WAGE BAND LINES
uu <- uu[uu$Organisation=='Total (All Departments)',]
uu <- uu[uu$Wage.band=='Total',]

# MERGE FILTER/GROUP DATA INTO MAIN DATA
uu <- uu[uu$Gender!='Total',]

# CREATE TOTALS PER GROUP
totals <- uu[uu$Civil.Service.grad=='Total',]
uu <- uu[uu$Civil.Service.grad!='Total',]
uu <- ddply(uu, .(Gender, Date, Civil.Service.grad),
               summarise, count=sum(count, na.rm=TRUE))

totals <- ddply(totals, .(Date), summarise,
                total=sum(count, na.rm=TRUE))

# MERGE TOTALS INTO MAIN FILE
uu <- merge(uu, totals)
uu$share <- uu$count/uu$total

# Make female share negative
uu$share[uu$Gender=='Female'] <- -uu$share[uu$Gender=='Female']
uu$count[uu$Gender=='Female'] <- -uu$count[uu$Gender=='Female']

# SELECT YEAR
uu <- uu[uu$Date=='2012' | uu$Date=='2010',]
uu$Civil.Service.grad = factor(uu$Civil.Service.grad,
                                  levels(uu$Civil.Service.grad)[c(1,2,4,5,3,6)])
uu <- uu[uu$Civil.Service.grad!='Not reported',]

uu <- RelabelGrades(uu)
uu$grp <- paste0(uu$Gender,' ',uu$Date)

# Build plot --------------------------------------------------------------
plotformat='eps'
plotname <- 'plot_GeGrYr'
plottitle <- 'Civil Servants by gender and grade'
ylabel <- 'Staff in grade as % of whole Civil Service'
xlabel <- ''
pw=15.3/2
ph=24.5/4

uu$yvar <- uu$share
maxY <- max(abs(uu$yvar),na.rm=TRUE)
ylimits <- c(-maxY*1.04, maxY*1.04)
ybreaks <- signif(seq(ylimits[1],ylimits[2],length.out=5),1)
ylabels <- paste0(abs(ybreaks*100),'%')

plot_GeGrYr <- ggplot(uu, aes(x=Civil.Service.grad, y=yvar)) +
  geom_bar(position='dodge', width=.9,data=uu[uu$Gender=='Male',],
           aes(fill=as.factor(grp)),stat='identity') +
  geom_bar(position='dodge', width=.9,data=uu[uu$Gender=='Female',],
           aes(fill=as.factor(grp)),stat='identity') +
  scale_fill_manual(values=c('Female 2010'=IfGcols[3,2],
                             'Female 2012'=IfGcols[3,1],
                             'Male 2010'=IfGcols[2,2],
                             'Male 2012'=IfGcols[2,1])) +
  scale_y_continuous(limits=c(-maxY,maxY),
                     labels=c('20%','10%','0','10%','20%'),
                     breaks=c(-.2,-.1,0,.1,.2)) +  coord_flip() +
  guides(fill=guide_legend(override.aes=list(shape=NA,size=1.2),
                           label.vjust=.5,order=2),
         shape=guide_legend(override.aes=list(size=4),
                            label.vjust=.5,order=2,hjust=0)) +
  labs(title=plottitle, y=ylabel, x=xlabel) +
  theme(axis.line=element_line(colour=IfGcols[1,1]),
        legend.direction='vertical',
        legend.position=c(.2,.8),
        text=element_text(family=fontfamily,size=8),
        plot.title=element_text(family=fontfamily,size=10))

# Draw plot ---------------------------------------------------------------

plot_GeGrYr

# Save plot ---------------------------------------------------------------

SavePlot(plotformat=plotformat,plotname=plotname,ffamily=fontfamily)
plot_GeGrYr
