source('./src/lib/lib_acses.R')

# Load data ---------------------------------------------------------------

filename <- 'ACSES_Gender_Dept_Age_Grade_data.tsv'
origdata <- LoadAcsesData(filename,location)
whitehallonly <- TRUE

# Process data ------------------------------------------------------------
uu <- origdata

# LOAD DATA WITH GROUPINGS AND FILTER - MADE IN EXCEL
uu <- AddOrgData(uu, whitehallonly)

# FILTER OUT UNWANTED LINES
uu <- uu[uu$Gender!='Total',]
uu <- uu[uu$Civil.Service.grad=='Total',]

# CREATE TOTALS PER GROUP
uu <- ddply(uu, .(Group, Date, Age.band),
               summarise, count=sum(count, na.rm=TRUE))

totals <- uu[uu$Age.band=='Total',]
totals <- ddply(totals, .(Group,Date), summarise,
                total = sum(count))

# MERGE TOTALS INTO MAIN FILE
uu <- merge(uu, totals)
uu$share <- uu$count/uu$total

uu$Age.band <- gsub('Aged ','',uu$Age.band)
uu$Age.band <- gsub('and over','+',uu$Age.band)

# Filter out unneeded things
uu <- uu[uu$Age.band!='Total',]
uu <- uu[uu$Age.band!='Unknown age',]

uu <- RelabelAgebands(uu)

# Select years
uu <- uu[uu$Date=='2012',]

# Sort departments --------------------------------------------------------
gradevalues <- data.frame('gradeval'=c(1:length(levels(uu$Age.band))),
                          'Age.band'=levels(uu$Age.band))
uu <- merge(uu,gradevalues)
xtot <- ddply(uu,.(Group, Date, Age.band),
              summarise,sharebothgenders=sum(share, na.rm=TRUE))
uu <- merge(uu,xtot)
uu <- merge(uu,gradevalues)
uu$gradescore <- uu$gradeval*uu$sharebothgenders
xtot <- ddply(uu,.(Group,Date),summarise,meangradescore=mean(gradescore))
uu <- merge(uu,xtot)
uu$sorter <- uu$meangradescore
#make Whole CS category go last
uu$sorter[uu$Group=='Whole Civil Service'] <- max(uu$sorter)*1.1
#reorder grouping variable
uu$Group <- reorder(uu$Group,uu$sorter,mean)

# Make female share negative
uu$share[uu$Gender=='Female'] <- -uu$share[uu$Gender=='Female']

# Build plot --------------------------------------------------------------

plotname <- 'plot_AgeDeYr'

plottitle='Civil Servants in departments by gender and age group'
ylabel = 'Staff in age group as % of whole department'
xlabel = ''

uu$yvar <- uu$share

maxY <- max(abs(uu$yvar),na.rm=TRUE)
ylimits <- c(-maxY*1.04, maxY*1.04)
ybreaks <- c(-.3,-.15,0,.15,.3)
ylabels <- paste0(abs(ybreaks*100),'%')

plot_AgeDeYr <- ggplot(uu, aes(x=Age.band, y=share)) +
  geom_bar(position='identity', width=1, aes(fill=as.factor(Date)),stat='identity') +
  scale_fill_manual(values=c(IfGcols[3,1],IfGcols[2,1]),
                    labels=c('Female   ', 'Male')) +
  guides(col=guide_legend(ncol=3)) +
  theme_few() +
  scale_y_continuous(labels=ylabels,breaks=ybreaks,limits=ylimits) +
  theme() +
  facet_wrap(~Group, nrow=3) +
  ggtitle(plottitle) +
  coord_flip() +
  labs(y=ylabel,title=plottitle)
plot_AgeDeYr

# Save plot ---------------------------------------------------------------

SavePlot(plotname=plotname,plotformat=plotformat,ploth=ph,plotw=pw,ffamily=fontfamily)
