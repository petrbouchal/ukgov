source('./src/acses_lib.R')

# Load data ---------------------------------------------------------------

filename <- 'ACSES_Gender_Dept_Age_Grade_data.tsv'
origdata <- LoadAcsesData(filename,location)
whitehallonly=TRUE

# Process data ------------------------------------------------------------
uu <- origdata

# LOAD DATA WITH GROUPINGS AND FILTER - MADE IN EXCEL
uu <- AddOrgData(uu,whitehallonly)

# FILTER OUT UNWANTED LINES
uu <- uu[uu$Gender!='Total',]
uu <- uu[uu$Civil.Service.grad=='Total',]
uu <- uu[uu$Wage.band=='Total',]

uu <- RelabelAgebands(uu)

# CREATE TOTALS PER GROUP
uu <- ddply(uu, .(Group, Date, Age.band, Gender),
               summarise, count=sum(count, na.rm=TRUE))

totals <- uu[uu$Age.band=='Total',]
# Filter out unneeded things
uu <- uu[uu$Age.band!='Total',]
totals <- ddply(totals, .(Group,Date), summarise,
                total = sum(count))
uu <- uu[uu$Age.band!='Unknown age',]

# MERGE TOTALS INTO MAIN FILE
uu <- merge(uu, totals)
uu$share <- uu$count/uu$total

# Select years
uu <- uu[uu$Date=='2012',]

# Sort departments --------------------------------------------------------
gradevalues <- data.frame('gradeval'=c(1:length(levels(as.factor(uu$Age.band)))),
                          'Age.band'=levels(as.factor(uu$Age.band)))
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

ph = 15.3
pw = 24.5
plotname <- 'plot_AgeDeGe'

plotname <- 'plot_DeGeGrYr'
plottitle <- 'Civil Servants by gender and age'
xlabel = 'Staff in age group as % of'
ylabel = 'ordered by age composition of staff (youngest workforce first)'
if(whitehallonly){
  plottitle=paste0(plottitle,' - Whitehall departments')
  xlabel = paste0(ylabel,' Whitehall dept')
  ylabel = paste0('% of staff in age group. Whitehall departments ',ylabel)
  plotname = paste0(plotname,'_WH')
} else {
  plottitle=paste0(plottitle,' - departmental groups')
  xlabel = paste0(xlabel,' departmental group')
  ylabel = paste0('% of staff in age group. Departmental groups ',ylabel)
  plotname = paste0(plotname,'_Group')
}

uu$yvar <- uu$share

maxY <- max(abs(uu$yvar),na.rm=FALSE)
ylimits <- c(-maxY*1.04, maxY*1.04)
ybreaks <- c(-.3,-.15,0,.15,.3)
ylabels <- paste0(abs(ybreaks*100),'%')

plot_AgeDeGe <- ggplot(uu, aes(x=Age.band, y=yvar)) +
  geom_bar(position='identity', width=1, aes(fill=Gender),stat='identity') +
  scale_fill_manual(values=c('Female'=IfGcols[2,1],'Male'=IfGcols[5,1]),
                    labels=c('Female   ', 'Male')) +
  guides(col=guide_legend(ncol=3)) +
  scale_y_continuous(labels=ylabels,breaks=ybreaks,limits=ylimits) +
  facet_wrap(~Group, nrow=3) +
  ggtitle(plottitle) +
  coord_flip() +
  labs(y=ylabel,x=xlabel,title=NULL) +
  theme(panel.border=element_rect(fill=NA,color=IfGcols[1,2]))
plot_AgeDeGe

# Save plot ---------------------------------------------------------------

#SavePlot(plotname=plotname,plotformat=plotformat,ploth=ph,plotw=pw,ffamily=fontfamily)
