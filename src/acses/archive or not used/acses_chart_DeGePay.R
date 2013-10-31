source('./src/lib/lib_acses.R')

# Load data ---------------------------------------------------------------

filename <- 'ACSES_Gender_Dept_Grade_Pay_data.tsv'
origdata <- LoadAcsesData(filename,location)
whitehallonly <- TRUE

# Process data ------------------------------------------------------------

# FILTER OUT GRADE LINES
uu <- origdata
uu <- uu[uu$Civil.Service.grad=='Total',]
uu <- uu[uu$Gender!='Total',]

# MERGE FILTER/GROUP DATA INTO MAIN DATA
uu <- AddOrgData(uu,whitehallonly)

# CREATE TOTALS PER GROUP
totals <- uu[uu$Wage.band=='Total',]
uu <- uu[uu$Wage.band!='Total',]
uu <- ddply(uu, .(Group, Gender, Date, Wage.band),
               summarise, count=sum(count, na.rm=TRUE))

totals <- ddply(totals, .(Group, Date), summarise,
                  total=sum(count, na.rm=TRUE))

# MERGE TOTALS INTO MAIN FILE
uu <- merge(uu, totals)
uu$share <- uu$count/uu$total

# SELECT YEAR
uu <- uu[uu$Date=='2012',]

# FILTER OUT
uu <- uu[uu$Wage.band!='not reported',]
uu$grp <- paste0(uu$Group, uu$Gender) 

uu <- RelabelPaybands(uu)

# Sort departments --------------------------------------------------------
gradevalues <- data.frame('gradeval'=c(1:length(levels(uu$Wage.band))),
                          'Wage.band'=levels(uu$Wage.band))
uu <- merge(uu,gradevalues)
xtot <- ddply(uu,.(Group, Date, Wage.band),
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
uu$count[uu$Gender=='Female'] <- -uu$count[uu$Gender=='Female']

# Build plot --------------------------------------------------------------

plotformat='wmf'
pw=24.5
ph=15.3
plotname <- 'plot_DeGePay'
plottitle='Civil Service pay in Whitehall departments by gender'
xlabel="Salary range, £000"
ylabel='Staff in grade and pay range'
if(whitehallonly){
  plottitle=paste0(plottitle,' - Whitehall departments')
  xlabel = paste0(xlabel)
  ylabel = paste0(ylabel, ', as % of staff in age group. Whitehall departments ordered by pay composition')
  plotname = paste0(plotname,'_WH')
} else {
  plottitle=paste0(plottitle,' - departmental groups')
  xlabel = paste0(xlabel)
  ylabel = paste0(ylabel, ' % of staff in age group. Departmental groups ordered by pay composition')
  plotname = paste0(plotname,'_Group')
}

uu$yvar <- uu$share
maxY <- max(abs(uu$share),na.rm=TRUE)
ylimits <- c(-maxY, maxY)
ybreaks <- c(-.2,.2)
ylabels <- paste0(abs(ybreaks*100),'%')

plot_DeGePay <- ggplot(uu, aes(Wage.band, yvar)) +
  geom_bar(position='identity', width=1, aes(fill=Gender),stat='identity') +
  coord_flip() +
  scale_fill_manual(values=c(IfGcols[2,1],IfGcols[5,1]),
                    labels=c('Female   ', 'Male')) +
  guides(col=guide_legend(ncol=3)) +
  scale_y_continuous(breaks=ybreaks,limits=ylimits,labels=ylabels) +
  theme(panel.border=element_rect(fill=NA,colour=IfGcols[1,2])) +
  facet_wrap(~Group, nrow=3) +
  labs(x=xlabel,y=ylabel,title=plottitle)
plot_DeGePay

# Save plot ---------------------------------------------------------------

SavePlot(plotname=plotname,plotformat=plotformat,ploth=ph,plotw=pw)