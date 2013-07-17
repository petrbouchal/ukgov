source('./src/acses_lib.R')

# Load data ---------------------------------------------------------------

filename <- 'ACSES_Gender_Dept_Age_Grade_data.tsv'
origdata <- LoadAcsesData(filename,location)
grouporwh='WH'

# Process data ------------------------------------------------------------
uu <- origdata

# LOAD DATA WITH GROUPINGS AND FILTER - MADE IN EXCEL
uu <- AddOrgData(uu)
if(grouporwh=='NWH') {
  uu$Whitehall[uu$Group=='HMRC'] <- 'WH'
  uu$Whitehall[uu$Group=='DWP'] <- 'WH'
  uu <- uu[uu$Whitehall=='WH' | uu$Whitehall=='Total',]
}

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

# create group for area plotting
#uu$grp <- paste0(uu$Group, uu$Civil.Service.grad)

# reshape to create year-on-year change figure
# uu <- melt(uu, id=c('Group','Date','Age.band'))
# uu <- dcast(uu, ... ~ variable + Date, drop=TRUE)
# uu$sharediff <- (uu$share_2012 - uu$share_2010)

# Build plot --------------------------------------------------------------

ph = 15.3
pw = 24.5
plotname <- './charts/ACSES charts/plot_AgeDeGe.png'

if(grouporwh=='WH'){
  plottitle='Civil Servants by gender and age group - Whitehall departments'
  ylabel = 'Staff in age group as % of Whitehall dept'
} else {
  plottitle='Civil Servants by gender and age group - departmental groups'
  ylabel = 'Staff in age group as % of departmental group'
}
xlabel = ''

uu$yvar <- uu$share

maxY <- max(abs(uu$yvar),na.rm=FALSE)
ylimits <- c(-maxY*1.04, maxY*1.04)
ybreaks <- c(-.3,-.15,0,.15,.3)
ylabels <- paste0(abs(ybreaks*100),'%')

plot_AgeDeGe <- ggplot(uu, aes(x=Age.band, y=yvar)) +
  geom_bar(position='identity', width=1, aes(fill=Gender),stat='identity') +
  scale_fill_manual(values=c(IfGcols[2,1],IfGcols[5,1]),
                    labels=c('Female   ', 'Male')) +
  guides(col=guide_legend(ncol=3)) +
  scale_y_continuous(labels=ylabels,breaks=ybreaks,limits=ylimits) +
  facet_wrap(~Group, nrow=3) +
  ggtitle(plottitle) +
  coord_flip() +
  labs(y=ylabel,title=plottitle,x=xlabel) +
  theme(panel.border=element_rect(fill=NA,color=IfGcols[1,3]))
plot_AgeDeGe

# Save plot ---------------------------------------------------------------

SavePlot(plotname=plotname,plotformat=plotformat,ploth=ph,plotw=pw,ffamily=fontfamily)
