source('./src/acses_lib.R')

# Load data ---------------------------------------------------------------

filename <- 'ACSES_Gender_Dept_Grade_Pay_data.tsv'
origdata <- LoadAcsesData(file_name=filename,location=location)

# Process data ------------------------------------------------------------
uu <- origdata

# FILTER OUT WAGE BAND LINES
uu <- uu[uu$Wage.band=='Total',]
uu <- AddOrgData(uu)

# MERGE FILTER/GROUP DATA INTO MAIN DATA
uu <- uu[uu$Include=='Yes',]
uu <- uu[uu$Gender!='Total',]

# CREATE TOTALS PER GROUP
totals <- uu[uu$Civil.Service.grad=='Total',]
uu <- uu[uu$Civil.Service.grad!='Total',]
uu <- ddply(uu, .(Group, Gender, Date, Civil.Service.grad),
               summarise, count=sum(count, na.rm=TRUE))

totals <- ddply(totals, .(Group, Date), summarise,
                  total=sum(count, na.rm=TRUE))

# MERGE TOTALS INTO MAIN FILE
uu <- merge(uu, totals)
uu$share <- uu$count/uu$total

# SELECT YEAR
uu <- uu[uu$Date=='2012',]
uu$Civil.Service.grad = factor(uu$Civil.Service.grad,
                                  levels(uu$Civil.Service.grad)[c(1,2,4,5,3,6)])
uu <- uu[uu$Civil.Service.grad!='Not reported',]
uu$grp <- paste0(uu$Group, uu$Gender) 

uu <- RelabelGrades(uu)

# Sort departments --------------------------------------------------------
gradevalues <- data.frame('gradeval'=c(1:length(levels(uu$Civil.Service.grad))),
                          'Civil.Service.grad'=levels(uu$Civil.Service.grad))
uu <- merge(uu,gradevalues)
xtot <- ddply(uu,.(Group, Date, Civil.Service.grad),
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

plotformat='eps'
plotname <- 'plot_DeGeGr'
plottitle <- 'Civil Servants by gender and grade'
ylabel <- 'Staff in grade as % of whole Civil Service'
xlabel <- ''
ph=15.3
pw=24.5

uu$yvar <- uu$share

maxY <- max(abs(uu$yvar),na.rm=TRUE)
ylimits <- c(-maxY*1.04, maxY*1.04)
ybreaks <- c(-.3,-.15,0,.15,.3)
ylabels <- paste0(abs(ybreaks*100),'%')

plot_DeGeGr <- ggplot(uu, aes(Civil.Service.grad, share)) +
  geom_bar(position='identity', width=1, aes(fill=Gender),stat='identity') +
  coord_flip() +
  scale_fill_manual(values=c(IfGcols[3,1],IfGcols[2,1]),
                    labels=c('Female   ', 'Male')) +
  guides(colour = guide_legend(ncol = 1)) +
  guides(col=guide_legend(ncol=3)) +
  scale_y_continuous(breaks=ybreaks,limits=ylimits,labels=ylabels) +
  facet_wrap(~Group, nrow=3) +
  labs(title=plottitle, y=ylabel,x=xlabel) +
  theme(panel.border=element_rect(fill=NA,color=IfGcols[1,3]))
plot_DeGeGr

# Save plot ---------------------------------------------------------------

SavePlot(plotname=plotname,plotformat=plotformat,ploth=ph,plotw=pw,ffamily=fontfamily)
