source('./src/lib/lib_acses.R')
if (!batchproduce) {
  whitehallonly <- FALSE # uncomment line to override global WH-only set in lib
}

# Load data ---------------------------------------------------------------

filename <- 'ACSES_Gender_Dept_Age_Grade_data.tsv'
origdata <- LoadAcsesData(filename,location)

# Process data ------------------------------------------------------------
uu <- origdata

# LOAD DATA WITH GROUPINGS AND FILTER - MADE IN EXCEL
uu <- AddOrgData(uu,whitehallonly)

# FILTER OUT UNWANTED LINES
uu <- uu[uu$Gender!='Total',]
uu <- uu[uu$Age.band=='Total',]
uu <- uu[uu$Wage.band=='Total',]


# CREATE TOTALS PER GROUP
uu <- ddply(uu, .(Group, Date, Civil.Service.grad, Gender),
               summarise, count=sum(count, na.rm=TRUE))

totals <- uu[uu$Civil.Service.grad=='Total',]
totals <- ddply(totals, .(Group,Date), summarise,
                total=sum(count))

# Filter out unneeded things
uu <- uu[uu$Civil.Service.grad!='Not reported',]
uu <- uu[uu$Civil.Service.grad!='Total',]

# MERGE TOTALS INTO MAIN FILE
uu <- merge(uu, totals)

uu <- RelabelGrades(uu)

# CREATE WHITEHALL TOTAL IF NEEDED
if(whitehallonly) {
  uu <- uu[uu$Group!='Whole Civil Service',]
  whtotal <- ddply(uu,.(Date,Civil.Service.grad,Gender),summarise,
                   count=sum(count),total=sum(total))
  whtotal$Group <- 'Whitehall'
  uu <- rbind(uu,whtotal)
}

# Calculate share
uu$share <- uu$count/uu$total

# Select years
uu <- uu[uu$Date=='2013',]

# Sort departments --------------------------------------------------------
gradevalues <- data.frame('gradeval'=c(1:length(levels(as.factor(uu$Civil.Service.grad)))),
                          'Grade'=levels(as.factor(uu$Civil.Service.grad)))
uu <- merge(uu,gradevalues)
xtot <- ddply(uu,.(Group, Date, Civil.Service.grad),
              summarise,sharebothgenders=sum(share, na.rm=TRUE))
uu <- merge(uu,xtot)
uu <- merge(uu,gradevalues)
uu$gradescore <- uu$gradeval*uu$sharebothgenders
xtot <- ddply(uu,.(Group,Date),summarise,meangradescore=mean(gradescore))
uu <- merge(uu,xtot)
uu$sorter <- uu$meangradescore
#reorder grouping variable
uu$Group <- reorder(uu$Group,uu$sorter,mean)

# Make female share negative
uu$share[uu$Gender=='Female'] <- -uu$share[uu$Gender=='Female']

# Mark totals category
uu$totalgroup <- ifelse((uu$Group=='Whole Civil Service' | uu$Group=='Whitehall' & 
                           uu$Gender=='Female' & uu$Civil.Service.grad=='EO'),
                        TRUE,FALSE)

# Build plot --------------------------------------------------------------

plotname <- 'plot_AgeDeGr_tree'

HLcol <- ifelse(whitehallonly,IfGcols[2,3],IfGcols[4,3])
HLmarg <- ifelse(whitehallonly,IfGcols[2,1],IfGcols[4,1])

plottitle <- 'Civil Servants by gender and grade'
xlabel='Civil Service grade'
ylabel='ordered by grade composition of staff (more senior workforce first)'
if(whitehallonly){
  plottitle=paste0(plottitle,' - Whitehall departments')
  ylabel=paste0('% of Civil Servants in grade. Whitehall departments ',ylabel)
  plotname=paste0(plotname,'_WH')
} else {
  plottitle=paste0(plottitle,' - departmental groups')
  ylabel=paste0('% of Civil Servants in grade. Departmental groups ',ylabel)
  plotname=paste0(plotname,'_Group')
}

uu$yvar <- uu$share

maxY <- max(abs(uu$yvar),na.rm=FALSE)
ylimits <- c(-maxY*1.04, maxY*1.04)
ybreaks <- c(-.3,-.15,0,.15,.3)
ylabels <- paste0(abs(ybreaks*100),'%')

plot_AgeDeGe <- ggplot(uu, aes(x=Civil.Service.grad, y=yvar)) +
  geom_rect(data=uu[uu$totalgroup==TRUE,],stat='identity',position='identity',
            fill=HLcol,xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf,alpha=1) +
  geom_rect(data=uu[uu$totalgroup==TRUE,],stat='identity',position='identity',
            colour=HLmarg,xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf,size=1,fill=NA) +
  geom_bar(position='identity', width=1, aes(fill=Gender),stat='identity') +
  facet_wrap(~Group, nrow=3) +
  coord_flip() +
  scale_fill_manual(values=c('Female'=IfGcols[2,1],'Male'=IfGcols[5,1]),
                    labels=c('Female   ', 'Male')) +
  guides(col=guide_legend(ncol=3)) +
  scale_y_continuous(labels=ylabels,breaks=ybreaks,limits=ylimits) +
  ggtitle(plottitle) +
  labs(y=ylabel,x=xlabel,title=NULL) +
  theme(panel.border=element_rect(fill=NA,color=IfGcols[1,2],size=0.5),
        axis.ticks.y=element_blank(),panel.grid=element_blank(),
        strip.background=element_rect(colour='red',size=.5))
plot_AgeDeGe

# Save plot ---------------------------------------------------------------

SavePlot(plotname=plotname,plotformat=plotformat,ploth=ph,plotw=pw,ffamily=fontfamily)
