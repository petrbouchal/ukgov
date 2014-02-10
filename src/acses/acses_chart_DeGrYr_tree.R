source('./src/lib/lib_acses.R')

# Load data ---------------------------------------------------------------

filename <- 'ACSES_Gender_Dept_Grade_Pay_data.tsv'
origdata <- LoadAcsesData(file_name=filename,location=location)
whitehallonly <- TRUE

# Process data ------------------------------------------------------------
uu <- origdata

# FILTER OUT WAGE BAND LINES
uu <- uu[uu$Wage.band=='Total',]
uu <- AddOrgData(uu,whitehallonly)

# MERGE FILTER/GROUP DATA INTO MAIN DATA
uu <- uu[uu$Gender=='Total',]

# CREATE TOTALS PER GROUP
totals <- uu[uu$Civil.Service.grad=='Total',]
uu <- uu[uu$Civil.Service.grad!='Total',]
uu <- ddply(uu, .(Group, Gender, Date, Civil.Service.grad),
               summarise, count=sum(count, na.rm=TRUE))

write.csv(uu,file='./data-output/ACSES_DeGeGr.csv')

totals <- ddply(totals, .(Group, Date), summarise,
                  total=sum(count, na.rm=TRUE))

# MERGE TOTALS INTO MAIN FILE
uu <- merge(uu, totals)
uu$share <- uu$count/uu$total

# SELECT YEAR
uu <- uu[uu$Date=='2013' | uu$Date=='2010',]
uu <- uu[uu$Civil.Service.grad!='Not reported',]
uu$grp <- paste0(uu$Group, uu$Date) 

uu <- RelabelGrades(uu)

# Sort departments --------------------------------------------------------
gradevalues <- data.frame('gradeval'=c(1:length(levels(uu$Civil.Service.grad))),
                          'Civil.Service.grad'=levels(uu$Civil.Service.grad))
uu <- merge(uu,gradevalues)
xtot <- ddply(uu,.(Group, Date, Civil.Service.grad),
              summarise,share=sum(share, na.rm=TRUE))
xtot <- xtot[xtot$Date==2013,]
xtot$Date <- NULL
uu <- merge(uu,xtot,all.x=TRUE)
uu <- merge(uu,gradevalues)
uu$gradescore <- uu$gradeval*uu$share
xtot <- ddply(uu,.(Group,Date),summarise,meangradescore=mean(gradescore))
uu <- merge(uu,xtot,all.x=TRUE)
uu$sorter <- uu$meangradescore
#make Whole CS category go last
#uu$sorter[uu$Group=='Whole Civil Service'] <- max(uu$sorter)*1.1
#reorder grouping variable
uu$Group <- reorder(uu$Group,-uu$sorter,mean)

# Make 2010 negative
uu$share[uu$Date==2010] <- -uu$share[uu$Date==2010]
uu$count[uu$Date==2010] <- -uu$count[uu$Date==2010]

uu$totalgroup <- ifelse(uu$Group=='Whole Civil Service',TRUE,FALSE)

# Build plot --------------------------------------------------------------

if(whitehallonly) {
  uu$Group <- revalue(uu$Group,c("Whole Civil Service"="Whitehall"))
}
HLcol <- ifelse(whitehallonly,IfGcols[4,1],IfGcols[3,1])

plotname <- 'plot_DeGeGr'
plottitle <- 'Civil Servants by grade'
ylabel = 'ordered by grade composition of staff in 2013 (most senior workforce first)'
if(whitehallonly){
  plottitle=paste0(plottitle,' - Whitehall departments')
  ylabel = paste0('% of Civil Servants in grade. Whitehall departments ',ylabel)
  plotname = paste0(plotname,'_WH')
} else {
  plottitle=paste0(plottitle,' - departmental groups')
  ylabel = paste0('% of Civil Servants in grade. Departmental groups ',ylabel)
  plotname = paste0(plotname,'_Group')
}

uu$yvar <- uu$share

maxY <- max(abs(uu$yvar),na.rm=TRUE)
ylimits <- c(-maxY*1.04, maxY*1.04)
ybreaks <- c(-.45,-.3,-.15,0,.15,.3,.45)
ylabels <- paste0(abs(ybreaks*100),'%')

plot_DeGeGr <- ggplot(uu, aes(Civil.Service.grad, yvar)) +
  geom_rect(data = uu[uu$totalgroup,],fill=HLcol,xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf,alpha = .01) +
  geom_bar(position='identity', width=1, aes(fill=as.factor(Date)),stat='identity') +
  geom_rect(data = uu[uu$totalgroup,],colour=HLcol,xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf,alpha = 1,fill=NA,size=2) +
  coord_flip() +
  scale_fill_manual(values=c(IfGcols[5,1],IfGcols[2,1]),
                    labels=c('2010   ', '2013')) +
  guides(colour = guide_legend(ncol = 1)) +
  guides(col=guide_legend(ncol=3)) +
  scale_y_continuous(breaks=ybreaks,limits=ylimits,labels=ylabels) +
  facet_wrap(~Group, nrow=3) +
  labs(y=ylabel) +
  theme(panel.border=element_rect(fill=NA,color=IfGcols[1,2]),
        axis.ticks=element_line(colour=IfGcols[1,2]),axis.ticks.y=element_blank(),
        plot.title=element_blank(),axis.title.y=element_blank())
plot_DeGeGr

# Save plot ---------------------------------------------------------------

SavePlot(plotname=plotname,plotformat=plotformat,ploth=ph,plotw=pw,ffamily=fontfamily)
