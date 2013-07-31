source('./src/acses_lib.R')

# Load data ---------------------------------------------------------------

filename <- 'ACSES_Gender_Dept_Grade_Pay_data.tsv'
origdata <- LoadAcsesData(file_name=filename,location=location)

# Process data ------------------------------------------------------------
whitehallonly=TRUE
uu <- origdata

# FILTER OUT WAGE BAND LINES
uu <- uu[uu$Wage.band=='Total',]
uu <- AddOrgData(uu,whitehallonly)

totals <- uu[uu$Gender=='Total',]
uu <- uu[uu$Gender!='Total',]

# CREATE TOTALS PER GROUP
uu <- ddply(uu, .(Group, Gender, Date, Civil.Service.grad),
               summarise, count=sum(count, na.rm=TRUE))

totals <- ddply(totals, .(Group, Date,Civil.Service.grad), summarise,
                  total=sum(count, na.rm=TRUE))

# MERGE TOTALS INTO MAIN FILE
uu <- merge(uu, totals)
uu$share <- uu$count/uu$total

uu <- RelabelGrades(uu)

# SELECT DATA
uu <- uu[uu$Civil.Service.grad=='SCS' | uu$Civil.Service.grad=='All grades',]
uu$grp <- paste0(uu$Group, uu$Civil.Service.grad) 
uu <- uu[uu$Gender=='Female',]

# Sort departments --------------------------------------------------------

xtot <- ddply(uu[uu$Date==2012 & uu$Civil.Service.grad=='All grades',],.(Group),
              summarise,sorter=sum(share))
uu <- merge(uu,xtot,all.x=T)
#make Whole CS category go last
#uu$sorter[uu$Group=='Whole Civil Service'] <- max(uu$sorter)*10
#reorder grouping variable
uu$Group <- reorder(uu$Group,-uu$sorter)
uu$totalgroup <- ifelse(uu$Group=='Whole Civil Service',TRUE,FALSE)

# Build plot --------------------------------------------------------------

if(whitehallonly) {
  uu$Group <- revalue(uu$Group,c("Whole Civil Service"="Whitehall"))
}
HLcol <- ifelse(whitehallonly,IfGcols[2,3],IfGcols[3,3])

plotname <- 'plot_DeGeGrYr'
plottitle <- 'Civil Servants by gender and grade'
ylabel = 'Female Civil Servants as % of grade in'
xlabel = 'ordered by % of female Civil Servants in 2012'
if(whitehallonly){
  plottitle=paste0(plottitle,' - Whitehall departments')
  ylabel = paste0(ylabel,' Whitehall dept')
  xlabel = paste0('Whitehall departments ',xlabel)
  plotname = paste0(plotname,'_WH')
} else {
  plottitle=paste0(plottitle,' - departmental groups')
  ylabel = paste0(ylabel,' departmental group')
  xlabel = paste0('Departmental groups ',xlabel)
  plotname = paste0(plotname,'_Group')
}

uu$yvar <- uu$share

maxY <- max(abs(uu$yvar),na.rm=TRUE)
ylimits <- c(0, maxY*1.04)
ybreaks <- c(0,0.25,0.5,0.75)
ylabels <- paste0(abs(ybreaks*100),'%')

plot_DeGeGrYr <- ggplot(uu, aes(as.factor(Date), y=yvar,group=grp)) +
  geom_rect(data = uu[uu$totalgroup & uu$Date==2012 & uu$Civil.Service.grad=='SCS',],
            fill=HLcol,xmin = -Inf,xmax = Inf,ymin = -Inf,ymax = Inf,alpha = 1) +
  geom_line(size=1, aes(colour=Civil.Service.grad),stat='identity') +
  geom_point(aes(colour=Civil.Service.grad),pch=16,show_guide=TRUE) +
  scale_colour_manual(values=c('All grades' = IfGcols[2,1],'SCS'=IfGcols[3,1]),
                      labels=c('All grades','Senior Civil Service')) +
  scale_fill_manual(values=c('All grades' = IfGcols[2,1],'SCS'=IfGcols[3,1]),
                      labels=c('All grades','Senior Civil Service')) +
  guides(colour = guide_legend(ncol = 2)) +
  scale_y_continuous(breaks=ybreaks,limits=ylimits,labels=ylabels,expand=c(0,0)) +
  facet_wrap(~Group, nrow=3) +
  labs(title=plottitle, y=ylabel,x=xlabel) +
  theme(panel.border=element_rect(fill=NA,color=IfGcols[1,2]),
        axis.text.x=element_text(angle=90,vjust=0.5),
        axis.ticks=element_line(colour=IfGcols[1,2]),
        panel.grid=element_line(colour=IfGcols[1,3]),panel.grid.minor=element_blank(),
        panel.grid.major.x=element_blank(),
        legend.key.width=unit(0.5,'cm'))
plot_DeGeGrYr

# Save plot ---------------------------------------------------------------

SavePlot(plotname=plotname,plotformat=plotformat,ploth=ph,plotw=pw,ffamily=fontfamily)
