library(pbtools)
source('./src/lib/lib_acses.R')
library(plyr)

if(!batchproduce){ # avoid overriding when batch charting
  whitehallonly <- FALSE # use this to override global set in lib
}

# Load data ---------------------------------------------------------------

filename <- 'ACSES_Gender_Dept_Grade_Pay_data.tsv'
origdata <- LoadAcsesData(file_name=filename,location=location)

# Process data ------------------------------------------------------------
uu <- origdata

# FILTER OUT WAGE BAND LINES
uu <- uu[uu$Wage.band=='Total',]
uu <- AddOrgData(uu,whitehallonly)

# CREATE TOTALS PER GROUP
uu <- ddply(uu, .(Group, Gender, Date, Civil.Service.grad),
               summarise, count=sum(count, na.rm=TRUE))
uu <- uu[uu$Gender!='Total',]
totals <- ddply(uu, .(Group, Date, Civil.Service.grad),
               summarise, total=sum(count, na.rm=TRUE))

# MERGE TOTALS INTO MAIN FILE
uu <- merge(uu, totals)

# create Whitehall total if needed
if(whitehallonly) {
  uu <- uu[uu$Group!='Whole Civil Service',]
  whtotal <- ddply(uu,.(Date,Civil.Service.grad,Gender),summarise,
                   count=sum(count),total=sum(total))
  whtotal$Group <- 'Whitehall'
  uu <- rbind(uu,whtotal)
}

# calculate totals
uu$share <- uu$count/uu$total

uu <- RelabelGrades(uu)

# SELECT DATA
uu <- uu[uu$Civil.Service.grad=='SCS' | uu$Civil.Service.grad=='All grades',]
uu$grp <- paste0(uu$Group, uu$Civil.Service.grad) 
#uu <- uu[uu$Gender=='Female',]

# Sort departments --------------------------------------------------------

xtot <- ddply(uu[uu$Date==2013 & uu$Civil.Service.grad=='All grades' & uu$Gender=='Female',],
              .(Group),summarise,sorter=sum(share))
uu <- merge(uu,xtot,all.x=T)
#reorder grouping variable
uu$Group <- reorder(uu$Group,-uu$sorter)
uu$gensort <- ifelse(uu$Gender=='Male',0,1)
uu <- uu[with(uu, order(-gensort)), ]

# mark totals category
uu$totalgroup <- ifelse(uu$Group=='Whole Civil Service' | uu$Group=='Whitehall',
                        TRUE,FALSE)

# Build plot --------------------------------------------------------------

if(whitehallonly) {
  uu$Group <- revalue(uu$Group,c("Whole Civil Service"="Whitehall"))
}
HLcol <- ifelse(whitehallonly,ifgcolours[2,3],ifgcolours[4,3])
HLmarg <- ifelse(whitehallonly,ifgcolours[2,1],ifgcolours[4,1])

plotname <- 'plot_DeGeGrYr_alt'

plottitle <- 'Civil Servants by gender and grade'
ylabel = 'Female Civil Servants as % of grade in'
xlabel = 'ordered by % of female Civil Servants in 2013'
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
ylimits <- c(0, 1)
ybreaks <- c(0,0.25,0.5,0.75,1)
ylabels <- paste0(abs(ybreaks*100),'%')

plot_DeGeGrYr <- ggplot(uu, aes(as.factor(Date), y=yvar,group=grp)) +
  geom_rect(data = uu[uu$totalgroup & uu$Date==2013 & uu$Civil.Service.grad=='SCS',],
            fill=HLcol,xmin = -Inf,xmax = Inf,ymin = -Inf,ymax = Inf,alpha = 1) +
  geom_area(data=uu[uu$Civil.Service.grad!='SCS',],width=.5,
           size=1, aes(fill=Gender,group=Gender),stat='identity', position='stack') +
  geom_line(data=uu[uu$Civil.Service.grad=='SCS' & uu$Gender=='Female',],
            size=1, aes(colour=Civil.Service.grad,ymax=1),position='identity') +
  geom_point(data=uu[uu$Civil.Service.grad=='SCS' & uu$Gender=='Female',],
                     aes(colour=Civil.Service.grad),pch=16,show_guide=TRUE) +
  geom_rect(data = uu[uu$totalgroup & uu$Date==2013 & uu$Civil.Service.grad=='SCS',],
            colour=HLmarg,fill=NA,xmin = -Inf,xmax = Inf,ymin = -Inf,ymax = Inf,size = 1) +
  scale_colour_manual(values=c('All grades' = ifgcolours[2,1],'SCS'=ifgcolours[3,1]),
                      labels=c('Senior Civil Service')) +
  scale_y_continuous(labels=percent) +
  scale_fill_manual(values=c('Male' = ifgcolours[5,1],'Female'=ifgcolours[2,1]),
                      labels=c('Female','Male')) +
  guides(colour = guide_legend(ncol = 2),
         fill=guide_legend(override.aes=list(colour=NA),order=1)) +
  #scale_y_continuous(breaks=ybreaks,limits=ylimits,labels=ylabels,expand=c(0,0)) +
  facet_wrap(~Group, nrow=3,scales='fixed') +
  labs(title=plottitle, y=ylabel,x=xlabel) +
  theme(panel.border=element_rect(fill=NA,color=ifgcolours[1,2]),
        axis.text.x=element_text(angle=90,vjust=0.5),
        panel.grid.major.y=element_line(colour=ifgcolours[1,3]),
        axis.title.y=element_text(size=9),axis.ticks.x=element_blank())
plot_DeGeGrYr

# Save plot ---------------------------------------------------------------

# SavePlot(plotname=plotname,plotformat=plotformat,ploth=ph,plotw=pw,ffamily=fontfamily)
