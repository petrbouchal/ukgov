source('./src/lib/lib_acses.R')
if(!batchproduce) {
  whitehallonly <- FALSE
} 

# Load data ---------------------------------------------------------------

filename <- 'ACSES_Gender_Dept_Disab_Grade_data.tsv'
origdata <- LoadAcsesData(filename,location)

# Process data ------------------------------------------------------------
uu <- origdata

# FILTER OUT WAGE BAND LINES
uu <- uu[uu$Wage.band=='Total',]
uu <- uu[uu$Gende=='Total',]

uu <- AddOrgData(uu,whitehallonly)

# MERGE FILTER/GROUP DATA INTO MAIN DATA
uu <- uu[uu$Disability.statu!='Total',]
uu <- uu[uu$Disability.statu!='Not reported / Not declared',]

# CREATE TOTALS PER GROUP
#uu <- uu[uu$Civil.Service.grad!='Total',]
uu <- ddply(uu, .(Disability.statu, Date, Civil.Service.grad,Group),
            summarise, count=sum(count, na.rm=TRUE))

totals <- ddply(uu, .(Date, Civil.Service.grad,Group), summarise,
                total=sum(count, na.rm=TRUE))

# MERGE TOTALS INTO MAIN FILE
uu <- merge(uu, totals)

# CREATE WHITEHALL TOTAL IF NEEDED
if(whitehallonly) {
  uu <- uu[uu$Group!='Whole Civil Service',]
  whtotal <- ddply(uu,.(Date,Civil.Service.grad,Disability.statu),summarise,
                   count=sum(count),total=sum(total))
  whtotal$Group <- 'Whitehall'
  uu <- rbind(uu,whtotal)
}

# calculate share
uu$share <- uu$count/uu$total

# ADJUST FACTOR LABELS
uu <- RelabelGrades(uu)

# Filter out unneeded things
uu <- uu[uu$Civil.Service.grad=='SCS' | uu$Civil.Service.grad=='All grades',]
uu <- uu[uu$Disability.statu!='Non-disabled',]

# Sort departments --------------------------------------------------------

xtot <- ddply(uu[uu$Date==2012 & uu$Civil.Service.grad=='All grades',],.(Group),
              summarise,sorter=sum(share))
uu <- merge(uu,xtot,all.x=T)
#reorder grouping variable
uu$Group <- reorder(uu$Group,-uu$sorter)

# Mark totals category
uu$totalgroup <- ifelse(uu$Group=='Whole Civil Service' | uu$Group=='Whitehall',
                        TRUE,FALSE)

# Build plot --------------------------------------------------------------


HLcol <- ifelse(whitehallonly,IfGcols[2,3],IfGcols[4,3])
HLmarg <- ifelse(whitehallonly,IfGcols[2,1],IfGcols[4,1])
uu$grp <- paste0(uu$Group,uu$Civil.Service.grad)

plotname <- paste0('plot_DeDisabGrYr',
                   if(whitehallonly) { '_WH' } else {'_Group'})
ylabel <- paste0('Civil Servants as % of disclosed in ',
                if(whitehallonly){'Whitehall department'
                                  } else {'departmental group'})
xlabel <- paste0(ifelse(whitehallonly,'Whitehall departments ','Departmental groups '),
                 'ordered by % of disabled staff in workforce in 2012')

uu$yvar <- uu$share
maxY <- max(abs(uu$yvar)*1.04,na.rm=TRUE)
ylimits <- c(0, maxY*1.04)
ybreaks <- c(0,.03,.06,.09)
ylabels <- paste0(abs(ybreaks*100),'%')

plot_DeDisabGrYr <- ggplot(uu,aes(as.factor(Date), yvar,group=grp)) +
  geom_rect(data = uu[uu$totalgroup & uu$Date==2012 & uu$Civil.Service.grad=='SCS',],
            fill=HLcol,xmin = -Inf,xmax = Inf,ymin = -Inf,ymax = Inf,alpha = 1) +
  geom_bar(aes(fill=Civil.Service.grad),
           width=.6, stat='identity',position='dodge') +
  geom_rect(data = uu[uu$totalgroup & uu$Date==2012 & uu$Civil.Service.grad=='SCS',],
            colour=HLmarg,xmin = -Inf,xmax = Inf,ymin = -Inf,ymax = Inf,size = 1,fill=NA) +
  scale_colour_manual(values=c('All grades'=IfGcols[2,1],'SCS'=IfGcols[3,1]),
                    labels=c('All grades','Senior Civil Service')) +
  scale_fill_manual(values=c('All grades'=IfGcols[2,1],'SCS'=IfGcols[3,1]),
                    labels=c('All grades','Senior Civil Service')) +
  guides(colour=guide_legend(order=1),
         fill=guide_legend(order=2,override.aes=list(size=1))) +
  scale_y_continuous(limits=c(0,maxY),labels=percent,expand=c(0,0)) +
  labs(y=ylabel,x=xlabel) +
  facet_wrap(~Group,nrow=3)+
  theme(axis.line=element_blank(),
        panel.border=element_rect(fill=NA, colour=IfGcols[1,2]),
        plot.title=element_blank(), axis.text.x=element_text(angle=90),
        axis.ticks=element_line(colour=IfGcols[1,2]),axis.ticks.x=element_blank(),
        panel.grid=element_line(colour=IfGcols[1,3]),panel.grid.major.x=element_blank(),
        panel.grid.minor=element_blank())
plot_DeDisabGrYr

# Save plot ---------------------------------------------------------------

SavePlot(ffamily=fontfamily,plotformat=plotformat,ploth=ph,plotw=pw, plotname=plotname)