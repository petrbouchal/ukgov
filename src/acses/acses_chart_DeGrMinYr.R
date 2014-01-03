source('./src/lib/lib_acses.R')
if(!batchproduce) { # don't override global when 
  whitehallonly <- TRUE # use to override global in lib
}

# Load data ---------------------------------------------------------------

filename <- 'ACSES_Gender_Dept_Ethn_Grade_Pay_data.tsv'
origdata <- LoadAcsesData(filename,location)

# Process data ------------------------------------------------------------
uu <- origdata

# FILTER OUT WAGE BAND LINES
uu <- uu[uu$Wage.band=='Total',]
uu <- uu[uu$Gender=='Total',]
uu <- AddOrgData(uu,whitehallonly)

# MERGE FILTER/GROUP DATA INTO MAIN DATA
uu <- uu[uu$Ethnic.grou!='Total',]
uu <- uu[uu$Ethnic.grou!='Not reported / Not declared',]

# CREATE TOTALS PER GROUP
#uu <- uu[uu$Civil.Service.grad!='Total',]
uu <- ddply(uu, .(Ethnic.grou, Date, Civil.Service.grad,Group),
            summarise, count=sum(count, na.rm=TRUE))

totals <- ddply(uu, .(Date, Civil.Service.grad,Group), summarise,
                total=sum(count, na.rm=TRUE))

# MERGE TOTALS INTO MAIN FILE
uu <- merge(uu, totals)

# CREATE WHITEHALL TOTAL IF NEEDED
if(whitehallonly) {
  uu <- uu[uu$Group!='Whole Civil Service',]
  whtotal <- ddply(uu,.(Date,Ethnic.grou,Civil.Service.grad),summarise,
                   count=sum(count),total=sum(total))
  whtotal$Group <- 'Whitehall'
  uu <- rbind(uu,whtotal)
}

# Calculate share
uu$share <- uu$count/uu$total


# ADJUST FACTOR LABELS
uu <- RelabelGrades(uu)

# Filter out unneeded things
uu <- uu[uu$Civil.Service.grad=='SCS' | uu$Civil.Service.grad=='All grades',]
uu <- uu[uu$Ethnic.grou!='White',]

# Sort departments --------------------------------------------------------

xtot <- ddply(uu[uu$Date==2012 & uu$Civil.Service.grad=='All grades',],.(Group),
              summarise,sorter=sum(share))
uu <- merge(uu,xtot,all.x=T)
#make Whole CS category go last
#uu$sorter[uu$Group=='Whole Civil Service'] <- max(uu$sorter)*1.1
#reorder grouping variable
uu$Group <- reorder(uu$Group,-uu$sorter)

# Mark totals category
uu$totalgroup <- ifelse(uu$Group=='Whole Civil Service' | uu$Group=='Whitehall',
                        TRUE,FALSE)

# Build plot --------------------------------------------------------------

if(whitehallonly) {
  uu$Group <- revalue(uu$Group,c("Whole Civil Service"="Whitehall"))
}
HLcol <- ifelse(whitehallonly,IfGcols[2,3],IfGcols[4,3])
HLmarg <- ifelse(whitehallonly,IfGcols[2,1],IfGcols[4,1])
                
uu$grp <- paste(uu$Group,uu$Civil.Service.grad)

plotname <- 'plot_DeGrMinYr'
plottitle <- 'Ethnic minority civil servants'
ylabel='Ethnic minority Civil Servants as % of disclosed in'
xlabel='ordered by % of ethnic minority Civil Servants in 2012'
if(whitehallonly){
  plottitle=paste0(plottitle,' - Whitehall departments')
  ylabel=paste0(ylabel,' Whitehall dept.')
  xlabel=paste0('Whitehall departments ',xlabel)
  plotname=paste0(plotname,'_WH')
} else {
  plottitle=paste0(plottitle,' - departmental groups')
  ylabel=paste0(ylabel,' dept. group')
  xlabel=paste0('% of staff in grade. Departmental groups ',xlabel)
  plotname=paste0(plotname,'_Group')
}

uu$yvar <- uu$share
maxY <- max(uu$yvar,na.rm=TRUE)
ylimits <- c(0, maxY*1.04)
ylabels <- paste0(abs(ybreaks*100),'%')

uu$minpop <- 0.14

plot_DeGrMinYr <- ggplot(uu,aes(as.factor(Date), yvar,group=grp)) +
  geom_rect(data=uu[uu$totalgroup & uu$Date==2012 & uu$Civil.Service.grad=='SCS',],
            fill=HLcol,xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf) +
  geom_segment(data=uu[uu$Civil.Service.grad=='SCS' & uu$Date=='2012',],
            aes(x=-Inf,xend=Inf, y=minpop, yend=minpop,linetype='UK population (Census 2011)'),
            colour=IfGcols[1,1],show_guide=T,stat='identity', size=.3) +
  geom_bar(aes(fill=Civil.Service.grad,group=Civil.Service.grad),
           width=.6, stat='identity',position='dodge') +
  geom_rect(data=uu[uu$totalgroup & uu$Date==2012 & uu$Civil.Service.grad=='SCS',],
            colour=HLmarg,xmin=-Inf,xmax=Inf,
            ymin=-Inf,ymax=Inf,fill=NA,size=1) +
  scale_fill_manual(values=c('All grades'=IfGcols[5,1],'SCS'=IfGcols[4,1]),
                      labels=c('All grades','Senior Civil Service')) +
  scale_linetype_manual(values=c('UK population (Census 2011)'='dashed')) +
  guides(colour=guide_legend(order=1),
         fill=guide_legend(order=2, override.aes=list(colour=NA)),
         linetype=guide_legend(order=3, keywidth=unit(1,'cm'))) +
  scale_y_continuous(limits=ylimits,labels=percent,expand=c(0,0)) +
  labs(x=xlabel,y=ylabel,title=plottitle) +
  facet_wrap(~Group,nrow=3) +
  theme(axis.line=element_blank(),
        panel.border=element_rect(fill=NA, colour=IfGcols[1,2]),
        axis.text.x=element_text(angle=90),axis.title.y=element_text(size=9),
        axis.ticks=element_line(colour=IfGcols[1,2]),axis.ticks.x=element_blank(),
        panel.grid=element_line(colour=IfGcols[1,3]),panel.grid.major.x=element_blank(),
        panel.grid.minor=element_blank())
plot_DeGrMinYr

# Save plot ---------------------------------------------------------------

# SavePlot(ffamily=fontfamily,plotformat=plotformat,plotname=plotname,ploth=ph,plotw=pw)