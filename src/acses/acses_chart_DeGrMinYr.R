library(plyr)
library(pbtools)
source('./src/lib/lib_acses.R')

if(!batchproduce) { # don't override global when 
  whitehallonly <- TRUE # use to override global in lib
}

# Load data ---------------------------------------------------------------

filename <- 'ACSES_Gender_Dept_Ethn_Grade_Pay_data.tsv'
origdata <- LoadAcsesData(filename,location)

# Process data ------------------------------------------------------------
uu <- origdata %>%
  filter(Wage.band=='Total' & Gender=='Total') %>%
  AddOrgData(whitehallonly) %>%
  group_by(Group, Date, Ethnic.grou, Civil.Service.grad) %>%
  summarise(count=sum(count, na.rm=TRUE)) %>%
  group_by(Date, Group, Civil.Service.grad) %>%
  mutate(total=sum(count[Ethnic.grou=='Total'])) %>%
  filter(Ethnic.grou=='Ethnic Minority') %>%
  RelabelGrades() %>%
  filter(Civil.Service.grad=='SCS' | Civil.Service.grad=='All grades') %>%
  mutate(share=count/total)

# CREATE WHITEHALL TOTAL IF NEEDED
if(whitehallonly) {
  whtotal <- uu %>%
    filter(Group!='Whole Civil Service') %>%
    group_by(Date, Ethnic.grou, Civil.Service.grad) %>%
    summarise(count=sum(count),total=sum(total), share=count/total) %>%
    mutate(Group = 'Whitehall')
  uu <- rbind(uu[uu$Group!='Whole Civil Service',],whtotal)
}

# Sort departments --------------------------------------------------------

xtot <- ddply(uu[uu$Date==2013 & uu$Civil.Service.grad=='All grades',],.(Group),
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

HLcol <- ifelse(whitehallonly,ifgcolours[2,1],ifgcolours[4,1])
                
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
            fill=HLcol,xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf, alpha=.05) +
  geom_segment(data=uu[uu$Civil.Service.grad=='SCS' & uu$Date=='2012',],
            aes(x=-Inf,xend=Inf, y=minpop, yend=minpop,linetype='UK population (Census 2011)'),
            colour=ifgcolours[1,1],show_guide=T,stat='identity', size=.3) +
  geom_point(aes(colour=Civil.Service.grad,group=Civil.Service.grad),
           size=4, pch=19, stat='identity') +
  geom_line(aes(colour=Civil.Service.grad,group=Civil.Service.grad),
           size=1, stat='identity') +
  geom_rect(data=uu[uu$totalgroup & uu$Date==2012 & uu$Civil.Service.grad=='SCS',],
            colour=HLcol,xmin=-Inf,xmax=Inf, alpha=1,
            ymin=-Inf,ymax=Inf,fill=NA,size=1) +
  scale_colour_manual(values=c('All grades'=ifgcolours[5,1],'SCS'=ifgcolours[4,1]),
                      labels=c('All grades','Senior Civil Service')) +
  scale_linetype_manual(values=c('UK population (Census 2011)'='dashed')) +
  guides(colour=guide_legend(order=1),
         fill=guide_legend(order=2, override.aes=list(colour=NA)),
         linetype=guide_legend(order=3, keywidth=unit(1,'cm'))) +
  scale_y_continuous(limits=ylimits,labels=percent,expand=c(.01,0)) +
  labs(x=xlabel,y=ylabel,title=NULL) +
  facet_wrap(~Group,nrow=4) +
  theme(axis.line=element_blank(),
        panel.border=element_rect(fill=NA, colour=ifgcolours[1,4]),
        axis.text.x=element_text(angle=90),axis.title.y=element_text(size=9, angle=90),
        axis.ticks=element_blank(),axis.ticks.x=element_blank(),
        panel.grid=element_line(colour=ifgcolours[1,4]),panel.grid.major.x=element_blank(),
        panel.grid.minor=element_blank())
plot_DeGrMinYr

# Save plot ---------------------------------------------------------------

# saveplot(ffamily=fontfamily,plotformat=plotformat,plotname=plotname,ploth=ph,plotw=pw)