library(pbtools)
source('./src/lib/lib_acses.R')
library(plyr)

if(!batchproduce){ # avoid overriding when batch charting
  whitehallonly <- TRUE # use this to override global set in lib
}

# Load data ---------------------------------------------------------------

filename <- 'ACSES_Gender_Dept_Grade_Pay_data.tsv'
origdata <- LoadAcsesData2014(file_name=filename,location=location)
  
# Process data ------------------------------------------------------------
uu <- origdata %>%
  filter(Wage.band=='Total' & Gender!='Total') %>%
  AddOrgData(whitehallonly) %>%
  group_by(Group, Gender, Date, Civil.Service.grad) %>%
  summarise(count=sum(count, na.rm=T)) %>%
  group_by(Group, Date, Civil.Service.grad) %>%
  mutate(share=count/sum(count)) %>%
  RelabelGrades() %>%
  filter(Civil.Service.grad=='SCS' | Civil.Service.grad=='All grades') %>%
  mutate(grp=paste0(Group, Civil.Service.grad))

# create Whitehall total if needed
if(whitehallonly) {
  uu <- filter(uu, Group!='Whole Civil Service')
  whtotal <- uu %>% 
    filter(Group!='Whole Civil Service') %>%
    group_by(Civil.Service.grad, Date, Gender) %>%
    summarise(count=sum(count)) %>%
    group_by(Civil.Service.grad, Date) %>%
    mutate(share=count/(sum(count)))
  whtotal$Group <- 'Whitehall'
  whtotal$grp <- paste0(whtotal$Group, whtotal$Civil.Service.grad)
  uu <- rbind(uu,whtotal)
}

# Sort departments --------------------------------------------------------

xtot <- ddply(uu[uu$Date==2014 & uu$Civil.Service.grad=='SCS' & uu$Gender=='Female',],
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
xlabel = 'ordered by % of female Senior Civil Servants in 2014'
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
xlabels <- c('2008', '09', '10', '11', '12', '2014')

loadcustomthemes(ifgbasecolours, 'Calibri')
plot_DeGeGrYr <- ggplot(uu, aes(as.factor(Date), y=yvar,group=grp)) +
  geom_rect(data = uu[uu$totalgroup & uu$Date==2014 & uu$Civil.Service.grad=='SCS',],
            fill=HLcol,xmin = -Inf,xmax = Inf,ymin = -Inf,ymax = Inf,alpha = .5) +
  geom_area(data=uu[uu$Civil.Service.grad!='SCS',],width=.5,
           size=1, aes(fill=Gender,group=Gender),stat='identity', position='stack') +
  geom_line(data=uu[uu$Civil.Service.grad=='SCS' & uu$Gender=='Female',],
            size=1, aes(colour=Civil.Service.grad,ymax=1),position='identity') +
  geom_point(data=uu[uu$Civil.Service.grad=='SCS' & uu$Gender=='Female',],
                     aes(colour=Civil.Service.grad),pch=16,show_guide=TRUE) +
#   geom_rect(data = uu[uu$totalgroup & uu$Date==2014 & uu$Civil.Service.grad=='SCS',],
#             colour=HLmarg,fill=NA,xmin = -Inf,xmax = Inf,ymin = -Inf,ymax = Inf,size = 1) +
  scale_colour_manual(values=c('All grades' = ifgcolours[2,1],'SCS'=ifgcolours[3,1]),
                      labels=c('Senior Civil Service')) +
  scale_y_continuous(labels=percent) +
  scale_x_discrete(labels=xlabels) +
  scale_fill_manual(values=c('Male' = ifgcolours[5,1],'Female'=ifgcolours[2,1]),
                      labels=c('Female','Male')) +
  guides(colour = guide_legend(ncol = 2),
         fill=guide_legend(override.aes=list(colour=NA),order=1)) +
  #scale_y_continuous(breaks=ybreaks,limits=ylimits,labels=ylabels,expand=c(0,0)) +
  facet_wrap(~Group, nrow=4,scales='fixed') +
  labs(x=xlabel) +
  theme(panel.border=element_blank(),
        axis.text.x=element_text(angle=0,vjust=0.5),
        panel.grid.major.y=element_line(),
        axis.title.x=element_text(),
        axis.title.y=element_blank(),axis.ticks.x=element_blank())
plot_DeGeGrYr

# Save plot ---------------------------------------------------------------

# SavePlot(plotname=plotname,plotformat=plotformat,ploth=ph,plotw=pw,ffamily=fontfamily)
