library(plyr)
library(pbtools)
source('./src/lib/lib_acses.R')

if(!batchproduce){ # avoid overriding when batch charting
  whitehallonly <- FALSE # use this to override global set in lib
}

# Load data ---------------------------------------------------------------

filename <- 'ACSES_Gender_Dept_Grade_Pay_data.tsv'
origdata <- LoadAcsesData(file_name=filename,location=location)

# Process data ------------------------------------------------------------
uu <- origdata %>%
  # Filter out unneeded totals
  filter(Wage.band=='Total') %>%
  filter(Civil.Service.grad=='Total' | Civil.Service.grad=='Senior Civil Service') %>%
  # Add organisation data and exclude what isn't needed
  AddOrgData(whitehallonly) %>%
  filter(Include=='Yes') %>%
  # Drop unneeded vars
  select(Group, Whitehall, Gender, Civil.Service.grad, Date, count, Organisation) %>%
  # Summarise by departmental group
  group_by(Group, Gender, Date, Civil.Service.grad) %>%
  summarise(count=sum(count, na.rm=T)) %>%
  # Create total variable - dept group total on each row
  group_by(Group, Date, Civil.Service.grad) %>%
  mutate(total=sum(count[Gender=='Total'])) %>%
  filter(Gender=='Female') %>%
  # create share variable
  mutate(share=count/total, grp = paste0(Group, Civil.Service.grad)) %>%
  RelabelGrades()

# Sort departments --------------------------------------------------------
uu <- uu %>%
  group_by(Group) %>%
  mutate(sorter=mean(share[Date==2013 & Civil.Service.grad=='All grades'])) %>%
  ungroup() %>%
  mutate(Group=reorder(Group,-sorter,mean)) %>%
  mutate(totalgroup = ifelse(Group=='Whole Civil Service', TRUE, FALSE))

# Build plot --------------------------------------------------------------

if(whitehallonly) {
  uu$Group <- revalue(uu$Group,c("Whole Civil Service"="Whitehall"))
}
HLcol <- ifelse(whitehallonly,ifgcolours[4,1],ifgcolours[3,1])

plotname <- 'plot_DeGeGrYr'
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
ylimits <- c(0, maxY*1.04)
ybreaks <- c(0,0.25,0.5,0.75)
ylabels <- paste0(abs(ybreaks*100),'%')
xlabels <- c('2008', '09', '10', '11', '12', '2013')

loadcustomthemes(ifgcolours, 'Calibri')
plot_DeGeGrYr <- ggplot(uu, aes(as.factor(Date), y=yvar,group=grp)) +
  geom_rect(data = uu[uu$totalgroup,],fill=HLcol,xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf,alpha = .01) +
  geom_rect(data = uu[uu$totalgroup,],colour=HLcol,xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf,alpha = 1,fill=NA,size=1) +
  geom_line(size=1, aes(colour=Civil.Service.grad),stat='identity') +
  geom_point(aes(colour=Civil.Service.grad),pch=16,show_guide=TRUE) +
  scale_colour_manual(values=c('All grades' = ifgcolours[2,1],'SCS'=ifgcolours[3,1]),
                      labels=c('All grades','Senior Civil Service')) +
  scale_fill_manual(values=c('All grades' = ifgcolours[2,1],'SCS'=ifgcolours[3,1]),
                      labels=c('All grades','Senior Civil Service')) +
  guides(colour = guide_legend(ncol = 2)) +
  scale_y_continuous(breaks=ybreaks,limits=ylimits,labels=ylabels,expand=c(0,0)) +
  scale_x_discrete(labels=xlabels) +
  facet_wrap(~Group, nrow=4) +
  labs(title=NULL, y=ylabel,x=xlabel) +
  theme(panel.border=element_rect(fill=NA,color=ifgcolours[1,4]),
        axis.text.x=element_text(angle=0,vjust=0.5, size=8),
        panel.grid.major.y=element_line(colour=ifgcolours[1,3]),
        legend.key.width=unit(0.5,'cm'), axis.title.y=element_text(size=9, angle=90))
plot_DeGeGrYr

# Save plot ---------------------------------------------------------------

saveplot(plotname=plotname,plotformat='png',ploth=ph,plotw=pw,ffamily=fontfamily,
         plotdir='./charts-output/',dpi=300)
