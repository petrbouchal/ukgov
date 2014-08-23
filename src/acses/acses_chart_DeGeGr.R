library(plyr)
library(pbtools)
source('./src/lib/lib_acses.R')

# Load data ---------------------------------------------------------------

filename <- 'ACSES_Gender_Dept_Grade_Pay_data.tsv'
origdata <- LoadAcsesData(file_name=filename,location=location)
whitehallonly <- FALSE

# Process data ------------------------------------------------------------
uu <- origdata %>%
  # Filter out unneeded totals
  filter(Wage.band=='Total' & Gender!='Total') %>%
  # Add organisation data and exclude what isn't needed
  AddOrgData(whitehallonly) %>%
  filter(Include=='Yes') %>%
  # Drop unneeded vars
  select(Group, Gender, Civil.Service.grad, Date, count, Organisation) %>%
  # Summarise by departmental group
  group_by(Group, Gender, Date, Civil.Service.grad) %>%
  summarise(count=sum(count, na.rm=T)) %>%
  # Create total variable - dept group total on each row
  group_by(Group, Date) %>%
  mutate(total=sum(count[Civil.Service.grad=='Total'])) %>%
  # Exclude unneeded grades
  filter(Civil.Service.grad!='Total' & Civil.Service.grad!='Not reported') %>%
  # create share variable
  mutate(share=count/total, grp = paste0(Group, Gender)) %>%
  RelabelGrades()
  
write.csv(uu,file='./data-output/ACSES_DeGeGr.csv')

# Sort departments --------------------------------------------------------
gradevalues <- data.frame('gradeval'=c(1:length(levels(uu$Civil.Service.grad))),
                          'Civil.Service.grad'=levels(uu$Civil.Service.grad))
uu <- merge(uu,gradevalues) %>%
  group_by(Group, Date, Civil.Service.grad) %>%
  mutate(sharebothgenders=sum(share, na.rm=TRUE)) %>%
  merge(gradevalues) %>%
  mutate(gradescore = gradeval*sharebothgenders) %>%
  group_by(Group,Date) %>%
  mutate(meangradescore=mean(gradescore), sorter=meangradescore) %>%
  ungroup() %>%
  filter(Date=='2013') %>%
  mutate(Group=reorder(Group,-sorter,mean)) %>%
  mutate(share = ifelse(Gender=='Female', -share, share),
         count = ifelse(Gender=='Female', -count, count),
         totalgroup = ifelse(Group=='Whole Civil Service', TRUE, FALSE))


# Build plot --------------------------------------------------------------

if(whitehallonly) {
  uu$Group <- revalue(uu$Group,c("Whole Civil Service"="Whitehall"))
}
HLcol <- ifelse(whitehallonly,ifgcolours[4,1],ifgcolours[3,1])

plotname <- 'plot_DeGeGr'
plottitle <- 'Civil Servants by gender and grade'
ylabel = 'ordered by grade composition of staff (most senior workforce first)'
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
ybreaks <- c(-.3,-.15,0,.15,.3)
ylabels <- paste0(abs(ybreaks*100),'%')

loadcustomthemes(ifgcolours, 'Calibri')
plot_DeGeGr <- ggplot(uu, aes(Civil.Service.grad, share)) +
  geom_rect(data = uu[uu$totalgroup,],fill=HLcol,xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf,alpha = .01) +
  geom_rect(data = uu[uu$totalgroup,],colour=HLcol,xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf,alpha = 1,fill=NA,size=2) +
  geom_bar(position='identity', width=1, aes(fill=Gender),stat='identity') +
  coord_flip() +
  facet_wrap(~Group, nrow=4) +
  scale_fill_manual(values=c(ifgcolours[c(2,5),1]),
                    labels=c('Female   ', 'Male')) +
  guides(fill=guide_legend(ncol=3)) +
  scale_y_continuous(breaks=ybreaks,limits=ylimits,labels=ylabels) +
  labs(y=NULL) +
  theme(panel.border=element_rect(fill=NA,color=ifgcolours[1,4]),
        plot.title=element_blank(),axis.title.y=element_blank(),
        panel.grid.major.x=element_line(color=ifgcolours[1,4]), axis.ticks=element_blank(),
        panel.grid.major.y=element_blank())
plot_DeGeGr

# Save plot ---------------------------------------------------------------

saveplot(plotname=plotname,plotformat=plotformat,ploth=pw,plotw=pw,ffamily=fontfamily,
         plotdir='./charts-output/', dpi=300)
