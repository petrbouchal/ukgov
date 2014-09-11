library(plyr)
library(pbtools)
source('./src/lib/lib_acses.R')

# Load data ---------------------------------------------------------------

filename <- 'ACSES_Gender_Dept_Grade_Pay_data.tsv'
origdata <- LoadAcsesData(file_name=filename,location=location)
whitehallonly <- FALSE
managed <- FALSE

# Process data ------------------------------------------------------------
uu <- origdata %>%
  # Filter out unneeded totals
  filter(Wage.band=='Total' & Gender=='Total') %>%
  # Add organisation data and exclude what isn't needed
  AddOrgData(managedonly = managed) %>%
  # Drop unneeded vars
  select(Group, Civil.Service.grad, Date, count, Organisation) %>%
  # Summarise by departmental group
  group_by(Group, Date, Civil.Service.grad) %>%
  summarise(count=sum(count, na.rm=T)) %>%
  # Create total variable - dept group total on each row
  group_by(Group, Date) %>%
  mutate(total=sum(count[Civil.Service.grad=='Total'])) %>%
  # Exclude unneeded grades
  filter(Civil.Service.grad!='Total' & Civil.Service.grad!='Not reported') %>%
  # create share variable
  mutate(share=count/total) %>%
  RelabelGrades()

# Create 'managed' total if needed
if(managed) {
  managedtotal <- uu %>%
    filter(Group!='Whole Civil Service') %>%
    group_by(Date, Civil.Service.grad) %>%
    summarise(count=sum(count),total=sum(total), share=count/total) %>%
    mutate(Group = 'All managed')
  uu <- rbind(uu[uu$Group!='Whole Civil Service',],managedtotal)
}
  
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
  filter(Date=='2013' | Date=='2010') %>%
  mutate(Group=reorder(Group,-sorter,mean)) %>%
  mutate(totalgroup = ifelse(Group=='Whole Civil Service' | Group=='All managed',
                             TRUE, FALSE)) %>%
  select(-meangradescore, -sharebothgenders)


# Build plot --------------------------------------------------------------

# create 'left' and 'right' data
uu$share2 <- uu$share/2
uu$left <- TRUE
uu2 <- uu
uu2$share2 <- -uu2$share/2
uu2$left <- FALSE

uu <- rbind(uu,uu2)
uu$grp <- paste0(uu$left, uu$Date)
uu <- arrange(uu, Group, grp)


HLcol <- ifelse(managed,ifgcolours[4,1],ifgcolours[3,1])

plotname <- 'plot_DeGrYr_overlapped'

plottitle <- 'Civil Servants by gender and grade'
ylabel = ' Most senior workforce top left'
if(managed){
  plottitle=paste0(plottitle,' - managed departments')
  ylabel = paste0('% of Civil Servants in grade. Managed departments ',ylabel)
  plotname = paste0(plotname,'_WH')
} else {
  plottitle=paste0(plottitle,' - departmental groups.')
  ylabel = paste0('% of Civil Servants in grade. Departmental groups ',ylabel)
  plotname = paste0(plotname,'_Group')
}

uu$yvar <- uu$share2

maxY <- max(abs(uu$yvar),na.rm=TRUE)
ylimits <- c(-maxY*1.04, maxY*1.04)
ybreaks <- c(-.3,-.15,0,.15,.3)
ylabels <- paste0(abs(ybreaks*100),'%')

loadcustomthemes(ifgcolours, 'Calibri')
plot_DeGeGr <- ggplot(uu, aes(Civil.Service.grad, share2, group=grp)) +
  geom_rect(data = uu[uu$totalgroup,],fill=HLcol,xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf,alpha = .01) +
#   geom_rect(data = uu[uu$totalgroup,],colour=HLcol,xmin = -Inf,xmax = Inf,
#             ymin = -Inf,ymax = Inf,alpha = 1,fill=NA,size=2) +
  geom_area(position='identity', width=1,stat='identity',
            data=uu[uu$Date=='2010',], aes(fill='col1'), alpha=1) +
  geom_area(position='identity', width=1,stat='identity',
            data=uu[uu$Date=='2013',], aes(fill='col2'), alpha=.4) +
  geom_text(data=uu[uu$grp=='TRUE2010' & uu$Civil.Service.grad=='SCS',],
            aes(label=Group, x=sorter*5), colour='white',y=0,
            fontface='bold',size=3) +
  coord_flip() +
  facet_wrap(~Group, nrow=5) +
  guides(fill=guide_legend(ncol=3)) +
  scale_fill_manual(values=c('col1'=ifgcolours[5,2],'col2'=ifgcolours[2,1]),
                    labels=c('2010', '2013')) +
  scale_y_continuous(breaks=ybreaks,limits=ylimits,labels=ylabels,
                     expand=c(0,0)) +
  scale_x_discrete(expand=c(0,0)) +
  labs(y=ylabel, x=NULL) +
  theme(panel.border=element_rect(fill=NA,color=NA,size=.5),
        axis.ticks.y=element_blank(),panel.grid=element_blank(),
        strip.text=element_blank(), axis.title.x=element_text())
plot_DeGeGr

# Save plot ---------------------------------------------------------------

saveplot(plotname=plotname,plotformat=plotformat,ploth=pw,plotw=pw,ffamily=fontfamily,
         plotdir='./charts-output/', dpi=300)
