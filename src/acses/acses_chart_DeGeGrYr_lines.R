library(plyr)
library(pbtools)
source('./src/lib/lib_acses.R')

if(!batchproduce){ # avoid overriding when batch charting
  managed <- TRUE # use this to override global set in lib
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
  AddOrgData(managedonly = managed) %>%
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

# Create 'managed' total if needed
if(managed) {
  managedtotal <- uu %>%
    filter(Group!='Whole Civil Service') %>%
    group_by(Date, Civil.Service.grad, Gender) %>%
    summarise(count=sum(count),total=sum(total), share=count/total) %>%
    mutate(Group = 'All managed', grp = paste0(Group, Civil.Service.grad))
  uu <- rbind(uu[uu$Group!='Whole Civil Service',],managedtotal)
}

# Sort departments --------------------------------------------------------
uu <- uu %>%
  group_by(Group) %>%
  mutate(sorter=mean(share[Date==2013 & Civil.Service.grad=='SCS'])) %>%
  ungroup() %>%
  mutate(Group=reorder(Group,-sorter,mean)) %>%
  mutate(totalgroup = ifelse(Group=='Whole Civil Service' | Group=='All managed',
                             TRUE, FALSE))

# Build plot --------------------------------------------------------------

HLcol <- ifelse(managed,ifgcolours[4,1],ifgcolours[3,1])

plotname <- 'plot_DeGeGrYr'
plottitle <- 'Civil Servants by gender and grade'
ylabel = 'Female Civil Servants as % of grade in'
xlabel = 'ordered by % of female Senior Civil Servants in 2013'
if(managed){
  plottitle=paste0(plottitle,' - managed departments')
  ylabel = paste0(ylabel,' managed dept')
  xlabel = paste0('managed departments ',xlabel)
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
ybreaks <- c(0,0.25,0.5,0.75, 1)
ylabels <- paste0(abs(ybreaks*100),'%')
xlabels <- c('\'08', '\'09', '\'10', '\'11', '\'12', '\'13')

loadcustomthemes(ifgcolours, 'Calibri')
plot_DeGeGrYr <- ggplot(uu, aes(as.factor(Date), y=yvar,group=grp)) +
  geom_rect(data = uu[uu$totalgroup,],fill=HLcol,xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf,alpha = .01) +
#   geom_rect(data = uu[uu$totalgroup,],colour=HLcol,xmin = -Inf,xmax = Inf,
#             ymin = -Inf,ymax = Inf,alpha = 1,fill=NA,size=1) +
  geom_line(size=1, aes(colour=Civil.Service.grad),stat='identity') +
  geom_point(aes(colour=Civil.Service.grad),pch=16,show_guide=TRUE) +
  scale_colour_manual(values=c('All grades' = ifgcolours[2,1],'SCS'=ifgcolours[3,1]),
                      labels=c('All grades','Senior Civil Service')) +
  scale_fill_manual(values=c('All grades' = ifgcolours[2,1],'SCS'=ifgcolours[3,1]),
                      labels=c('All grades','Senior Civil Service')) +
  guides(colour = guide_legend(ncol = 2)) +
  scale_y_continuous(breaks=ybreaks,limits=ylimits,labels=ylabels,expand=c(0,0)) +
  scale_x_discrete(labels=xlabels,expand=c(0,0.15)) +
  facet_wrap(~Group, nrow=4) +
  labs(title=NULL, y=ylabel,x=xlabel) +
  theme(panel.border=element_rect(fill=NA,color=NA,size=.5),
        panel.grid.major.y=element_line(colour=ifgcolours[1,4]),
        axis.ticks.y=element_blank(),
        axis.title=element_text(size=11, colour=ifgbasecolours[1]),
        legend.key.width=unit(.5,'cm'),legend.key.height=unit(.2,'cm'),
        axis.text = element_text(colour=ifgbasecolours[1],size=10),
        axis.text.x = element_text(size=8),
        strip.text=element_text(size=12),
        legend.text=element_text(size=12))
plot_DeGeGrYr

# Save plot ---------------------------------------------------------------

saveplot(plotname=plotname,plotformat='pdf',ploth=15,plotw=17.5,ffamily=fontfamily,
         plotdir='./charts-output/',dpi=300)
