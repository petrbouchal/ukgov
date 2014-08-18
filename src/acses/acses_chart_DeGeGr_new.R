source('./src/lib/lib_acses.R')

# Load data ---------------------------------------------------------------

filename <- 'ACSES_Gender_Dept_Grade_Pay_data.tsv'
origdata <- LoadAcsesData(file_name=filename,location=location)
whitehallonly <- TRUE

# Process data ------------------------------------------------------------
uu <- origdata %>%
  filter(Wage.band=='Total' & Gender!='Total' & Wage.band=='Total') %>%
  AddOrgData(whitehallonly) %>%
  filter(Include=='Yes') %>%
  select(Group, Whitehall, Gender, Civil.Service.grad, Date, count, Organisation) %>%
  group_by(Group, Whitehall, Gender, Date, Civil.Service.grad) %>%
  summarise(count=sum(count, na.rm=T)) %>%
  group_by(Group, Whitehall, Date) %>%
  mutate(total=sum(count[Civil.Service.grad=='Total'])) %>%
  filter(Civil.Service.grad!='Total' & Civil.Service.grad!='Not reported') %>%
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
HLcol <- ifelse(whitehallonly,IfGcols[4,1],IfGcols[3,1])

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

plot_DeGeGr <- ggplot(uu, aes(Civil.Service.grad, share)) +
  geom_rect(data = uu[uu$totalgroup,],fill=HLcol,xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf,alpha = .01) +
  geom_bar(position='identity', width=1, aes(fill=Gender),stat='identity') +
  geom_rect(data = uu[uu$totalgroup,],colour=HLcol,xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf,alpha = 1,fill=NA,size=2) +
  coord_flip() +
  scale_fill_manual(values=c(IfGcols[2,1],IfGcols[5,1]),
                    labels=c('Female   ', 'Male')) +
  guides(colour = guide_legend(ncol = 1)) +
  guides(col=guide_legend(ncol=3)) +
  scale_y_continuous(breaks=ybreaks,limits=ylimits,labels=ylabels) +
  facet_wrap(~Group, nrow=3) +
  labs(y=ylabel) +
  theme(panel.border=element_rect(fill=NA,color=IfGcols[1,3]),
        plot.title=element_blank(),axis.title.y=element_blank(),
        panel.grid.major.x=element_line(), axis.ticks=element_blank(),
        panel.grid.major.y=element_blank())
plot_DeGeGr

# Save plot ---------------------------------------------------------------

# SavePlot(plotname=plotname,plotformat=plotformat,ploth=ph,plotw=pw,ffamily=fontfamily)
