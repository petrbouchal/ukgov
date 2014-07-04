source('./src/lib/lib_acses.R')
library(pbtools)
if (!batchproduce) {
  whitehallonly <- FALSE # uncomment line to override global WH-only set in lib
}

# Load data ---------------------------------------------------------------

filename <- 'ACSES_Gender_Dept_Age_Grade_data.tsv'
origdata <- LoadAcsesData(filename,location)

# Process data ------------------------------------------------------------
uu <- data.table(origdata)

# LOAD DATA WITH GROUPINGS AND FILTER - MADE IN EXCEL
uu <- AddOrgData(uu,whitehallonly)

# PROCESS DATA
uu <- filter(uu, Gender!='Total' & Civil.Service.grad=='Total' & Wage.band=='Total') %>%
  select(Age.band,Group,Gender,count,Whitehall,Managed,Include,Date) %>%
  filter(Age.band!='Total') %>%
  group_by(Group, Date) %>%
  mutate(total=sum(count, na.rm=TRUE)) %>%
  ungroup() %>%
  group_by(Group,Date, Age.band, Gender) %>%
  summarise(count=sum(count,na.rm=TRUE), total=mean(total,na.rm=TRUE)) %>%
  filter(Age.band!='Unknown age')

uu <- RelabelAgebands(uu)

# CREATE WHITEHALL TOTAL IF NEEDED
if(whitehallonly) {
  whtotal <- group_by(uu,Date,Age.band,Gender) %>%
    filter(Group!='Whole Civil Service') %>%
    summarise(count=sum(count),total=sum(total)) %>%
    mutate(Group = 'Whitehall')
  uu <- rbind(uu,whtotal)
}

# Calculate share
uu$share <- uu$count/uu$total

# Select years
uu <- uu[Date=='2013']

# Sort departments --------------------------------------------------------
uu$Age.band <- as.factor(uu$Age.band)
gradevalues <- data.frame('gradeval'=c(1:length(levels(as.factor(uu$Age.band)))),
                          'Age.band'=levels(as.factor(uu$Age.band)))
uu <- merge(uu,gradevalues,by='Age.band')
xtot <- ddply(uu,.(Group, Date, Age.band),
              summarise,sharebothgenders=sum(share, na.rm=TRUE))
uu <- merge(uu,xtot)
uu$gradescore <- uu$gradeval*uu$sharebothgenders
xtot <- ddply(uu,.(Group,Date),summarise,meangradescore=mean(gradescore))
uu <- merge(uu,xtot)
uu$sorter <- uu$meangradescore
#reorder grouping variable
uu$Group <- reorder(uu$Group,uu$sorter,mean)

# Make female share negative
uu[Gender=='Female', share] <- uu[Gender=='Female',share]

# Mark totals category
uu[,totalgroup] <- ifelse(uu[Group=='Whole Civil Service'] | uu[Group=='Whitehall'],
                        TRUE,FALSE)

# Build plot --------------------------------------------------------------

plotname <- 'plot_AgeDeGe'

HLcol <- ifelse(whitehallonly,IfGcols[2,3],IfGcols[4,3])
HLmarg <- ifelse(whitehallonly,IfGcols[2,1],IfGcols[4,1])

plottitle <- 'Civil Servants by gender and age'
xlabel='Age group (years)'
ylabel='ordered by age composition of staff (youngest workforce first)'
if(whitehallonly){
  plottitle=paste0(plottitle,' - Whitehall departments')
  ylabel=paste0('% of Civil Servants in age group. Whitehall departments ',ylabel)
  plotname=paste0(plotname,'_WH')
} else {
  plottitle=paste0(plottitle,' - departmental groups')
  ylabel=paste0('% of Civil Servants in age group. Departmental groups ',ylabel)
  plotname=paste0(plotname,'_Group')
}

uu$yvar <- uu$share

maxY <- max(abs(uu$yvar),na.rm=FALSE)
ylimits <- c(-maxY*1.04, maxY*1.04)
ybreaks <- c(-.3,-.15,0,.15,.3)
ylabels <- paste0(abs(ybreaks*100),'%')

plot_AgeDeGe <- ggplot(uu, aes(x=Age.band, y=yvar)) +
  geom_rect(data=uu[uu$totalgroup & uu$Date==2013 & uu$Age.band=='< 29' & uu$Gender=='Female',],
            fill=HLcol,xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf,alpha=1) +
  geom_bar(position='identity', width=1, aes(fill=Gender),stat='identity') +
  geom_rect(data=uu[uu$totalgroup & uu$Date==2013 & uu$Age.band=='< 29' & uu$Gender=='Female',],
            colour=HLmarg,xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf,size=1,fill=NA) +
  scale_fill_manual(values=c('Female'=IfGcols[2,1],'Male'=IfGcols[5,1]),
                    labels=c('Female   ', 'Male')) +
  guides(col=guide_legend(ncol=3)) +
  scale_y_continuous(labels=ylabels,breaks=ybreaks,limits=ylimits) +
  facet_wrap(~Group, nrow=3) +
  ggtitle(plottitle) +
  coord_flip() +
  labs(y=ylabel,x=xlabel,title=NULL) +
  theme(panel.border=element_rect(fill=NA,color=IfGcols[1,2],size=.5),
        axis.ticks.y=element_blank(),panel.grid=element_blank())
plot_AgeDeGe

# Save plot ---------------------------------------------------------------

SavePlot(plotname=plotname,plotformat=plotformat,ploth=ph,plotw=pw,ffamily=fontfamily)
