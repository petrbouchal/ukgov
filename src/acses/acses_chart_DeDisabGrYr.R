source('./src/lib/lib_acses.R')
if(!batchproduce) {
  whitehallonly <- FALSE
} 

# Load data ---------------------------------------------------------------

filename <- 'ACSES_Gender_Dept_Disab_Grade_data.tsv'
origdata <- LoadAcsesData(filename,location)

# Process data ------------------------------------------------------------
uu <- origdata %>%
  filter(Wage.band=='Total' & Gender=='Total') %>%
  select(Date, Disability.statu,Civil.Service.grad,Organisation, count) %>%
  filter(Disability.statu!='Not reported / Not declared') %>%
  AddOrgData(whitehallonly) %>%
  group_by(Group,Date,Disability.statu,Civil.Service.grad) %>%
  summarise(count=sum(count, na.rm=T)) %>%
  group_by(Group, Date, Civil.Service.grad) %>%
  mutate(total=count[Disability.statu=='Total'],share=count/total) %>%
  RelabelGrades() %>%
  filter(Disability.statu=='Disabled') %>%
  filter(Civil.Service.grad=='SCS' | Civil.Service.grad=='All grades')

# CREATE WHITEHALL TOTAL IF NEEDED
if(whitehallonly) {
  whtotal <- uu %>%
    filter(Group!='Whole Civil Service') %>%
    group_by(Date,Civil.Service.grad,Disability.statu) %>% 
    summarise(count=sum(count),total=sum(total), share=count/total) %>%
    mutate(Group = 'Whitehall')
  uu <- rbind(uu[uu$Group!='Whole Civil Service',],whtotal)
}

# Sort departments --------------------------------------------------------

xtot <- ddply(uu[uu$Date==2013 & uu$Civil.Service.grad=='All grades',],.(Group),
              summarise,sorter=sum(share))
uu <- merge(uu,xtot,all.x=T)

#reorder grouping variable
uu$Group <- reorder(uu$Group,-uu$sorter)

# Mark totals category
uu$totalgroup <- ifelse(uu$Group=='Whole Civil Service' | uu$Group=='Whitehall',
                        TRUE,FALSE)

# Build plot --------------------------------------------------------------

HLcol <- ifelse(whitehallonly,ifgcolours[2,1],ifgcolours[4,1])
uu$grp <- paste0(uu$Group,uu$Civil.Service.grad)

plotname <- paste0('plot_DeDisabGrYr',
                   if(whitehallonly) { '_WH' } else {'_Group'})
ylabel <- paste0('Civil Servants as % of disclosed in ',
                if(whitehallonly){'Whitehall department'
                                  } else {'departmental group'})
xlabel <- paste0(ifelse(whitehallonly,'Whitehall departments ','Departmental groups '),
                 'ordered by % of disabled staff in workforce in 2013')

uu$yvar <- uu$share
maxY <- max(abs(uu$yvar)*1.04,na.rm=TRUE)
ylimits <- c(0, maxY*1.04)
ybreaks <- c(0,.03,.06,.09)
ylabels <- paste0(abs(ybreaks*100),'%')

loadcustomthemes(ifgcolours, 'Calibri')
plot_DeDisabGrYr <- ggplot(uu,aes(as.factor(Date), yvar,group=grp)) +
  geom_rect(data = uu[uu$totalgroup & uu$Date==2013 & uu$Civil.Service.grad=='SCS',],
            fill=HLcol,xmin = -Inf,xmax = Inf,ymin = -Inf,ymax = Inf,alpha = .1) +
  geom_line(aes(colour=Civil.Service.grad),size=1.1, stat='identity') +
  geom_point(aes(colour=Civil.Service.grad), stat='identity',size=3, pch=19) +
  geom_rect(data = uu[uu$totalgroup & uu$Date==2013 & uu$Civil.Service.grad=='SCS',],
            colour=HLcol,xmin = -Inf,xmax = Inf,ymin = -Inf,ymax = Inf,size = 1,fill=NA,
            alpha=1) +
  scale_colour_manual(values=c('All grades'=ifgcolours[2,1],'SCS'=ifgcolours[3,1]),
                    labels=c('All grades','Senior Civil Service')) +
  guides(colour=guide_legend(order=1),
         fill=guide_legend(order=2,override.aes=list(size=1))) +
  scale_y_continuous(limits=c(0,maxY),labels=percent,expand=c(0,0)) +
  labs(y=ylabel,x=xlabel) +
  facet_wrap(~Group,nrow=4)+
  theme(axis.line=element_blank(),
        panel.border=element_rect(fill=NA, colour=ifgcolours[1,4]),
        plot.title=element_blank(), axis.text.x=element_text(angle=90),
        panel.grid=element_line(colour=ifgcolours[1,3]),panel.grid.major.x=element_blank(),
        panel.grid.minor=element_blank())
plot_DeDisabGrYr

# Save plot ---------------------------------------------------------------

saveplot(ffamily=fontfamily,plotformat=plotformat,ploth=ph,plotw=pw, plotname=plotname,
         plotdir='./charts-output/',dpi=300)
