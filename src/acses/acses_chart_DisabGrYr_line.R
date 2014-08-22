library(pbtools)
source('./src/lib/lib_acses.R')

# Load data ---------------------------------------------------------------

filename <- 'ACSES_Gender_Dept_Disab_Grade_data.tsv'
origdata <- LoadAcsesData(filename,location)

# Process data ------------------------------------------------------------
uu <- origdata %>%
  filter(Wage.band=='Total' & Gender=='Total'& Organisation=='Total (All Departments)') %>%
  filter(Disability.statu!='Total' & Disability.statu!='Not reported / Not declared') %>%
  group_by(Disability.statu, Date, Civil.Service.grad) %>%
  summarise(count=sum(count, na.rm=TRUE)) %>%
  group_by(Date, Civil.Service.grad) %>%
  mutate(total=sum(count, na.rm=TRUE)) %>%
  mutate(share = count/total) %>%
  RelabelGrades() %>%
  filter(Civil.Service.grad=='SCS' | Civil.Service.grad=='All grades') %>%
  filter(Disability.statu!='Non-disabled')

# Build plot --------------------------------------------------------------

plotname <- 'plot_DisabGrYr'

plottitle <- ''
ylabel <- 'Staff as % of disclosed'
plottitle <- 'Civil Servants identifying as disabled'
ylabel <- 'Disabled as % of disclosed'
xlabel <- ''
pw=12
ph=9

uu$yvar <- uu$share
maxY <- max(abs(uu$share),na.rm=TRUE)
ylimits <- c(0, .1)
ybreaks <- c(0,.025,.05,0.075,.1)
ylabels <- paste0(abs(ybreaks*100),'%')

loadcustomthemes(ifgcolours, 'Calibri')
plot_DisabGrYr <- ggplot(uu,aes(as.factor(Date), yvar)) +
  geom_line(aes(colour=Civil.Service.grad,group=Civil.Service.grad)) +
  geom_point(aes(colour=Civil.Service.grad),size=4) +
  scale_colour_manual(values=c('All grades'=ifgcolours[2,1],'SCS'=ifgcolours[3,1]),
                      labels=c('All grades','Senior Civil Service')) +
  scale_colour_manual(values=c('All grades'=ifgcolours[2,1],'SCS'=ifgcolours[3,1]),
                    labels=c('All grades','Senior Civil Service')) +
  guides(colour=guide_legend(order=1),
         fill=guide_legend(order=2,
                             override.aes=list(size=1))) +
  scale_y_continuous(breaks=ybreaks,limits=ylimits,labels=ylabels,expand=c(0,0)) +
  scale_x_discrete(labels=yearlabels) +
  labs(y=ylabel,x=NULL) +
  theme(axis.line=element_line(colour=ifgcolours[1,2]),axis.line.y=element_blank(),
        text=element_text(family=fontfamily,size=10),plot.title=element_blank(),
        legend.position='bottom',plot.title=element_text(size=12),
        panel.grid=element_line(colour=ifgcolours[1,3]),panel.grid.minor=element_blank(),
        panel.grid.major.x=element_blank(),axis.ticks=element_blank(),
        axis.ticks.x=element_blank(),legend.key.width=unit(1,'cm'))
plot_DisabGrYr

# Save plot ---------------------------------------------------------------

# saveplot(ffamily=fontfamily,plotformat=plotformat,ploth=ph,plotw=pw, plotname=plotname)
