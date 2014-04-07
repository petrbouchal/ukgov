source('./src/lib/lib_acses.R')

# Load data ---------------------------------------------------------------

filename <- 'ACSES_Gender_Dept_Disab_Grade_data.tsv'
origdata <- LoadAcsesData(filename,location)

# Process data ------------------------------------------------------------
uu <- origdata

# FILTER OUT WAGE BAND LINES
uu <- uu[uu$Wage.band=='Total',]
uu <- uu[uu$Gender=='Total',]
uu <- uu[uu$Organisation=='Total (All Departments)',]

# MERGE FILTER/GROUP DATA INTO MAIN DATA
uu <- uu[uu$Disability.statu!='Total',]
uu <- uu[uu$Disability.statu!='Not reported / Not declared',]

# CREATE TOTALS PER GROUP
#uu <- uu[uu$Civil.Service.grad!='Total',]
uu <- ddply(uu, .(Disability.statu, Date, Civil.Service.grad),
               summarise, count=sum(count, na.rm=TRUE))

totals <- ddply(uu, .(Date, Civil.Service.grad), summarise,
                  total=sum(count, na.rm=TRUE))

# MERGE TOTALS INTO MAIN FILE
uu <- merge(uu, totals)
uu$share <- uu$count/uu$total

# ADJUST FACTOR LABELS
uu <- RelabelGrades(uu)

# Filter out unneeded things
uu <- uu[uu$Civil.Service.grad=='SCS' | uu$Civil.Service.grad=='All grades',]
uu <- uu[uu$Disability.statu!='Non-disabled',]

# Build plot --------------------------------------------------------------

plotname <- 'plot_DisabGrYr'
plottitle <- ''
ylabel <- 'Staff as % of disclosed'
plottitle <- 'Civil Servants identifying as disabled'
ylabel <- 'Disabled as % of disclosed'
xlabel <- ''
pw=15.3/3*2
ph=24.5/3

uu$yvar <- uu$share
maxY <- max(abs(uu$share),na.rm=TRUE)
ylimits <- c(0, .1)
ybreaks <- c(0,.025,.05,0.075,.1)
ylabels <- paste0(abs(ybreaks*100),'%')

plot_DisabGrYr <- ggplot(uu,aes(as.factor(Date), yvar)) +
  geom_line(aes(colour=Civil.Service.grad)) +
  geom_point(aes(colour=Civil.Service.grad),size=4) +
  scale_colour_manual(values=c('All grades'=IfGcols[2,1],'SCS'=IfGcols[3,1]),
                      labels=c('All grades','Senior Civil Service')) +
  scale_colour_manual(values=c('All grades'=IfGcols[2,1],'SCS'=IfGcols[3,1]),
                    labels=c('All grades','Senior Civil Service')) +
  guides(colour=guide_legend(order=1),
         fill=guide_legend(order=2,
                             override.aes=list(size=1))) +
  scale_y_continuous(breaks=ybreaks,limits=ylimits,labels=ylabels,expand=c(0,0)) +
  scale_x_discrete(labels=yearlabels) +
  labs(y=ylabel,x=xlabel) +
  theme(axis.line=element_line(colour=IfGcols[1,2]),axis.line.y=element_blank(),
        text=element_text(family=fontfamily,size=10),plot.title=element_blank(),
        legend.position='bottom',plot.title=element_text(size=12),
        panel.grid=element_line(colour=IfGcols[1,3]),panel.grid.minor=element_blank(),
        panel.grid.major.x=element_blank(),axis.ticks=element_blank(),
        axis.ticks.x=element_blank())
plot_DisabGrYr

# Save plot ---------------------------------------------------------------

SavePlot(ffamily=fontfamily,plotformat=plotformat,ploth=ph,plotw=pw, plotname=plotname)
