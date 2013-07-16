source('./src/acses_lib.R')

# Load data ---------------------------------------------------------------

filename <- 'ACSES_Gender_Dept_Disab_Grade_data.tsv'
origdata <- LoadAcsesData(filename,location)

# Process data ------------------------------------------------------------
uu <- origdata

# FILTER OUT WAGE BAND LINES
uu <- uu[uu$Wage.band=='Total',]
uu <- uu[uu$Gende=='Total',]

uu <- AddOrgData(uu)

# MERGE FILTER/GROUP DATA INTO MAIN DATA
uu <- uu[uu$Disability.statu!='Total',]
uu <- uu[uu$Disability.statu!='Not reported / Not declared',]

# CREATE TOTALS PER GROUP
#uu <- uu[uu$Civil.Service.grad!='Total',]
uu <- ddply(uu, .(Disability.statu, Date, Civil.Service.grad,Group),
            summarise, count=sum(count, na.rm=TRUE))

totals <- ddply(uu, .(Date, Civil.Service.grad,Group), summarise,
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

uu$grp <- paste0(uu$Group,uu$Civil.Service.grad)

plotformat='eps'
plotname <- 'plot_DeDisabGrYr'
plottitle <- 'Civil Servants identifying as disabled'
ylabel <- 'Staff as % of disclosed'
xlabel <- ''
pw=15.3/2
ph=24.5/4

uu$yvar <- uu$share
maxY <- max(abs(uu$yvar),na.rm=TRUE)
ylimits <- c(0, maxY*1.04)
ybreaks <- c(0,.03,.06,.09)
ylabels <- paste0(abs(ybreaks*100),'%')

plot_DeDisabGrYr <- ggplot(uu,aes(as.factor(Date), yvar,group=grp)) +
  geom_bar(aes(fill=Civil.Service.grad),
           width=.6, stat='identity',position='dodge') +
  scale_colour_manual(values=c('All grades'=IfGcols[2,1],'SCS'=IfGcols[3,1])) +
  scale_fill_manual(values=c('All grades'=IfGcols[2,1],'SCS'=IfGcols[3,1])) +
  guides(colour=guide_legend(order=1),
         fill=guide_legend(order=2,override.aes=list(size=1))) +
  scale_y_continuous(breaks=ybreaks,limits=ylimits,labels=ylabels,expand=c(0,0)) +
  labs(title=plottitle,y=ylabel,x=xlabel) +
  facet_wrap(~Group,nrow=3)+
  theme(axis.line=element_line(colour=IfGcols[1,1]),
        panel.border=element_rect(fill=NA, colour=IfGcols[1,1]),
        text=element_text(family=fontfamily,size=8),
        plot.title=element_text(size=10))
plot_DeDisabGrYr

# Save plot ---------------------------------------------------------------

SavePlot(ffamily=fontfamily,plotformat=plotformat,ploth=ph,plotw=pw, plotname=plotname)