source('./src/acses_lib.R')
origdata <- LoadAcsesData('ACSES_Gender_Dept_Age_Grade_data.tsv',location)

# Process data ------------------------------------------------------------
uu <- origdata

# FILTER OUT LINES
uu <- uu[uu$Gender!='Total' & uu$Civil.Service.grad=='Total',]
uu <- uu[uu$Organisation=='Total (All Departments)',]

uu <- RelabelAgebands(uu)

# SUMMARISE BY GROUP & CATEGORY
uu <- ddply(uu, .(Date, Age.band, Gender),
               summarise, count=sum(count, na.rm=TRUE))

# CREATE TOTALS PER GROUP
totals <- uu[uu$Age.band!='Total',]
totals <- ddply(totals, .(Date), summarise,
                total = sum(count))

# MERGE TOTALS INTO MAIN FILE
uu <- merge(uu, totals)
uu$share <- uu$count/uu$total

# Filter out unneeded things
uu <- uu[uu$Age.band!='Total',]
uu <- uu[uu$Age.band!='Unknown age',]

# Select years and flip one year's value into negative
uu <- uu[uu$Date=='2012' | uu$Date=='2010',]
uu$share[uu$Gender=='Female'] <- -uu$share[uu$Gender=='Female']
uu$count[uu$Gender=='Female'] <- -uu$count[uu$Gender=='Female']

# reshape to create year-on-year change figure
# uu <- melt(uu, id=c('Date','Age.band','Gender'))
# uu <- dcast(uu, ... ~ variable + Date, drop=TRUE)
# uu$sharediff <- (uu$share_2012 - uu$share_2010)

# Build plot --------------------------------------------------------------

plotformat='pdf'
plotname <- 'plot_AgeYr'
plottitle <- 'Civil Servants by age'
ylabel <- 'Staff in age group as % of whole Civil Service'
xlabel <- ''
pw=15.3/2
ph=24.5/4

uu$yvar <- uu$share

maxY <- max(abs(uu$yvar),na.rm=TRUE)
ylimits <- c(-maxY*1.04, maxY*1.04)
ybreaks <- signif(seq(ylimits[1],ylimits[2],length.out=5),1)
ylabels <- paste0(abs(ybreaks*100),'%')

uu$transp <- 1
uu$transp[uu$Date==2012] <- 1

plot_AgeYr <- ggplot(uu, aes(x=Age.band, y=yvar,group=Gender)) +
   geom_bar(position='identity', width=.9,data=uu[uu$Date==2012,],
            aes(fill=Gender),stat='identity') +
   geom_point(data=uu[uu$Date==2010,],aes(pch='2010'),
              colour='grey30',size=10,show_guide=T) +
  scale_fill_manual(values=c('Female'=IfGcols[3,1],'Male'=IfGcols[2,1])) +
  scale_y_continuous(limits=ylimits,
                      labels=ylabels,
                      breaks=ybreaks) +
  scale_shape_manual(values=c('2010'='I')) +
  guides(fill=guide_legend(override.aes=list(shape=NA,size=1.2),
                           label.vjust=.5,order=2),
         shape=guide_legend(override.aes=list(size=4),
         label.vjust=.5,order=2,keyheight=.5,hjust=0)) +
  ggtitle(plottitle) +
  ylab(ylabel) +
  xlab(xlabel) +
  theme(plot.margins=c(0,0,0,0),panel.margins=c(0,0,0,0),
        legend.box.just='bottom') +
  coord_flip()

# Draw plot ---------------------------------------------------------------

plot_AgeYr
dev.off()

# Save plot ---------------------------------------------------------------

SavePlot(plotname='AgeYr',ffamily='Calibri',)
plot_AgeYr
