# Load library - includes functions, theme, colours, and libraries --------
source('./src/acses_lib.R')

# Load data ---------------------------------------------------------------

filename <- 'ACSES_Gender_Dept_Grade_Pay_data.tsv'
origdata <- LoadAcsesData(file_name=filename,location=location)

# Process data ------------------------------------------------------------

uu <- origdata
# FILTER OUT WAGE BAND LINES AND LIMIT TO ALL DEPTS LINE
uu <- uu[uu$Organisation=='Total (All Departments)',]
uu <- uu[uu$Wage.band=='Total',]

# PICK WHETHER GENDER BREAKDOWN IS NEEDED
uu <- uu[uu$Gender=='Total',]

# CREATE TOTALS PER GROUP
totals <- uu[uu$Civil.Service.grad=='Total',]
uu <- uu[uu$Civil.Service.grad!='Total',]
uu <- ddply(uu, .(Gender, Date, Civil.Service.grad),
            summarise, count=sum(count, na.rm=TRUE))

totals <- ddply(totals, .(Date), summarise,
                total=sum(count, na.rm=TRUE))

# MERGE TOTALS INTO MAIN FILE
uu <- merge(uu, totals)
uu$share <- uu$count/uu$total

uu <- RelabelGrades(uu)

# SELECT YEAR
uu <- uu[uu$Date=='2012' | uu$Date=='2010',]
uu <- melt(uu, id=c('Civil.Service.grad','Date','Gender'))
uu <- dcast(uu,... ~ variable + Date,na.rm=TRUE)
uu$change <- (uu$count_2012-uu$count_2010)/uu$count_2010
uu <- uu[uu$Civil.Service.grad!='Not reported',]

# Build plot --------------------------------------------------------------

plotformat='tiff'
plotname <- 'plot_GrYr'
plottitle <- 'Civil Servants by grade'
ylabel <- 'Staff in grade as % of whole Civil Service'
xlabel <- ''
pw=15.3
ph=24.5/2

maxY <- max(abs(uu$change),na.rm=TRUE)
ylimits <- c(-maxY*1.04, maxY*1.04)
ybreaks <- signif(seq(ylimits[1],ylimits[2],length.out=5),1)
ylabels <- paste0(abs(ybreaks*100),'%')

plot_GrYr <- ggplot(uu, aes(x=Civil.Service.grad, y=change)) +
  geom_bar(position='dodge', width=.9, fill=IfGcols[2,1],stat='identity') +
  scale_fill_manual(values=c('2010'=IfGcols[2,1],'2012'=IfGcols[3,1])) +
  scale_y_continuous(limits=ylimits, labels=ylabels, breaks=ybreaks) +
  guides(fill=guide_legend(override.aes=list(shape=NA,size=1.2),
                         label.vjust=.5,order=2),
         shape=guide_legend(override.aes=list(size=4),
                            label.vjust=.5,order=2,keyheight=.5,hjust=0)) +
  ggtitle(plottitle) +
  ylab(ylabel) +
  xlab(xlabel) +
  coord_flip()

# Draw plot ---------------------------------------------------------------

plot_GrYr

# Save plot ---------------------------------------------------------------

if(plotformat=='pdf' | plotformat=='eps') {
  ggsave(paste0(plotimagepath,plotname,'.',plotformat), family=fontfamily, device=cairo_pdf, height=ph, width=pw, units='cm')  
} else {
  ggsave(paste0(plotimagepath,plotname,'.',plotformat), family=fontfamily, height=ph, width=pw, units='cm')
}
dev.off()
save(plot_GrYr,file=paste0(plotobjpath,plotname,'.ggp'))
plot_GrYr
