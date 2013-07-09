source('./src/acses_lib.R')

# Load data ---------------------------------------------------------------

filename <- 'ACSES_Gender_Dept_Grade_Pay_data.tsv'
origdata <- LoadAcsesData(filename, location)


# Process data ------------------------------------------------------------
uu <- origdata
uu <- AddOrgData(uu)

# FILTER OUT WAGE BAND LINES
uu <- uu[uu$Organisation=='Total (All Departments)',]
uu <- uu[uu$Wage.band=='Total',]

# MERGE FILTER/GROUP DATA INTO MAIN DATA
uu <- uu[uu$Gender!='Total',]

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

# Make female share negative
uu$share[uu$Gender=='Female'] <- -uu$share[uu$Gender=='Female']
uu$count[uu$Gender=='Female'] <- -uu$count[uu$Gender=='Female']

# SELECT YEAR
uu <- uu[uu$Date=='2012' | uu$Date=='2010',]
uu$Civil.Service.grad = factor(uu$Civil.Service.grad,
                                  levels(uu$Civil.Service.grad)[c(1,2,4,5,3,6)])
uu <- uu[uu$Civil.Service.grad!='Not reported',]

uu <- RelabelGrades(uu)

# Build plot --------------------------------------------------------------
plotformat='png'
plotname <- 'plot_GeGrYr'
plottitle <- 'Civil Servants by age'
ylabel <- 'Staff in age group as % of whole Civil Service'
xlabel <- ''
pw=15.3
ph=24.5/2

uu$yvar <- uu$share
maxY <- max(abs(uu$yvar),na.rm=TRUE)
ylimits <- c(-maxY*1.04, maxY*1.04)
ybreaks <- signif(seq(ylimits[1],ylimits[2],length.out=5),1)
ylabels <- paste0(abs(ybreaks*100),'%')

plot_GeGrYr <- ggplot(uu, aes(x=Civil.Service.grad, y=yvar)) +
  #   geom_area(position='identity',stat='identity',
  #             aes(fill=as.factor(Date),group=Date,alpha=transp,order=-Date)) +
  #   geom_line(position='identity',stat='identity',aes(fill=as.factor(Date),group=Date,
  #             colour=as.factor(Date)),size=1) +
  geom_bar(position='identity', width=.9,data=uu[uu$Date==2012,],
           aes(fill=Gender),stat='identity') +
  geom_point(data=uu[uu$Date==2010,],aes(pch='2010'),
             colour='grey30',size=10,show_guide=T) +
  scale_fill_manual(values=c('Female'='#d40072','Male'='#00ccff')) +
  scale_y_continuous(limits=c(-maxY,maxY),
                     labels=c('20%','10%','0','10%','20%'),
                     breaks=c(-.2,-.1,0,.1,.2)) +
  scale_shape_manual(values=c('2010'='|')) +
  guides(fill=guide_legend(override.aes=list(shape=NA,size=1.2),
                           label.vjust=.5,order=2),
         shape=guide_legend(override.aes=list(size=4),
                            label.vjust=.5,order=2,keyheight=.5,hjust=0)) +
  ggtitle(plottitle) +
  ylab(ylabel) +
  xlab(xlabel) +
  coord_flip()

# Draw plot ---------------------------------------------------------------

plot_GeGrYr

# Save plot ---------------------------------------------------------------

if(plotformat=='pdf' | plotformat=='eps') {
  ggsave(paste0(plotimagepath,plotname,'.',plotformat), family=fontfamily, device=cairo_pdf, height=ph, width=pw, units='cm')  
} else {
  ggsave(paste0(plotimagepath,plotname,'.',plotformat), family=fontfamily, height=ph, width=pw, units='cm')
}
dev.off()
save(plot_GeGrYr,file=paste0(plotobjpath,plotname,'.ggp'))
plot_GeGrYr
