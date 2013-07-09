
source('./src/acses_lib.R')

# Load data ---------------------------------------------------------------

filename <- 'ACSES_Gender_Dept_Grade_Pay_data.tsv'
origdata <- LoadAcsesData(file_name=filename,location=location)


# Process data ------------------------------------------------------------
uu <- origdata
uu <- AddOrgData(uu)

# FILTER OUT WAGE BAND LINES
uu <- uu[uu$Wage.band=='Total',]

# MERGE FILTER/GROUP DATA INTO MAIN DATA
uu <- uu[uu$Include=='Yes',]
uu <- uu[uu$Gender!='Total',]

# CREATE TOTALS PER GROUP
totals <- uu[uu$Civil.Service.grad=='Total',]
uu <- uu[uu$Civil.Service.grad!='Total',]
uu <- ddply(uu, .(Group, Gender, Date, Civil.Service.grad),
               summarise, count=sum(count, na.rm=TRUE))

totals <- ddply(totals, .(Group, Date), summarise,
                  total=sum(count, na.rm=TRUE))

# MERGE TOTALS INTO MAIN FILE
uu <- merge(uu, totals)
uu$share <- uu$count/uu$total

# Make female share negative
uu$share[uu$Gender=='Female'] <- -uu$share[uu$Gender=='Female']
uu$count[uu$Gender=='Female'] <- -uu$count[uu$Gender=='Female']

# SELECT YEAR
uu <- uu[uu$Date=='2012',]
uu$Civil.Service.grad = factor(uu$Civil.Service.grad,
                                  levels(uu$Civil.Service.grad)[c(1,2,4,5,3,6)])
uu <- uu[uu$Civil.Service.grad!='Not reported',]
uu$grp <- paste0(uu$Group, uu$Gender) 

uu <- RelabelGrades(uu)

# Build plot --------------------------------------------------------------

plotformat='eps'
plotname <- 'plot_DeGeGr'
plottitle <- 'Civil Servants by grade'
ylabel <- 'Staff in grade as % of whole Civil Service'
xlabel <- ''
pw=15.3
ph=24.5/2

maxY <- max(abs(uu$change),na.rm=TRUE)
ylimits <- c(-maxY*1.04, maxY*1.04)
ybreaks <- signif(seq(ylimits[1],ylimits[2],length.out=5),1)
ylabels <- paste0(abs(ybreaks*100),'%')

plot_DeGeGr <- ggplot(uu, aes(Civil.Service.grad, share)) +
  geom_bar(position='identity', width=1, aes(fill=Gender),stat='identity') +
#  geom_area(aes(group=grp, fill=Gender), data=uu[uu$Gender=='Female',]) +
  coord_flip() +
  scale_fill_manual(values=c(IfGcols[2,1],IfGcols[3,1]),
                    labels=c('Female   ', 'Male')) +
  guides(colour = guide_legend(ncol = 1)) +
  guides(col=guide_legend(ncol=3)) +
  scale_y_continuous(breaks=ybreaks,
                     limits=ylimits,
                     labels=ylabels +
  facet_wrap(~Group, nrow=3) +
  ggtitle(plottitle) +
  ylab(ylabel)+
  xlab(xlabel)

# Draw plot ---------------------------------------------------------------

plot_DeGeGr

# Save plot ---------------------------------------------------------------

if(plotformat=='pdf') {
  ggsave(paste0(plotimagepath,plotname,'.',plotformat), family=fontfamily, device=cairo_pdf, height=ph, width=pw, units='cm')  
} else {
  ggsave(paste0(plotimagepath,plotname,'.',plotformat), family=fontfamily, height=ph, width=pw, units='cm')
}
dev.off()
save(plot_DeGeGr,file=paste0(plotobjpath,plotname,'.ggp'))
plot_DeGeGr
