source('./src/lib/lib_acses.R')

# Load data ---------------------------------------------------------------

filename <- 'ACSES_Gender_Dept_Grade_Pay_data.tsv'
origdata <- LoadAcsesData(filename,location)

# Process data ------------------------------------------------------------
uu <- origdata

# FILTER
uu <- uu[uu$Organisation=='Total (All Departments)' & uu$Gender!='Total',]

# CREATE TOTALS PER GROUP
totals <- uu[uu$Wage.band=='Total',]
uu <- uu[uu$Wage.band!='Total',]

totals <- ddply(totals, .(Date, Civil.Service.grad,Gender), summarise,
                  total=sum(count, na.rm=TRUE))

# MERGE TOTALS INTO MAIN DATA
uu <- merge(uu, totals, by=c('Civil.Service.grad','Date','Gender'))
uu$share <- uu$count/uu$total

# Make female share negative
uu$share[uu$Gender=='Female'] <- -uu$share[uu$Gender=='Female']
uu$count[uu$Gender=='Female'] <- -uu$count[uu$Gender=='Female']

# FILTER OUT UNNEEDED BITS
uu <- uu[uu$Civil.Service.grad!='Not reported',]
uu <- uu[uu$Wage.band!='not reported',]

# SELECT YEAR
uu <- uu[uu$Date==2012,]

uu <- RelabelGrades(uu)
uu <- RelabelPaybands(uu)

# Build plot --------------------------------------------------------------
uu$grp <- paste0(uu$Civil.Service.grad, uu$Gender) 

plotname <- './charts/ACSES charts/plot_GeGrPay.pdf'

plottitle <- 'Civil Service pay by grade and gender'
xlabel="Salary range, £000"
ylabel='Staff in grade, as proportion of all staff in department'
pw=24.5
ph=15.3

maxY <- max(abs(uu$share),na.rm=TRUE)
ylimits <- c(-maxY,maxY)
ybreaks <- c(-maxY,maxY)
ylabels <- paste0(abs(ybreaks*100),'%')

uu$yvar <- uu$share

plot_GeGrPay <- ggplot(uu, aes(Wage.band, yvar)) +
  geom_bar(position='identity', width=1, aes(fill=Gender),stat='identity') +
  coord_flip() +
  scale_fill_manual(values=c(IfGcols[2,1],IfGcols[5,1]),
                    labels=c('Female   ', 'Male')) +
  guides(colour = guide_legend(ncol = 1)) +
  guides(col=guide_legend(ncol=3)) +
  scale_y_continuous(labels=c('20%','0','20%'),
                     limits=c(-maxY,
                              maxY),
                     breaks=c(-.2,0,.2)) +
  facet_wrap(~Civil.Service.grad, nrow=1) +
  labs(title=plottitle,x=xlabel,y=ylabel) +
  theme(panel.border=element_rect(fill=NA,colour=IfGcols[1,3]))
plot_GeGrPay

# Save plot ---------------------------------------------------------------

SavePlot()