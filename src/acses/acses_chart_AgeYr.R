source('./src/lib/lib_acses.R')
origdata <- LoadAcsesData('ACSES_Gender_Dept_Age_Grade_data.tsv',location)

# Process data ------------------------------------------------------------
uu <- origdata

# FILTER OUT LINES
uu <- uu[uu$Gender!='Total' & uu$Civil.Service.grad=='Total',]
uu <- uu[uu$Organisation=='Total (All Departments)',]

uu <- RelabelAgebands(uu)
uu <- ddply(uu, .(Organisation,Date,Age.band,Gender))

# SUMMARISE BY GROUP & CATEGORY
uu <- ddply(uu, .(Date, Age.band, Gender),
            summarise, count=sum(count, na.rm=TRUE))

# CREATE TOTALS PER GROUP
totals <- uu[uu$Age.band!='Total',]
totals <- ddply(totals, .(Date), summarise, total = sum(count))

# MERGE TOTALS INTO MAIN FILE
uu <- merge(uu, totals)
uu$share <- uu$count/uu$total

# Filter out unneeded things
uu <- uu[uu$Age.band!='Total',]
uu <- uu[uu$Age.band!='Unknown age',]

# Select years and flip one year's value into negative
uu <- uu[uu$Date=='2013' | uu$Date=='2010',]

# reshape to create year-on-year change figure
uu2 <- ddply(uu, .(Age.band,Date),summarise,total=sum(total),count=sum(count))
uu2$total <- uu2$total/2
uu2 <- melt(uu2, id=c('Date','Age.band'))
uu2 <- dcast(uu2, ... ~ variable + Date, drop=TRUE,fun.aggregate=sum)
uu2$sharediff <- (uu2$count_2013/uu2$total_2013 - uu2$count_2010/uu2$total_2010)

uu2 <- uu2[,c('Age.band','sharediff')]
uu <- merge(uu,uu2,by='Age.band')

rm(uu2)

uu$share[uu$Gender=='Female'] <- -uu$share[uu$Gender=='Female']
uu$count[uu$Gender=='Female'] <- -uu$count[uu$Gender=='Female']

# Build plot --------------------------------------------------------------

plotname <- 'plot_AgeYr'
plottitle <- 'Civil Servants by gender and age'
ylabel <- 'Civil Servants in age group as % of Civil Service'
xlabel <- 'Age group (years)'
pw=15.3*1/1.8
ph=21.0/3

uu$yvar <- uu$share
sharedifflabels <- paste0(ifelse(uu$sharediff<0,'','+'),
                             signif(uu$sharediff*100,2),'%')[c(1,5,9,13,17,21)]
shdl <- data.frame('shlab'=sharedifflabels,'cat'=unique(uu$Age.band),
                   'labypos'=.2)

maxY <- max(abs(uu$yvar),na.rm=TRUE)
ylimits <- c(-.2, .2)
ybreaks <- c(-.2,-.15,-.1,-.05,0,.05,.1,.15,.2)
ylabels <- paste0(abs(ybreaks*100),'%')

uu$grp <- paste0(uu$Gender,' ',uu$Date)

plot_AgeYr <- ggplot(data=uu) +
  geom_bar(position='identity', width=.9,data=uu[uu$Date==2013,],
           aes(x=Age.band, y=yvar,fill=Gender,colour=Gender),stat='identity') +
  geom_bar(position='identity', width=.9,data=uu[uu$Date==2010,],
           aes(x=Age.band,y=yvar,fill=NA,colour=as.factor(Date)),
           stat='identity',linetype='dashed') +
  scale_fill_manual(values=c('Female'=IfGcols[2,1],'Male'=IfGcols[5,1]),
                    labels=c('Female','Male')) +
  labs(x=xlabel,y=ylabel) +
  scale_colour_manual(values=c('2010'=IfGcols[1,1],
                               'Male'=IfGcols[5,1],'Female'=IfGcols[2,1]),
                      labels=c('Female','Male')) +
  scale_y_continuous(limits=ylimits,labels=ylabels,breaks=ybreaks) +
  guides(fill=guide_legend('2013', override.aes=list(colour=NA,size=1.2),
                           label.vjust=.5,order=2,nrow=1),
         colour=guide_legend('2010',override.aes=list(fill=NA,colour='black',
                                                      linetype='dashed'))) +
  coord_flip() +
  theme(axis.line=element_line(colour=IfGcols[1,2]),
        legend.direction='horizontal',
        legend.box='horizontal',
        legend.position='bottom',
        legend.key.width=unit(.3,'cm'),
        legend.title=element_text(face='bold',vjust=.5),
        legend.key.width=unit(.5,'cm'),axis.ticks=element_line(colour=IfGcols[1,2]),
        axis.ticks.y=element_blank(),panel.grid=element_blank())
plot_AgeYr

# Save plot ---------------------------------------------------------------

SavePlot(plotformat=plotformat,plotname=plotname,ffamily=fontfamily,ploth=ph,plotw=pw)