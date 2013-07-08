library(plyr)
library(ggplot2)
library(scales)
library(grid)
library(ggthemes)
library(extrafont)
library(reshape2)

# Load data ---------------------------------------------------------------

source('./src/acses_lib.R')
ac_ch <- LoadAcsesData('ACSES_Dept_Age_Grade_Pay_data.tsv',
                       'home')

# Process data ------------------------------------------------------------

# FILTER OUT LINES
ac_ch <- ac_ch[ac_ch$Gender!='Total' & ac_ch$Civil.Service.grad=='Total',]
ac_ch <- ac_ch[ac_ch$Organisation=='Total (All Departments)',]

ac_ch <- RelabelAgebands(ac_ch)

# SUMMARISE BY GROUP & CATEGORY
ac_ch <- ddply(ac_ch, .(Date, Age.band),
               summarise, count=sum(count, na.rm=TRUE))

# CREATE TOTALS PER GROUP
totals <- ac_ch[ac_ch$Age.band!='Total',]
totals <- ddply(totals, .(Date), summarise,
                total = sum(count))

# MERGE TOTALS INTO MAIN FILE
ac_ch <- merge(ac_ch, totals)
ac_ch$share <- ac_ch$count/ac_ch$total

# Filter out unneeded things
ac_ch <- ac_ch[ac_ch$Age.band!='Total',]
ac_ch <- ac_ch[ac_ch$Age.band!='Unknown age',]

# Select years and flip one year's value into negative
ac_ch <- ac_ch[ac_ch$Date=='2012' | ac_ch$Date=='2010',]
ac_ch$share[ac_ch$Gender=='Female'] <- -ac_ch$share[ac_ch$Gender=='Female']
ac_ch$count[ac_ch$Gender=='Female'] <- -ac_ch$count[ac_ch$Gender=='Female']

# create group for area plotting
ac_ch$grp <- paste0(ac_ch$Group, ac_ch$Age.band)

# reshape to create year-on-year change figure
# ac_ch <- melt(ac_ch, id=c('Group','Date','Age.band'))
# ac_ch <- dcast(ac_ch, ... ~ variable + Date, drop=TRUE)
# ac_ch$sharediff <- (ac_ch$share_2012 - ac_ch$share_2010)

# Build plot --------------------------------------------------------------

#loadfonts()
#loadfonts(device='win')
#fonts()

plottitle='Civil Servants by age group and gender, 2010 and 2012'
ph = 6.3
pw = 9.7

ac_ch$transp <- 1
ac_ch$transp[ac_ch$Date==2012] <- 1

fontfamily = 'Calibri'
plotname <- './charts/ACSES charts/plot_AgeYr.png'

maxY <- max(abs(ac_ch$count),na.rm=TRUE)

plot_AgeYr <- ggplot(ac_ch, aes(x=Age.band, y=count,group=Gender)) +
#   geom_area(position='identity',stat='identity',
#             aes(fill=as.factor(Date),group=Date,alpha=transp,order=-Date)) +
#   geom_line(position='identity',stat='identity',aes(fill=as.factor(Date),group=Date,
#             colour=as.factor(Date)),size=1) +
  geom_step(data=ac_ch[ac_ch$Date==2010,],
           aes(colour=Gender),alpha=.4,stat='identity',position='identity') +
  geom_step(data=ac_ch[ac_ch$Date==2012,].direction='vh',
           aes(colour=Gender),direction='hv') +
#   geom_bar(position='identity', width=.9,data=ac_ch[ac_ch$Date==2010,],
#            aes(fill=Gender),stat='identity',alpha=.4) +
#   geom_bar(position='identity', width=.9,data=ac_ch[ac_ch$Date==2012,],
#            aes(fill=Gender),stat='identity') +
#   geom_point(data=ac_ch[ac_ch$Date==2010,],aes(pch='2010'),
#              colour='grey30',size=10,show_guide=T) +
#  scale_fill_manual(values=c('Female'='#d40072','Male'='#00ccff')) +
#   scale_y_continuous(limits=c(-maxY,maxY),
#                      labels=c('10%','0','10%'),
#                      breaks=c(-.1,0,.1)) +
#  scale_shape_manual(values=c('2010'='I')) +
  guides(fill=guide_legend(override.aes=list(shape=NA,size=1.2),
                           label.vjust=.5,order=2),
         shape=guide_legend(override.aes=list(size=4),
         label.vjust=.5,order=2,keyheight=.5,hjust=0)) +
  theme_few() +
  theme(text = element_text(family=fontfamily,size=10),
        axis.text = element_text(colour='grey30'),
        axis.text.x = element_text(angle = 0),
        axis.text.y= element_text(vjust=0),
        axis.title=element_text(colour='grey30'),
        axis.ticks=element_blank(),
        axis.title=element_text(),
        legend.title=element_blank(),
        legend.position=c(.85,.05),
        legend.box='horizontal',
        legend.direction='horizontal',
        legend.key.size=unit(.3,units='cm'),
        legend.text = element_text(vjust=1),
        panel.margin=unit(c(0,0,0,0),'cm'),
        panel.border=element_rect(colour='grey80'),
        plot.margin=unit(c(1,1,0,0),'cm'),
        plot.title=element_text(family=fontfamily,face='bold',size=14,
                                lineheight=2.5, vjust=2)) +
  ggtitle(plottitle) +
  ylab('Staff in age group as % of whole department') +
  xlab('Age group') +
  coord_flip()

# Save plot ---------------------------------------------------------------

ggsave(plot=plot_AgeYr, filename=plotname, family=fontfamily,heigh=ph, width=pw)
dev.off()
#embed_fonts(plotname, outfile=plotname)

# Draw plot ---------------------------------------------------------------

plot_AgeYr
