library(plyr)
library(ggplot2)
library(scales)
library(grid)
library(ggthemes)
library(extrafont)
library(reshape2)

# Load data ---------------------------------------------------------------

#path  <- '/Users/petrbouchal/Downloads/ACSES/'
path  <- 'P:/Research & Learning/Research/19. Transforming Whitehall/Whitehall Monitor/Data Sources/ONS Civil Service Statistics/Nomis ACSES/'
filename <- 'ACSES_Gender_Dept_Age_Grade_data.tsv'
fullpath <- paste0(path, filename)
acses <- read.delim(fullpath, sep='\t')
acses$value[acses$value=='#'] <- NA
acses$value[acses$value=='..'] <- NA

# Process data ------------------------------------------------------------

ac_ch <- acses

# RENAME Org variable
ac_ch$Organisation <- ac_ch$new1
ac_ch$new1 <- NULL

# FILTER OUT LINES
ac_ch <- ac_ch[ac_ch$Gender!='Total',]
ac_ch <- ac_ch[ac_ch$Civil.Service.grad=='Total',]
ac_ch <- ac_ch[ac_ch$Organisation=='Total (All Departments)',]

# MERGE FILTER/GROUP DATA INTO MAIN DATA
ac_ch$count <- as.numeric(as.character(ac_ch$value))
ac_ch <- merge(ac_ch,orgs, all.x=TRUE)
ac_ch <- ac_ch[ac_ch$Include=='Yes',]
ac_ch <- unique(ac_ch) # removes duplicate lines for DfE and GEO

ac_ch$Age.band <- gsub('Aged ','',ac_ch$Age.band)
ac_ch$Age.band <- gsub('and over','+',ac_ch$Age.band)

# CREATE TOTALS PER GROUP & CATEGORY
ac_ch <- ddply(ac_ch, .(Group, Date, Age.band, Gender),
               summarise, count=sum(count, na.rm=TRUE))

# CREATE TOTALS PER GROUP
totals <- ac_ch[ac_ch$Age.band=='Total',]
totals <- ddply(totals, .(Group,Date,Gender), summarise,
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

plottitle='Civil Servants by age group, 2010 and 2012'
ph = 6.3
pw = 9.7

ac_ch$transp <- 1
ac_ch$transp[ac_ch$Date==2012] <- 1

fontfamily = 'Calibri'
plotname <- './charts/ACSES charts/plot_AgeYr.png'

maxY <- max(abs(ac_ch$share),na.rm=TRUE)

plot_AgeYr <- ggplot(ac_ch, aes(x=Age.band, y=share)) +
#   geom_area(position='identity',stat='identity',
#             aes(fill=as.factor(Date),group=Date,alpha=transp,order=-Date)) +
#   geom_line(position='identity',stat='identity',aes(fill=as.factor(Date),group=Date,
#             colour=as.factor(Date)),size=1) +
  geom_bar(position='identity', width=.9,data=ac_ch[ac_ch$Date==2012,],
           aes(fill=Gender),stat='identity') +
  geom_point(data=ac_ch[ac_ch$Date==2010,],colour='grey30',pch='I',size=10,show_guide=FALSE) +
  scale_fill_manual(values=c('#d40072','#00ccff'),
                    labels=c('2010   ', '2012')) +
  scale_colour_manual(values=c('#d40072','#00ccff'),
                    labels=c('2010   ', '2012')) +
  scale_alpha(guide='none',range=c(0,1), limits=c(0,1), na.value = 0) +
  scale_size_manual(guide='none',limits=c(0,1)) +
  scale_y_continuous(limits=c(-maxY,maxY),
                     labels=c('20%','0','20%'),
                     breaks=c(-.2,0,.2)) +
  theme_few() +
  theme(text = element_text(family=fontfamily,size=10),
        axis.text = element_text(colour='grey30'),
        axis.text.x = element_text(angle = 0),
        axis.text.y= element_text(vjust=0),
        axis.title=element_text(colour='grey30'),
        axis.title.y=element_text(),
        axis.title.x=element_blank(),
        axis.ticks=element_blank(),
        legend.title=element_blank(),
        legend.position='bottom',
        legend.direction='horizontal',
        legend.key.size=unit(.3,units='cm'),
        legend.text = element_text(vjust=1),
        panel.margin=unit(c(.1,.1,.1,.1),'cm'),
        panel.border=element_rect(colour='grey80'),
        plot.margin=unit(c(1,1,0,0),'cm'),
        plot.title=element_text(family=fontfamily,face='bold',size=14,
                                lineheight=2.5, vjust=2)) +
  ggtitle(plottitle) +
  ylab('Staff in age group as % of whole department') +
  xlab('') +
  coord_flip()

# Save plot ---------------------------------------------------------------

ggsave(plot=plot_AgeYr, filename=plotname, family=fontfamily,heigh=ph, width=pw)
dev.off()
#embed_fonts(plotname, outfile=plotname)

# Draw plot ---------------------------------------------------------------

plot_AgeYr
