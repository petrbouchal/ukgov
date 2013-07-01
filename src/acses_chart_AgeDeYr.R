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

# LOAD DATA WITH GROUPINGS AND FILTER - MADE IN EXCEL
orgs <- read.csv('./data-input/acses_orgs.csv')

# Process data ------------------------------------------------------------

# FILTER OUT GENDER LINES
ac_ch <- acses
ac_ch <- ac_ch[ac_ch$Gender=='Total',]
ac_ch <- ac_ch[ac_ch$Civil.Service.grad=='Total',]

# RENAME Org variable
ac_ch$Organisation <- ac_ch$new1
ac_ch$new1 <- NULL

# MERGE FILTER/GROUP DATA INTO MAIN DATA
ac_ch$count <- as.numeric(as.character(ac_ch$value))
ac_ch <- merge(ac_ch,orgs, all.x=TRUE)
ac_ch <- ac_ch[ac_ch$Include=='Yes',]
ac_ch <- unique(ac_ch) # removes duplicate lines for DfE and GEO

# CREATE TOTALS PER GROUP
ac_ch <- ddply(ac_ch, .(Group, Date, Age.band),
               summarise, count=sum(count, na.rm=TRUE))

totals <- ac_ch[ac_ch$Age.band=='Total',]
totals <- ddply(totals, .(Group,Date), summarise,
                total = sum(count))

# MERGE TOTALS INTO MAIN FILE
ac_ch <- merge(ac_ch, totals)
ac_ch$share <- ac_ch$count/ac_ch$total

ac_ch$Age.band <- gsub('Aged ','',ac_ch$Age.band)
ac_ch$Age.band <- gsub('and over','+',ac_ch$Age.band)

# Filter out unneeded things
ac_ch <- ac_ch[ac_ch$Age.band!='Total',]
ac_ch <- ac_ch[ac_ch$Age.band!='Unknown age',]

# Select years and flip one year's value into negative
ac_ch <- ac_ch[ac_ch$Date=='2012' | ac_ch$Date=='2010',]
#ac_ch$share[ac_ch$Date=='2010'] <- -ac_ch$share[ac_ch$Date=='2010']

# create group for area plotting
#ac_ch$grp <- paste0(ac_ch$Group, ac_ch$Civil.Service.grad)

# reshape to create year-on-year change figure
ac_ch <- melt(ac_ch, id=c('Group','Year','Age.band','grp'))
ac_ch <- dcast(ac_ch, ... ~ variable + Date, drop=TRUE)
ac_ch$sharediff <- (ac_ch$share_2012 - ac_ch$share_2010)

# Build plot --------------------------------------------------------------

#loadfonts()
#loadfonts(device='win')
#fonts()

plottitle='Civil Servants in departments by age and grade'
ph = 6.3
pw = 9.7

fontfamily = 'Calibri'
plotname <- './charts/ACSES charts/plot_AgeDeYr.png'

maxY = max(abs(ac_ch$share),na.rm=TRUE)

plot_AgeDeYr <- ggplot(ac_ch,aes(Age.band, sharediff)) +
  geom_bar(position='identity', width=1, aes(fill=as.factor(Date)),stat='identity') +
  scale_fill_manual(values=c('#d40072','#00ccff'),
                    labels=c('2010   ', '2012')) +
  guides(colour = guide_legend(ncol = 1)) +
  guides(col=guide_legend(ncol=3)) +
  theme_few() +
  scale_y_continuous(breaks=c(-.2,0,.2),
                     limits=c(-max(abs(ac_ch$share),na.rm=TRUE),
                              max(abs(ac_ch$share),na.rm=TRUE)),
                     labels=c('20%','0','20%')) +
  theme(text = element_text(family=fontfamily,size=10),
        axis.text = element_text(colour='grey'),
        axis.text.x = element_text(angle = 0),
        axis.ticks=element_blank(),
        axis.text.y= element_text(vjust=0),
        axis.title.y=element_blank(),
        axis.title.x=element_text(colour='grey'),
        legend.title=element_blank(),
        legend.position='bottom',
        legend.direction='horizontal',
        legend.key.size=unit(.3,units='cm'),
        legend.text = element_text(vjust=1),
        panel.margin=unit(c(.1,.1,.1,.1),'cm'),
        panel.border=element_rect(colour='grey'),
        strip.text=element_text(face='bold',size=12),
        plot.margin=unit(c(1,1,1,0),'cm'),
        plot.title=element_text(family=fontfamily,face='bold',size=14,
                                lineheight=2.5, vjust=2)) +
  facet_wrap(~Group, nrow=3) +
  ggtitle(plottitle) +
  ggtitle(plottitle) +
  ylab('Staff in age group as % of whole department')

# Save plot ---------------------------------------------------------------

#ggsave(plot=plot_AgeDeGr, filename=plotname, family=fontfamily,heigh=ph, width=pw, device=cairo_pdf)
ggsave(plot=plot_AgeDeYr, filename=plotname, family=fontfamily,heigh=ph, width=pw)
dev.off()
#embed_fonts(plotname, outfile=plotname)

# Draw plot ---------------------------------------------------------------

plot_AgeDeYr