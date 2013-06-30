library(plyr)
library(ggplot2)
library(scales)
library(grid)
library(ggthemes)
library(extrafont)
library(reshape2)

# Load data ---------------------------------------------------------------

path  <- '/Users/petrbouchal/Downloads/ACSES/'
#path  <- 'P:/Research & Learning/Research/19. Transforming Whitehall/Whitehall Monitor/Data Sources/ONS Civil Service Statistics/Nomis ACSES/'
filename <- 'ACSES_Gender_Dept_Ethn_Grade_Pay_data.tsv'
fullpath <- paste0(path, filename)
acses <- read.delim(fullpath, sep='\t')
acses$value[acses$value=='#'] <- NA
acses$value[acses$value=='..'] <- NA

# LOAD DATA WITH GROUPINGS AND FILTER - MADE IN EXCEL
orgs <- read.csv('./data-input/acses_orgs.csv')

# Process data ------------------------------------------------------------

# FILTER OUT WAGE BAND LINES
ac_ch <- acses
ac_ch <- ac_ch[ac_ch$Wage.band=='Total',]
ac_ch <- ac_ch[ac_ch$Gender=='Total',]

# RENAME Org variable
ac_ch$Organisation <- ac_ch$new1
ac_ch$new1 <- NULL

# MERGE FILTER/GROUP DATA INTO MAIN DATA
ac_ch <- ac_ch[ac_ch$Ethnic.grou!='Total',]
ac_ch <- ac_ch[ac_ch$Ethnic.grou!='Not reported / Not declared',]
ac_ch$count <- as.numeric(as.character(ac_ch$value))
ac_ch <- merge(ac_ch,orgs, all.x=TRUE)
ac_ch <- ac_ch[ac_ch$Include=='Yes',]
ac_ch <- unique(ac_ch) # removes duplicate lines for DfE and GEO

# CREATE TOTALS PER GROUP
#ac_ch <- ac_ch[ac_ch$Civil.Service.grad!='Total',]
ac_ch <- ddply(ac_ch, .(Group, Ethnic.grou, Date, Civil.Service.grad),
               summarise, count=sum(count, na.rm=TRUE))

totals <- ddply(ac_ch, .(Group, Date, Civil.Service.grad), summarise,
                  total=sum(count, na.rm=TRUE))

# MERGE TOTALS INTO MAIN FILE
ac_ch <- merge(ac_ch, totals)
ac_ch$share <- ac_ch$count/ac_ch$total

# ADJUST FACTOR LABELS
levels(ac_ch$Civil.Service.grad)[levels(ac_ch$Civil.Service.grad)=="Administrative officers and assistants"] <- "AO"
levels(ac_ch$Civil.Service.grad)[levels(ac_ch$Civil.Service.grad)=="Executive officer"] <- "EO"
levels(ac_ch$Civil.Service.grad)[levels(ac_ch$Civil.Service.grad)=="Senior and higher executive officer"] <- "SEO/HEO"
levels(ac_ch$Civil.Service.grad)[levels(ac_ch$Civil.Service.grad)=="Senior Civil Service"] <- "SCS"
levels(ac_ch$Civil.Service.grad)[levels(ac_ch$Civil.Service.grad)=="Total"] <- "All grades"

# Filter out unneeded things
ac_ch$Civil.Service.grad = factor(ac_ch$Civil.Service.grad,
                                  levels(ac_ch$Civil.Service.grad)[c(7,1,2,4,5,3,6)])
ac_ch <- ac_ch[ac_ch$Civil.Service.grad!='Not reported',]
ac_ch <- ac_ch[ac_ch$Civil.Service.grad!='AO',]
ac_ch <- ac_ch[ac_ch$Civil.Service.grad!='SEO/HEO',]
ac_ch <- ac_ch[ac_ch$Civil.Service.grad!='Grades 6 & 7',]
ac_ch <- ac_ch[ac_ch$Civil.Service.grad!='EO',]
ac_ch <- ac_ch[ac_ch$Ethnic.grou!='White',]
ac_ch$grp <- paste0(ac_ch$Group, ac_ch$Civil.Service.grad)

# Build plot --------------------------------------------------------------

#loadfonts()
#loadfonts(device='win')
#fonts()

plottitle='Share of minority Civil Servants in departments 2008-12, SCS and all'
ph = 6.3
pw = 9.7

fontfamily = 'Calibri'
plotname <- './charts/ACSES charts/plot_DeGrMinYr.pdf'
ac_ch$alpha <- 1
ac_ch$minpop <- .14

maxY = max(abs(ac_ch$share),na.rm=TRUE)

plot_DeGrMinYr <- ggplot(ac_ch,aes(as.factor(Date), share, group=grp)) +
  geom_line(aes(y=minpop, group=grp, colour='UK population'), size=.5) +
  geom_line(aes(group=grp, col=Civil.Service.grad), size=1) +
#  geom_area(aes(group=grp, fill=Gender), data=ac_ch[ac_ch$Gender=='Female',]) +
#  geom_area(aes(group=grp, fill=Gender), data=ac_ch[ac_ch$Gender=='Male',]) +
#  geom_point(aes(col=Gender), pch=21, size=2) +
#  coord_flip() +
  guides(fill=guide_legend(order=1), colour=guide_legend(order=2)) +
  scale_colour_manual(values=c('All grades'='#d40072','UK population'='grey',
                               'SCS'='#00ccff')) +
  theme_few() +
  scale_y_continuous(breaks=c(0,.25,.5),
                     limits=c(0,.5),
                     labels=c('0','25%','50%')) +
  #scale_x_discrete(labels = c('All grades','AO','EO','SEO/HEO','G 6/7','SCS')) +
  theme(text=element_text(family=fontfamily),
        axis.text = element_text(colour='grey',hjust=1),
        axis.text.x= element_text(angle = 90,colour='grey',hjust=1),
        axis.text.y= element_text(angle=0, colour='grey'),
        legend.title=element_blank(),
        legend.position='bottom',
        legend.direction='horizontal',
        legend.key.size=unit(.4,units='cm'),
        legend.text = element_text(vjust=1),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        panel.margin=unit(c(.1,.1,.1,.1),'cm'),
        strip.text=element_text(face='bold', size=unit(11,'pt')),
        panel.border=element_rect(colour='grey'),
        plot.title=element_text(family=fontfamily,face='bold',size=20,
                                lineheight=2.5, vjust=2)) +
  ggtitle(plottitle) +
  facet_wrap(~Group, nrow=3)

# Draw plot ---------------------------------------------------------------

plot_DeGrMinYr

# Save plot ---------------------------------------------------------------

ggsave(plotname, family=fontfamily, device=cairo_pdf,heigh=ph, width=pw)
dev.off
#embed_fonts(plotname, outfile=plotname)