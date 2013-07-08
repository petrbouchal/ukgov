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
ac_ch <- acses

# RENAME Org variable
ac_ch$Organisation <- ac_ch$new1
ac_ch$new1 <- NULL

# FILTER OUT WAGE BAND LINES
ac_ch <- ac_ch[ac_ch$Wage.band=='Total',]
ac_ch <- ac_ch[ac_ch$Gender=='Total',]
ac_ch <- ac_ch[ac_ch$Organisation=='Total (All Departments)',]


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

totals <- ddply(ac_ch, .(Date, Civil.Service.grad), summarise,
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
ac_ch <- RelabelGrades(ac_ch)

# Filter out unneeded things
ac_ch$Civil.Service.grad = factor(ac_ch$Civil.Service.grad,
                                  levels(ac_ch$Civil.Service.grad)[c(7,1,2,4,5,3,6)])
ac_ch <- ac_ch[ac_ch$Civil.Service.grad!='Not reported',]
ac_ch <- ac_ch[ac_ch$Civil.Service.grad!='AO',]
ac_ch <- ac_ch[ac_ch$Civil.Service.grad!='SEO/HEO',]
ac_ch <- ac_ch[ac_ch$Civil.Service.grad!='Grades 6 & 7',]
ac_ch <- ac_ch[ac_ch$Civil.Service.grad!='EO',]
ac_ch <- ac_ch[ac_ch$Ethnic.grou!='White',]

# Build plot --------------------------------------------------------------

#loadfonts()
#loadfonts(device='win')
#fonts()

plottitle='Minority Civil Servants 2008-12, SCS and whole CS'
ph = 6.3
pw = 9.7

fontfamily = 'Calibri'
plotname <- './charts/ACSES charts/plot_GrMinYr.pdf'
ac_ch$alpha <- 1
ac_ch$minpop <- .14

maxY = max(abs(ac_ch$share),na.rm=TRUE)

plot_GrMinYr <- ggplot(ac_ch,aes(as.factor(Date), share)) +
  geom_line(aes(colour=Civil.Service.grad,group=Civil.Service.grad),
           size=1, stat='identity') +
  geom_line(aes(y=minpop,group=Civil.Service.grad,linetype='UK Population (2011)'),
            colour='grey',show_guide=T,
            stat='identity', size=1) +
  scale_colour_manual(values=c('All grades'='#d40072',
                               'SCS'='#00ccff')) +
  scale_fill_manual(values=c('All grades'='#d40072',
                               'SCS'='#00ccff')) +
  scale_linetype_manual(values=c('UK Population (2011)'='dotted')) +
  guides(fill=guide_legend(order=1), colour=guide_legend(order=2),
         linetype=guide_legend(override.aes=list(linetype='dotted'),
                               keywidth=unit(1,'cm'))) +
  theme_few() +
  scale_y_continuous(breaks=c(0,.1,.2),
                     limits=c(0,.2),
                     labels=c('0','10%','20%')) +
  theme(line=element_line(lineend='square'),
        text = element_text(family=fontfamily,size=10),
        axis.text=element_text(colour='grey30'),
        axis.text.x = element_text(angle = 0),
        axis.text.y = element_text(vjust=0),
        axis.ticks=element_blank(),
        axis.title=element_text(colour='grey30'),
        axis.title.x=element_blank(),
        legend.title=element_blank(),
        legend.justification=c(1,0),
        legend.position=c(1,0),
        legend.direction='horizontal',
        legend.box='vertical',
        legend.key.size=unit(.3,units='cm'),
        legend.text = element_text(vjust=1),
        panel.margin=unit(c(.1,.1,.1,.1),'cm'),
        panel.border=element_rect(colour='grey80'),
        plot.margin=unit(c(1,1,0,0),'cm'),
        strip.text=element_text(face='bold',size=12),
        plot.title=element_text(family=fontfamily,face='bold',size=14,
                                lineheight=2.5, vjust=2)) +
  ggtitle(plottitle) +
  ylab('Minority staff as proportion of those declaring ethnicity')

# Save plot ---------------------------------------------------------------

ggsave(plot=plot_GrMinYr, filename=plotname, family=fontfamily, device=cairo_pdf,heigh=ph, width=pw)
dev.off()
#embed_fonts(plotname, outfile=plotname)

# Draw plot ---------------------------------------------------------------

plot_GrMinYr