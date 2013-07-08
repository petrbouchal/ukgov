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

# RENAME Org variable
ac_ch$Organisation <- ac_ch$new1
ac_ch$new1 <- NULL

# MERGE FILTER/GROUP DATA INTO MAIN DATA
ac_ch <- ac_ch[ac_ch$Civil.Service.grad!='Not reported',]
ac_ch$count <- as.numeric(as.character(ac_ch$value))
ac_ch <- merge(ac_ch,orgs, all.x=TRUE)
ac_ch <- ac_ch[ac_ch$Include=='Yes',]
ac_ch <- unique(ac_ch) # removes duplicate lines for DfE and GEO

# CREATE TOTALS PER GROUP
#ac_ch <- ac_ch[ac_ch$Civil.Service.grad!='Total',]
ac_ch <- ddply(ac_ch, .(Group, Date, Civil.Service.grad, Age.band),
               summarise, count=sum(count, na.rm=TRUE))

totals <- ac_ch[ac_ch$Age.band=='Total',]
totals <- ddply(totals, .(Group,Date,Civil.Service.grad), summarise,
                total = sum(count))

# MERGE TOTALS INTO MAIN FILE
ac_ch <- merge(ac_ch, totals)
ac_ch$share <- ac_ch$count/ac_ch$total

# ADJUST FACTOR LABELS & REORDER
levels(ac_ch$Civil.Service.grad)[levels(ac_ch$Civil.Service.grad)=="Administrative officers and assistants"] <- "AO"
levels(ac_ch$Civil.Service.grad)[levels(ac_ch$Civil.Service.grad)=="Executive officer"] <- "EO"
levels(ac_ch$Civil.Service.grad)[levels(ac_ch$Civil.Service.grad)=="Senior and higher executive officer"] <- "SEO/HEO"
levels(ac_ch$Civil.Service.grad)[levels(ac_ch$Civil.Service.grad)=="Senior Civil Service"] <- "SCS"
levels(ac_ch$Civil.Service.grad)[levels(ac_ch$Civil.Service.grad)=="Total"] <- "All grades"

# Filter out unneeded things
ac_ch <- ac_ch[ac_ch$Age.band!='Total' & ac_ch$Civil.Service.grad!='All grades',]
ac_ch <- ac_ch[ac_ch$Age.band!='Unknown age',]
ac_ch <- ac_ch[ac_ch$Date=='2012',]

# create group for area plotting
ac_ch$grp <- paste0(ac_ch$Group, ac_ch$Civil.Service.grad)

# Build plot --------------------------------------------------------------

#loadfonts()
#loadfonts(device='win')
#fonts()

plottitle='Civil Servants in departments by age and grade'
ph = 6.3
pw = 9.7

fontfamily = 'Calibri'
plotname <- './charts/ACSES charts/plot_AgeDeGr.png'

maxY = max(abs(ac_ch$share),na.rm=TRUE)

plot_AgeDeGr <- ggplot(ac_ch,aes(Age.band, Civil.Service.grad)) +
  geom_tile(aes(fill=share)) +
  scale_fill_gradient(low='white',high='#d40072',limits=c(0,.6),
                      name="Staff as % of all in grade",
                      breaks=c(0,.3,.6),
                      labels=c('0%','30%','60%')) +
 theme_few() +
  theme(line=element_line(lineend='square'),
        text = element_text(family=fontfamily,size=10),
        axis.text=element_text(colour='#495462'),
        axis.text.x = element_text(angle = 90,vjust=0),
        axis.text.y = element_text(vjust=0),
        axis.ticks=element_blank(),
        axis.title=element_text(colour='#495462'),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position=c(0.5,-.15),
        legend.direction='horizontal',
        legend.key.size=unit(.3,units='cm'),
        panel.margin=unit(c(.1,.1,.1,.1),'cm'),
        panel.border=element_rect(colour='#495462'),
        plot.margin=unit(c(1,1,1,0),'cm'),
        strip.text=element_text(face='bold',size=12),
        plot.title=element_text(family=fontfamily,face='bold',size=14,
                                lineheight=2.5, vjust=2)) +
  facet_wrap(~Group, nrow=3) +
  ggtitle(plottitle) +
  ylab('Staff in each age group as proportion of grade')

# Save plot ---------------------------------------------------------------

#ggsave(plot=plot_AgeDeGr, filename=plotname, family=fontfamily,heigh=ph, width=pw, device=cairo_pdf)
ggsave(plot=plot_AgeDeGr, filename=plotname, family=fontfamily,heigh=ph, width=pw)
dev.off()
#embed_fonts(plotname, outfile=plotname)

# Draw plot ---------------------------------------------------------------

plot_AgeDeGr