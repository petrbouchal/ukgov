library(plyr)
library(ggplot2)
library(scales)
library(grid)
library(ggthemes)
library(extrafont)

# Load data ---------------------------------------------------------------

#path  <- '/Users/petrbouchal/Downloads/ACSES/'
path  <- 'P:/Research & Learning/Research/19. Transforming Whitehall/Whitehall Monitor/Data Sources/ONS Civil Service Statistics/Nomis ACSES/'
filename <- 'ACSES_Gender_Dept_Grade_Pay_data.tsv'
fullpath <- paste0(path, filename)
acses <- read.delim(fullpath, sep='\t')
acses$value[acses$value=='#'] <- NA
acses$value[acses$value=='..'] <- NA


# LOAD DATA WITH GROUPINGS AND FILTER - MADE IN EXCEL
orgs <- read.csv('./data-input/acses_orgs.csv')

# Process data ------------------------------------------------------------

# FILTER OUT GRADE LINES
ac_ch <- acses[acses$Civil.Service.grad=='Total',]

# RENAME Org variable
ac_ch$Organisation <- ac_ch$new1
ac_ch$new1 <- NULL

# MERGE FILTER/GROUP DATA INTO MAIN DATA
ac_ch <- merge(ac_ch,orgs, all.x=TRUE)
ac_ch <- ac_ch[ac_ch$Include=='Yes',]
ac_ch <- ac_ch[ac_ch$Gender!='Total',]
ac_ch$value <- as.numeric(as.character(ac_ch$value))
ac_ch <- unique(ac_ch) # removes duplicate lines for DfE and GEO

# CREATE TOTALS PER GROUP
totals <- ac_ch[ac_ch$Wage.band=='Total',]
ac_ch <- ac_ch[ac_ch$Wage.band!='Total',]
ac_ch <- ddply(ac_ch, .(Group, Gender, Date, Wage.band),
               summarise, count=sum(value, na.rm=TRUE))

totals <- ddply(totals, .(Group, Date), summarise,
                  total=sum(value, na.rm=TRUE))

# MERGE TOTALS INTO MAIN FILE
ac_ch <- merge(ac_ch, totals)
ac_ch$share <- ac_ch$count/ac_ch$total

# Make female share negative
ac_ch$share[ac_ch$Gender=='Female'] <- -ac_ch$share[ac_ch$Gender=='Female']
ac_ch$count[ac_ch$Gender=='Female'] <- -ac_ch$count[ac_ch$Gender=='Female']

# SELECT YEAR
ac_ch <- ac_ch[ac_ch$Date=='2012',]

# FILTER OUT
#ac_ch$Civil.Service.grad = factor(ac_ch$Civil.Service.grad,
#                                  levels(ac_ch$Civil.Service.grad)[c(1,2,4,5,3,6)])
ac_ch$Wage.band = factor(ac_ch$Wage.band,
                         levels(ac_ch$Wage.band)[c(8,9,10,1:7)])
ac_ch <- ac_ch[ac_ch$Wage.band!='not reported',]
#ac_ch <- ac_ch[ac_ch$Civil.Service.grad!='Not reported',]
ac_ch$grp <- paste0(ac_ch$Group, ac_ch$Gender) 


# Build plot --------------------------------------------------------------

# fix labels

levels(ac_ch$Wage.band)[levels(ac_ch$Wage.band)=="up to Â£20,000"] <- "< 20"
levels(ac_ch$Wage.band)[levels(ac_ch$Wage.band)=="Â£20,001 - Â£30,000"] <- "20-30"
levels(ac_ch$Wage.band)[levels(ac_ch$Wage.band)=="Â£30,001 - Â£40,000"] <- "30-40"
levels(ac_ch$Wage.band)[levels(ac_ch$Wage.band)=="Â£40,001 - Â£50,000"] <- "40-50"
levels(ac_ch$Wage.band)[levels(ac_ch$Wage.band)=="Â£50,001 - Â£60,000"] <- "50-60"
levels(ac_ch$Wage.band)[levels(ac_ch$Wage.band)=="Â£60,001 - Â£70,000"] <- "60-70"
levels(ac_ch$Wage.band)[levels(ac_ch$Wage.band)=="Â£70,001 - Â£80,000"] <- "60-70"
levels(ac_ch$Wage.band)[levels(ac_ch$Wage.band)=="more than Â£80,000"] <- "> 80"

#loadfonts()
#loadfonts(device='win')
#fonts()

fontfamily = 'Calibri'
plotname <- './charts/ACSES charts/plot_DeGeGr.pdf'

plot_DeGeGr <- ggplot(ac_ch, aes(Wage.band, share)) +
  geom_bar(position='identity', width=1, aes(fill=Gender),stat='identity') +
#  geom_line(aes(group=grp, col=Gender), size=2) +
#  geom_area(aes(group=grp, fill=Gender), data=ac_ch[ac_ch$Gender=='Female',]) +
#  geom_area(aes(group=grp, fill=Gender), data=ac_ch[ac_ch$Gender=='Male',]) +
#  geom_point(aes(col=Gender), pch=21, size=2) +
#  geom_point(col='white', pch=19, size=1.5) +
  coord_flip() +
  scale_fill_manual(values=c('#d40072','#00ccff')) +
  scale_colour_manual(values=c('#d40072','#00ccff')) +
  guides(colour = guide_legend(ncol = 1)) +
  guides(col=guide_legend(ncol=3)) +
  theme_few() +
  scale_y_continuous(breaks=c(-.2,0,.2),
                     limits=c(-max(abs(ac_ch$share),na.rm=TRUE),
                              max(abs(ac_ch$share),na.rm=TRUE)),
                     labels=c('20%','0','20%')) +
#  scale_x_discrete(labels = c('AO','EO','SEO/HEO','G6/7','SCS')) +
  theme(axis.text.x = element_text(angle = 0),
        axis.text.y= element_text(vjust=0),
        legend.title=element_blank(),
        legend.position='bottom',
        legend.direction='horizontal',
        legend.key.size=unit(.4,units='cm'),
        legend.text = element_text(vjust=1),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        axis.text=element_text(colour='grey'),
        panel.margin=unit(c(.1,.1,.1,.1),'cm'),
        strip.text=element_text(face='bold'),
        panel.border=element_rect(colour='grey'),
        plot.title=element_text(family=fontfamily,face='bold',size=20,
                                lineheight=2.5, vjust=2)) +
  facet_wrap(~Group, nrow=3) +
  ggtitle('Civil Service pay in Whitehall departments by gender')


# Draw plot ---------------------------------------------------------------


plot_DeGeGr


# Save plot ---------------------------------------------------------------

ggsave(plotname, family=fontfamily, device=cairo_pdf)
#embed_fonts(plotname, outfile=plotname)