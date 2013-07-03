library(plyr)
library(ggplot2)
library(scales)
library(grid)
library(ggthemes)
library(extrafont)
library(gridExtra)

# Load data ---------------------------------------------------------------

path  <- '/Users/petrbouchal/Downloads/ACSES/'
#path  <- 'P:/Research & Learning/Research/19. Transforming Whitehall/Whitehall Monitor/Data Sources/ONS Civil Service Statistics/Nomis ACSES/'
filename <- 'ACSES_Gender_Dept_Grade_Pay_data.tsv'
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

# MERGE FILTER/GROUP DATA INTO MAIN DATA
ac_ch <- merge(ac_ch,orgs, all.x=TRUE)
ac_ch <- ac_ch[ac_ch$Include=='Yes',]
ac_ch <- ac_ch[ac_ch$Gender!='Total',]
ac_ch$value <- as.numeric(as.character(ac_ch$value))
ac_ch <- unique(ac_ch) # removes duplicate lines for DfE and GEO

# CREATE TOTALS PER GROUP
totals <- ac_ch[ac_ch$Civil.Service.grad=='Total',]
ac_ch <- ac_ch[ac_ch$Civil.Service.grad!='Total',]
ac_ch <- ddply(ac_ch, .(Group, Gender, Date, Civil.Service.grad),
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
ac_ch$Civil.Service.grad = factor(ac_ch$Civil.Service.grad,
                                  levels(ac_ch$Civil.Service.grad)[c(1,2,4,5,3,6)])
ac_ch <- ac_ch[ac_ch$Civil.Service.grad!='Not reported',]
ac_ch$grp <- paste0(ac_ch$Group, ac_ch$Gender) 


# Build plot --------------------------------------------------------------

#loadfonts()
#loadfonts(device='win')
#fonts()

levels(ac_ch$Civil.Service.grad)[levels(ac_ch$Civil.Service.grad)=="Administrative officers and assistants"] <- "AO"
levels(ac_ch$Civil.Service.grad)[levels(ac_ch$Civil.Service.grad)=="Executive officer"] <- "EO"
levels(ac_ch$Civil.Service.grad)[levels(ac_ch$Civil.Service.grad)=="Senior and higher executive officer"] <- "SEO/HEO"
levels(ac_ch$Civil.Service.grad)[levels(ac_ch$Civil.Service.grad)=="Senior Civil Service"] <- "SCS"
levels(ac_ch$Civil.Service.grad)[levels(ac_ch$Civil.Service.grad)=="Total"] <- "All grades"

plottitle <- 'Civil Servants in Whitehall departments by grade and gender'
ph=6.3
pw=9.7

fontfamily = 'Calibri'
plotname <- './charts/ACSES charts/plot_DeGeGr2.pdf'
plot_DeGeGr <- ggplot(ac_ch, aes(Civil.Service.grad, share)) +
  geom_bar(position='identity', width=1, aes(fill=Gender),stat='identity') +
#  geom_area(aes(group=grp, fill=Gender), data=ac_ch[ac_ch$Gender=='Female',]) +
  coord_flip() +
  scale_fill_manual(values=c('#d40072','#00ccff'),
                    labels=c('Female   ', 'Male')) +
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
  #annotate("text", label = "Breached", x = 0, y = 0) +
  facet_wrap(~Group, nrow=3) +
  ggtitle(plottitle) +
  ylab('Staff in grade, as proportion of all staff in department')

# Save plot ---------------------------------------------------------------

ggsave(plot = plot_DeGeGr, filename=plotname, family=fontfamily, device=cairo_pdf, height=ph, width=pw)
#embed_fonts(plotname, outfile=plotname)
dev.off()

# Draw plot ---------------------------------------------------------------

plot_DeGeGr
