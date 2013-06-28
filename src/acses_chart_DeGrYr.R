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

# FILTER OUT WAGE BAND LINES
ac_ch <- acses[acses$Gender=='Total',]
ac_ch <- ac_ch[ac_ch$Wage.band=='Total',]

# RENAME Org variable
ac_ch$Organisation <- ac_ch$new1
ac_ch$new1 <- NULL

# MERGE FILTER/GROUP DATA INTO MAIN DATA
ac_ch <- merge(ac_ch,orgs, all.x=TRUE)
ac_ch <- ac_ch[ac_ch$Include=='Yes',]
#ac_ch <- ac_ch[ac_ch$Gender!='Total',]
ac_ch$value <- as.numeric(as.character(ac_ch$value))
ac_ch <- unique(ac_ch) # removes duplicate lines for DfE and GEO

# CREATE TOTALS PER GROUP
dept_tot <- ac_ch[ac_ch$Civil.Service.grad=='Total',]
ac_ch <- ac_ch[ac_ch$Civil.Service.grad!='Total',]
ac_ch <- ddply(ac_ch, .(Group, Date, Civil.Service.grad),
               summarise, count=sum(value, na.rm=TRUE))

dept_tot <- ddply(dept_tot, .(Group, Date), summarise,
                  total=sum(value, na.rm=TRUE))

# MERGE TOTALS INTO MAIN FILE
ac_ch <- merge(ac_ch, dept_tot)
ac_ch$share <- ac_ch$count/ac_ch$total

# Make one year
ac_ch$share[ac_ch$Date==2008] <- -ac_ch$share[ac_ch$Date==2008]
ac_ch$count[ac_ch$Gender==2008] <- -ac_ch$count[ac_ch$Date==2008]

# SELECT YEAR
ac_ch <- ac_ch[ac_ch$Date=='2012' | ac_ch$Date=='2008',]
ac_ch$Civil.Service.grad = factor(ac_ch$Civil.Service.grad,
                                  levels(ac_ch$Civil.Service.grad)[c(1,2,4,5,3,6)])
ac_ch <- ac_ch[ac_ch$Civil.Service.grad!='Not reported',]
ac_ch$grp <- paste0(ac_ch$Group, ac_ch$Date) 


# Build plot --------------------------------------------------------------


#loadfonts()
#loadfonts(device='win')
#fonts()

fontfamily = 'Calibri'
plotname <- './graphs/ACSES graphs/plot_DeGrYr.pdf'

plot_DeGrYr <- ggplot(ac_ch, aes(Civil.Service.grad, share)) +
  geom_bar(position='identity', width=1, aes(fill=as.factor(Date)),stat='identity') +
#  geom_line(aes(group=grp, col=Gender), size=2) +
#  geom_area(aes(group=grp, fill=as.factor(Date)), data=ac_ch[ac_ch$Date=='2008',]) +
#  geom_area(aes(group=grp, fill=as.factor(Date)), data=ac_ch[ac_ch$Date=='2012',]) +
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
  scale_x_discrete(labels = c('AO','EO','SEO/HEO','G6/7','SCS')) +
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
  ggtitle('Civil Servants in Whitehall departments by grade: 2010 and 2012')


# Draw plot ---------------------------------------------------------------


plot_DeGrYr


# Save plot ---------------------------------------------------------------

#ggsave(plotname, family=fontfamily, device=cairo_pdf)
#embed_fonts(plotname, outfile=plotname)


