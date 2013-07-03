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
ac_ch <- ac_ch[ac_ch$Organisation=='Total (All Departments)',]
ac_ch <- ac_ch[ac_ch$Wage.band=='Total',]

# MERGE FILTER/GROUP DATA INTO MAIN DATA
ac_ch <- ac_ch[ac_ch$Gender!='Total',]
ac_ch$value <- as.numeric(as.character(ac_ch$value))
ac_ch <- unique(ac_ch) # removes duplicate lines for DfE and GEO

# CREATE TOTALS PER GROUP
totals <- ac_ch[ac_ch$Civil.Service.grad=='Total',]
ac_ch <- ac_ch[ac_ch$Civil.Service.grad!='Total',]
ac_ch <- ddply(ac_ch, .(Gender, Date, Civil.Service.grad),
               summarise, count=sum(value, na.rm=TRUE))

totals <- ddply(totals, .(Date), summarise,
                total=sum(value, na.rm=TRUE))

# MERGE TOTALS INTO MAIN FILE
ac_ch <- merge(ac_ch, totals)
ac_ch$share <- ac_ch$count/ac_ch$total

# Make female share negative
ac_ch$share[ac_ch$Gender=='Female'] <- -ac_ch$share[ac_ch$Gender=='Female']
ac_ch$count[ac_ch$Gender=='Female'] <- -ac_ch$count[ac_ch$Gender=='Female']

# SELECT YEAR
ac_ch <- ac_ch[ac_ch$Date=='2012' | ac_ch$Date=='2010',]
ac_ch$Civil.Service.grad = factor(ac_ch$Civil.Service.grad,
                                  levels(ac_ch$Civil.Service.grad)[c(1,2,4,5,3,6)])
ac_ch <- ac_ch[ac_ch$Civil.Service.grad!='Not reported',]

# Build plot --------------------------------------------------------------

#loadfonts()
#loadfonts(device='win')
#fonts()

levels(ac_ch$Civil.Service.grad)[levels(ac_ch$Civil.Service.grad)=="Administrative officers and assistants"] <- "AO"
levels(ac_ch$Civil.Service.grad)[levels(ac_ch$Civil.Service.grad)=="Executive officer"] <- "EO"
levels(ac_ch$Civil.Service.grad)[levels(ac_ch$Civil.Service.grad)=="Senior and higher executive officer"] <- "SEO/HEO"
levels(ac_ch$Civil.Service.grad)[levels(ac_ch$Civil.Service.grad)=="Senior Civil Service"] <- "SCS"
levels(ac_ch$Civil.Service.grad)[levels(ac_ch$Civil.Service.grad)=="Total"] <- "All grades"

plotname <- './charts/ACSES charts/plot_GeGrYr.pdf'
plottitle <- 'Civil Servants by grade and gender'
ph=6.3
pw=9.7

maxY = max(abs(ac_ch$count),na.rm=TRUE)

fontfamily = 'Calibri'
plot_GeGrYr <- ggplot(ac_ch, aes(x=Civil.Service.grad, y=count)) +
  #   geom_area(position='identity',stat='identity',
  #             aes(fill=as.factor(Date),group=Date,alpha=transp,order=-Date)) +
  #   geom_line(position='identity',stat='identity',aes(fill=as.factor(Date),group=Date,
  #             colour=as.factor(Date)),size=1) +
  geom_bar(position='identity', width=.9,data=ac_ch[ac_ch$Date==2010,],
           aes(fill=Gender),stat='identity',alpha=.4) +
  geom_bar(position='identity', width=.9,data=ac_ch[ac_ch$Date==2012,],
           aes(fill=Gender),stat='identity') +
  geom_point(data=ac_ch[ac_ch$Date==2010,],aes(pch='2010'),
             colour='grey30',size=10,show_guide=T) +
  scale_fill_manual(values=c('Female'='#d40072','Male'='#00ccff')) +
  scale_y_continuous(limits=c(-maxY,maxY),
                     labels=c('20%','10%','0','10%','20%'),
                     breaks=c(-.2,-.1,0,.1,.2)) +
  scale_shape_manual(values=c('2010'='I')) +
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
        legend.position=c(.85,.85),
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
  xlab('') +
  coord_flip()

# Save plot ---------------------------------------------------------------

ggsave(plot = plot_GeGrYr, filename=plotname, family=fontfamily, device=cairo_pdf, height=ph, width=pw)
#embed_fonts(plotname, outfile=plotname)
dev.off()

# Draw plot ---------------------------------------------------------------

plot_GeGrYr
