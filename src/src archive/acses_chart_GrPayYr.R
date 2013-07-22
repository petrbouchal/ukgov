
# Load packages -----------------------------------------------------------

library(plyr)
library(ggplot2)
library(scales)
library(grid)
library(ggthemes)
library(extrafont)

# Load data ---------------------------------------------------------------

path  <- '/Users/petrbouchal/Downloads/ACSES/'
#path  <- 'P:/Research & Learning/Research/19. Transforming Whitehall/Whitehall Monitor/Data Sources/ONS Civil Service Statistics/Nomis ACSES/'
filename <- 'ACSES_Gender_Dept_Grade_Pay_data.tsv'
fullpath <- paste0(path, filename)
acses <- read.delim(fullpath, sep='\t')
acses$value[acses$value=='#'] <- NA
acses$value[acses$value=='..'] <- NA
orgnames <- as.data.frame(unique(acses$new1))

# Process data ------------------------------------------------------------

# FILTER
org = 'Total (All Departments)'
ac_ch <- acses[acses$new1==org &
                 acses$Gender=='Total',]

# turn value into numeric 'count' variable
ac_ch$count <- as.numeric(as.character(ac_ch$value))

# MERGE FILTER/GROUP DATA INTO MAIN DATA
# ac_ch <- merge(ac_ch,orgs, all.x=TRUE)
# ac_ch <- ac_ch[ac_ch$Include=='Yes',]
# ac_ch <- unique(ac_ch) # removes duplicate lines for DfE and GEO

# CREATE TOTALS PER GROUP
totals <- ac_ch[ac_ch$Wage.band=='Total',]
ac_ch <- ac_ch[ac_ch$Wage.band!='Total',]

totals <- ddply(totals, .(Date, Civil.Service.grad), summarise,
                  total=sum(count, na.rm=TRUE))

# MERGE TOTALS INTO MAIN DATA
ac_ch <- merge(ac_ch, totals, by=c('Civil.Service.grad','Date'))
ac_ch$share <- ac_ch$count/ac_ch$total

# Make one year share negative
ac_ch$share[ac_ch$Date=='2010'] <- -ac_ch$share[ac_ch$Date=='2010']
ac_ch$count[ac_ch$Date=='2010'] <- -ac_ch$count[ac_ch$Date=='2010']

# REORDER LEVELS
ac_ch$Civil.Service.grad = factor(ac_ch$Civil.Service.grad,
                                  levels(ac_ch$Civil.Service.grad)[c(1,2,4,5,3,6,7)])
ac_ch$Wage.band = factor(ac_ch$Wage.band,
                                  levels(ac_ch$Wage.band)[c(8,9,10,1:7)])
# FILTER OUT UNNEEDED BITS
ac_ch <- ac_ch[ac_ch$Civil.Service.grad!='Not reported',]
ac_ch <- ac_ch[ac_ch$Wage.band!='not reported',]

# CREATE GROUPINGS
ac_ch$grp <- paste0(ac_ch$Civil.Service.grad, ac_ch$Date) 

# SELECT YEAR
ac_ch <- ac_ch[ac_ch$Date==2012 | ac_ch$Date==2010,]

# Build plot --------------------------------------------------------------

#loadfonts()
#loadfonts(device='win')
#fonts()

fontfamily = 'Calibri'
plotname <- './graphs/ACSES graphs/plot_GrPayYr.pdf'

pw='9.7'
ph='3.15'

# fix labels
levels(ac_ch$Civil.Service.grad)[levels(ac_ch$Civil.Service.grad)=="Administrative officers and assistants"] <- "AO"
levels(ac_ch$Civil.Service.grad)[levels(ac_ch$Civil.Service.grad)=="Executive officer"] <- "EO"
levels(ac_ch$Civil.Service.grad)[levels(ac_ch$Civil.Service.grad)=="Senior and higher executive officer"] <- "SEO/HEO"
levels(ac_ch$Civil.Service.grad)[levels(ac_ch$Civil.Service.grad)=="Senior Civil Service"] <- "SCS"
levels(ac_ch$Civil.Service.grad)[levels(ac_ch$Civil.Service.grad)=="Total"] <- "All grades"

# levels(ac_ch$Wage.band)[levels(ac_ch$Wage.band)=="up to Â£20,000"] <- "< 20"
# levels(ac_ch$Wage.band)[levels(ac_ch$Wage.band)=="Â£20,001 - Â£30,000"] <- "20-30"
# levels(ac_ch$Wage.band)[levels(ac_ch$Wage.band)=="Â£30,001 - Â£40,000"] <- "30-40"
# levels(ac_ch$Wage.band)[levels(ac_ch$Wage.band)=="Â£40,001 - Â£50,000"] <- "40-50"
# levels(ac_ch$Wage.band)[levels(ac_ch$Wage.band)=="Â£50,001 - Â£60,000"] <- "50-60"
# levels(ac_ch$Wage.band)[levels(ac_ch$Wage.band)=="Â£60,001 - Â£70,000"] <- "60-70"
# levels(ac_ch$Wage.band)[levels(ac_ch$Wage.band)=="Â£70,001 - Â£80,000"] <- "60-70"
# levels(ac_ch$Wage.band)[levels(ac_ch$Wage.band)=="more than Â£80,000"] <- "> 80"

levels(ac_ch$Wage.band)[levels(ac_ch$Wage.band)=="up to £20,000"] <- "< 20"
levels(ac_ch$Wage.band)[levels(ac_ch$Wage.band)=="£20,001 - £30,000"] <- "20-30"
levels(ac_ch$Wage.band)[levels(ac_ch$Wage.band)=="£30,001 - £40,000"] <- "30-40"
levels(ac_ch$Wage.band)[levels(ac_ch$Wage.band)=="£40,001 - £50,000"] <- "40-50"
levels(ac_ch$Wage.band)[levels(ac_ch$Wage.band)=="£50,001 - £60,000"] <- "50-60"
levels(ac_ch$Wage.band)[levels(ac_ch$Wage.band)=="£60,001 - £70,000"] <- "60-70"
levels(ac_ch$Wage.band)[levels(ac_ch$Wage.band)=="£70,001 - £80,000"] <- "60-70"
levels(ac_ch$Wage.band)[levels(ac_ch$Wage.band)=="more than £80,000"] <- "> 80"

maxY <- max(abs(ac_ch$share),na.rm=TRUE)

plot_GrPayYr <- ggplot(ac_ch, aes(Wage.band, share)) +
  geom_bar(position='identity', width=1, aes(fill=as.factor(Date)),stat='identity') +
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
  scale_y_continuous(labels=c('50%','0','50%'),
                     breaks=c(-.5,0,.5),
                     limits=c(-maxY, maxY)) +
  #scale_x_discrete(labels = c('AO','EO','SEO/HEO','G6/7','SCS')) +
  theme(text=element_text(family=fontfamily),
        axis.text=element_text(colour='grey'),
        axis.title=element_text(colour='grey'),
        axis.ticks=element_blank(),
        legend.title=element_blank(),
        legend.position='bottom',
        legend.direction='horizontal',
        legend.key.size=unit(.4,units='cm'),
        legend.text = element_text(vjust=1),
        panel.margin=unit(c(.1,.1,.1,.1),'cm'),
        strip.text=element_text(face='bold'),
        panel.border=element_rect(colour='grey'),
        plot.title=element_text(family=fontfamily,face='bold',size=20,
                                lineheight=2.5, vjust=2)) +
  facet_wrap(~Civil.Service.grad, nrow=1) +
  ggtitle('Civil Service pay by grade, 2010 and 2012') +
  xlab("Salary range, £000") +
  ylab('Staff in grade and pay range, as proportion of department')

# Draw plot ---------------------------------------------------------------

plot_GrPayYr

# Save plot ---------------------------------------------------------------

#ggsave(plotname, family=fontfamily, device=cairo_pdf, width=pw, height=ph)
#embed_fonts(plotname, outfile=plotname)