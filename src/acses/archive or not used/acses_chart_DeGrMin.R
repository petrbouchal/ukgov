source('./src/lib/lib_acses.R')

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

# SELECT YEAR
ac_ch <- ac_ch[ac_ch$Date=='2013' | ac_ch$Date=='2010',]

# Reshape for calculation
ac_ch <- melt(ac_ch, id.vars=c('Group','Date','Ethnic.grou','Civil.Service.grad'))
ac_ch <- dcast(ac_ch, ... ~ variable + Date, drop=TRUE)
ac_ch$sharediff <- (ac_ch$share_2012 - ac_ch$share_2010)

#Sort levels
ac_ch$Civil.Service.grad = factor(ac_ch$Civil.Service.grad,
                                  levels(ac_ch$Civil.Service.grad)[c(7,1,2,4,5,3,6)])

# Filter out unneeded things
ac_ch <- ac_ch[ac_ch$Civil.Service.grad!='Not reported',]
ac_ch <- ac_ch[ac_ch$Ethnic.grou!='White',]
ac_ch$grp <- paste0(ac_ch$Group, ac_ch$Gender)

# Build plot --------------------------------------------------------------

# fix labels
ac_ch <- RelabelGrades(ac_ch)

#loadfonts()
#loadfonts(device='win')
#fonts()

plottitle='Share of minority Civil Servants in departments by grade, 2010-12'
plotname <- './charts/ACSES charts/plot_DeGrMin.pdf'
ph = 6.3
pw = 9.7

fontfamily = 'Calibri'
ac_ch$trans <- 1
ac_ch$trans[ac_ch$Civil.Service.grad=='All grades'] <- .5

maxY = max(abs(ac_ch$share_2012),na.rm=TRUE)

plot_DeGrMin <- ggplot(ac_ch,aes(Civil.Service.grad, share_2012)) +
  geom_bar(position='identity', width=.6,stat='identity',
           aes(alpha=trans,fill='2012'),guide='legend') +
  geom_segment(aes(y=share_2010, yend=share_2012, xend=Civil.Service.grad,
                   colour='Change from 2010'),
               size=1,show_guide=T,
               arrow=arrow(length=unit(.1,'cm'),type='closed')) +
  scale_colour_manual(values=c('Change from 2010'='#d40072','#00ccff')) +
  scale_fill_manual(values=c('2012'='#00ccff','#d40072'),guide='legend') +
  guides(colour=guide_legend(order=2,
                             override.aes=list(fill=NA)),
         fill=guide_legend(order=1,
                           override.aes = list(fill = '#00ccff',
                                               colour=NA))) +
  coord_flip() +
  theme_few() +
  scale_y_continuous(breaks=c(0,.25,.5),
                     limits=c(0,maxY),
                     labels=c('0','25%','50%')) +
  scale_alpha_continuous(range=c(.5,1),guide='none',breaks=c(1)) +
  theme(line=element_line(lineend='square'),
        text = element_text(family=fontfamily,size=10),
        axis.text=element_text(colour='grey30'),
        axis.text.x = element_text(angle = 0),
        axis.text.y= element_text(vjust=0),
        axis.ticks=element_blank(),
        axis.title=element_text(colour='grey30'),
        axis.title.y=element_blank(),
        legend.key = element_blank(),
        legend.title=element_blank(),
        legend.position='bottom',
        legend.direction='horizontal',
        legend.key.size=unit(.3,units='cm'),
        legend.box='horizontal',
        legend.text = element_text(vjust=1),
        panel.margin=unit(c(.1,.1,.1,.1),'cm'),
        panel.border=element_rect(colour='grey80'),
        plot.margin=unit(c(1,1,1,0),'cm'),
        plot.title=element_text(family=fontfamily,face='bold',size=14,
                                lineheight=2.5, vjust=2)) +
  facet_wrap(~Group, nrow=3) +
  ggtitle(plottitle) +
  ylab('Minority staff as proportion of those declaring ethnicity')

# Save plot ---------------------------------------------------------------

ggsave(plot=plot_DeGrMin,filename=plotname, family=fontfamily, device=cairo_pdf,heigh=ph, width=pw)
dev.off()
#embed_fonts(plotname, outfile=plotname)

# Draw plot ---------------------------------------------------------------

plot_DeGrMin