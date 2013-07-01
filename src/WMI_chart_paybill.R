library(plyr)
library(ggplot2)
library(scales)
library(grid)
library(ggthemes)
library(extrafont)
library(reshape2)

wmi <- read.csv('./data-input/WHI_PaybillHeadcountChange.csv')
wmi <- melt(wmi,id=c('Department','Quarter'))
wmi$grp <- paste0(wmi$Department)
wmi <- wmi[wmi$variable!='Paybill.without.outliers' & wmi$variable!='Headcount.without.outliers',]
#wmi <- wmi[wmi$variable!='Paybill' & wmi$variable!='Headcount',]
wmi$value <- as.numeric(wmi$value)
wmi$value[wmi$Quarter=='2011 Q4'] <- 0

plottitle='Change in paybill and staff numbers'

wmiplot <- ggplot(wmi,aes(Quarter, value))+
  geom_line(aes(group=variable,colour=variable),size=1)+
  scale_colour_discrete(labels=c('Paybill','Headcount'))+
  scale_y_continuous(labels=percent)+
  facet_wrap(~Department,nrow=4) +
  theme_few() +
  theme(line=element_line(lineend='square'),
        text = element_text(family=fontfamily,size=10),
        axis.text=element_text(colour='dark grey'),
        axis.text.x = element_text(angle = 90,vjust=0),
        axis.text.y = element_text(vjust=0),
        axis.ticks=element_blank(),
        axis.title=element_text(colour='dark grey'),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.title=element_blank(),
        legend.position=c(0.5,-.15),
        legend.direction='horizontal',
        legend.key.size=unit(.3,units='cm'),
        panel.margin=unit(c(.1,.1,.1,.1),'cm'),
        panel.border=element_rect(colour='dark grey'),
        plot.margin=unit(c(1,1,1,0),'cm'),
        strip.text=element_text(face='bold',size=12),
        plot.title=element_text(family=fontfamily,face='bold',size=14,
                                lineheight=2.5, vjust=2)) +
  ggtitle(plottitle) +
  ylab('Staff in each age group as proportion of grade')
wmiplot