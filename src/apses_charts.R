library(plyr)
library(ggplot2)
library(scales)
library(grid)
library(ggthemes)
library(extrafont)

path  <- '/Users/petrbouchal/Downloads/ACSES/ACSES_Gender_Dept_Grade_Pay_data.tsv'
acses <- read.delim(path, sep='\t')
acses$value[acses$value=='#'] <- NA
acses$value[acses$value=='..'] <- NA
orgs <- read.csv('./data-input/acses_orgs.csv')

ac_ch <- acses[acses$Wage.band=='Total',]
ac_ch$Organisation <- ac_ch$new1
ac_ch$new1 <- NULL

ac_ch <- merge(ac_ch,orgs, all.x=TRUE)
ac_ch <- ac_ch[ac_ch$Include=='Yes',]
ac_ch <- ac_ch[ac_ch$Gender!='Total',]
ac_ch$value <- as.numeric(as.character(ac_ch$value))
ac_ch <- unique(ac_ch) # removes duplicate lines for DfE and GEO

dept_tot <- ac_ch[ac_ch$Civil.Service.grad=='Total',]
ac_ch <- ac_ch[ac_ch$Civil.Service.grad!='Total',]
ac_ch <- ddply(ac_ch, .(Group, Gender, Date, Civil.Service.grad),
               summarise, count=sum(value, na.rm=TRUE))

dept_tot <- ddply(dept_tot, .(Group, Date, Gender), summarise,
                  total=sum(value, na.rm=TRUE))
ac_ch <- merge(ac_ch, dept_tot)
ac_ch$share <- ac_ch$count/ac_ch$total

# Make female share negative
ac_ch$share[ac_ch$Gender=='Female'] <- -ac_ch$share[ac_ch$Gender=='Female']
ac_ch$count[ac_ch$Gender=='Female'] <- -ac_ch$count[ac_ch$Gender=='Female']

ac_ch <- ac_ch[ac_ch$Date==2012,]
ac_ch$Civil.Service.grad = factor(ac_ch$Civil.Service.grad,
                                  levels(ac_ch$Civil.Service.grad)[c(1,2,4,5,3,6)])
ac_ch <- ac_ch[ac_ch$Civil.Service.grad!='Not reported',]
ac_ch$grp <- paste0(ac_ch$Group, ac_ch$Gender) 

plot <- ggplot(ac_ch, aes(Civil.Service.grad, share)) +
  geom_bar(position='identity', width=.1, aes(fill=Gender)) +
#   geom_line(aes(group=grp, col=Gender), size=2) +
  geom_area(aes(group=grp, fill=Gender), data=ac_ch[ac_ch$Gender=='Female',]) +
  geom_area(aes(group=grp, fill=Gender), data=ac_ch[ac_ch$Gender=='Male',]) +
  geom_point(aes(col=Gender), pch=21, size=2) +
  geom_point(col='white', pch=19, size=1.5) +
  coord_flip() +
  scale_fill_manual(values=c('#d40072','#00ccff')) +
  scale_colour_manual(values=c('#d40072','#00ccff')) +
  guides(colour = guide_legend(ncol = 1)) +
  guides(col=guide_legend(ncol=3)) +
  theme_few() +
  scale_y_continuous(labels=percent) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title=element_blank(),
        legend.position='bottom',
        legend.direction='horizontal',
        axis.ticks=element_blank(),
        plot.title=element_text(family="Calibri",face='bold',size=20,
                                lineheight=2.5, vjust=1)) +
  facet_wrap(~Group, nrow=2)
plot