library(reshape2)
library(ggthemes)
library(scales)
library(ggplot2)
library(stringr)
BP_ts <- read.csv('./data-input/BP_ts_depts_20130510.csv.csv')
BPl <- melt(BP_ts)
BPl$monthlabel <- str_sub(BPl$variable,1,3)

plot <- ggplot(BPl, aes(x=variable, y=1-value,group=1)) +
  geom_line(colour='magenta',size=1)+
  geom_point(colour='white')+
  geom_point(colour='magenta',size=1)+
  facet_wrap(~ Dept)+
  scale_y_continuous(labels=percent,limits=c(0,1))+
  scale_x_discrete(labels=c('Nov','Dec','Jan','Feb','Mar','Apr','May')) +
  theme_economist_white()
plot