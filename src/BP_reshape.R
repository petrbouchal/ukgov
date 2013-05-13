library(reshape2)
library(ggthemes)
library(scales)
library(ggplot2)
library(stringr)
BP_ts <- read.csv('./data-input/BP_ts_depts.csv')
BPl <- melt(BP_ts)
BPl$variable <- str_sub(BPl$variable,1,3)

plot <- ggplot(BPl, aes(x=variable, y=1-value,group=1)) +
  geom_line(colour='magenta',size=1)+
  geom_point(colour='white',size=2)+
  facet_wrap(~ Dept)+
  scale_y_continuous(labels=percent)+
  theme_economist_white()
plot