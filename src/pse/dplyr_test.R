source('./src/pse/PSE_Reshape.R')
# detach(plyr)
library(dplyr)
uu <- change
xx <- uu %.%
  filter(measure=='Cumulative_Perc_net_change') %.%
  arrange(Dept,Whitehall,Period) %.%
  group_by(Whitehall,Dept) %.%
  mutate(change=value/first(value))

write.csv(xx,'./data-output/onschange.csv',row.names=F)
