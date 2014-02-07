require(reshape2)
require(stringr)
require(plyr)

praw <- read.csv('./data-input/Professions.csv')
plong <- melt(praw,id.vars=c(1:6))
plong$variable <- str_replace_all(plong$variable,fixed('.'),' ')
plong$value <- str_replace_all(plong$value,fixed('#N/A'),NA)
plong$value <- str_replace_all(plong$value,fixed('..'),NA)
plong$value <- as.numeric(plong$value)
plong <- rename(plong,c("variable" = "Profession"))

profgroups <- read.csv('./data-input/profgroups.csv')
plong <- merge(plong,profgroups)

write.csv(plong,'./data-output/Professions_long.csv',row.names=FALSE)
