library(plyr)
library(stringr)
library(ggthemes)
change <- read.csv("./data-input/ChangeDeptsFTE.csv")
names(change)

# clean up names
names(change) <- gsub(".","_", names(change), fixed=TRUE)
names(change) <- gsub("_FTE","", names(change), fixed=TRUE)
names(change) <- gsub("Whitehall_","Whitehall", names(change), fixed=TRUE)
names(change) <- gsub("_Q","Q", names(change), fixed=TRUE)
names(change) <- gsub("_to","", names(change), fixed=TRUE)
names(change) <- gsub("_201",".201", names(change), fixed=TRUE)
names(change)

change <- change[,c(1:42,51:70,45:47,50)]
names(change)

# remove variables here

# need to adjust reshape arguments to work with this dataset

changel <- melt(change)
changel$variable <- gsub(".","-",changel$variable, fixed=TRUE) 
changel$measure <- gsub("-.*","",changel$variable)
changel$Period <- gsub(".*-","",changel$variable)
changel$Dept <- gsub(" ",".",changel$Dept)
changel$variable <- NULL
changel$group=paste(changel$Dept,changel$Whitehall,sep='-')

plotPSE <- ggplot(data=subset(changel,changel$measure=='Cumulative_perc_endog_change'),
                  aes(x=Period,y=value, group=group, colour=Whitehall)) + 
  geom_line(size=1) +
  geom_point(aes(colour=Whitehall), size=1) +
  geom_point(colour='white', size=.8) +
  facet_wrap(~Dept) +
  theme_few()
plotPSE
  