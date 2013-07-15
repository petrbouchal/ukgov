library(plyr)
library(stringr)
library(ggplot2)
library(grid)
library(ggthemes)
library(reshape)
library(reshape2)

# ideally load CSV here as raw text and edit before loading as data
# to minimise need for manual adjustment in Excel 

# load data
change <- read.csv("./data-input/ChangeDeptsFTE.csv",)
names(change)

# clean up names
names(change) <- gsub(".","_", names(change), fixed=TRUE)
names(change) <- gsub("_FTE","", names(change), fixed=TRUE)
names(change) <- gsub("Whitehall_","Whitehall", names(change), fixed=TRUE)
names(change) <- gsub("_Q","Q", names(change), fixed=TRUE)
names(change) <- gsub("_to","", names(change), fixed=TRUE)
names(change) <- gsub("_201",".201", names(change), fixed=TRUE)
names(change) <- gsub("X__","Perc_", names(change), fixed=TRUE)
names(change) <- gsub("___","_Perc_", names(change), fixed=TRUE)
names(change)

# remove and reorder variables
#change <- change[,c(1:42,51:70,45:47,50)]
names(change)

# reshape
changel <- melt(change)

# get rid of rows where 'variable' does not contain year
changel <- changel[grepl('201', changel$variable),]

# create vars identifying measure and period
changel$variable <- gsub(".","-",changel$variable, fixed=TRUE) 
changel$measure <- gsub("-.*","",changel$variable)
changel$Period <- gsub(".*-","",changel$variable)
changel$variable <- NULL

# create variable to group points into chart lines
changel$group=paste(changel$Dept,changel$Whitehall,sep='-')

write.csv(changel,'./data-output/PSE_change_long.csv', row.names=FALSE)