source('./src/lib/lib_acses.R')

# ideally load CSV here as raw text and edit before loading as data
# to minimise need for manual adjustment in Excel 

# load data
change <- read.csv("./data-input/ChangeDeptsFTE_2013Q2.csv",comment.char='#')
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

# remove and reorder variables
#change <- change[,c(1:42,51:70,45:47,50)]

# reshape
change <- melt(change)

# get rid of rows where 'variable' does not contain year
change <- change[grepl('201', change$variable),]

# create vars identifying measure and period
change$variable <- gsub(".","-",change$variable, fixed=TRUE) 
change$measure <- gsub("-.*","",change$variable)
change$Period <- gsub(".*-","",change$variable)
change$variable <- NULL

# create variable to group points into chart lines
change$group=paste(change$Dept,change$Whitehall,sep='-')

write.csv(change,'./data-output/PSE_change_long.csv', row.names=FALSE)