source('./src/lib/lib_acses.R')
library(pbtools)
library(xlsx)
# ideally load CSV here as raw text and edit before loading as data
# to minimise need for manual adjustment in Excel 

# load data
# change <- read.csv("./data-input/ChangeDeptsFTE_2013Q4.csv",comment.char='#')
onspath <- 'P:/Research & Learning/Research/19. Transforming Whitehall/Whitehall Monitor/Data Sources/ONS Public Sector Employment/Analysis/'
onsfilename <- 'ONS PSE Analysis v0.87.xlsx'
onsfilepath <- paste0(onspath,onsfilename)
change <- read.xlsx(onsfilepath,sheetName = 'Calculations',comment.char='#',startRow = 3)
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

# reshape into long
change <- melt(change,id.vars=c('Dept','Whitehall'))

# get rid of rows where 'variable' does not contain year
change <- change[grepl('201', change$variable),]

change$Dept <- gsub('Defra','DEFRA',change$Dept,fixed=TRUE)

# create vars identifying measure and period
change$variable <- gsub('.','--',change$variable,fixed=TRUE)
change$measure <- sapply(str_split(as.character(change$variable),'--'), "[", 1)
change$Period <- sapply(str_split(as.character(change$variable),'--'), "[", 2)
change$variable <- NULL

# turn variable into proper numeric
change$value <- ifelse(grepl('%',change$value),
                       as.numeric(sub("%","",change$value))/100,
                       as.numeric(change$value))

# create variable to group points into chart lines
change$group=paste(change$Dept,change$Whitehall,sep='-')

write.csv(change,'./data-output/PSE_change_long.csv', row.names=FALSE)
write.csv(change,'./src/shiny_experiments/onspse/PSE_change_long.csv', row.names=FALSE)