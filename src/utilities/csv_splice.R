csvdir <- 'P:/Research & Learning/Research/19. Transforming Whitehall/Whitehall Monitor/Data Sources/Workforce Management Information/Original data/DH/new'
filepattern <- '^20'

csvfilelist <- list.files(csvdir,filepattern)

csvcounter <- 1
for(i in csvfilelist) {
  csvsingle <- read.csv(paste0(csvdir,'/',i), stringsAsFactors = FALSE)
  csvsingle$Month <- paste0(substr(i,1,4),'-',substr(i,5,6),'-01')
  csvsingle$Shortform <- ''
  csvsingle$IfGclass <- ''
  csvsingle$X.31 <- NULL # hack for DfE only
  if(csvcounter==1) {
    csvout <- csvsingle
  } else {
    if(length(names(csvsingle)) == length(names(csvout))) {
      names(csvsingle) <- names(csvout)
    } else {
      stop('Different number of columns')
    }
    csvout <- rbind(csvout, csvsingle)
  }
  csvcounter = csvcounter + 1
}

# reorder rows to match master dataset
csvout <- csvout[,c(42:44,1:(length(csvout)-3))]

# remove lines with blanks in 4th column (org name)
csvout <- csvout[csvout[4]!='',]

# write
write.csv(csvout, paste0(csvdir,'/ALL.csv'), row.names=FALSE)
rm(csvout)
