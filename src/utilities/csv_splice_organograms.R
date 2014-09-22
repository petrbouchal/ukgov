csvdir <- 'P:/Research & Learning/Research/19. Transforming Whitehall/Whitehall Monitor/Data Sources/Organograms/Data/HMT'
filepattern <- 'HMT-J'

csvfilelist <- list.files(csvdir,filepattern)

csvcounter <- 1
for(i in csvfilelist) {
  csvsingle <- read.csv(paste0(csvdir,'/',i), stringsAsFactors = FALSE)
  csvsingle$Month <- paste0(substr(i,1,4),'-',substr(i,5,6),'-01')
  csvsingle$X <- NULL
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

# write
write.csv(csvout, paste0(csvdir,'/', filepattern, '_', 'ALL.csv'), row.names=FALSE)
rm(csvout)
