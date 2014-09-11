# Fn: load and clean ACSES data -------------------------------------------------

LoadAcsesData <- function (file_name, location='home') {
  if(location=='home') {
    directory  <- '/Users/petrbouchal/Downloads/ACSES/'
  } else if(location=='ifg') {
    directory  <- 'P:/Research & Learning/Research/19. Transforming Whitehall/Whitehall Monitor/Data Sources/ONS Civil Service Statistics/Nomis ACSES/2008-13/'
  } else {
    directory <- location
  }
  fullpath <- paste0(directory, file_name)
  dataset <- read.delim(fullpath, sep='\t')
  dataset$value[dataset$value=='#'] <- NA
  dataset$value[dataset$value=='..'] <- NA
  dataset$Organisation <- dataset$new1
  dataset$new1 <- NULL
  dataset$count <- as.numeric(as.character(dataset$value))
  dataset <- unique(dataset) # removes duplicate lines for DfE, DfID, Ofsted and GEO
  # remove duplicate line in DfE
  dataset <- dataset[!(dataset$Organisation == 'Education, Department for' & dataset$Date == 2013),]
  dataset$value <- NULL
  dataset$flag <- NULL
  dataset$value.type <- NULL
  return(dataset)
}