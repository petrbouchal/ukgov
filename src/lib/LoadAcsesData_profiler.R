# Fn: load and clean ACSES data -------------------------------------------------

library(pbtools)

LoadAcsesDataFread <- function (file_name, location='home') {
  if(location=='home') {
    directory  <- '/Users/petrbouchal/Downloads/ACSES/'
  } else if(location=='ifg') {
    directory  <- 'P:/Research & Learning/Research/19. Transforming Whitehall/Whitehall Monitor/Data Sources/ONS Civil Service Statistics/Nomis ACSES/2008-13/'
  } else {
    directory <- location
  }
  fullpath <- paste0(directory, file_name)
  dataset <- fread(fullpath, sep='\t')
  dataset$value[dataset$value=='#'] <- NA
  dataset$value[dataset$value=='..'] <- NA
  dataset$Organisation <- dataset$new1
  dataset$new1 <- NULL
  dataset$count <- as.numeric(as.character(dataset$value))
  dataset <- unique(dataset) # removes duplicate lines for DfE, DfID, Ofsted and GEO
  dataset$value <- NULL
  dataset$flag <- NULL
  dataset$value.type <- NULL
  return(dataset)
}

LoadAcsesDataTable <- function (file_name, location='home') {
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
  dataset$value <- NULL
  dataset$flag <- NULL
  dataset$value.type <- NULL
  return(dataset)
}

filename <- 'ACSES_Gender_Dept_Ethn_Grade_Pay_data.tsv'

system.time(testdata <- LoadAcsesDataFread(file_name=filename,location=location))
system.time(testdata <- LoadAcsesDataTable(file_name=filename,location=location))
