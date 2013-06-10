require(stringr)
require(reshape)
require(reshape2)

dataname <- 'Annual Civil Service Employment Survey'
firstidline <- 'Area type: \"countries\"'

fileName <- './data-input/Nomis_ACSS_1.csv'
data <- readChar(fileName, file.info(fileName)$size)

tables <- strsplit(data, dataname)
for i in tables[[1]] {
  subtables <- strsplit(i, '\n\n')
  idlines <- subtables[[1]][1]
  idlines <- gsub('^.?\n','',idlines) # delete all chars before first newline
  idlines <- gsub('^.?\n','',idlines)
  subheaderR <- strsplit(subtables[[1]][2],',\\\"', fixed=T)
  con_dataR <- textConnection(subtables[[1]][3])
  data_table <- read.csv(con_dataR, header=F)
  names(data_table) <- subheaderR
  
  idlines <- read.csv(idlinesR)
}