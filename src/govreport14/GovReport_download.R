library(pbtools)

file <- tempfile()
urldata <- 'http://www.governancereport.org/fileadmin/governancereport/2014/downloads/GovReportDataset2014.csv'
urlindex <- 'http://www.governancereport.org/fileadmin/governancereport/2014/downloads/GovRep2014Indexes.csv'
download.file(urldata,destfile='./data-input/GovReport2014_data.csv')
download.file(urlindex,destfile='./data-input/GovReport2014_index.csv')
GovReportIndex2014 <- read.csv('./data-input/GovReport2014_index.csv')
GovReportData2014 <- read.csv('./data-input/GovReport2014_data.csv')
