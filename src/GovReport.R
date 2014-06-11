library(reshape2)

file <- tempfile()
url <- 'http://www.governancereport.org/fileadmin/governancereport/2014/downloads/GovReportDataset2014.RData'
download.file(url,destfile=file)
load(file)
grr <- melt(GovReportDataset2014,id.vars = 'country')
 