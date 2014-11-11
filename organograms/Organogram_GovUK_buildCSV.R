library(gtools)
source('./src/organograms/Organogram_processGovUK.R')
source('./src/lib/MatchOrganogramHeadings.R')

datetime <- '20140209_230453'
datetime <- '20140212_141229'
path_files <- paste0('C:/Users/bouchalp/SCSgovUK/output/','govukpubfiles','/',datetime)
path_files <- paste0('C:/Users/bouchalp/GitHub/SCSGovUK/output/','govukpubfiles','/',datetime)

csvlog <- data.frame(filename=character(),linestatus=character())

names.senior <- c('UniqueID','Name','Grade','JobTitle','TeamFunction','ParentDept',
              'Organisation','Unit','Phone','Email','ReportsTo','SalaryCostOfReports','FTE',
              'ActualPayFloor','ActualPayCeiling','X','Profession','Notes','ValidCheck','NA','NA')

filecounter <- 1
orgfilesfinal <- govukcore[govukcore$extension=='csv',]
seniorfiles <- orgfilesfinal[orgfilesfinal$senjun=='senior',]
# seniorfiles <- seniorfiles[1:30,]
for(i in 1:nrow(seniorfiles)) {
  if(is.na(seniorfiles$filename[i])) {
    status <- 'No file to read'
    statusrow <- data.frame(filename=i,linestatus=status)
    csvlog <- rbind(csvlog,statusrow)
    next
  }
#   thiscsv <- read.csv(paste0(path_files,'/',seniorfiles$filename[i]),
#                       fileEncoding="latin1")
  thiscsv <- read.csv(paste0(path_files,'/',seniorfiles$filename[i]))
  if(length(thiscsv)<2) {
    status <- 'File data invalid'
    statusrow <- data.frame(filename=i,linestatus=status)
    csvlog <- rbind(csvlog,statusrow)
    next
  }
  thiscsv2 <- MatchOrganogramHeadings(thiscsv)
  thiscsv2$date <- seniorfiles$date[i] 
  thiscsv2$filename <- seniorfiles$filename[i] 
  thiscsv2$pub_url <- seniorfiles$puburl[i] 
  thiscsv2$file_url <- seniorfiles$url[i] 
  status <- 'OK'
  if(i==1){
    allcsvs <- thiscsv2
  }
  else {
    allcsvs <- smartbind(allcsvs,thiscsv2)
  }
  statusrow <- data.frame(filename=seniorfiles$filename[i],linestatus=status)
  csvlog <- rbind(csvlog,statusrow)
}
table(csvlog$linestatus,exclude=NULL)
table(allcsvs$date,exclude=NULL)
table(allcsvs$ParentDept, exclude=NULL)
write.csv(allcsvs,'./data-output/organograms_senior_ALL.csv', row.names=F)
write.csv(seniorfiles,'./data-output/organograms_senior_filedata.csv', row.names=F)