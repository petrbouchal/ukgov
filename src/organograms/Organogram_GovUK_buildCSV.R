library(gtools)

orgfilesfinal <- govukcore[govukcore$extension=='csv',]
datetime <- '20140212_141229'
path_files <- paste0('C:/Users/bouchalp/GitHub/SCSGovUK/output/','govukpubfiles','/',datetime)

csvlog <- data.frame(filename=NA,linestatus=NA)

names.senior <- c('UniqueID','Name','Grade','JobTitle','TeamFunction','ParentDept',
              'Organisation','Unit','Phone','Email','ReportsTo','SalaryCostOfReports','FTE',
              'ActualPayFloor','ActualPayCeiling','X','Profession','Notes','ValidCheck','NA','NA')

filecounter <- 1
seniorfiles <- orgfilesfinal[orgfilesfinal$senjun=='senior',]
for(i in 1:nrow(seniorfiles)) {
  thiscsv <- read.csv(paste0(path_files,'/',seniorfiles$filename[i]))
  if(is.na(seniorfiles$filename[i])) {
    status <- 'No file to read'
    statusrow <- data.frame(filename=i,linestatus=status)
    csvlog <- rbind(csvlog,statusrow)
    next
  }
  if(length(thiscsv)>11) {
    maxlen <- ifelse(length(thiscsv)>19,19,length(thiscsv))
    thiscsv <- thiscsv[,1:maxlen]
    names(thiscsv) <- names.senior[1:length(names(thiscsv))]
    thiscsv$date <- seniorfiles$date[i] 
    thiscsv$filename <- seniorfiles$filename[i] 
    status <- 'OK'
  }
  else {
    status <- 'Different number of fields'
  }
  if(i==1){
    allcsvs <- thiscsv
  }
  else {
    allcsvs <- smartbind(allcsvs,thiscsv)
  }
  statusrow <- data.frame(filename=seniorfiles$filename[i],linestatus=status)
  csvlog <- rbind(csvlog,statusrow)
}
table(csvlog$linestatus)
table(allcsvs$date)
table(allcsvs$ParentDept)
# write.csv(allcsvs,'./data-output/organograms_ALL', row.names=F)