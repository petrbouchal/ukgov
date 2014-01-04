library('RCurl')
library('RJSONIO')
library('gtools')

apibase <- 'http://data.gov.uk/api/2/rest'
limit  <- 1000
searchterm <- 'organogram'

labels <- c('organograms-and-salaries','organogram','senior-civil-servant-pay',
            'senior-civil-Service')
query <- paste0(apibase,'/search/dataset?limit=',limit,'&q=',searchterm)
query
data <- getURI(query)
jdata <- fromJSON(data)

firstdataset <- TRUE
firstfile <- TRUE
for(i in jdata$results) {
  dquery <- paste0(apibase,'/dataset/',i)
  ddata <- fromJSON(getURI(dquery))
  # write into database of datasets here
  datarow <- ddata
  datarow$extras <- NULL
  datarow$resources <- NULL
  datarow$groups <- NULL
  datarow$relationships <- NULL
  datarow$organization <- NULL
  datarow$tracking_summary <- NULL
  datarow <- append(datarow,ddata$extras)
  datarow <- append(datarow,ddata$groups)
  datarow <- as.data.frame(lapply(datarow, FUN=unlist))
  if(firstdataset) db_datasets <- datarow else db_datasets <- 
    smartbind(db_datasets,datarow)
  num <- 1
  firstdataset <- FALSE
  for(ii in ddata$resources) {
    num <- num+1
    ii$url
    filename <- paste0(ddata$id,'_',ii$id,'.',ii$format)
    t <- try(download.file(ii$cache_url,paste0('./data-output/orgdata/',filename),mode='wb'))
    dlerror <- FALSE
    if("try-error" %in% class(t)) {
      dlerror <- TRUE
      print('downloading failed')
      filerow <- as.data.frame(t(ii))
      filerow$downloaderror <- dlerror
      filerow$filename <- filename
      if(firstfile) db_datafiles <- filerow else db_datafiles <- smartbind(db_datafiles,filerow)
      firstfile <- FALSE
      next
    }
    filerow <- as.data.frame(t(ii))
    filerow$downloaderror <- dlerror
    filerow$filename <- filename
    if(firstfile==TRUE) db_datafiles <- filerow else db_datafiles <- smartbind(db_datafiles,filerow)
    firstfile <- FALSE
  }
}

db_datafiles <- lapply(db_datafiles, FUN=unlist)

# write CSV with database of datasets and datafiles
save(db_datasets, file='db_datasets.Rdata')
save(db_datafiles, file='db_datafiles.Rdata')

write.csv(db_datasets,file=paste0('./data-output/datasets_',searchterm, '.csv'),row.names=FALSE)
write.csv(db_datafiles,file=paste0('./data-output/datafiles_',searchterm, '.csv'),row.names=FALSE)

