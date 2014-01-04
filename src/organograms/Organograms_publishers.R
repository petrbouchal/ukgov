library('RCurl')
library('RJSONIO')
library('gtools')
library('plyr')

apibase <- 'http://data.gov.uk/api/'

# download map of organisations - all publishers
pdata <- getURI(paste0(apibase,'/2/rest/group'))
jpdata <- fromJSON(pdata)
orgdatarownum <- 1
pb1 <- txtProgressBar(min=0, max = length(jpdata))
for(i in jpdata) {
  pdata <- getURI(paste0(apibase,'/2/rest/group/',i))
  jodata <- fromJSON(pdata)
  if(jodata$id=='68e4746a-7a79-4cf7-9525-b29e64b096c6') next
  if(!is.null(jodata$extras['category'])) {
    ocategory <- jodata$extras['category']
  } else {ocategory <- 'none'}
  if(!is.null(jodata$groups[1][[1]])) {
    idgroup <- jodata$groups[[1]]$id
  } else {idgroup <- NA}
  if(!is.null(jodata$extras['abbreviation'])) {
    oabbreviation <- jodata$extras['abbreviation']
  } else {oabbreviation <- 'none'}
  newrow <- data.frame(name=jodata$name,
                       id=jodata$id,
                       title=jodata$title,
                       abbreviation=oabbreviation,
                       displayname=jodata$display_name,
                       category=ocategory,
                       parentid=idgroup,row.names=NULL)
  if(orgdatarownum==1) {
    dgukorgdata <- newrow
  } else {
    dgukorgdata <- rbind(dgukorgdata,newrow)    
  }
  orgdatarownum <- orgdatarownum + 1
  setTxtProgressBar(pb1,orgdatarownum)
}

dgukorgssubset <- dgukorgdata[!is.na(dgukorgdata$parentid), ]
pb2 <- txtProgressBar(min=0, max = nrow(dgukorgssubset))

parentorgs <- dgukorgdata[,2:3]
parentorgs <- rename(parentorgs,c('id'='parentid','title'='parent.title'))
dgukorghierarchy <- merge(dgukorgdata,parentorgs)

dgukorghierarchy <- rename(parentorgs,c('parentid'='parent2id','title'='parent2.title'))
parentorgs <- rename(parentorgs,c('id'='parent2id','title'='parent1.title'))

sorgdatarownum <- 1
for(dept in 1:nrow(dgukorgssubset)) {
  
  sorgdatarownum <- sorgdatarownum + 1
  setTxtProgressBar(pb2,sorgdatarownum)
}

# save data files with organisations
save(dgukorgdata, file='dgukorgdata.Rdata')
save(dgukorghierarchy, file='dgukorghierarchy.Rdata')

# write CSV with organisations
write.csv(dgukorgdata, file='dgukorgdata.csv', row.names=FALSE)
write.csv(dgukorghierarchy, file='dgukorghierarchy.csv', row.names=FALSE)