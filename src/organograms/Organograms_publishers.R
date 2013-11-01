library('RCurl')
library('rjson')
library('gtools')

apibase <- 'http://data.gov.uk/api/'

# download map of organisations - all publishers
pdata <- getURI(paste0(apibase,'/2/rest/group'))
jpdata <- fromJSON(pdata)
dgukorgdata <- data.frame(name=character(),id=character(),title=character(),
                          abbreviation=character(),displayname=character(),
                          category=character(),
                          stringsAsFactors=FALSE)
for(i in jpdata) {
  pdata <- getURI(paste0(apibase,'/2/rest/group/',i))
  jodata <- fromJSON(pdata)
  if(!is.null(jodata$extras$category)) {
    ocategory <- jodata$extras$category
  } else {ocategory <- 'none'}
  newrow <- list(jodata$name, jodata$id, jodata$title, jodata$extras$abbreviation,
              jodata$display_name, ocategory)
  if(is.null(newrow[[4]])) {newrow[[4]] <- NA}
  dgukorgdata[nrow(dgukorgdata)+1,] <- newrow
}

dgukorghierarchy <- data.frame(namedept=character(),iddept=character(),
                               nameorg=character(),idorg=character(),
                          stringsAsFactors=FALSE)
for(dept in 1:nrow(dgukorgdata[dgukorgdata$category=='core-department',])) {
  sdata <- getURI(paste0(apibase,'2/rest/group/',
                         dgukorgdata$id[dgukorgdata$category=='core-department'][dept]))
  jsdata <- fromJSON(sdata)
  if(length(jsdata$groups)==0){next}
  for(orgnum in 1:length(jsdata$groups)) {
    deptid <- dgukorgdata$id[dgukorgdata$category=='core-department'][dept]
    deptname <- dgukorgdata$name[dgukorgdata$category=='core-department'][dept]
    sorgrow <- c(deptname, deptid, jsdata$groups[[orgnum]]$name, jsdata$groups[[orgnum]]$id)
    dgukorghierarchy[nrow(dgukorghierarchy)+1,] <- sorgrow    
  }
}

# write CSV with organisations