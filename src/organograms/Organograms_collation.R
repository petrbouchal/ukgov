# This script loads

source('./src/lib/load_packages.R')

load('./data-output/datafiles.Rdata')
load('./data-output/datasets.Rdata')

db_datafiles <- as.data.frame(db_datafiles)
db_datasets <- as.data.frame(db_datasets)

for(i in names(db_datasets)) {
  db_datasets[i] <- lapply(db_datasets[i], FUN="[",1)
  #db_datasets[i] <- unlist(db_datasets[i])
}

db_datafiles <- db_datafiles[db_datafiles$downloaderror==FALSE,]
db_datafiles <- db_datafiles[db_datafiles$format=="CSV",]
db_datafiles$datetxt <- db_datafiles$date

db_datafiles$mergeid <- unlist(db_datafiles$package_id)

db_datasets$mergeid <- unlist(db_datasets$id)
uu <- merge(db_datafiles, db_datasets, by='mergeid')

uu$description <- unlist(uu$description)
uu$cache_url <- unlist(uu$cache_url)
uu$date.x <- unlist(uu$date.x)
uu$date.y <- unlist(uu$date.y)
uu$temporalcoverageto <- uu$`temporal_coverage-to`
uu$temporalcoveragefrom <- uu$`temporal_coverage-from`
uu$temporalcoverageto <- unlist(uu$temporalcoverageto)
uu$temporalcoverageto <- str_trim(uu$temporalcoverageto)
uu$temporalcoverageto[uu$temporalcoverageto==''] <- NA
uu$datetxt <- unlist(uu$datetxt)
uu$notes <- unlist(uu$notes)
uu$name.y <- unlist(uu$name.y)

uu$datetxt[is.na(uu$datetxt) & !is.na(uu$temporalcoverageto)] <- 
  uu$temporalcoverageto[is.na(uu$datetxt) & !is.na(uu$temporalcoverageto)]

uu$dateindesc <- str_extract(uu$description, "[3][013]/[0]*[369]{1}/201[0123]{1}")
uu$dateinnotes <- str_extract(uu$notes, "[3][013]/[0]*[369]{1}/201[0123]{1}")
uu$dateinfilename <- str_extract(uu$cache_url, "[3][013]-[0]*[369]{1}-(20)*1[0123]{1}")
uu$monthindesc <- str_extract(uu$description, "(([Ss]ep[t|tember]*)|([Mm]ar[ch]*)|([Aa]pr[il]*)|([Jj]un[e]*)|(SEP|MAR|APR|JUN)) (201[0123]{1})")
uu$monthinnotes <- str_extract(uu$notes, "(([Ss]ep[t|tember]*)|([Mm]ar[ch]*)|([Aa]pr[il]*)|([Jj]un[e]*)|(SEP|MAR|APR|JUN)) (201[0123]{1})")
uu$monthinfilename <- str_extract(uu$cache_url, "([Ss]ep[t|tember]*|[Mm]ar[ch]*|[Aa]pr[il]*|[Jj]un[e]*|SEP|MAR|APR|JUN)[-|_]*(201[0123]{1})")
uu$dateinnamey <- str_extract(uu$name.y, "[3][013]/[0]*[369]{1}/201[0123]{1}")
uu$monthinnamey <- str_extract(uu$name.y, "([Ss]ep[t|tember]*|[Mm]ar[ch]*|[Aa]pr[il]*|[Jj]un[e]*|SEP|MAR|APR|JUN)[-|_| ]*(201[0123]{1})")

uu$datetxt[is.na(uu$datetxt) & !is.na(uu$dateindesc)]  <- 
  uu$dateindesc[is.na(uu$datetxt) & !is.na(uu$dateindesc)]
uu$datetxt[is.na(uu$datetxt) & !is.na(uu$monthindesc)]  <- 
  uu$monthindesc[is.na(uu$datetxt) & !is.na(uu$monthindesc)]
uu$datetxt[is.na(uu$datetxt) & !is.na(uu$monthinfilename)]  <- 
  uu$monthinfilename[is.na(uu$datetxt) & !is.na(uu$monthinfilename)]
uu$datetxt[is.na(uu$datetxt) & !is.na(uu$dateinnotes)]  <- 
  uu$dateinnotes[is.na(uu$datetxt) & !is.na(uu$dateinnotes)]
uu$datetxt[is.na(uu$datetxt) & !is.na(uu$dateinnotes)]  <- 
  uu$monthinnotes[is.na(uu$datetxt) & !is.na(uu$dateinnotes)]
uu$datetxt[is.na(uu$datetxt) & !is.na(uu$dateinfilename)]  <- 
  uu$dateinfilename[is.na(uu$datetxt) & !is.na(uu$dateinfilename)]
uu$datetxt[is.na(uu$datetxt) & !is.na(uu$dateinnamey)]  <- 
  uu$dateinnamey[is.na(uu$datetxt) & !is.na(uu$dateinnamey)]
uu$datetxt[is.na(uu$datetxt) & !is.na(uu$monthinnamey)]  <- 
  uu$monthinnamey[is.na(uu$datetxt) & !is.na(uu$monthinnamey)]
uu$datetxt[is.na(uu$datetxt) & !is.na(uu$date.x)]  <- 
  uu$date.x[is.na(uu$datetxt) & !is.na(uu$date.x)]
uu$datetxt[is.na(uu$datetxt) & !is.na(uu$date.y) & is.na(uu$date.x)]  <- 
  uu$date.y[is.na(uu$datetxt) & !is.na(uu$date.y) & is.na(uu$date.x)]

table(is.na(uu$datetxt))
table(uu$datetxt)

uu$date_m <- NA
uu$date_y <- NA
uu$date_m <- ifelse(str_detect(uu$datetxt, "([Ff]eb|02)"),'02',uu$date_m)
uu$date_m <- ifelse(str_detect(uu$datetxt, "([Nn]ov|/11/|-11-)"),'11',uu$date_m)
uu$date_m <- ifelse(str_detect(uu$datetxt, "([Ss]ep|09|/9/|-9-)"),'09',uu$date_m)
uu$date_m <- ifelse(str_detect(uu$datetxt, "([Mm]ar|03|/3/|-3-)"),'03',uu$date_m)
uu$date_m <- ifelse(str_detect(uu$datetxt, "([Aa]ug|08)"),'08',uu$date_m)
uu$date_m <- ifelse(str_detect(uu$datetxt, "([Aa]pr|04)"),'04',uu$date_m)
uu$date_m <- ifelse(str_detect(uu$datetxt, "([Jj]un|06)"),'06',uu$date_m)
uu$date_m <- ifelse(str_detect(uu$datetxt, "([Oo]ct|/10/|-10-)"),'10',uu$date_m)
uu$date_y <- str_extract(uu$datetxt, "(201[0123]{1})")

table(uu$date_m, uu$datetxt)
table(uu$date_y, uu$datetxt)
uu$datetxt[is.na(uu$date_m) & !is.na(uu$datetxt)]

#source('./src/organograms/Organograms_publishers.R')

uu$groups <- lapply(uu$groups, FUN="[", 1)
uu$groups <- unlist(uu$groups)

table(is.na(uu$date_m), is.na(uu$datetxt))

uu$datefinaltxt <- paste0('30/',uu$date_m,'/',uu$date_y)
uu$datefinal <- strptime(paste0('30/',uu$date_m,'/',uu$date_y),'%d/%m/%Y')


uu <- merge(uu, dgukorghierarchy, by.x='groups', by.y='nameorg')
table(uu$datefinal, uu$namedept)
