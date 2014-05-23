library(pbtools)
library(stringdist)
library(gdata)

mpnames <- fread('./data-input/majorprojectnames.csv',na.strings = '')
cmpnames <- expand.grid(mpnames)
cmpnames$dist <- stringdist(a = cmpnames$Projects2012,b = cmpnames$Projects2013,
                            method='lcs')
cmpnames <- cmpnames[complete.cases(cmpnames),]
cmpnames <- sort_df(cmpnames,'dist')

cmpnames2012 <- group_by(cmpnames,'Projects2012') %>%
  summarise(lowestdist=min(dist)) %>%
  do(rename.vars(.,from='Projects2012',to='projectnames'))
table(cmpnames2012$lowestdist)

cmpnames2013 <- group_by(cmpnames,'Projects2013') %>%
  summarise(lowestdist=min(dist)) %>%
  do(rename.vars(.,from='Projects2013',to='projectnames'))
  
table(cmpnames2013$lowestdist)
  
all <- merge(cmpnames2012,cmpnames2013,all=T)
all <- sort_df(all,'lowestdist')
