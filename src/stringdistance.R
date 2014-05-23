library(pbtools)
library(stringdist)

mpnames <- fread('./data-input/majorprojectnames.csv',na.strings = '')
cmpnames <- expand.grid(mpnames)
cmpnames$dist <- stringdist(a = cmpnames$Projects2012,b = cmpnames$Projects2013,
                            method='lcs')
cmpnames <- cmpnames[complete.cases(cmpnames),]
cmpnames <- sort_df(cmpnames,'dist')

cmpnames2012 <- group_by(cmpnames,'Projects2012') %>%
  summarise(lowestdist=min(dist))
table(cmpnames2012$lowestdist)

cmpnames2013 <- group_by(cmpnames,'Projects2013') %>%
  summarise(lowestdist=min(dist))
table(cmpnames2013$lowestdist)
  
