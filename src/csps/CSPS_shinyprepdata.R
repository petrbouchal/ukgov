library(dplyr)

csps <- read.csv('./src/csps/cspeoplesurvey/csps.csv')
csps <- select(csps, dimension, split, value, measure, questiontext, split_n)

csps$value <- csps$value*100

gradesinorder <- c('SCS','G6/G7','SEO/HEO','EO','AA/AO')

csps$split <- tapply(gradesinorder, function(x) {relevel(csps$split, x)})

csps$split <- relevel(csps$split, 'AO/AA')
csps$split <- relevel(csps$split, 'EO')
csps$split <- relevel(csps$split, 'SEO/HEO')
csps$split <- relevel(csps$split, 'G6/G7')
csps$split <- relevel(csps$split, 'SCS')

save(csps, file='./src/csps/cspeoplesurvey/csps.rda')

