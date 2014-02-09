library(plyr)
library(reshape2)
library(stringr)

datetime = '20140209_200303'
path = '~/PycharmProjects/SCSgovUK/output/'

govukpubs <- read.csv(paste0(path,'pubpages_',datetime,'.csv'))
govukfiles <- read.csv(paste0(path,'pubfiles_',datetime,'.csv'))

govuk <- merge(govukfiles, govukpubs,all=T)
govuk$extension <- tolower(govuk$extension)

# exclude files I can't work with
govuk <- govuk[govuk$extension!='pdf',]
govuk <- govuk[govuk$extension!='rdf',]
govuk <- govuk[govuk$extension!='ods',]

# discover year
govuk$date_year <- str_extract(govuk$pubtitle, "(201[0123]{1})")
govuk$date_year[is.na(govuk$date_year)] <- str_extract(govuk$filetitle[is.na(govuk$date_year)],
                                                        "(201[0123]{1})")
govuk$date_year[is.na(govuk$date_year)] <- str_extract(govuk$url[is.na(govuk$date_year)],
                                                        "(201[0123]{1})")
table(govuk$date_year,exclude=NULL)
# View(govuk[is.na(govuk$date_year), ])
table(govuk$extension,is.na(govuk$date_year))

# discover month
govuk$month <- NA
months <- c('January','March','September','April')
pattern = "([Jj]anuary)|([Mm]arch)|([Ss]eptember)|([Aa]pril)|([Jj]une)|([Jj]uly)|([Oo]ctober)|([Dd]ecember)"

govuk$month <- str_extract(govuk$pubtitle, pattern)
govuk$month[is.na(govuk$month)] <- str_extract(govuk$filetitle[is.na(govuk$month)],
                                                       pattern)
govuk$month[is.na(govuk$month)] <- str_extract(govuk$url[is.na(govuk$month)],
                                                       pattern)
# table(govuk$month,govuk$organogram,exclude=NULL)
table(govuk$month,govuk$date_year,exclude=NULL)

# View(govuk[govuk$date_year==2013 & is.na(govuk$month),])

# discover senior or junior
govuk$senjun <- NA
govuk$senjun <- str_extract(govuk$filetitle,"([Ss][eu]|[Jj][u])nior")
govuk$senjun[is.na(govuk$senjun)] <- str_extract(govuk$url[is.na(govuk$senjun)],
                                                 "([Ss][eu]|[Jj][u])nior")
govuk$senjun <- tolower(govuk$senjun)
table(govuk$senjun)
# check for markers of real organograms
govuk$organogram <- FALSE
govuk$organogram <- str_extract(govuk$filetitle,"[Oo]rganogram")
govuk$organogram[is.na(govuk$organogram)] <- str_extract(govuk$url[is.na(govuk$organogram)],
                                                 "[Oo]rganogram")
govuk$organogram[is.na(govuk$organogram)] <- str_extract(govuk$puburl[is.na(govuk$organogram)],
                                                 "[Oo]rganogram")
govuk$organogram[is.na(govuk$organogram)] <- str_extract(govuk$pubtitle[is.na(govuk$organogram)],
                                                 "[Oo]rganogram")
govuk$organogram <- tolower(govuk$organogram)
table(govuk$organogram,govuk$senjun,exclude=NULL)

govuk$staff <- FALSE
govuk$staff <- str_extract(govuk$filetitle,"[Ss]taff")
govuk$staff[is.na(govuk$staff)] <- str_extract(govuk$url[is.na(govuk$staff)],
                                                         "[Ss]taff")
govuk$staff <- tolower(govuk$staff)
table(govuk$organogram,govuk$staff,exclude=NULL)
table(govuk$organogram,govuk$staff,govuk$senjun,exclude=NULL)
count(govuk, c('organogram','staff','senjun'))
unique(govuk$puborg)

coredepts <- read.csv('./data-input/Core_depts_GovUK.csv')
govuk <- merge(govuk, coredepts, by.x='puborg', all.x=TRUE)
govuk$coredept[govuk$coredept!=TRUE] <- FALSE
table(govuk$coredept, exclude=F)
