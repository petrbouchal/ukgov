library(plyr)
library(reshape2)
library(stringr)

datetime = '20140209_211543'
path = '~/PycharmProjects/SCSgovUK/output/'

govukpubs <- read.csv(paste0(path,'pubpages_',datetime,'.csv'))
govukfiles <- read.csv(paste0(path,'pubfiles_',datetime,'.csv'))

govuk <- merge(govukfiles, govukpubs,all=T)
govuk$extension <- tolower(govuk$extension)

# exclude files I can't work with
govuk <- govuk[govuk$extension!='pdf',]
govuk <- govuk[govuk$extension!='rdf',]
govuk <- govuk[govuk$extension!='ods',]
govuk <- govuk[govuk$extension!='ppt',]
govuk <- govuk[govuk$extension!='txt',]

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
table(govuk$senjun, exclude=NULL)
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

govuk$staff <- tolower(str_extract(govuk$filetitle,"[Ss]taff"))
govuk$staff[is.na(govuk$staff)] <- tolower(str_extract(govuk$url[is.na(govuk$staff)],
                                                         "[Ss]taff"))

govuk$workforce <- tolower(str_extract(govuk$pubtitle,"[Ww]orkforce"))
govuk$workforce[is.na(govuk$workforce)] <- str_extract(govuk$url[is.na(govuk$workforce)],
                                                 "[Ww]orkforce")
govuk$workforce[is.na(govuk$workforce)] <- str_extract(govuk$filetitle[is.na(govuk$workforce)],
                                                 "[Ww]orkforce")
govuk$annual <- tolower(str_extract(govuk$pubtitle,"[Aa]nnual"))
govuk$payments <- tolower(str_extract(govuk$pubtitle,"[Pp]ayments"))
govuk$strategy <- tolower(str_extract(govuk$pubtitle,"[Ss]trategy"))
govuk$special <- tolower(str_extract(govuk$pubtitle,"[Ss]pecial"))
govuk$recruitment <- tolower(str_extract(govuk$pubtitle,"[Rr]ecruitment"))

govuk$staff <- tolower(govuk$staff)
govuk$staff <- tolower(govuk$workforce)
table(govuk$organogram,govuk$staff,exclude=NULL)
table(govuk$organogram,govuk$staff,govuk$senjun,exclude=NULL)
# unique(govuk$puborg)

coredepts <- read.csv('./data-input/Core_depts_GovUK.csv')
govuk <- merge(govuk, coredepts, by.x='puborg', all.x=TRUE)
govuk$coredept[govuk$coredept!=TRUE] <- FALSE
table(govuk$coredept, exclude=F)

govuk$include <- TRUE
govuk$include[!is.na(govuk$annual)] <- FALSE
govuk$include[!is.na(govuk$workforce)] <- FALSE
govuk$include[!is.na(govuk$payments)] <- FALSE
govuk$include[!is.na(govuk$special)] <- FALSE
govuk$include[!is.na(govuk$strategy)] <- FALSE
govuk$include[!is.na(govuk$recruitment)] <- FALSE

govuknarrow <- govuk[govuk$include==TRUE,]

govuknarrow <- govuknarrow[govuknarrow$ext=='csv',]
govuknarrow$filename <- paste(govuknarrow$saved.as,govuknarrow$extension,sep='.')

count(govuknarrow, c('organogram','staff','senjun'))
table(govuk$month,govuk$date_year,exclude=NULL)
table(govuknarrow$month, govuknarrow$date_year,exclude=NULL)
table(govuknarrow$coredept, exclude=NULL)
View(govuknarrow[is.na(govuknarrow$senjun) & is.na(govuknarrow$staff) &
                   is.na(govuknarrow$organogram),])
