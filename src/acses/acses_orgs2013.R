source('./src/lib/lib_acses.R')

filename <- 'ACSES_Gender_Dept_Ethn_Grade_Pay_data.tsv'
origdata <- LoadAcsesData(filename,location)

orgsallnames <- as.data.frame(unique(origdata$Organisation))
orgsallnames$Organisation <- orgsallnames$`unique(origdata$Organisation)`
orgsallnames$`unique(origdata$Organisation)` <- NULL
orgs2013report <- read.csv('./data-input/acses_orgs_2013-report.csv')

orgs2008_2013 <- merge(orgsallnames,orgs2013report,all.x=TRUE)
write.csv(orgs2008_2013,'./data-output/orgs2008_2013.csv',row.names=FALSE)