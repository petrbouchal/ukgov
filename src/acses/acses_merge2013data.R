
# This code picks up 3 files (male, female, total) and creates a single
# file and a single cleaned dataset with the 2013 data


directory <- 'P:/Research & Learning/Research/19. Transforming Whitehall/Whitehall Monitor/Data Sources/ONS Civil Service Statistics/Nomis ACSES/2013/'

filename <- 'ACSES_2013_WHNWH_Disab_Ethn_Grade_Pay_FEMALE.tsv'
fullpath <- paste0(directory, filename)
acses_male <- read.delim(fullpath)

filename <- 'ACSES_2013_WHNWH_Disab_Ethn_Grade_Pay_MALE.tsv'
fullpath <- paste0(directory, filename)
acses_female <- read.delim(fullpath)

filename <- 'ACSES_2013_WHNWH_Disab_Ethn_Grade_Pay_TOTAL.tsv'
fullpath <- paste0(directory, filename)
acses_total <- read.delim(fullpath)

acses_all <- rbind(acses_male,acses_female,acses_total)
filename <- 'ACSES_2013_WHNWH_Disab_Ethn_Gender_Grade_Pay.tsv'
fullpath <- paste0(directory, filename)
#write.table(acses_all,fullpath,sep='\t',row.names=F)

acses_all$value[acses_all$value=='#'] <- NA
acses_all$value[acses_all$value=='..'] <- NA
acses_all$Organisation <- acses_all$new1
acses_all$new1 <- NULL
acses_all$count <- as.numeric(as.character(acses_all$value))
acses_all <- unique(acses_all) # removes duplicate lines for DfE and GEO
acses_all$value <- NULL

acses_all_2013 <- acses_all
rm(acses_all)