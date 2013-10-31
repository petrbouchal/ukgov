# This code takes 2012-8 data and adds 2013 data to it

source('./src/lib/lib_acses.R')

listofcharts <- c('AgeDeGe','AgeYr','DeDisabGrYr',
                  'DeGeGr','DeGeGrYr','DeGePay','DeGrMin',
                  'DeGrMinYr','DisabGrYr','GrMinYr')

listofdatafiles <- c('ACSES_Gender_Dept_Age_Grade',
                     'ACSES_Gender_Dept_Grade_Pay',
                     'ACSES_Gender_Dept_Disab_Grade',
                     'ACSES_Gender_Dept_Ethn_Grade_Pay')

dir_old <- 'P:/Research & Learning/Research/19. Transforming Whitehall/Whitehall Monitor/Data Sources/ONS Civil Service Statistics/Nomis ACSES/2008-12/'
dir_2013 <- 'P:/Research & Learning/Research/19. Transforming Whitehall/Whitehall Monitor/Data Sources/ONS Civil Service Statistics/Nomis ACSES/2013/'
dir_new <- 'P:/Research & Learning/Research/19. Transforming Whitehall/Whitehall Monitor/Data Sources/ONS Civil Service Statistics/Nomis ACSES/2008-13/'

for(dfile in listofdatafiles){
  path_old <- paste0(dir_old,dfile,'_data.tsv')
  path_2013 <- paste0(dir_2013,dfile,'_2013.tsv')
  path_new <- paste0(dir_new,dfile,'_data.tsv')
  data_old <- read.delim(path_old)
  data_2013 <- read.delim(path_2013)
  data_new <- rbind(data_old,data_2013)
  write.table(data_new,path_new,sep='\t',row.names=F)
}