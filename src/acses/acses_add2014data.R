# This code takes 2008-13 data and adds 2014 data to it

library(pbtools)

listofcharts <- c('AgeDeGe','AgeYr','DeDisabGrYr',
                  'DeGeGr','DeGeGrYr','DeGePay','DeGrMin',
                  'DeGrMinYr','DisabGrYr','GrMinYr')

listofdatafiles <- c('ACSES_Gender_Dept_Age_Grade',
                     'ACSES_Gender_Dept_Grade_Pay',
                     'ACSES_Gender_Dept_Disab_Grade',
                     'ACSES_Gender_Dept_Ethn_Grade_Pay')

dir_old <- 'P:/Research & Learning/Research/19. Transforming Whitehall/Whitehall Monitor/Data Sources/ONS Civil Service Statistics/Nomis ACSES/2008-13/'
dir_2014 <- 'P:/Research & Learning/Research/19. Transforming Whitehall/Whitehall Monitor/Data Sources/ONS Civil Service Statistics/Nomis ACSES/2014/'
dir_new <- 'P:/Research & Learning/Research/19. Transforming Whitehall/Whitehall Monitor/Data Sources/ONS Civil Service Statistics/Nomis ACSES/2008-14/'

for(dfile in listofdatafiles){
  path_old <- paste0(dir_old,dfile,'_data.tsv')
  path_2014 <- paste0(dir_2014,dfile,'_2014.tsv')
  path_new <- paste0(dir_new,dfile,'_data.tsv')
  data_old <- read.delim(path_old)
  data_2014 <- read.delim(path_2014)
  data_new <- rbind(data_old,data_2014)
  write.table(data_new,path_new,sep='\t',row.names=F)
}