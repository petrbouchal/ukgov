# Setup -------------------------------------------------------------------
source('./src/lib/lib_acses.R')

listofdatafiles <- c('ACSES_Gender_Dept_Age_Grade',
                     'ACSES_Gender_Dept_Grade_Pay',
                     'ACSES_Gender_Dept_Disab_Grade',
                     'ACSES_Gender_Dept_Ethn_Grade_Pay')

dir_in <- 'P:/Research & Learning/Research/19. Transforming Whitehall/Whitehall Monitor/Data Sources/ONS Civil Service Statistics/Nomis ACSES/2008-13/'
dir_out <- 'P:/Research & Learning/Research/19. Transforming Whitehall/Whitehall Monitor/Data Sources/ONS Civil Service Statistics/Nomis ACSES/2008-13 with IfG classifications/'

for(dfile in listofdatafiles){
  path_in <- paste0(dir_in,dfile,'_data.tsv')
  path_out <- paste0(dir_out,dfile,'_data.tsv')
  path_outcsv <- paste0(dir_out,dfile,'_data.csv')
  data_in <- read.delim(path_in)
  data_in$Organisation <- data_in$new1
  data_in$new1 <- NULL
  data_out <- AddOrgData(data_in,whitehallonly=F)
  write.table(data_out,path_out,sep='\t',row.names=F)
  write.csv(data_out,path_outcsv,row.names=F)
}

rm(data_in)
# rm(data_out)
