require(stringr)
require(reshape)
require(reshape2)

path  <- '/Users/petrbouchal/Downloads/ACSES/ACSES_Gender_Dept_Grade_Pay_data.tsv'

apses <- read.delim(path, sep='\t')

write.csv(unique(unique(apses$new1)),file='./data-output/acses_depts.csv')

dataname <- 'Annual Civil Service Employment Survey'
