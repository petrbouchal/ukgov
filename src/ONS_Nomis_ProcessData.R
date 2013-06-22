require(stringr)
require(reshape)
require(reshape2)

path  <- 'path'
filename <- 'filename'

apses <- csv.read(paste0(path, filename))

write.csv(unique(unique(apses$new1)),file='./data-output/acses_depts.csv')