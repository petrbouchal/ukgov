
filecounter <- 1
for(i in govuknarrow$filename) {
  if(filecounter==1){
    allcsvs <- read.csv(paste0(path,filename))
  } else {
    thiscsv <- read.csv(paste0(path,filename))
    allcsvs <- cbind(allcsvs,thiscv)
  }  
  filecounter <- filecounter+1
}
write.csv(allcsvs,'./data-output/organograms_ALL', row.names=F)