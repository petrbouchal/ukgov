renamevars <- function(dataset,renamelist) {
  renamelist <- as.data.frame(renamelist,stringsAsFactors=FALSE)
  for (i in names(dataset)) {
    if(i %in% renamelist[,1]) {
      names(dataset)[names(dataset)==i] <- renamelist[renamelist[1]==i,2]
    }
  }
  return(dataset)
}

qlist <- list(c('month','blah'),c('wasmonth','notblah'))
qdf <- as.data.frame(qlist,stringsAsFactors=F)
wmi2 <- renamevars(wmi, qdf)
