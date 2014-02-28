source('./src/lib/lib_acses.R')
library(stringr)

read.tcsv = function(file, header=TRUE, sep=",", ...) {
  # from http://stackoverflow.com/questions/17288197/reading-a-csv-file-organized-horizontally
  n = max(count.fields(file, sep=","), na.rm=TRUE)
  x = readLines(file)
  
  .splitvar = function(x, sep, n) {
    var = unlist(strsplit(x, split=sep))
    length(var) = n
    return(var)
  }
  
  x = do.call(cbind, lapply(x, .splitvar, sep=sep, n=n))
  x = apply(x, 1, paste, collapse=sep) 
  out = read.csv(text=x, sep=sep, header=header, ...)
  return(out)
  
}

path=paste0(whmdatafolder,'Civil Service People Survey')
path='./data-input/csps2013_demographic_results_transposed.csv'
path='http://resources.civilservice.gov.uk/wp-content/uploads/2014/02/csps2013_demographic_results.csv'
psdet <- as.data.frame(t(read.csv(path,header=F)))

headers <- psdet[1,]
headers <-  str_replace_all(headers,'\\[','')
headers <-  str_replace_all(headers,'\\]','')
headers <-  str_replace_all(headers,'\\(','')
headers <-  str_replace_all(headers,'\\)','')
headers <-  str_replace_all(headers,'\\=','xx')
headers <-  str_replace_all(headers,'\\,','yy')
headers <-  str_replace_all(headers,'/','')
headers <-  str_replace_all(headers,' : ','')
headers <-  str_replace_all(headers,'\\:','')
headers <-  str_replace_all(headers,'-:','.')
# headers <-  str_replace_all(headers,'[^ ]*','.')
headers
names(psdet) <- headers
str(psdet)
