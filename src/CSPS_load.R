source('./src/lib/lib_acses.R')

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
path='P:/Research & Learning/Research/19. Transforming Whitehall/Whitehall Monitor/Data Sources/Civil Service People Survey/CSPS 2013 - Benchmark/csps2013_demographic_results_transposed.csv'
psdet <- read.csv(path)
