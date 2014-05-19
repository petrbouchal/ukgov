# Loads Civil Service People Survey detailed dataset

# source('./src/lib/lib_acses.R')
library(pbtools)
library(zoo)

whmdatafolder <- 'P:/Research & Learning/Research/19. Transforming Whitehall/Whitehall Monitor/Data Sources/'

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

## Load data
path=paste0(whmdatafolder,'Civil Service People Survey')
path='./data-input/csps2013_demographic_results_transposed.csv'
path='http://resources.civilservice.gov.uk/wp-content/uploads/2014/02/csps2013_demographic_results.csv'
psdet <- as.data.frame(t(read.csv(path,header=F)))

psdet[,87] <- NULL # remove empty last column

## Clean up variable names
headers <- t(psdet[1,])
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
headers <-  str_replace_all(headers,' $','')

## Turn variable names into codebook and usable column names
codebookQ <- str_split_fixed(headers,'\\. ',2)
# headers <-  str_replace_all(headers,'[^ ]*','.')
newheaders <- str_replace_all(codebookQ[,1],' ','.')
names(psdet) <- tolower(newheaders)
rownames(psdet) <- NULL
psdet <- psdet[-1,] # remove first row which contains row names
names(psdet)

## Split measure variable
psdet$dimension <- str_split_fixed(psdet$measure,':',2)[,1]
psdet$split <- str_split_fixed(psdet$measure,':',2)[,2]

psdet$dimension[psdet$dimension==''] <- NA
psdet$dimension <- na.locf(psdet$dimension)
psdet$measure <- NULL # remove original measure variable

## Reshape
psdetl <- melt(psdet,id.vars = c('dimension','split'))
psdetl$value <- as.numeric(str_replace(psdetl$value,'%',''))/100 # fix %

## Add question text from codebook
codebookdf <- as.data.frame(codebookQ)
codebookdf$V1 <- tolower(codebookdf$V1)
psdetl <- merge(psdetl,codebookdf,by.x = 'variable', by.y='V1')
rename(psdetl$V2,replace = 'questiontext')
