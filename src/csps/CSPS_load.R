# Loads Civil Service People Survey detailed dataset

library(pbtools)
library(zoo)

whmdatafolder <- 'P:/Research & Learning/Research/19. Transforming Whitehall/Whitehall Monitor/Data Sources/'

## Load data
path=paste0(whmdatafolder,'Civil Service People Survey')
path='./data-input/csps2013_demographic_results_transposed.csv'
path='http://resources.civilservice.gov.uk/wp-content/uploads/2014/02/csps2013_demographic_results.csv'
psdet <- as.data.frame(t(read.csv(path,header=F)))

psdet[,87] <- NULL # remove empty last column

## Clean up variable names
headers <- t(psdet[1,])
headers <-  str_replace_all(headers,'-:','.')
headers <-  str_trim(headers)

## Turn variable names into codebook and usable column names
codebookQ <- as.data.frame(str_split_fixed(headers,'\\. ',2))
codebookQ$newheaders <- tolower(str_replace_all(codebookQ[,1],' ','.'))
names(codebookQ) <- c('measure','questiontext','mergevar')
names(psdet) <- tolower(codebookQ$mergevar)

rownames(psdet) <- NULL
psdet <- psdet[-1,] # remove first row which contains row names
names(psdet)

qnames <- cbind()

## Split measure variable
splitmeasure <- str_split_fixed(psdet$measure,':',2)
psdet$dimension <- splitmeasure[,1]
psdet$split <- splitmeasure[,2]

psdet$dimension[psdet$dimension==''] <- NA
psdet$dimension <- na.locf(psdet$dimension)
psdet$measure <- NULL # remove original measure variable

## Reshape
psdetl <- melt(psdet,id.vars = c('dimension','split'))
psdetl$value <- as.numeric(str_replace(psdetl$value,'%',''))/100 # fix %

## Add question text from codebook
psdetl <- merge(psdetl,codebookQ,by.x = 'variable', by.y='mergevar',all.x=T)

## Clean up split variable

# Create split N variable
splitsplit <- str_split_fixed(psdetl$split,'\\(n=',n=2)
psdetl$split <- str_trim(splitsplit[,1])

split_n <- str_replace(splitsplit[,2],'\\)','')
split_n <- str_replace(split_n,',','')
split_n <- str_trim(split_n)
psdetl$split_n <- as.numeric(split_n)

# Create dimension code variable
dimsplit <- str_split_fixed(psdetl$dimension,'\\[',n=2)
psdetl$dimension <- str_trim(dimsplit[,1])

dim_id <- str_replace(dimsplit[,2],'\\]','')
psdetl$dim_id <- str_trim(dim_id)

## Mark engagement score components
psdetl$eng_score_component <- FALSE
psdetl$eng_score_component[str_detect(psdetl$variable,'\\.')] <- TRUE

## Write to CSV
write.csv(psdetl,'./data-output/CSPS_demographic_long.csv',row.names=F)

