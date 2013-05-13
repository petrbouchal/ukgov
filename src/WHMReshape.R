library(plyr)
change <- read.csv("./data-input/ChangeDeptsFTE.csv")

# clean up names
names(change) <- gsub("_FTE","", names(change))
names(change) <- gsub("Whitehall.","Whitehall", names(change), fixed=TRUE)
names(change) <- gsub("._","", names(change), fixed=TRUE)
names(change) <- gsub("X","", names(change), fixed=TRUE)
names(change) <- gsub("[2010]{4}[.]{1}","2010", names(change), fixed=FALSE)
names(change) <- gsub("[2011]{4}[.]{1}","2011", names(change), fixed=FALSE)
names(change) <- gsub("[2012]{4}[.]{1}","2012", names(change), fixed=FALSE)
names(change) <- gsub("...",".", names(change), fixed=TRUE)
names(change) <- gsub("..",".", names(change), fixed=TRUE)
names(change) <- gsub("..",".", names(change), fixed=TRUE)
names(change) <- gsub(".Change","Change", names(change), fixed=TRUE)

# remove variables here

# need to adjust reshape arguments to work with this dataset
changelong <- reshape(change, idvar = c("Dept"), 
                        varying = list(c(2:15),c(16:29)),
                        v.names = c("FTE_WH", "FTE_NWH"),
                        times = c("2009Q1","2009Q2","2009Q3","2009Q4",
                                  "2010Q1","2010Q2","2010Q3","2010Q4",
                                  "2011Q1","2011Q2","2011Q3","2011Q4",
                                  "2012Q1","2012Q2","2012Q3","2012Q4"),
                        timevar = "Period",
                        direction = "long")