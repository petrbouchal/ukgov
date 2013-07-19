setwd(dir="~/github/local/R test project/")
rm(earners2012)
earners2012 <- read.csv("/Users/petrbouchal/Downloads/High Earners/150k-Master-2012-20-9-12-CSV.csv", header=FALSE, sep=",", quote="\"", fileEncoding="UTF-8")
names(earners2012) <- earners2012[,1]
earners2012 = earners2012[-1,]