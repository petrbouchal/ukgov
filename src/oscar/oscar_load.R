library(data.table)
library(dplyr)

# Load three parts of dataset
os3 <- fread('P:/Research & Learning/Research/19. Transforming Whitehall/Whitehall Monitor/Data Sources/Financial data/OSCAR COINS/Data/OSCAR/2013-10-22 includes full past years/2013_OSCAR_Extract_2012_13_Part_3.txt',
             sep='|')
os2 <- fread('P:/Research & Learning/Research/19. Transforming Whitehall/Whitehall Monitor/Data Sources/Financial data/OSCAR COINS/Data/OSCAR/2013-10-22 includes full past years/2013_OSCAR_Extract_2012_13_Part_2.txt',
             sep='|')
os1 <- fread('P:/Research & Learning/Research/19. Transforming Whitehall/Whitehall Monitor/Data Sources/Financial data/OSCAR COINS/Data/OSCAR/2013-10-22 includes full past years/2013_OSCAR_Extract_2012_13_Part_1.txt',
             sep='|')

# Bind them together and remove originals
os <- rbind_list(os1, os2, os3)
rm(os1, os2, os3)

# Turn AMOUNT variable into numeric
os$AMOUNT <- as.numeric(os$AMOUNT)

# Check observations in each period
table(os$QUARTER_SHORT_NAME, os$MONTH_SHORT_NAME)

# Find which variables only take one value
sapply(os,function(x) length(unique(x)))

# Remoe them
os$YEAR_NO <- NULL
os$ACCOUNTING_ARRANGEMENTS_CODE <- NULL
os$ACCOUNTING_ARRANGEMENTS_LONG_NAME <- NULL
os$NA_AGGREGATE_LONG_NAME <- NULL

# os at this stage saved as 'oscar_all.Rda' in data-output
# save(os, file='./data-output/oscar_all.Rda')
# load('./data-output/oscar_all.Rda')

# Drop everything except final return
os1 <- os[os$VERSION_CODE=='R13',]
# rm(os)

# Create a list of variables to group by, based on those not to group by
notgroup <- c('AMOUNT','MONTH_SHORT_NAME','QUARTER_SHORT_NAME')
groupinglist <- names(os1)[!(names(os1) %in% notgroup)]

vars2 <- lapply(groupinglist, as.symbol)

os2 <- os1 %>%
  filter(MONTH_SHORT_NAME != 'Period 0 - 12-13') %>%
  regroup(vars2) %>%
  summarise(AMOUNT=sum(AMOUNT))

# Save data as CSV
write.csv(os1,
          'P:/Research & Learning/Research/19. Transforming Whitehall/Whitehall Monitor/Data Sources/Financial data/OSCAR COINS/Data/OSCAR/2013-10-22 includes full past years/2013_OSCAR_FINALOUTTURNBYMONTH.csv',
            row.names=FALSE)
write.csv(os2,
          'P:/Research & Learning/Research/19. Transforming Whitehall/Whitehall Monitor/Data Sources/Financial data/OSCAR COINS/Data/OSCAR/2013-10-22 includes full past years/2013_OSCAR_FINALOUTTURNYEARAGGREGATE.csv',
            row.names=FALSE)

# Save data as Rda
# save(os1, file='./data-output/oscar_outturnonly_bymonth.Rda')
# save(os2, file='./data-output/oscar_outturnonly_yearaggreg.Rda')

library(lattice)
histogram(~ AMOUNT | MONTH_SHORT_NAME, data=os1, breaks=1000000)
