# ONS datasets can be found at
# http://www.ons.gov.uk/ons/datasets-and-tables/index.html
# (Filter out reference tables at the right of the page)

# from: https://gist.github.com/jamestrimble/6442229

library(reshape2)
library(plyr)
library(ggplot2)

# This is the time-consuming part. To get the maximum use out of your data,
# create a CSV file listing CDIDs (variable codes) and some metadata.
# It's obviously better to use a csv file rather than a long string in real life!

metadata <- read.csv(
  text=paste('CDID,geo,sex,age,variable,seasonally_adj',
             'LF3Y,England,All,16-64,Employment Rate,SA',
             'LF4D,England,Female,16-64,Employment Rate,SA',
             'YCLL,England,Male,16-64,Employment Rate,SA',
             'LF5Z,Northern Ireland,All,16-64,Employment Rate,SA',
             'LF62,Northern Ireland,Female,16-64,Employment Rate,SA',
             'ZSFJ,Northern Ireland,Male,16-64,Employment Rate,SA',
             'LF42,Scotland,All,16-64,Employment Rate,SA',
             'LF4F,Scotland,Female,16-64,Employment Rate,SA',
             'YCLN,Scotland,Male,16-64,Employment Rate,SA',
             'LF24,UK,All,16-64,Employment Rate,SA',
             'LF25,UK,Female,16-64,Employment Rate,SA',
             'MGSV,UK,Male,16-64,Employment Rate,SA',
             'LF3Z,Wales,All,16-64,Employment Rate,SA',
             'LF4E,Wales,Female,16-64,Employment Rate,SA',
             'YCLM,Wales,Male,16-64,Employment Rate,SA',
             sep='\n'))

dataset <- 'lms'

uri <- 'http://www.ons.gov.uk/ons/datasets-and-tables/downloads/csv.csv'
uri <- paste(uri,
             '?dataset=',
             dataset,
             '&cdid=',
             paste(metadata$CDID, collapse='%2C'),
             sep='')

data_string <- readLines(uri)
data_string <- paste(data_string, collapse='\n')
data_string <- sub('\n\n.*', '', data_string) # remove copyright notice and metadata

csv <- read.csv(text=data_string)
names(csv)[1] <- 'time'

# Just use the quarterly data
csv$time <- as.character(csv$time)
csv <- csv[nchar(csv$time)==7, ]
years <- as.numeric(substr(csv$time, 1, 4))
quarters <- as.numeric(substr(csv$time, 7, 7))

# Change quarters to fractional numbers (I'm not sure if this is the best way to use the data)
csv$time <- years + (quarters-1)/4

# Convert table to long for for use with ggplot2
data_long <- melt(csv, id.vars='time', variable.name='CDID', value.name='value')
data_long <- join(data_long, metadata)
data_long$value <- as.numeric(data_long$value)

# plot employment rate time series by country (colour) and sex (panel)
plot_data <- data_long[!is.na(data_long$value), ]
ggplot(plot_data, aes(x=time, y=value, colour=geo, group=geo)) +
  facet_grid(sex ~ .) +
  geom_line()