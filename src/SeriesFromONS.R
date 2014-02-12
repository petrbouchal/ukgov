# ONS datasets can be found at
# http://www.ons.gov.uk/ons/datasets-and-tables/index.html
# (Filter out reference tables at the right of the page)

# adapted from: https://gist.github.com/jamestrimble/6442229

library(reshape2)
library(plyr)
library(ggplot2)

# This is the time-consuming part. To get the maximum use out of your data,
# create a CSV file listing CDIDs (variable codes) and some metadata.
# It's obviously better to use a csv file rather than a long string in real life!

metadata <- read.csv(
  text=paste('CDID,sector,geo,variable,seasonally_adj,unit,series.name',
             'G7AU,XX,UK,HC,xSA,thousands,Public Sector',
             'G7K5,XX,UK,HC,xSA,thousands,Private sector',
             'G7D6,XX,UK,HC,xSA,thousands,Civil Service',
             'K8PD,XX,UK,HC,xSA,thousands,Publicly owned fin. corps.',
             'KSL8,XX,UK,HC,xSA,thousands,Publicly owned FE & 6th form colleges',
             sep='\n'))

dataset <- 'pse'

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
data_long <- rename(data_long,c('variable'='CDID'))
data_long <- join(data_long, metadata,by='CDID')
data_long$value <- as.numeric(data_long$value)

# plot employment rate time series by country (colour) and sex (panel)
plot_data <- data_long[!is.na(data_long$value), ]
ggplot(plot_data, aes(x=time, y=value, colour=series.name, group=series.name)) +
#   facet_grid(sex ~ .,scales='free_y') +
  geom_line(size=.8)

write.csv(data_long,paste0('./data-output/ONS_downloaded_PSEtimeseries_',
                           format(Sys.time(), format="%Y-%m-%d-%H%M")))
