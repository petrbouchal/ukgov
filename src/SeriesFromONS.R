# ONS datasets can be found at
# http://www.ons.gov.uk/ons/datasets-and-tables/index.html
# (Filter out reference tables at the right of the page)

# adapted from: https://gist.github.com/jamestrimble/6442229

library(pbtools)

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

ons_string <- readLines(uri)
ons_string <- paste(ons_string, collapse='\n')
ons_string <- sub('\n\n.*', '', ons_string) # remove copyright notice and metadata

csv <- read.csv(text=ons_string)
names(csv)[1] <- 'time'

# Just use the quarterly data
csv$time <- as.character(csv$time)
csv <- csv[nchar(csv$time)==7, ]
years <- as.numeric(substr(csv$time, 1, 4))
quarters <- as.numeric(substr(csv$time, 7, 7))

# Change quarters to fractional numbers (I'm not sure if this is the best way to use the data)
csv$time <- years + (quarters-1)/4

# Convert table to long for for use with ggplot2
ons_long <- melt(csv, id.vars='time', variable.name='CDID', value.name='value')
ons_long <- rename(ons_long,c('variable'='CDID'))
ons_long <- join(ons_long, metadata,by='CDID')
ons_long$value <- as.numeric(ons_long$value)

# Create change variable if needed
toplot <- ons_long %>% group_by(CDID, series.name) %>%
  mutate(change=value/lag(value)-1)

# Plot employment rate time series
LoadCustomThemes(ifgbasecolours, 'Calibri')
plot_data <- toplot[!is.na(ons_long$value), ]
ggplot(plot_data, aes(x=time, y=change, colour=series.name, group=series.name)) +
#   facet_grid(sex ~ .,scales='free_y') +
  geom_line(size=.8)

# save data
write.csv(ons_long,paste0('./data-output/ONS_downloaded_PSEtimeseries_',
                           format(Sys.time(), format="%Y-%m-%d-%H%M%S"),'.csv'))
