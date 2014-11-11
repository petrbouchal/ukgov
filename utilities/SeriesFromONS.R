# ONS datasets can be found at
# http://www.ons.gov.uk/ons/datasets-and-tables/index.html
# (Filter out reference tables at the right of the page)

# adapted from: https://gist.github.com/jamestrimble/6442229

library(pbtools)

# This is the time-consuming part. To get the maximum use out of your data,
# create a CSV file listing CDIDs (variable codes) and some metadata.
# It's obviously better to use a csv file rather than a long string in real life!

metadata <- read.csv(
  text=paste('CDID,sector,geo,measure,seasonally_adj,unit,series.name',
             'G7AU,XX,UK,HC,xSA,thousands,Public Sector HC',
             'G7G3,XX,UK,FTE,xSA,thousands,Public Sector FTE',
             'G7K5,XX,UK,HC,xSA,thousands,Private sector',
             'G7G6,XX,UK,FTE,xSA,thousands,Civil Service FTE',
             'G7D6,XX,UK,HC,xSA,thousands,Civil Service HC',
             'K8PD,XX,UK,HC,xSA,thousands,Publicly owned fin. corps.',
             'KSL8,XX,UK,HC,xSA,thousands,Publicly owned FE & 6th form colleges',
             'MFY7,XX,UK,HC,xSA,thousands,PS HC excl. major reclassifications',
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
ons_long <- merge(ons_long, metadata,by='CDID')
ons_long$value <- as.numeric(ons_long$value)

# Create change variable if needed
toplot <- ons_long %>% group_by(CDID, series.name) %>%
  mutate(change=value/lag(value)-1) %>%
  arrange(series.name, time) %>%
  mutate(cumchange=(value-first(value))/first(value))

# Plot employment rate time series
loadcustomthemes(ifgbasecolours, 'Calibri')
plot_data <- toplot[!is.na(ons_long$value), ]
ggplot(plot_data, aes(x=time, y=value, colour=series.name, group=series.name)) +
#   facet_grid(sex ~ .,scales='free_y') +
  scale_y_continuous(limits=c(4*10E2,6.6*10E2)) +
  geom_line(size=.8)

# save data
write.csv(ons_long,paste0('./data-output/ONS_downloaded_PSEtimeseries_',
                           format(Sys.time(), format="%Y-%m-%d-%H%M%S"),'.csv'))

library(ggvis)

ons2 <- plot_data
ons2$seriesname <- ons2$series.name
ons2 %>% ungroup() %>%
  ggvis(x=~time, y=~value, stroke=~seriesname) %>%
  layer_lines() %>%
  add_tooltip(html=function(x) {paste0(names(x), ": ", format(x), collapse = "<br />")})

mtcars %>% 
  dplyr::mutate(cyl2 = factor(cyl)) %>% 
  ggvis(~wt, ~mpg, stroke = ~cyl2) %>% 
  layer_lines()
