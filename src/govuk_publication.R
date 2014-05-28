## This script loads JSON data on publicaions from gov.uk, turns it into a 
## dataset, and creates a chart of publication by time in the week

library(rjson)
library(pbtools)
library(lubridate)

LoadCustomThemes(mycols=ifgbasecolours,fontfamily = 'Calibri')

# load pages and create a long list of all results rows (list of lists)
results <- list()
for (i in 1:500) {
  url <- paste0('https://www.gov.uk/government/publications.json?page=',i)
  pubsfile <- file(url)
  pubs <- readLines(pubsfile)
  pubsj <- fromJSON(pubs)
  results <- append(results, pubsj$results)
  close(pubsfile)
  print(i)
}

str(results)

# turn the list of lists into a data frame
# first fix NULL values
json_file <- lapply(results, function(x) {
  x[sapply(x, is.null)] <- NA
  unlist(x)
})

# turn into df
df <- as.data.frame(do.call("rbind", json_file))

# fix dates & create additional time/date vars
df$date <- strptime(df$public_timestamp, format='%Y-%m-%dT%H:%M:%S')
df$dayname <- wday(df$date,label = T)
df$daynum <- wday(df$date,label = F)
df$hour <- hour(df$date)
df$weekid <- paste0(week(df$date),'_',year(df$date))
df$dayhour <- paste0(df$daynum, '_', df$hour)
df$date <- NULL

# create aggregate variables and filter out what's not needed
dfs <- group_by(df, weekid, display_type) %>%
  mutate(a=n(), aa=sum(a)) %>%
  ungroup() %>%
  group_by(weekid, hour, display_type) %>%
  mutate(b=n(),
         c=b/aa) %>%
  filter(display_type=='Transparency data')

# plot
plot <- ggplot(dfs, aes(x=hour, y=..count..)) +
  geom_bar(stat="bin",position = 'stack') +
  facet_wrap(~day, scales = 'free_x') +
  scale_colour_discrete(breaks=ifgcolours)
plot

plot <- ggplot(dfs, aes(x=dayhour, y=c, fill=dayname)) +
  geom_bar(stat="identity",position = 'stack') +
  scale_fill_manual(values=ifgcolours)
plot

