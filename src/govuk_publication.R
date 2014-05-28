## This script loads JSON data on publicaions from gov.uk, turns it into a 
## dataset, and creates a chart of publication by time in the week

library(rjson)
library(pbtools)
library(RCurl)

LoadCustomThemes(mycols=ifgbasecolours[,1],fontfamily = 'Calibri',tints = c(0.75,0.5,0.25))

# load pages and create a long list of all results rows (list of lists)
results <- list()
temporaryFile <- tempfile()
for (i in 1:500) {
  url <- paste0('https://www.gov.uk/government/publications.json?page=',i)
  pubsfile <- download.file(url,destfile = temporaryFile, method='curl',quiet = T)
  pubs <- readLines(temporaryFile)
  pubsj <- fromJSON(pubs)
  results <- append(results, pubsj$results)
  if (i%%10==0) {print(i)}
}

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
df$hour <- sprintf('%02s',hour(df$date))
df$weekid <- paste0(week(df$date),'_',year(df$date))
df$dayhour <- paste0(df$daynum, '_', df$hour)


# dig out organisation from HTML - TODO
# df$orgname <- str_extract(df$organisations,)

# create aggregate variables and filter out what's not needed
dfs <- select(df, dayname, hour, dayhour, display_type, weekid) %>%
  mutate(count_all=n()) %>% # count of everything in the dataset
  group_by(dayhour) %>%
  mutate(count_dayhour=n(),share_dayhour=n()/count_all) %>% # rate of evrthng by hour
  ungroup() %>%  
  group_by(display_type) %>%
  mutate(counttype=n()) %>% # count by type
  group_by(dayname, hour, dayhour, display_type) %>% 
  mutate(sharedayhourtype=n()/counttype) %>% # rate by type
  summarise(share_type=mean(sharedayhourtype),count_type=n(), # means
            count_all=mean(count_all),share_all=mean(share_dayhour)) %>%
  filter(display_type=='Transparency data') %>%
  melt() %>% # reshape
#   filter(variable=='share_type' | variable=='share_all') %>%
  filter(variable=='share_type') %>%
  mutate(display_type=as.character(display_type), # reformulate labels
         display_type=ifelse(variable=='share_all','All publications',display_type))


# plot

plot <- ggplot(dfs, aes(x=hour, y=value, fill=dayname)) +
  geom_bar(stat="identity",position = 'stack') +
  scale_fill_manual(values=ifgbasecolours) +
  facet_wrap(~dayname, nrow=1)
plot

plot <- ggplot(dfs, aes(x=hour, y=value, fill=display_type, group=display_type)) +
  geom_bar(stat="identity",position='dodge') +
  scale_fill_manual(values=ifgbasecolours) +
  facet_wrap(~dayname, nrow=1) 
plot

plot <- ggplot(dfs, aes(x=dayname, y=value, fill=dayname)) +
  geom_bar(stat="identity",position = 'stack') +
  scale_fill_manual(values=ifgbasecolours)
plot

sum(dfs$value[as.numeric(dfs$hour)<15 & dfs$dayname!='Fri'])
mean(dfs$value[as.numeric(dfs$hour)<15 & dfs$dayname!='Fri'])
sum(dfs$value[as.numeric(dfs$hour)>14 & dfs$dayname=='Fri'])
mean(dfs$value[as.numeric(dfs$hour)>14 & dfs$dayname=='Fri'])

