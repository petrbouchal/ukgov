vac <- read.csv('http://api.scraperwiki.com/api/1.0/datastore/sqlite?format=csv&name=civil_service_job_adverts&query=select+*+from+`swdata`&apikey=')

require(reshape2)
require(reshape)
require(stringr)
require(plyr)
require(ggplot2)
require(ggthemes)

# export departments to CSV
write.csv(unique(vac$Department),file='./data-output/jobs_depts.csv')

# turn date/time string into proper datetime
vac$jobtimeaccessed <- as.POSIXct(vac$jobtimeaccessed)

# create time series set: each job exists or does not exist at each time point
vac$isjob <- TRUE

dates <- unique(vac$jobtimeaccessed)
jobids <- unique(vac$jobid)

# create dataset with every combination of dates and job IDs
grid <- expand.grid(dates, jobids)
names(grid) <- c('jobtimeaccessed','jobid')
# check at what dates each job existed
#FIXME: THESE NEED BETTER COLUMN REFERENCES AS NUMBER OF COLUMNS IN INPUT DATA CHANGES!!
jobatdate <- merge(grid, vac[,c(11,32,89)], all.x=TRUE)
jobatdate$isjob[is.na(jobatdate$isjob)] <- FALSE

# create long dataset
jatl <- melt(jobatdate,id.vars=c(1,2))
# add department name into long dataset
vacp <- unique(data.frame(vac$jobid,vac$Department,vac$Number_of_Vacancies))
names(vacp) <- c('jobid','Department','Number_of_Vacancies')
jatl <- merge(jatl, vacp)

# check whether each job was seen first and last
timelines <- ddply(jatl[jatl$value==TRUE,],
                   .(jobid),
                   summarise, firstseen = min(jobtimeaccessed),
                   lastseen = max(jobtimeaccessed))

timelines$newnow <- FALSE
timelines$livenow <- FALSE

timelines$newnow[timelines$firstseen == max(dates)] <- TRUE
timelines$livenow[timelines$lastseen == max(dates)] <- TRUE

# FIXME: THIS LINE DOESN'T WORK
# timelines$duration <- difftime(as.Date(timelines$lastseen) - as.Date(timelines$firstseen),"days")

timelines <- merge(timelines, vacp)
counts <- ddply(timelines, .(Department), summarise, count=length(unique(jobid))) 
timelines <- merge(timelines, counts)

# NOTE IT's NOT CLEAR HOW THE 'LIVE' ATTRIBUTE GETS AGGREGATED/DISPLAYED

groupings <- read.csv('./data-input/jobs_depts_links.csv')
timelines <- merge(timelines, groupings)

whplotdata <- timelines[timelines$Whitehall=='Yes',]
whplotdata <- ddply(whplotdata, .(Department, firstseen), summarise,
                    Vacancies = sum(Number_of_Vacancies),
                    livenow = mean(livenow))

# NOTE: Need to build chart from summed data set to allow proper labelling
require(ggplot2)
plot <- ggplot(whplotdata,
               aes(x=Department,y=as.Date(firstseen), col=livenow)) +
  geom_tile(aes(fill=Vacancies),size=1.5) +
  scale_fill_continuous(low="yellow",high="red") +
  scale_y_date() +
  geom_text(aes(label=Vacancies),
            col='black', size=2.5) +
  coord_flip()
plot
