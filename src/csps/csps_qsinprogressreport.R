csps <- read.csv('./src/csps/data-input/csps_2009_2013_qs_benchmark.csv', stringsAsFactors=FALSE)

csps[3:7] <- sapply(csps[3:7], function(x) as.numeric(x))

csps$change0913 <- csps$CSPS.2013-csps$CSPS.2009
table(is.na(csps$change0913))
start2012 <- c('B58', 'B59', 'B60')
csps$change0913 <- ifelse(csps$ID %in% start2012,
                          csps$CSPS.2013-csps$CSPS.2012,
                          csps$change0913)
table(is.na(csps$change0913))


csps$publication <- FALSE
pickedqs <- c('B42','B43','B20', 'B58', 'B18', 'B60',
              'B19', 'B49', 'B29', 'B59', 'B21')

csps <- csps[!grepl('E02',csps$ID),]

csps$publication <- ifelse(csps$ID %in% pickedqs, TRUE, csps$publication)
table(csps$publication)

library(pbtools)

loadcustomthemes(ifgbasecolours, 'Calibri')

ggplot(csps, aes(x=change0913, fill=publication)) +
  geom_histogram(stat='bin', binwidth=.01, size=.8) +
  scale_x_continuous(labels=percent) + 
  scale_fill_manual(name='Is in CSRP pogress report', values=ifgbasecolours,
                    labels = c('Not in progress report', 'In progress report')) + 
  geom_vline(aes(xintercept=mean(csps$change0913, na.rm=T)), colour=ifgbasecolours[1]) + 
  geom_vline(aes(xintercept=mean(csps$change0913[csps$publication], na.rm=T)), colour=ifgbasecolours[2]) + 
#   geom_vline(aes(xintercept=0, colour=ifgbasecolours[3])) + 
  theme(axis.title.x=element_text()) + labs(x='Change in score 2009-13')

mean(csps$change0913[csps$publication], na.rm=T)
mean(csps$change0913, na.rm=T)
