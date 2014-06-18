library(pbtools)

file <- tempfile()
# urldata <- 'http://www.governancereport.org/fileadmin/governancereport/2014/downloads/GovReportDataset2014.csv'
# urlindex <- 'http://www.governancereport.org/fileadmin/governancereport/2014/downloads/GovRep2014Indexes.csv'
# download.file(urldata,destfile='./data-input/GovReport2014_data.csv')
# download.file(urlindex,destfile='./data-input/GovReport2014_index.csv')
GovReportIndex2014 <- read.csv('./data-input/GovReport2014_index.csv')
GovReportData2014 <- read.csv('./data-input/GovReport2014_data.csv')

countries1 <- c('Germany','New Zealand','Canada','Australia','France',
               'United Kingdom', 'United States','Singapore','China',
               'Sweden','Finland','Denmark')
countries2 <- c('Czech Republic','Poland','Slovakia','Hungary','Germany','Slovenia',
                'Estonia','Latvia','Lithuania','United Kingdom','New Zealand')
variables_indiv <- c('stat.score','researchers.unesco','a5010','a5011','a5074','a5020',
               'a5081','a5074','a5022','a5072')

# CODEBOOK AT
# http://www.governancereport.org/fileadmin/governancereport/2014/downloads/GovReportCodebook2014.pdf
# a5010: Degree of collaboration between ministries
# a5011: degree of coordination/collaboration within administrations
# a5075: Overall coherence of public policies
# a5072: Authorities capacity to adapt policies to changes in the 
          # economic and social contexts
# a5071: Is evaluation a common practice?
# a5020: are actions of public authorities in line with a long-term strategic vision?
# a5022: do the pub auths have capacity to encourage public and private stakeholders to work
# towards that vision?
# a5040: are there public or private think tanks producing ...
# a5081: 'Is the capacity of national public authorities hampered by divisions within the State apparatus?'
# a5074: overall coherence

# INDEXES CODEBOOK AT
# http://www.governancereport.org/fileadmin/governancereport/2014/downloads/IndexesCodebook.pdf
variables_index <- c('regulatoryindex100','coordexpindex100','regqualindex100',
                    'intelligenceindex100','delivery_overallindex100')

# check that the country and variable names are correct in lists for filtering
sapply(countries2, function(x) x %in% GovReportData2014$country)
sapply(countries1, function(x) x %in% GovReportData2014$country)
sapply(variables_indiv, function(x) x %in% GovReportData2014$variable)
sapply(variables_index, function(x) x %in% GovReportData2014$variable)

grr2 <- melt(GovReportIndex2014,id.vars = 'country') %>%
  filter(country %in% countries2 & variable %in% variables_index) %>%
  group_by(country) %>%
  mutate(mean = mean(value[variable!='delivery_overallindex100'])) %>%
#   mutate(mean = mean(value,na.rm = T)) %>%
  ungroup() %>%
  dcast(country + mean ~ variable) %>%
  melt() %>%
  group_by(country) %>%
  mutate(mean = value[variable=='mean']) %>%
  ungroup() %>%
  group_by(variable) %>%
  ungroup() %>%
  arrange(variable,-value)

grr2$country <- with(grr2, reorder(country,mean))
grr2$variable <- factor(grr2$variable, levels=c('mean','regulatoryindex100',
                                                'intelligenceindex100',
                                                'coordexpindex100',
                                                'regqualindex100',
                                                'delivery_overallindex100'))
grr2$alphaval <- .5
grr2$alphaval[grr2$countr=='Czech Republic'] <- 1
grr2$fontface <- 'plain'
grr2$fontface[grr2$country %in% countries2] <- 'bold'

# Chart

loadcustomthemes(ifgcolours,'Calibri')
ggplot <- function(...) ggplot2::ggplot(...) + scale_fill_manual(values=ifgbasecolours) +
  scale_colour_manual(values=ifgbasecolours)

grrplot <- ggplot(grr2, aes(country, value,fill=variable,alpha=alphaval)) +
  geom_bar(stat='identity',position='dodge') +
  scale_fill_manual(values = ifgcolours, guide='none') +
  scale_alpha_identity() +
  facet_wrap(~ variable, nrow = 3,labeller=) +
  coord_flip() +
  theme(panel.grid.major.y=element_blank(),panel.grid.minor.x=element_line(),
        axis.text.y=element_text(face=grr2$fontface,size=12))
grrplot

grrplots <- c()
for(var in variables_index) {
  grrplot <- ggplot(grr2[grr2$variable==var,], aes(country, value,alpha=alphaval)) +
    geom_bar(stat='identity',position='dodge',fill=ifgcolours[1,1]) +
    scale_fill_manual(values = ifgcolours, guide='none') +
    scale_alpha_identity() +
    coord_flip() +
    theme(panel.grid.major.y=element_blank(),panel.grid.minor.x=element_line(),
          axis.text.y=element_text(face=grr2$fontface,size=12))
  grrplot
  append(grrplots,grrplot)
}

for(i in grrplots) {i}
grrplots

