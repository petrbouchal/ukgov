library(pbtools)
library(gridExtra)
source('./src/govreport14/GovReport_download.R')

countries <- c('Czech Republic','Poland','Slovakia','Hungary','Germany','Slovenia',
               'Estonia','Latvia','Lithuania','United Kingdom','New Zealand',
               'Netherlands')

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

variables_indiv <- c('stat.score','researchers.unesco','a5010','a5011','a5074','a5020',
                     'a5081','a5074','a5022','a5072')

# INDEXES CODEBOOK AT
# http://www.governancereport.org/fileadmin/governancereport/2014/downloads/IndexesCodebook.pdf
variables_index <- c('delivery_overallindex100','coordexpindex100','regqualindex100',
                     'intelligenceindex100')

# check that the country and variable names are correct in lists for filtering
sapply(countries, function(x) x %in% GovReportData2014$country)
sapply(variables_indiv, function(x) x %in% GovReportData2014$variable)
sapply(variables_index, function(x) x %in% GovReportData2014$variable)

grr <- melt(GovReportIndex2014,id.vars = 'country') %>%
  filter(country %in% countries & variable %in% variables_index) %>%
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
  arrange(-value)

grr$variable <- factor(grr$variable, levels=c('mean',
                                              'delivery_overallindex100',
                                              'intelligenceindex100',
                                              'coordexpindex100',
                                              'regqualindex100'))
grr$variable <- revalue(grr$variable, c('mean'='Průměr',
                                        'intelligenceindex100'='Analytická kapacita',
                                        'coordexpindex100'='Kvalita koordinace',
                                        'regqualindex100'='Kvalita regulace',
                                        'delivery_overallindex100'='Efektivita (výsledky)'))
variablesCZ <- c('Analytická kapacita','Kvalita koordinace',
                 'Kvalita regulace','Efektivita (výsledky)')
  
grr$alphaval <- .5
grr$alphaval[grr$countr=='Czech Republic'] <- 1

# Chart

loadcustomthemes(ifgcolours,'Myriad Pro')
ggplot <- function(...) ggplot2::ggplot(...) + scale_fill_manual(values=themebasecolours) +
  scale_colour_manual(values=themebasecolours)

grrplots <- mapply(function(x, y) {
  grr2 <- grr[grr$variable==x,]
  grr2$country <- with(grr2, reorder(country,value))
  grrplot <- ggplot(grr2[grr2$variable==x,], aes(country, value,alpha=alphaval)) +
    geom_bar(stat='identity',position='dodge',fill=y) +
    scale_alpha_identity() +
    coord_flip() +
    scale_y_continuous(limits=c(0,100),breaks=c(0,25,50,75,100)) +
    theme(panel.grid.major.y=element_blank(),panel.grid.minor.x=element_line(),
          axis.text.y=element_text(size=12), plot.title=element_text(size=14)) +
    labs(title=x)
}, variablesCZ, themebasecolours[1:4], SIMPLIFY=FALSE)

args.list <- c(grrplots, 2,2)
names(args.list) <- c("plot1", "plot2", "plot3", 'plot4', "nrow", "ncol")
do.call(grid.arrange, args.list)