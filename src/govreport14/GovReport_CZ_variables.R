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

# check that the country and variable names are correct in lists for filtering

variables_indiv <- c('proff','impar','X6.1','X6.3','a3032','a3033')

sapply(variables_indiv, function(x) x %in% GovReportData2014$variable)
sapply(countries, function(x) x %in% GovReportData2014$country)

grr <- melt(GovReportData2014,id.vars = 'country') %>%
  filter(country %in% countries & variable %in% variables_indiv) %>%
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
  arrange(value)

variablesCZ <- c('Profesionalita','Nestrannost státní správy',
                 'Efektivita regulací','Regulace bez prodleni',
                 'Efektivita výběru daní','Schopnost zabraňovat daňovým únikům')
grr$variable <- revalue(grr$variable, c('mean'='Průměr',
                                        'proff'='Profesionalita',
                                        'impar'='Nestrannost státní správy',
                                        'X6.1'='Efektivita regulací',
                                        'X6.3'='Regulace bez prodleni',
                                        'a3032'='Efektivita výběru daní',
                                        'a3033'='Schopnost zabraňovat daňovým únikům'))
  
grr$alphaval <- .5
grr$alphaval[grr$countr=='Czech Republic'] <- 1

# Chart

loadcustomthemes(ifgcolours,'Myriad Pro')
ggplot <- function(...) ggplot2::ggplot(...) + scale_fill_manual(values=themebasecolours) +
  scale_colour_manual(values=themebasecolours)

grrplots <- mapply(function(x, y) {
  grr2 <- grr[grr$variable==x,]
  grr2$value[is.na(grr2$value)] <- -200
  grr2$country <- with(grr2, reorder(country,value))
  grr2$textpos <- ifelse(grr2$value>0,grr2$value,0)+.1
  grrplot <- ggplot(grr2[grr2$variable==x,], aes(country, value,alpha=alphaval)) +
    geom_bar(stat='identity',position='dodge',fill=y) +
    geom_text(aes(label=country,y=textpos),hjust=0,size=4) +
    scale_alpha_identity() +
    coord_flip() +
    scale_y_continuous(limits=c(-2,3),
                       breaks=c(-2,-1,0,1,2,3)) +
    theme(panel.grid.major.y=element_blank(),panel.grid.minor.x=element_line(),
          axis.text.y=element_blank(), plot.title=element_text(size=14)) +
    labs(title=x)
}, variablesCZ, themebasecolours[1:6], SIMPLIFY=FALSE)

args.list <- c(grrplots, 3, 2)
names(args.list) <- c(as.character(1:length(grrplots)), "nrow", "ncol")
do.call(grid.arrange, args.list)