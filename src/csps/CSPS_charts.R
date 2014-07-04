library(devtools)
install_github('pbtools','petrbouchal')
library(pbtools)

csps <- fread('./data-output/CSPS_demographic_long.csv')
loadcustomthemes(ifgbasecolours,'Calibri')

csps2 <- csps[csps$eng_score_component & csps$dimension=='Time in current job',]
plot <- ggplot(data=csps2,aes(x=split,y=value)) +
  geom_bar(position='dodge',stat='identity',fill=ifgcolours[2,1]) +
  scale_y_continuous(labels=percent,breaks=c(0,.25,.5,.75,1),limits=c(0,1)) +
  facet_wrap(~measure,nrow = 5) +
  coord_flip() +
  theme(panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line())
plot

library(ggvis)
csps3 <- csps[csps$eng_score_component]

csps3 %>%
  ggvis(~split, ~value) %>%
  group_by(measure) %>%
  layer_histograms(binwidth=1)

library(ggvis)
ggvis(data=diamonds[diamonds$cut==input_select('Premium','Ideal',),],
        x = ~table, fill=~cut) %>%
  group_by(cut) %>%
  filter(cut=='Premium') %>%
  layer_histograms(binwidth=1,fill=input_select(c('a','n')))