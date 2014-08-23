# library(devtools)
# install_github('pbtools','petrbouchal')
library(pbtools)
library(ggvis)

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

csps3 <- csps2[csps2$measure=='Employee engagement index',]
csps3$split <- as.factor(csps3$split)
csps3$variable <- as.factor(csps3$variable)

csps2 %>%
  filter(eng_score_component & measure=='Employee engagement index') %>%
  ggvis(x = ~split, y = ~value) %>%
  layer_lines()

ggvis(data = csps3, x = ~split, y = ~value) %>%
  layer_lines()

