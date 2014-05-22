library(pbtools)

csps <- fread('./data-output/CSPS_demographic_long.csv')

csps2 <- csps[csps$eng_score_component & csps$dimension=='Time in current job',]
plot <- ggplot(data=csps2,aes(x=split,y=value)) +
  geom_bar(position='dodge',stat='identity',fill=themecols[1,1]) +
  scale_y_continuous(labels=percent,breaks=c(0,.25,.5,.75,1),limits=c(0,1)) +
  facet_wrap(~measure,nrow = 5) +
  coord_flip() +
  LoadCustomThemes(themecols[,1],'Verdana') +
  theme(panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),axis.title=element_blank())
plot
SavePlot