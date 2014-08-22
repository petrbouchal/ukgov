source('./src/estate/estate_load.R')

library(pbtools)

# filter, clean up and order data
estp <- est %>%
  filter(!is.na(Department)) %>%
  mutate(yvar = Total.property.cost.per.FTE..GBP.) %>%
  group_by(Department) %>%
  mutate(sorter=yvar[Year==2013]) %>%
  ungroup() %>%
  mutate(Department = reorder(Department, sorter, mean))

# plot
loadcustomthemes(ifgcolours,'Calibri')
estplot <- ggplot(estp,aes(factor(Year), yvar,group=Department)) + 
  geom_line(size=1, colour=ifgcolours[3,1]) +
  geom_point(size=4, colour=ifgcolours[3,1])+
  facet_wrap(~Department) +
  scale_y_continuous(labels=comma) +
  theme(panel.border = element_rect(colour=ifgcolours[1,4],fill=NA))
estplot
