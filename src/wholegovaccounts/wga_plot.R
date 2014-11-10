source('./src/wga/wga_load.R')

library(pbtools)

wga2 <- wga %>%
  select(Department, Aggregation,A.L, ends_with('latest')) %>%
  filter(A.L='Assets less liabilities' & Aggregation=='Dept group') %>%
  melt(id.vars=c('Department','Aggregation','A.L')) %>%
  group_by(Department, Aggregation, A.L, variable) %>%
  summarise(value=sum(value)) %>%
  mutate(grp=paste0(Department, A.L),
         variable=str_replace_all(variable,'.latest',''),
         variable=str_replace_all(variable,'X','')) %>%
  group_by(Department, Aggregation, A.L) %>%
  arrange(Department, A.L, variable) %>%
  mutate(change=(value-lag(value))/lag(value))

loadcustomthemes(ifgcolours, 'Calibri')
wgaplot <- ggplot(wga2, aes(variable, value, colour=A.L, group=A.L)) +
  geom_line(position='dodge',stat='identity') + 
  geom_point(position='dodge',stat='identity') + 
  facet_wrap(~Department, scales='free_y') +
  scale_colour_manual(values=ifgbasecolours[c(2,3)])
wgaplot
