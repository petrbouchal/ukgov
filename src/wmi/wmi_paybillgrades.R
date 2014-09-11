library(pbtools)

# Load data --------------------------------------------------------------------
wmi <- read.csv('./data-input/wmi2014_2.csv', stringsAsFactors=FALSE)

names(wmi) <- tolower(names(wmi))
wmi$month <- as.Date(wmi$month,format = '%d/%m/%Y')

wmi2 <- renamevars(wmi, renamelistfile = './data-input/wmi_renamevars.csv')

wmi3 <- wmi2 %>%
  select(month, department, managed, starts_with('fte_payroll')) %>%
  filter(managed=='Y' & month > '2012-03-01' & month < '2013-04-01') %>%
  select(-managed, -fte_payroll_total) %>%
  group_by(month, department) %>%
  mutate_each(funs(as.numeric)) %>%
  summarise_each(funs(sum)) %>%
  melt(id.vars=c('month','department')) %>%
  group_by(department, variable) %>%
  arrange(department, variable, month) %>%
  mutate(change = (value-first(value))/first(value))

wmi3$grp <- paste0(wmi3$department, wmi3$variable)

loadcustomthemes(ifgcolours, 'Calibri')
ggplot(wmi3, aes(month, value, fill=variable), group=grp) +
  geom_area(position='stack') + facet_wrap(~department, scales='free_y')

changeplot <- ggplot(wmi3, aes(month, change, colour=variable), group=grp) +
  geom_line(size=1.1) +
  facet_wrap(~department, scales='free_y') +
  scale_colour_manual(values=ifgbasecolours) +
  scale_y_continuous(labels=percent)
changeplot
