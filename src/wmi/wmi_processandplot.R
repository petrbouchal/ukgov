library(pbtools)

wmi <- read.csv('./data-input/wmi2014.csv', stringsAsFactors=FALSE)

names(wmi) <- tolower(names(wmi))
wmi$month <- as.Date(wmi$month,format = '%d/%m/%Y')

wmip <- wmi %>%
  filter(ifg.classification=='Whitehall' & month > '2012-02-01' & month < '2014-08-01') %>%
  group_by(month, short.form) %>%
#   summarise(payroll=sum(as.numeric(payroll.staff.costs.total.paybill.for.payroll.staff), na.rm=T),
  summarise(payroll=sum(as.numeric(payroll.staff.costs.salary), na.rm=T),
            fte=sum(as.numeric(payroll.staff.total.employees.full.time.equivalent), na.rm=T)) %>%
  ungroup() %>%
  arrange(short.form, month) %>%
  group_by(short.form) %>%
  mutate(changefte=fte/first(fte)-1,
         changepayroll=payroll/first(payroll)-1) %>%
  select(short.form, month, changefte, changepayroll) %>%
  melt(id.vars = c('short.form', 'month'))

# Remove observations with missing or obviously mistyped values
wmip$value[wmip$short.form=='MOD' & (wmip$month=='2012-04-01' | 
                                       wmip$month=='2012-05-01') & wmip$variable=='changepayroll'] <- NA
wmip$value[wmip$short.form=='Defra' & (wmip$month=='2013-03-01' & wmip$variable=='changepayroll')] <- NA
wmip$value[wmip$short.form=='HO' & (wmip$month=='2013-11-01' & wmip$variable=='changepayroll')] <- NA


loadcustomthemes()

ggplot(wmip, aes(month, value, colour=variable)) + geom_line(size=1) +
  facet_wrap(~short.form) + scale_colour_manual(values=ifgbasecolours[2:3])

ggplot(wmiw, aes(changefte, changepayroll)) + geom_point(size=1) +
  facet_wrap(~short.form) + geom_smooth(method='lm')

wmiw <- cast(wmip)

corrs <- wmiw %>% 
  group_by(short.form) %>%
  summarise(corrcol = cor(changepayroll, changefte, use='complete.obs'))
