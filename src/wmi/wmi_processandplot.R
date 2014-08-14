library(pbtools)

wmi <- read.csv('./data-input/wmi2014.csv', stringsAsFactors=FALSE)

names(wmi) <- tolower(names(wmi))
wmi$month <- as.Date(wmi$month,format = '%d/%m/%Y')

# filter out times and departments and calculate change rates variables
wmipa <- wmi %>%
  filter(ifg.classification=='Whitehall' & month > '2012-02-01' & month < '2014-04-01') %>%
  filter(short.form!='Scot Off' & short.form != 'Wales Off' & short.form != 'AG Depts') %>%
  group_by(month, short.form) %>%
  summarise(payroll=sum(as.numeric(payroll.staff.costs.total.paybill.for.payroll.staff), na.rm=T),
#   summarise(payroll=sum(as.numeric(payroll.staff.costs.salary), na.rm=T),
            fte=sum(as.numeric(payroll.staff.total.employees.full.time.equivalent), na.rm=T)) %>%
  ungroup() %>%
  arrange(short.form, month) %>%
  group_by(short.form) %>%
  mutate(changefte=fte/first(fte)-1,
         changepayroll=payroll/first(payroll)-1)

# calculate/add time variables
wmipa$calyr <- year(wmipa$month)
wmipa$calquarter <- quarter(wmipa$month)
wmipa$calyrquarter <- paste0(wmipa$calyr, ' ', wmipa$calquarter)
fytable <- read.csv('./data-input/finyeartable.csv')
fytable$month <- as.Date(fytable$month)
wmipa <- merge(wmipa, fytable)

# Remove observations with missing or obviously mistyped values
wmipa$changepayroll[wmipa$short.form=='MOD' & (wmipa$month=='2012-04-01' | 
                                       wmipa$month=='2012-05-01')] <- NA
wmipa$changepayroll[wmipa$short.form=='Defra' & (wmipa$month=='2013-03-01')] <- NA
wmipa$changepayroll[wmipa$short.form=='HO' & (wmipa$month=='2013-11-01')] <- NA
wmipa$changepayroll[wmipa$short.form=='HMT' & (wmipa$month=='2014-04-01')] <- NA
wmipa$changepayroll[wmipa$short.form=='MOD' & (wmipa$month=='2014-04-01')] <- NA

# create long format with only change variables - for ggplot
wmip <- wmipa %>%
  select(short.form, month, changefte, changepayroll,
         calyr, calquarter, calyrquarter, fy) %>%
  melt(id.vars = c('short.form', 'month', 'calyr',
                   'calquarter','calyrquarter', 'fy'))

# plots
loadcustomthemes(mycols = ifgbasecolours, 'Calibri')

# lines: change staff and change paybill by department
ggplot(wmip, aes(month, value, colour=variable)) +
  geom_hline(y=0, colour=ifgbasecolours[1]) +
  geom_line(size=1) +
  facet_wrap(~short.form) +
  scale_colour_manual(values=ifgbasecolours[3:2],
                      labels=c('Payroll staff (FTE)','Pay bill (payroll only)')) +
  scale_y_continuous(limits=c(-.4, .4), labels = percent) +
  scale_x_date(labels=date_format("%b %Y")) +
  guides(colour=guide_legend(keywidth=unit(1,'cm')))

# scatter: changes by department
ggplot(wmipa, aes(changefte, changepayroll)) +
  geom_point(size=1, colour=ifgcolours[3]) +
  facet_wrap(~short.form) +
  geom_smooth(method='lm', se=FALSE, colour=ifgcolours[1,2]) +
  scale_y_continuous(limits=c(-.4, .4), labels = percent_format()) + 
  scale_x_continuous(limits=c(-.4, .4), labels = percent_format())

# scatter: changes by financial year, comparing 2013 and 2014
ggplot(wmipa[wmipa$fy=='2012-13' | wmipa$fy=='2013-14',],
       aes(changefte, changepayroll, fill=fy, colour=fy)) +
  geom_point(size=3, shape=16, alpha=.6) +
  geom_smooth(method='lm', se=FALSE, size=1.2) +
  scale_colour_manual(values=ifgbasecolours[2:3]) +
  scale_fill_manual(values=ifgbasecolours[2:3])

# correlations for all departments
corrs <- wmipa %>% 
  group_by(short.form) %>%
  summarise(corrcol = cor(payroll, fte, use='complete.obs'))
