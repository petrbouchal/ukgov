library(pbtools)

wmi <- read.csv('./data-input/wmi2014.csv', stringsAsFactors=FALSE)

names(wmi) <- tolower(names(wmi))
wmi$month <- as.Date(wmi$month,format = '%d/%m/%Y')

# filter out times and departments and calculate change rates variables
wmipd <- wmi %>%
  filter(ifg.classification=='Whitehall' & month > '2012-02-01' & month < '2014-04-01') %>%
  filter(short.form!='Scot Off' & short.form != 'Wales Off' & short.form != 'AG Depts') %>%
  group_by(month, short.form) %>%
  summarise(payroll=sum(as.numeric(payroll.staff.costs.total.paybill.for.payroll.staff), na.rm=T),
            #   summarise(payroll=sum(as.numeric(payroll.staff.costs.salary), na.rm=T),
            fte=sum(as.numeric(payroll.staff.total.employees.full.time.equivalent), na.rm=T)) %>%
  ungroup() %>%
  arrange(short.form, month) %>%
  group_by(short.form) %>%
  mutate(changefte=fte/lag(fte)-1,
         changepayroll=payroll/lag(payroll)-1) %>%
  arrange(short.form, month)

# calculate/add time variables
wmipd$calyr <- year(wmipd$month)
wmipd$calquarter <- quarter(wmipd$month)
wmipd$calyrquarter <- paste0(wmipd$calyr, ' ', wmipd$calquarter)
fytable <- read.csv('./data-input/finyeartable.csv')
fytable$month <- as.Date(fytable$month)
wmipd <- merge(wmipd, fytable)

# Remove observations with missing or obviously mistyped values
wmipd$changepayroll[wmipd$short.form=='MOD' & (wmipd$month=='2012-04-01' | 
                                                 wmipd$month=='2012-05-01')] <- NA
wmipd$changepayroll[wmipd$short.form=='Defra' & (wmipd$month=='2013-03-01')] <- NA
wmipd$changepayroll[wmipd$short.form=='HO' & (wmipd$month=='2013-11-01')] <- NA
wmipd$changepayroll[wmipd$short.form=='HMT' & (wmipd$month=='2014-04-01')] <- NA
wmipd$changepayroll[wmipd$short.form=='MOD' & (wmipd$month=='2014-04-01')] <- NA
wmipd <- wmipd %>% arrange(short.form, month)

# create long format with only change variables - for ggplot
wmip <- wmipd %>%
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
ggplot(wmipd, aes(changefte, changepayroll)) +
  geom_point(size=1, colour=ifgcolours[3]) +
  facet_wrap(~short.form) +
  geom_smooth(method='lm', se=FALSE, colour=ifgcolours[1,2]) +
  scale_y_continuous(limits=c(-.4,.4), labels = percent) +
  scale_x_continuous(limits=c(-.4,.4), labels = percent) +
  theme(panel.grid.major.x=element_line()) + coord_fixed()

# scatter: changes by financial year, comparing 2013 and 2014
ggplot(wmipd[(wmipd$fy=='2012-13' | wmipd$fy=='2013-14') & wmipd$month!='2012-03-01',],
       aes(changefte, changepayroll, fill=fy, colour=fy)) +
  geom_point(size=3, shape=16, size=3, alpha=.6) +
  geom_smooth(method='lm', se=FALSE, size=1.2) +
  scale_colour_manual(values=ifgbasecolours[2:3], guide='none') +
  scale_fill_manual(values=ifgbasecolours[2:3], guide='none') + 
  scale_y_continuous(limits=c(-.5,.5), labels = percent, expand = c(0,0)) +
  scale_x_continuous(limits=c(-.5,.5), labels = percent, expand = c(0,0)) +
  theme(panel.grid.major.x=element_line()) +
  facet_wrap(~fy) + coord_fixed()

# correlations for all departments
corrs <- wmipd[wmipd$month!='2012-03-01',] %>% 
  group_by(short.form) %>%
  summarise(corrcol = cor(payroll, fte, use='complete.obs'))

corrs2 <- wmipd[wmipd$month!='2012-03-01',] %>% 
  group_by(short.form, fy) %>%
  summarise(corrcol = cor(changepayroll, changefte, use='complete.obs')) %>%
  cast(short.form ~ fy) %>%
  mutate(improved = `2012-13` < `2013-14`)
table(corrs2$improved)

corrs3 <- wmipd %>%
  ungroup() %>%
  group_by(fy) %>%
  summarise(corrcol = cor(changepayroll, changefte, use='complete.obs'))
