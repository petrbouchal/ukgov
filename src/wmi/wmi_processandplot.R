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
         changepayroll=payroll/first(payroll)-1) %>%
  arrange(short.form, month)

# create separate dataset for split of agency staff
wmipb <- wmi %>%
  filter(ifg.classification=='Whitehall' & month > '2012-02-01' & month < '2014-04-01') %>%
  filter(short.form!='Scot Off' & short.form != 'Wales Off' & short.form != 'AG Depts') %>%
  group_by(month, short.form) %>%
  summarise(agency=sum(as.numeric(non.payroll.staff.agency.staff..clerical.admin..full.time.equivalent), na.rm=T),
            interim=sum(as.numeric(non.payroll.staff.interim.managers.full.time.equivalent), na.rm=T),
            specialist=sum(as.numeric(non.payroll.staff.specialist.contractors.full.time.equivalent), na.rm=T),
            consultant=sum(as.numeric(non.payroll.staff.consultants.consultancy.full.time.equivalent), na.rm=T)            ) %>%
  ungroup() %>%
  arrange(short.form, month) %>%
  melt(id.vars=c('month','short.form'))

# payroll and non-payroll COSTs
wmipc <- wmi %>%
  filter(ifg.classification=='Whitehall' & month > '2012-02-01' & month < '2014-04-01') %>%
  filter(short.form!='Scot Off' & short.form != 'Wales Off' & short.form != 'AG Depts') %>%
  group_by(month, short.form) %>%
  summarise(payrollcost=sum(as.numeric(payroll.staff.costs.total.paybill.for.payroll.staff), na.rm=T),
            nonpayrollcost=sum(as.numeric(non.payroll.staff.total.non.payroll..ccl..staff.costs), na.rm=T),
            payrollfte=sum(as.numeric(payroll.staff.total.employees.full.time.equivalent), na.rm=T),
            nonpayrollfte=sum(as.numeric(non.payroll.staff.total.employees.full.time.equivalent), na.rm=T)) %>%
  ungroup() %>%
  arrange(short.form, month) %>%
  group_by(short.form) %>%
  mutate(changepayrollfte=payrollfte-first(payrollfte),
         changepayrollcost=payrollcost-first(payrollcost),
         changenonpayrollcost=nonpayrollcost-first(nonpayrollcost),
         changenonpayrollfte=nonpayrollfte-first(nonpayrollfte)) %>%
  select(starts_with('change'), -ends_with('fte'), month, short.form) %>%
  melt(id.vars=c('month', 'short.form'))

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
wmipa <- wmipa %>% arrange(short.form, month)

wmipc$changepayrollcost[wmipc$short.form=='MOD' & (wmipc$month=='2012-04-01' | 
                                                 wmipc$month=='2012-05-01')] <- NA
wmipc$changepayrollcost[wmipc$short.form=='Defra' & (wmipc$month=='2013-03-01')] <- NA
wmipc$changepayrollcost[wmipc$short.form=='HO' & (wmipc$month=='2013-11-01')] <- NA
wmipc$changepayrollcost[wmipc$short.form=='HMT' & (wmipc$month=='2014-04-01')] <- NA
wmipc$changepayrollcost[wmipc$short.form=='MOD' & (wmipc$month=='2014-04-01')] <- NA
wmipc <- wmipc %>% arrange(short.form, month)

# create long format with only change variables - for ggplot ------------
wmip <- wmipa %>%
  select(short.form, month, changefte, changepayroll,
         calyr, calquarter, calyrquarter, fy) %>%
  melt(id.vars = c('short.form', 'month', 'calyr',
                   'calquarter','calyrquarter', 'fy'))

# plots -----------------------------------------------------------------------
loadcustomthemes(mycols = ifgbasecolours, 'Calibri')

# lines: change staff and change paybill by department--------------------------
ggplot(wmip, aes(month, value, colour=variable)) +
  geom_hline(y=0, colour=ifgbasecolours[1]) +
  geom_line(size=1) +
  facet_wrap(~short.form) +
  scale_colour_manual(values=ifgbasecolours[3:2],
                      labels=c('Payroll staff (FTE)','Pay bill (payroll only)')) +
  scale_y_continuous(limits=c(-.4, .4), labels = percent) +
  scale_x_date(labels=date_format("%b %Y")) +
  guides(colour=guide_legend(keywidth=unit(1,'cm')))

# scatter: changes by department-------------------------------------
ggplot(wmipa, aes(changefte, changepayroll)) +
  geom_point(size=1, colour=ifgcolours[3]) +
  facet_wrap(~short.form) +
  geom_smooth(method='lm', se=FALSE, colour=ifgcolours[1,2]) +
  scale_y_continuous(limits=c(-.4,.4), labels = percent) +
  scale_x_continuous(limits=c(-.4,.4), labels = percent) +
  theme(panel.grid.major.x=element_line()) + coord_fixed()

# scatter: changes by financial year, comparing 2013 and 2014-----------------
ggplot(wmipa[(wmipa$fy=='2012-13' | wmipa$fy=='2013-14') & wmipa$month!='2012-03-01',],
       aes(changefte, changepayroll, fill=fy, colour=fy)) +
  geom_point(size=3, shape=16, size=3, alpha=.6) +
  geom_smooth(method='lm', se=FALSE, size=1.2) +
  scale_colour_manual(values=ifgbasecolours[2:3], guide='none') +
  scale_fill_manual(values=ifgbasecolours[2:3], guide='none') + 
  scale_y_continuous(limits=c(-.5,.5), labels = percent, expand = c(0,0)) +
  scale_x_continuous(limits=c(-.5,.5), labels = percent, expand = c(0,0)) +
  theme(panel.grid.major.x=element_line()) +
  facet_wrap(~fy) + coord_fixed()

# line: departmental breakdown of non-payroll staff numbers
wmipb_change <- wmipb %>%
  group_by(short.form, variable) %>%
  arrange(variable, short.form, month) %>%
  mutate(change = (value-first(value))/first(value)) %>%
  mutate(label=value[value==max(value)][1])
  

ggplot(wmipb_change, aes(month, change, colour=variable)) +
  geom_line(size=1) + 
  facet_wrap(~short.form, scales='free_y') + 
  scale_colour_manual(values=ifgbasecolours,
                      labels=c('Agency / clerical',
                               'Interim managers',
                               'Specialist contractors',
                               'Consultants / consultancy staff'))
# saveplot('Plot',plotformat = 'pdf',ploth = 10, plotw = 12)

# area: departmental breakdown of non-payroll staff numbers -----------------
ggplot(wmipb, aes(month, value, fill=variable)) +
  geom_area(position='stack') + 
  facet_wrap(~short.form) + 
  scale_fill_manual(values=ifgbasecolours,
                      labels=c('Agency / clerical',
                               'Interim managers',
                               'Specialist contractors',
                               'Consultants / consultancy staff')) + 
  scale_x_date(labels=date_format('%b %y'), breaks="6 months", 
               limits = as.Date(c('2012-04-01','2013-12-01'))) +
  scale_y_continuous(limits=c(0,3000))

saveplot('Tempsbydept',plotformat = 'png',ploth = 16, plotw = 16,
         plotdir = './charts-output/', dpi=300)

# line: cost changes by department, payroll and non-payroll --------------
ggplot(wmipc, aes(month, value, colour=variable)) +
  geom_line(size=1.1) + scale_colour_manual(values=ifgbasecolours[3:4],
                                            labels=c('Payroll (FTE)', 'Non-payroll (FTE)')) +
  facet_wrap(~short.form, scales='free_y')

# correlations for all departments ----------------------------------------
corrs <- wmipa[wmipa$month!='2012-03-01',] %>% 
  group_by(short.form) %>%
  summarise(corrcol = cor(payroll, fte, use='complete.obs'))

corrs2 <- wmipa[wmipa$month!='2012-03-01',] %>% 
  group_by(short.form, fy) %>%
  summarise(corrcol = cor(payroll, fte, use='complete.obs')) %>%
  cast(short.form ~ fy) %>%
  mutate(improved = `2012-13` < `2013-14`)
table(corrs2$improved)

corrs3 <- wmipa %>%
  ungroup() %>%
  group_by(fy) %>%
  summarise(corrcol = cor(changepayroll, changefte, use='complete.obs'))
