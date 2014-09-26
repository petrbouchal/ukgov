library(pbtools)

# Load data --------------------------------------------------------------------
wmi <- read.csv('./data-input/wmi_2014-09-19.csv', stringsAsFactors=FALSE)

names(wmi) <- tolower(names(wmi))
wmi$month <- as.Date(wmi$month,format = '%d/%m/%Y')

# filter out times and departments and calculate change rates variables
wmipa <- wmi %>%
  filter(ifg.boundary=='Y' & month > '2012-02-01' & month < '2014-04-01') %>%
  filter(department!='Scot Off' & department != 'Wales Off' & department != 'AG Depts') %>%
  group_by(month, department) %>%
  summarise(payroll=sum(as.numeric(payroll.staff.costs.total.paybill.for.payroll.staff), na.rm=T),
#   summarise(payroll=sum(as.numeric(payroll.staff.costs.salary), na.rm=T),
            fte=sum(as.numeric(payroll.staff.total.employees.full.time.equivalent), na.rm=T)) %>%
  ungroup() %>%
  arrange(department, month) %>%
  group_by(department) %>%
  mutate(changefte=fte/first(fte)-1,
         changepayroll=payroll/first(payroll)-1) %>%
  arrange(department, month)

# create separate dataset for split of agency staff
wmipb <- wmi %>%
  filter(ifg.boundary=='Y' & month > '2012-02-01' & month < '2014-04-01') %>%
  filter(department!='Scot Off' & department != 'FCO' & department != 'AGO') %>%
  group_by(month, department) %>%
  summarise(agency=sum(as.numeric(non.payroll.staff.agency.staff..clerical.admin..full.time.equivalent), na.rm=T),
            interim=sum(as.numeric(non.payroll.staff.interim.managers.full.time.equivalent), na.rm=T),
            specialist=sum(as.numeric(non.payroll.staff.specialist.contractors.full.time.equivalent), na.rm=T),
            consultant=sum(as.numeric(non.payroll.staff.consultants.consultancy.full.time.equivalent), na.rm=T)) %>%
  ungroup() %>%
  melt(id.vars=c('month','department')) %>%
  arrange(department, variable, month)

# payroll and non-payroll COSTs
wmipc <- wmi %>%
  filter(ifg.boundary=='Y' & month > '2012-02-01' & month < '2014-04-01') %>%
  filter(department!='Scot Off' & department != 'FCO' & department != 'AG Depts') %>%
  group_by(month, department) %>%
  summarise(payrollcost=sum(as.numeric(payroll.staff.costs.total.paybill.for.payroll.staff), na.rm=T),
            nonpayrollcost=sum(as.numeric(non.payroll.staff.total.non.payroll..ccl..staff.costs), na.rm=T),
            payrollfte=sum(as.numeric(payroll.staff.total.employees.full.time.equivalent), na.rm=T),
            nonpayrollfte=sum(as.numeric(non.payroll.staff.total.employees.full.time.equivalent), na.rm=T)) %>%
  ungroup() %>%
  arrange(department, month) %>%
  group_by(department) %>%
  mutate(changepayrollfte=payrollfte-first(payrollfte),
         changepayrollcost=payrollcost-first(payrollcost),
         changenonpayrollcost=nonpayrollcost-first(nonpayrollcost),
         changenonpayrollfte=nonpayrollfte-first(nonpayrollfte)) %>%
  select(starts_with('change'), -ends_with('fte'), month, department) %>%
  melt(id.vars=c('month', 'department'))

# calculate/add time variables
wmipa$calyr <- year(wmipa$month)
wmipa$calquarter <- quarter(wmipa$month)
wmipa$calyrquarter <- paste0(wmipa$calyr, ' ', wmipa$calquarter)
fytable <- read.csv('./data-input/finyeartable.csv')
fytable$month <- as.Date(fytable$month)
wmipa <- merge(wmipa, fytable)

# Remove observations with missing or obviously mistyped values
wmipa$changepayroll[wmipa$department=='MOD' & (wmipa$month=='2012-04-01' | 
                                       wmipa$month=='2012-05-01')] <- NA
wmipa$changepayroll[wmipa$department=='Defra' & (wmipa$month=='2013-03-01')] <- NA
wmipa$changepayroll[wmipa$department=='HO' & (wmipa$month=='2013-11-01')] <- NA
wmipa$changepayroll[wmipa$department=='HMT' & (wmipa$month=='2014-04-01')] <- NA
wmipa$changepayroll[wmipa$department=='MOD' & (wmipa$month=='2014-04-01')] <- NA
wmipa <- wmipa %>% arrange(department, month)

wmipc$changepayrollcost[wmipc$department=='MOD' & (wmipc$month=='2012-04-01' | 
                                                 wmipc$month=='2012-05-01')] <- NA
wmipc$changepayrollcost[wmipc$department=='Defra' & (wmipc$month=='2013-03-01')] <- NA
wmipc$changepayrollcost[wmipc$department=='HO' & (wmipc$month=='2013-11-01')] <- NA
wmipc$changepayrollcost[wmipc$department=='HMT' & (wmipc$month=='2014-04-01')] <- NA
wmipc$changepayrollcost[wmipc$department=='MOD' & (wmipc$month=='2014-04-01')] <- NA
wmipc <- wmipc %>% arrange(department, month)

# create long format with only change variables - for ggplot ------------
wmip <- wmipa %>%
  select(department, month, changefte, changepayroll,
         calyr, calquarter, calyrquarter, fy) %>%
  melt(id.vars = c('department', 'month', 'calyr',
                   'calquarter','calyrquarter', 'fy'))
