source('./src/w')

# plots -----------------------------------------------------------------------
loadcustomthemes(mycols = ifgbasecolours, 'Calibri')

# lines: change staff and change paybill by department--------------------------
ggplot(wmip, aes(month, value, colour=variable)) +
  geom_hline(y=0, colour=ifgbasecolours[1]) +
  geom_line(size=1) +
  facet_wrap(~department) +
  scale_colour_manual(values=ifgbasecolours[3:2],
                      labels=c('Payroll staff (FTE)','Pay bill (payroll only)')) +
  scale_y_continuous(limits=c(-.4, .4), labels = percent) +
  scale_x_date(labels=date_format("%b %Y")) +
  guides(colour=guide_legend(keywidth=unit(1,'cm')))

## scatter: changes by department-------------------------------------
ggplot(wmipa, aes(changefte, changepayroll)) +
  geom_point(size=1, colour=ifgcolours[3]) +
  facet_wrap(~department) +
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

# line: departmental change in non-payroll staff numbers (percent) -----------
wmipb_change <- wmipb %>%
  group_by(department, variable) %>%
  arrange(variable, department, month) %>%
  mutate(change = (value-first(value))/first(value)) %>%
  mutate(label=value[value==max(value)][1])

wmipb_change$label[wmipb_change$label != wmipb_change$value] <- NA 

ggplot(wmipb_change, aes(month, change, colour=variable)) +
  geom_line(size=1) + 
  facet_wrap(~department, scales='free_y') + 
  geom_text(aes(label=round(label,digits = 0)), vjust=-.2, size=2.5) +
  scale_y_continuous(expand=c(0,1.1), labels=percent) +
  scale_colour_manual(values=ifgbasecolours,
                      labels=c('Agency / clerical',
                               'Interim managers',
                               'Specialist contractors',
                               'Consultants / consultancy staff'))
# saveplot('Plot',plotformat = 'pdf',ploth = 10, plotw = 12)

# line: cost changes by department, payroll and non-payroll --------------
ggplot(wmipc, aes(month, value/10e6, colour=variable)) +
  geom_line(size=1.1) + scale_colour_manual(values=ifgbasecolours[3:4],
                                            labels=c('Payroll (FTE)', 'Non-payroll (FTE)')) +
  facet_wrap(~department, scales='free_y')

#### correlations for all departments ----------------------------------------
corrs <- wmipa[wmipa$month!='2012-03-01',] %>% 
  group_by(department) %>%
  summarise(corrcol = cor(payroll, fte, use='complete.obs'))

corrs2 <- wmipa[wmipa$month!='2012-03-01',] %>% 
  group_by(department, fy) %>%
  summarise(corrcol = cor(payroll, fte, use='complete.obs')) %>%
  cast(department ~ fy) %>%
  mutate(improved = `2012-13` < `2013-14`)
table(corrs2$improved)

corrs3 <- wmipa %>%
  ungroup() %>%
  group_by(fy) %>%
  summarise(corrcol = cor(changepayroll, changefte, use='complete.obs'))
