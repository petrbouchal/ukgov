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