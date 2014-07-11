library(pbtools)
library(dplyr)
mtcars %>%
  group_by(cyl, gear) %>%
  mutate(ccount = n())
