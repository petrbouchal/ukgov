library(xlsx)

estpath <- 'P:/Research & Learning/Research/19. Transforming Whitehall/Whitehall Monitor/Data Sources/Estate/'
estfilename <- 'Estate v0.2.xlsx'
estfilepath <- paste0(estpath,estfilename)
est <- read.xlsx(estfilepath,sheetName = 'BenchmarkAll',startRow = 1)
names(est)


