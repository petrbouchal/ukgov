wgapath <- 'P:/Research & Learning/Research/19. Transforming Whitehall/Whitehall Monitor/Data Sources/Financial data/Annual Report and Accounts/Analysis/'
wgafilename <- 'Assets and Liabilities Accounts Data (IfG) v0.6.xlsx'
wgafilepath <- paste0(wgapath,wgafilename)

library(xlsx)
wga <- read.xlsx(wgafilepath,sheetName = 'Collated')

names(wga)
