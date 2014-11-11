options(java.parameters = "-Xmx6000m")
# library(openxlsx)
library(xlsx)

comfile = './parlcommittees/committees.xlsx'

for(sheet in 1:24) {
  comname <- openxlsx::read.xlsx(comfile, sheet, startRow = 1,rowNames = FALSE)
  print(comname)
}

# have to use xlsx::read.xlsx (not openxlsx::read.xlsx or xlsx::read.xlsx2)
# to ensure dates are read in correctly
comall <- xlsx::read.xlsx(comfile,sheetName = 'Data')
# comall <- openxlsx::read.xlsx(comfile,sheetName = 'Data')
comall$HouseEndDate2 <- as.Date(substr(comall$HouseEndDate,1,10))

hist(comall$StartDate5[comall$StartDate5>'2001-01-01'],breaks = 400)
hist(comall$EndDate[comall$EndDate>'2001-01-01'],breaks = 400)
