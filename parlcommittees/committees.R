options(java.parameters = "-Xmx6000m")
library(openxlsx)
library(xlsx)

comfile = './parlcommittees/committees.xlsx'

for(sheet in 1:24) {
  comname <- openxlsx::read.xlsx(comfile, sheet, startRow = 1,rowNames = FALSE)
  print(comname)
}

system.time(withopen())
system.time(withjava2())
system.time(withjava())

comall <- xlsx::read.xlsx2(comfile,sheetName = 'Data')
# comall <- openxlsx::read.xlsx(comfile,sheetName = 'Data')
comall$HouseEndDate2 <- as.Date(substr(comall$HouseEndDate,1,10))

combook <- openxlsx::readWorkbook(comfile,sheet = 'Data')
