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

comall <- openxlsx::read.xlsx(comfile,sheet = 'Data')
comall$HouseEndDate2 <- as.Date(substr(comall$HouseEndDate,1,10))
