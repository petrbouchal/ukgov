options(java.parameters = "-Xmx6000m")
library(xlsx)

comfile = '//FLPT//FldrRedir$//bouchalp//Desktop//committees.xlsx'

for(sheet in 1:24) {
  comname <- read.xlsx2(comfile, sheet, startRow = 1,endRow = 1,colIndex = 1,header = FALSE)
  print(comname)
}

comall <- read.xlsx(comfile,sheetName = 'Data')
comall$HouseEndDate2 <- as.Date(substr(comall$HouseEndDate,1,10))
