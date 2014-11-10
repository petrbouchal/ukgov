library(maptools)
library(maps)
library(mapdata)
library(rgdal)
library(pbtools)

gor=readShapeSpatial('./data-input/geo/GOR/Regions.shp')
setwd('./data-input/geo/GOR/')
gor=readOGR('.','Regions')
names(gor)
summary(gor)
str(gor)

library(ggmap)
qplot(gor@proj4string)

map('world','uk')
plot(gor)
