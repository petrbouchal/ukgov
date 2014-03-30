source('./lib/load_packages.R')

source('./lib/GetColorTable.R')
source('./lib/TintShade.R')
source('./lib/rgb2col.R')

yearlabels <- c('2008','2009','2010\nSR10 baseline','2011','2012','2013')

IfGBasecols <- c('#37424a','#00ccff','#d40072','#83389b','#7a9393','#457e81','#be8b5e')
IfGcols <- TintShade(IfGBasecols,tints=c(.5,.25),hexin=T,)
dimnames(IfGcols) <- NULL
rm(IfGBasecols)

source('./lib/custom_themes.R')