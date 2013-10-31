source('./src/lib/load_packages.R')
# Set location ------------------------------------------------------------
if(Sys.info()[['sysname']]=='Darwin') {location='home'} else {location='ifg'}

# set parameters
if(!exists('batchproduce')) {batchproduce <- FALSE}
if(!batchproduce) {whitehallonly <- TRUE} # change here to produce WH or group charts

# Set parameters for saved chart ------------------------------------------
fontfamily='Calibri'
if(!batchproduce) { # don't override size & format variables if producing by batch
  ph=14-2.5
  pw=21
  plotformat <- 'pdf'
}

#font_import()
loadfonts(device='postscript',quiet=TRUE)
loadfonts(quiet=TRUE)
if(location=='ifg') {
  loadfonts(device='win',quiet=TRUE)
}

source('./src/lib/GetColorTable.R')
source('./src/lib/TintShade.R')
source('./src/lib/SortGroups.R')
source('./src/lib/rgb2col.R')
source('./src/lib/SavePlot.R')
source('./src/lib/AddOrgData.R')
source('./src/lib/LoadAcsesData.R')
source('./src/lib/RelabelAgebands.R')
source('./src/lib/RelabelPaybands.R')
source('./src/lib/RelabelGrades.R')

yearlabels <- c('2008','2009','2010\nSR10 baseline','2011','2012','2013')

IfGBasecols <- c('#37424a','#00ccff','#d40072','#83389b','#7a9393','#457e81','#be8b5e')
IfGcols <- TintShade(IfGBasecols,tints=c(.5,.25),hexin=T,)
dimnames(IfGcols) <- NULL
rm(IfGBasecols)

source('./src/lib/custom_themes.R')