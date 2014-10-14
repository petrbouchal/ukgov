# source('./src/lib/load_packages.R')

whmdatafolder <- 'P:/Research & Learning/Research/19. Transforming Whitehall/Whitehall Monitor/Data Sources/'

# Set location ------------------------------------------------------------
if(whatplatform()=='Darwin') {location='home'} else {location='ifg'}

# set parameters
if(!exists('batchproduce')) {batchproduce <- FALSE}
if(!batchproduce) {whitehallonly <- TRUE} # change here to produce WH or group charts

# Set parameters for saved chart ------------------------------------------
fontfamily='Calibri'
if(!batchproduce) { # don't override size & format variables if producing by batch
  ph=12
  pw=17.5
  plotformat <- 'png'
}

#font_import()
loadfonts(device='postscript',quiet=TRUE)
loadfonts(quiet=TRUE)
if(location=='ifg') {
  loadfonts(device='win',quiet=TRUE)
}

source('./src/lib/AddOrgData.R')
source('./src/lib/LoadAcsesData.R')
source('./src/lib/LoadAcsesData2014.R')
source('./src/lib/RelabelAgebands.R')
source('./src/lib/RelabelPaybands.R')
source('./src/lib/RelabelGrades.R')

yearlabels <- c('2008','2009','2010\nSR10 baseline','2011','2012','2013')

## THESE ARE NOW IN PBTOOLS:
# source('./src/lib/GetColorTable.R')
# source('./src/lib/TintShade.R')
# source('./src/lib/SavePlot.R')
# source('./src/lib/SortGroups.R')
# source('./src/lib/rgb2col.R')