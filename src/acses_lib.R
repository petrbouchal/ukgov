# Set location ------------------------------------------------------------

location='home'
#location='ifg'

# Load libraries ----------------------------------------------------------

library(plyr)
library(ggplot2)
library(scales)
library(grid)
library(ggthemes)
library(extrafont)
library(gridExtra)
library(reshape)
library(reshape2)

# Set parameters for saved chart ------------------------------------------

pw=14
ph=21

plotformat <- 'eps'

fontfamily='Calibri'
#font_import()
loadfonts(device='postscript',quiet=TRUE)
loadfonts(quiet=TRUE)
if(location=='ifg') {
  loadfonts(device='win',quiet=TRUE)
}

yearlabels <- c('2008','2009','2010\nSR10 baseline','2011','2012')

# Fn: load and clean ACSES data -------------------------------------------------

LoadAcsesData <- function (file_name, location='home') {
  if(location=='home') {
    directory  <- '/Users/petrbouchal/Downloads/ACSES/'
  } else if(location=='ifg') {
    directory  <- 'P:/Research & Learning/Research/19. Transforming Whitehall/Whitehall Monitor/Data Sources/ONS Civil Service Statistics/Nomis ACSES/'
  } else {
    directory <- location
  }
  fullpath <- paste0(directory, file_name)
  dataset <- read.delim(fullpath, sep='\t')
  dataset$value[dataset$value=='#'] <- NA
  dataset$value[dataset$value=='..'] <- NA
  dataset$Organisation <- dataset$new1
  dataset$new1 <- NULL
  dataset$count <- as.numeric(as.character(dataset$value))
  dataset <- unique(dataset) # removes duplicate lines for DfE and GEO
  dataset$value <- NULL
  return(dataset)
}

# Fn: Load org-group data -------------------------------------------------

AddOrgData <- function (dataset, whitehallonly=FALSE) {
  orgs <- read.csv('./data-input/acses_orgs.csv')
  dataset <- merge(dataset,orgs,all.x=TRUE)
  if(whitehallonly) {
    dataset$Whitehall[dataset$Group=='HMRC'] <- 'WH'
    dataset$Whitehall[dataset$Group=='DWP'] <- 'WH'
    dataset <- dataset[dataset$Whitehall=='WH' | dataset$Whitehall=='Total',]
  }
  dataset <- dataset[dataset$Include=='Yes',]
  return(dataset)
}

# IfG Colour palette ------------------------------------------------------

rgb2col <- function(rgb) {
  rgb <- as.integer(rgb)
  class(rgb) <- "hexmode"
  rgb <- as.character(rgb)
  rgb <- matrix(rgb, nrow=3)
  paste("#", apply(rgb, MARGIN=2, FUN=paste, collapse=""), sep="")
}

getColorTable <- function(col) {
  # Convert all colors into format "#rrggbb"
  rgb <- col2rgb(col);
  col <- rgb2col(rgb);
  sort(unique(col))
}

tintshade <- function(colors, tints=c(), shades=c(), hexin=TRUE) {
  if(hexin) {
    rgbcols <- col2rgb(colors)
  } else {
    rgbcols <- colors
  }
  rgbout <- matrix(colors,nrow=1)
  if(length(tints)>0) {
    for(i in tints) {
      rgbtint <- rgbcols+(255-rgbcols)*(1-i)
      coltint <- rgb2col(rgbtint)
      rgbout <- rbind(rgbout, coltint)
    }
  }
  if(length(shades)>0) {
    for(i in shades) {
      rgbshade <- rgbcols-(rgbcols)*(1-i)
      colshade <- rgb2col(rgbshade)
      rgbout <- rbind(rgbout, colshade)
    }
  }
  return(t(rgbout))
}

IfGBasecols <- c('#37424a','#00ccff','#d40072','#83389b',
              '#7a9393','#457e81','#be8b5e')
IfGcols <- tintshade(IfGBasecols,tints=c(.5,.25),hexin=T,)
dimnames(IfGcols) <- NULL
rm(IfGBasecols)
# can accsess by calling IfGcols[colour#, tint#] e.g. IfGcols[2,1] for full blue

# Custom WHM theme --------------------------------------------------------

theme_WHM <-theme_few() +
  theme(text = element_text(family=fontfamily,size=10),
        axis.text = element_text(colour=IfGcols[1,1],hjust=.5,vjust=.5),
        axis.text.x = element_text(angle = 0),
        #axis.text.y= element_text(vjust=0,hjust=1),
        axis.title=element_text(colour=IfGcols[1,1]),
        axis.ticks=element_blank(),
        axis.title=element_text(),
        axis.line=element_blank(),
        legend.title=element_blank(),
        #legend.position=c(.5,-0.17),
        legend.position='bottom',
        legend.box='horizontal',
        legend.direction='horizontal',
        legend.key=element_rect(colour=NA),
        legend.key.size=unit(.2,'cm'),
        legend.text = element_text(vjust=.5),
        legend.background=element_blank(),
        panel.margin=unit(c(1,1,1,1),'mm'),
        panel.border=element_blank(),
        panel.background=element_blank(),
        plot.background=element_blank(),
        plot.margin=unit(c(.25,.25,0,0),'cm'),
        plot.title=element_blank())

theme_set(theme_WHM)

# Fn: Relabel - grades ----------------------------------------------------

RelabelGrades <- function (dataset) {
  levels(dataset$Civil.Service.grad)[levels(dataset$Civil.Service.grad)=="Administrative officers and assistants"] <- "AO/AA"
  levels(dataset$Civil.Service.grad)[levels(dataset$Civil.Service.grad)=="Executive officer"] <- "EO"
  levels(dataset$Civil.Service.grad)[levels(dataset$Civil.Service.grad)=="Senior and higher executive officer"] <- "SEO/HEO"
  levels(dataset$Civil.Service.grad)[levels(dataset$Civil.Service.grad)=="Senior Civil Service"] <- "SCS"
  levels(dataset$Civil.Service.grad)[levels(dataset$Civil.Service.grad)=="Grades 6 & 7"] <- "G6/G7"
  levels(dataset$Civil.Service.grad)[levels(dataset$Civil.Service.grad)=="Total"] <- "All grades"
  dataset$Civil.Service.grad <- factor(dataset$Civil.Service.grad,
                                      levels(dataset$Civil.Service.grad)[c(7,1,2,4,5,3,6)])
  dataset$Civil.Service.grad <- droplevels(dataset$Civil.Service.grad)
  return(dataset)
}

# Fn: Relabel - paybands --------------------------------------------------

RelabelPaybands <- function (dataset) {
  dataset$Wage.band <- factor(dataset$Wage.band,
                              levels(dataset$Wage.band)[c(10,1:7,8,9)])
  levels(dataset$Wage.band)[levels(dataset$Wage.band)=="up to Â20,000"] <- "< 20"
  levels(dataset$Wage.band)[levels(dataset$Wage.band)=="Â20,001 - Â30,000"] <- "20-30"
  levels(dataset$Wage.band)[levels(dataset$Wage.band)=="Â30,001 - Â40,000"] <- "30-40"
  levels(dataset$Wage.band)[levels(dataset$Wage.band)=="Â40,001 - Â50,000"] <- "40-50"
  levels(dataset$Wage.band)[levels(dataset$Wage.band)=="Â50,001 - Â60,000"] <- "50-60"
  levels(dataset$Wage.band)[levels(dataset$Wage.band)=="Â60,001 - Â70,000"] <- "60-70"
  levels(dataset$Wage.band)[levels(dataset$Wage.band)=="Â70,001 - Â80,000"] <- "60-70"
  levels(dataset$Wage.band)[levels(dataset$Wage.band)=="more than Â80,000"] <- "> 80"
  levels(dataset$Wage.band)[levels(dataset$Wage.band)=="up to Â£20,000"] <- "< 20"
  levels(dataset$Wage.band)[levels(dataset$Wage.band)=="up to £20,000"] <- "< 20"
  levels(dataset$Wage.band)[levels(dataset$Wage.band)=="£20,001 - £30,000"] <- "20-30"
  levels(dataset$Wage.band)[levels(dataset$Wage.band)=="£30,001 - £40,000"] <- "30-40"
  levels(dataset$Wage.band)[levels(dataset$Wage.band)=="£40,001 - £50,000"] <- "40-50"
  levels(dataset$Wage.band)[levels(dataset$Wage.band)=="£50,001 - £60,000"] <- "50-60"
  levels(dataset$Wage.band)[levels(dataset$Wage.band)=="£60,001 - £70,000"] <- "60-70"
  levels(dataset$Wage.band)[levels(dataset$Wage.band)=="£70,001 - £80,000"] <- "60-70"
  levels(dataset$Wage.band)[levels(dataset$Wage.band)=="more than £80,000"] <- "> 80"
  levels(dataset$Wage.band)[levels(dataset$Wage.band)=="up to Â£20,000"] <- "< 20"
  levels(dataset$Wage.band)[levels(dataset$Wage.band)=="Â£20,001 - Â£30,000"] <- "20-30"
  levels(dataset$Wage.band)[levels(dataset$Wage.band)=="Â£30,001 - Â£40,000"] <- "30-40"
  levels(dataset$Wage.band)[levels(dataset$Wage.band)=="Â£40,001 - Â£50,000"] <- "40-50"
  levels(dataset$Wage.band)[levels(dataset$Wage.band)=="Â£50,001 - Â£60,000"] <- "50-60"
  levels(dataset$Wage.band)[levels(dataset$Wage.band)=="Â£60,001 - Â£70,000"] <- "60-70"
  levels(dataset$Wage.band)[levels(dataset$Wage.band)=="Â£70,001 - Â£80,000"] <- "60-70"
  levels(dataset$Wage.band)[levels(dataset$Wage.band)=="more than Â£80,000"] <- "> 80"
  dataset$Wage.band <- droplevels(dataset$Wage.band)
  return(dataset)
}

# Fn: Relabel - Ages + aggregate two lowest ages -------------------------

RelabelAgebands <- function (dataset) {
  dataset$Age.band <- gsub('Aged ','',dataset$Age.band)
  dataset$Age.band <- gsub('and over','+',dataset$Age.band)
  dataset$Age.band <- gsub('16-19','< 29',dataset$Age.band)
  dataset$Age.band <- gsub('20-29','< 29',dataset$Age.band)
  return(dataset)
}


# Fn: generic save function -----------------------------------------------

SavePlot <- function (plotname='Plot', plotformat='eps', ffamily='Helvetica',
                      splot=last_plot() ,ploth=210/2, plotw=14) {
  try(dev.off(),silent=TRUE)
  plotobjdir <- './charts/ACSES chart objects/'
  plotimagedir <- './charts/ACSES charts/'
  plotimagepath = paste0(plotimagedir,plotname,'.',plotformat)
  plotobjpath = paste0(plotobjdir,plotname,'.','ggp')
  if(plotformat=='pdf') {
    ggsave(plotimagepath, plot=splot, family=ffamily, device=cairo_pdf,
           height=ph, width=pw, units='cm')  
  } else if(plotformat=='eps') {
    ggsave(plotimagepath, plot=splot, family=ffamily, device=cairo_ps,
           height=ph, width=pw, units='cm')
  } else {
    ggsave(plotimagepath, plot=splot, family=ffamily,
           height=ploth, width=plotw, units='cm')
  }
  save(splot,file=plotobjpath)
  write.csv(splot$data,file=paste0(plotimagedir,plotname,'_data.csv'))
  splot
}

# Sort group factor by given variable - UNFINISHED!! ----------------------

SortDepts <- function (dataset, sortin, sortbycat, sortbyvar) {
  # orders factor (sortin) by how much of the staff (as counted by sortbyvar)
  # is in each category (sortbycat) - i.e. in effect by the mean grade / age / pay
  gradevalues <- data.frame('sortbyval'=c(1:length(levels(sortbycat))),
                            sortbycat=levels(sortbycat))
  dataset <- merge(dataset,gradevalues)
  xtot <- ddply(dataset,.(Group, Date, sortbycatname),
                summarise,sharewholegrp=sum(share, na.rm=TRUE))
  dataset <- merge(dataset,xtot)
  dataset <- merge(dataset,gradevalues)
  dataset$gradescore <- dataset$gradeval*dataset$sharewholegrp
  xtot <- ddply(dataset,.(Group,Date),summarise,meangradescore=mean(gradescore))
  dataset <- merge(dataset,xtot)
  dataset$sorter <- dataset$meangradescore
  #make Whole CS category go last
  dataset$sorter[dataset$Group=='Whole Civil Service'] <- max(dataset$sorter)*1.1
  #reorder grouping variable
  dataset$Group <- reorder(dataset$Group,dataset$sorter,mean)
  return(dataset)
}