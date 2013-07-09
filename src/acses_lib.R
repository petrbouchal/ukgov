# Set location ------------------------------------------------------------

location='home'
location='ifg'

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

pw=15.3
ph=24.5/2
fontfamily='Calibri'
plotformat='pdf'
savedevice = 'cairo_pdf'
plotobjpath <- './charts/ACSES chart objects/'
plotimagepath <- './charts/ACSES charts/'

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

AddOrgData <- function (dataset) {
  orgs <- read.csv('./data-input/acses_orgs.csv')
  dataset <- merge(dataset,orgs,all.x=TRUE)
  return(dataset)
}

# IfG Colour palette ------------------------------------------------------

IfGcols1 <- c('#37424a','#6E8494','#B6DAF5')
IfGcols2 <- c('#00ccff','#80e5ff','#c0f2ff')
IfGcols3 <- c('#d40072','#FF70BC','#FFC2E2')
IfGcols4 <- c('#83389b','#FF70BC','#FFC2E2')
IfGcols5 <- c('#7a9393','#7a9393','#7a9393')
IfGcols6 <- c('#457e81','#457e81','#457e81')
IfGcols7 <- c('#be8b5e','#be8b5e','#be8b5e')
IfGcols <- matrix(c(IfGcols1,IfGcols2,IfGcols3,IfGcols4,IfGcols5,IfGcols6,IfGcols7),
                  nrow=3,ncol=7)
IfGcols <- t(IfGcols)
# can accsess by calling IfGcols[colour#, tint#] e.g. IfGcols[2,1] for full blue
rm(IfGcols1,IfGcols2,IfGcols3,IfGcols4,IfGcols5,IfGcols6,IfGcols7)

# Custom WHM theme --------------------------------------------------------

theme_WHM <-theme_few() +
  theme(text = element_text(family=fontfamily,size=10),
        axis.text = element_text(colour='grey30'),
        axis.text.x = element_text(angle = 0),
        axis.text.y= element_text(vjust=0),
        axis.title=element_text(colour='grey30'),
        axis.ticks=element_blank(),
        axis.title=element_text(),
        axis.line=element_line(size=.5,colour=IfGcols[1,1]),
        legend.title=element_blank(),
        legend.position='bottom',
        legend.box='horizontal',
        legend.direction='horizontal',
        legend.key.size=unit(.3,units='cm'),
        legend.text = element_text(vjust=1),
        panel.margin=unit(c(0,0,0,0),'cm'),
        panel.border=element_blank(),
        plot.margin=unit(c(1,1,0,0),'cm'),
        plot.title=element_text(family='Calibri',face='bold',size=14,
                                lineheight=2.5, vjust=2))

theme_set(theme_WHM)

# Fn: Relabel - grades ----------------------------------------------------

RelabelGrades <- function (dataset) {
  levels(dataset$Civil.Service.grad)[levels(dataset$Civil.Service.grad)=="Administrative officers and assistants"] <- "AO"
  levels(dataset$Civil.Service.grad)[levels(dataset$Civil.Service.grad)=="Executive officer"] <- "EO"
  levels(dataset$Civil.Service.grad)[levels(dataset$Civil.Service.grad)=="Senior and higher executive officer"] <- "SEO/HEO"
  levels(dataset$Civil.Service.grad)[levels(dataset$Civil.Service.grad)=="Senior Civil Service"] <- "SCS"
  levels(dataset$Civil.Service.grad)[levels(dataset$Civil.Service.grad)=="Total"] <- "All grades"
  dataset$Civil.Service.grad = factor(dataset$Civil.Service.grad,
                                    levels(dataset$Civil.Service.grad)[c(7,1,2,4,5,3,6)])
  return(dataset)
}



# Fn: Relabel - paybands --------------------------------------------------

RelabelPaybands <- function (dataset) {
  levels(dataset$Wage.band)[levels(dataset$Wage.band)=="up to £20,000"] <- "< 20"
  levels(dataset$Wage.band)[levels(dataset$Wage.band)=="£20,001 - £30,000"] <- "20-30"
  levels(dataset$Wage.band)[levels(dataset$Wage.band)=="£30,001 - £40,000"] <- "30-40"
  levels(dataset$Wage.band)[levels(dataset$Wage.band)=="£40,001 - £50,000"] <- "40-50"
  levels(dataset$Wage.band)[levels(dataset$Wage.band)=="£50,001 - £60,000"] <- "50-60"
  levels(dataset$Wage.band)[levels(dataset$Wage.band)=="£60,001 - £70,000"] <- "60-70"
  levels(dataset$Wage.band)[levels(dataset$Wage.band)=="£70,001 - £80,000"] <- "60-70"
  levels(dataset$Wage.band)[levels(dataset$Wage.band)=="more than £80,000"] <- "> 80"
  return(dataset)
}

# Fn: Relabel - Ages ------------------------------------------------------

RelabelAgebands <- function (dataset) {
  dataset$Age.band <- gsub('Aged ','',dataset$Age.band)
  dataset$Age.band <- gsub('and over','+',dataset$Age.band)
  return(dataset)
}
