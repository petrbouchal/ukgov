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
