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
