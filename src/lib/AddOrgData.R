# Fn: Load org-group data -------------------------------------------------

AddOrgData <- function (dataset, whitehallonly=FALSE) {
  require(data.table)
  require(dplyr)
  # [Blah function description]
  #
  # Args:
  #   dataset: Description of Arg1
  #   whitehallonly: Description of Arg2
  orgs <- fread('./data-input/acses_orgs_fordatafrom2008to2013.csv')
  dataset <- merge(dataset,orgs, by='Organisation',all.x=TRUE)
  if(whitehallonly) {
    dataset$Whitehall[dataset$Group=='HMRC'] <- 'WH'
    dataset$Whitehall[dataset$Group=='DWP'] <- 'WH'
    dataset$Whitehall[dataset$Group=='HO'] <- 'WH'
    dataset$Whitehall[dataset$Group=='MoD'] <- 'WH'
    dataset <- dataset[dataset$Whitehall=='WH' | dataset$Whitehall=='Total',]
  }
  dataset <- dataset[dataset$Include=='Yes',]
  return(dataset)
}
