
# Fn: Relabel - Ages + aggregate two lowest ages -------------------------

RelabelAgebands <- function (dataset) {
  dataset$Age.band <- gsub('Aged ','',dataset$Age.band)
  dataset$Age.band <- gsub('65 and over','60+',dataset$Age.band)
  dataset$Age.band <- gsub('60-64','60+',dataset$Age.band)
  dataset$Age.band <- gsub('16-19','< 29',dataset$Age.band)
  dataset$Age.band <- gsub('20-29','< 29',dataset$Age.band)
  return(dataset)
}
