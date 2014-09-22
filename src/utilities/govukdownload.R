url <- 'http://data.gov.uk/dataset/staff-organograms-and-pay-department-for-education'
apiurl <- 'http://data.gov.uk/api/2/rest/package/staff-organograms-and-pay-department-for-education'

data <- fromJSON(apiurl)


graballfromdatagovuk <- function(url, extension, pattern) {
  require(jsonlite)
  
  # read in data
  data <- fromJSON(jsonurl)
  data$resources$url
  
}