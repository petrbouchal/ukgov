url <- 'http://data.gov.uk/dataset/staff-organograms-and-pay-department-for-education'
apiurl <- 'http://data.gov.uk/api/2/rest/package/staff-organograms-and-pay-department-for-education'

library(jsonlite)
data <- fromJSON(apiurl)

data$resources$url

