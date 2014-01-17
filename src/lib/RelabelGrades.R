# Fn: Relabel - grades ----------------------------------------------------

RelabelGrades <- function (dataset) {
  levels(dataset$Civil.Service.grad)[levels(dataset$Civil.Service.grad)=="Administrative officers and assistants"] <- "AO/AA"
  levels(dataset$Civil.Service.grad)[levels(dataset$Civil.Service.grad)=="Executive officer"] <- "EO"
  levels(dataset$Civil.Service.grad)[levels(dataset$Civil.Service.grad)=="Senior and higher executive officer"] <- "SEO/HEO"
  levels(dataset$Civil.Service.grad)[levels(dataset$Civil.Service.grad)=="Senior Civil Service"] <- "SCS"
  levels(dataset$Civil.Service.grad)[levels(dataset$Civil.Service.grad)=="Grades 6 & 7"] <- "G6/G7"
  levels(dataset$Civil.Service.grad)[levels(dataset$Civil.Service.grad)=="Total"] <- "All grades"
  dataset$Civil.Service.grad <- factor(dataset$Civil.Service.grad,
                                       levels(dataset$Civil.Service.grad)[c(7,4,1,2,5,3,6)],
                                       ordered=TRUE)
  dataset$Civil.Service.grad <- droplevels(dataset$Civil.Service.grad)
  return(dataset)
}
