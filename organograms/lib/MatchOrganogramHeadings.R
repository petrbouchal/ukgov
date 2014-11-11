MatchOrganogramHeadings <- function (dataset) {
  
#   if(nchar(names(dataset)[4])<4) {
#     names(dataset) <- dataset[1,]
#     dataset <- dataset[-1,]
#     if((nchar(dataset)[4])<4) {
#       names(dataset) <- dataset[1,]    
#       dataset <- dataset[-1,]
#       if(nchar(names(dataset)[4])<4) {
#         names(dataset) <- dataset[1,]
#         dataset <- dataset[-1,]
#         if(nchar(names(dataset)[4])<4) {
#           names(dataset) <- dataset[1,]
#           dataset <- dataset[-1,]
#           if(nchar(names(dataset)[4])<4) {
#             names(dataset) <- dataset[1,]
#             dataset <- dataset[-1,]
#             if(nchar(names(dataset)[4])<4) {
#               dataset <- dataset[-1,]
#               dataset[1,] <- NULL
#             }
#           }
#         }
#       }
#     }
#   }
  
  Name <- dataset[,grep("[Nn]ame", names(dataset), value=TRUE)]
  Grade <- dataset[,grep("[Gg]rade", names(dataset), value=TRUE)]
  Function <- dataset[,grep("[Ff]unction", names(dataset), value=TRUE)]
  UniqueID <- dataset[,grep("[Uu]nique", names(dataset), value=TRUE)]
  JobTitle <- dataset[,grep("[Tt]itle", names(dataset), value=TRUE)]
  ParentDept <- dataset[,grep("[Dd]ep", names(dataset), value=TRUE)]
  Organisation <- dataset[,grep("[Oo]rgani", names(dataset), value=TRUE)]
  Unit <- dataset[,grep("[Uu]nit", names(dataset), value=TRUE)]
  SalaryCostOfReports <- dataset[,grep("[Cc]ost", names(dataset), value=TRUE)]
  ReportsTo <- dataset[,grep("[Ss]enior", names(dataset), value=TRUE)]
  Email <- dataset[,grep("[Mm]ail", names(dataset), value=TRUE)]
  Phone <- dataset[,grep("([Pp]hone)|([N|n]umber)", names(dataset), value=TRUE)]
  PayFloor <- dataset[,grep("([Ff]loor)", names(dataset), value=TRUE)]
  PayCeiling <- dataset[,grep("([Cc]eiling)", names(dataset), value=TRUE)]
  Profession <- dataset[,grep("[Pp]rofession", names(dataset), value=TRUE)]
  FTE <- dataset[,grep("FTE", names(dataset), value=TRUE)]
  Notes <- dataset[,grep("[Nn]otes]", names(dataset), value=TRUE)]
  dataset2 <- data.frame(Name,Grade,Function,
                         UniqueID,JobTitle,
                         ParentDept, Organisation,
                         Unit,SalaryCostOfReports,
                         ReportsTo,Email,Phone,
                         PayFloor,PayCeiling,
                         FTE,Profession,Notes)
  return(dataset2)
}