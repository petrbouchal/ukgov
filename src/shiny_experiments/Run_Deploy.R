library(shiny)
source('./src/lib/deployPB.R')
app='./src/shiny_experiments/onspse'
runApp(app)

# deployPB(app)
deployPB('C:/Users/bouchalp/GitHub/Charting-government/src/shiny_experiments/onspse')
