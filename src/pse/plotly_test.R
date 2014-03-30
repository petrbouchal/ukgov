# library(devtools)
# install_github("R-api", "plotly")
library(plotly)

source('./src/pse/PSE_Reshape.R')
detach(plyr)
library(dplyr)

change <- uu
uu <- uu %.%
  filter(Dept=='BIS' & measure=='ONS')

p <- plotly(username= 'petrbouchal', key= 'a5dzaj09m6')
p$plotly(uu$Period,uu$value)
