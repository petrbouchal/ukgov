appdir='./src/shiny_experiments/deploytest/'

shinyapps::setAccountInfo(name="petrbouchal", token="F793D3F0F6F585B06F1E3D367786EEA5", secret="+II1tBNzPv0PiXu0AlvVX7Lq5Hjib5H7/DvH99IG")
setwd(appdir)

deployApp()