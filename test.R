library(profvis)
library(shiny)
# getCorrelationData(geneList = "ATM", Sample_Type = "Normal_Tissues", Species = "hsapiens")
options(shiny.reactlog=TRUE)
getwd()

runApp(launch.browser = T, display.mode = "normal")
# runApp( display.mode = "showcase")




# profvis(prof_output = "profFile.html",{
#   runApp(launch.browser = T)
# 
# })



