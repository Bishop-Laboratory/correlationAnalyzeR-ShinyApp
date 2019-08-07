library(shiny)
library(shinythemes)
library(shinyjs)
# Helper functions for shiny apps
require(RMySQL)
require(DBI)

source("scripts/modules.R")
source("scripts/helpers.R")
# # Load data for the global environment
load("data/geneInfo/HS_basicGeneInfo.RData")
load("data/geneInfo/MM_basicGeneInfo.RData")
load("data/symbol_suggestions.RData")
humanGeneOptions <- symbolsFinal$alias_symbol[which(symbolsFinal$species == "hsapiens")]
mouseGeneOptions <- symbolsFinal$alias_symbol[which(symbolsFinal$species == "mmusculus")]
# Create global data object
GlobalData <- list("HS_basicGeneInfo" = HS_basicGeneInfo,
                   'MM_basicGeneInfo' = MM_basicGeneInfo,
                   'humanGeneOptions' = humanGeneOptions,
                   'mouseGeneOptions' = mouseGeneOptions)

ui <- navbarPage(
  theme = shinytheme("united"),
  title = "Correlation AnalyzeR",
  tabPanel(
    title = "About",
    value = "aboutTab",
    fluidPage(
      br(),
      includeHTML("www/homepage.html")
    )
  ),
  tabPanel(
    title = "Single mode",
    value = "singleModeTab",
    sidebarLayout(
      sidebarPanel(
        width = 2,
        singleModeAnalysisUI("singleModeAnalysis")
      ),
      mainPanel(
        width = 10,
        singleModePlotsUI("singleModePlots")
      )
    )
  ),
  tabPanel(
    title = "Paired mode",
    value = "pairedModeTab",
    sidebarLayout(
      sidebarPanel(
        width = 2,
        pairedModeAnalysisUI("pairedModeAnalysis")
      ),
      mainPanel(
        width = 10,
        pairedModePlotsUI("pairedModePlots")
      )
    )
  ),
  tabPanel(
    title = "Topology mode",
    value = "topologyModeTab",
    sidebarLayout(
      sidebarPanel(
        width = 2,
        topologyModeAnalysisUI("topologyModeAnalysis")
      ),
      mainPanel(
        width = 10,
        topologyModePlotsUI("topologyModePlots")
      )
    )
  ),
  br(),
  br()
)


server <- function(input, output, session) {
  
  
  dataList <- reactiveValues(singleModeData = NULL, 
                             pairedModeData = NULL, 
                             topologyModeData = NULL)
  observe({
    # Pull out all available analyses
    dataList[["singleModeData"]] <- callModule(module = singleModeAnalysis, 
                                               id = "singleModeAnalysis", 
                                               parent_session = parent_session, 
                                               GlobalData = GlobalData)
    dataList[["pairedModeData"]] <- callModule(module = pairedModeAnalysis, 
                                               id = "pairedModeAnalysis", 
                                               parent_session = parent_session, 
                                               GlobalData = GlobalData)
    dataList[["topologyModeData"]] <- callModule(module = topologyModeAnalysis, 
                                               id = "topologyModeAnalysis", 
                                               parent_session = parent_session, 
                                               GlobalData = GlobalData)
    
    
    # Pass analyses to plotting modules
    callModule(module = singleModePlots,
               id = "singleModePlots",
               dataTables = dataList, 
               parent_session = session)
    callModule(module = pairedModePlots,
               id = "pairedModePlots",
               dataTables = dataList,
               parent_session = session,
               GlobalData = GlobalData)
    callModule(module = topologyModePlots,
               id = "topologyModePlots",
               dataTables = dataList,
               parent_session = session,
               GlobalData = GlobalData)
  })
}

shinyApp(ui, server)
