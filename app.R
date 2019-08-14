library(shiny)
library(shinythemes)
library(shinyjs)
# Helper functions for shiny apps
require(RMySQL)
require(DBI)
library(shinyBS)

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

ui <- tagList(
  tags$head(
    tags$link(rel = "shortcut icon", href = "favicon.ico"),
    tags$link(rel = "apple-touch-icon", sizes = "180x180", href = "favicon.ico"),
    tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "/favicon-32x32.png"),
    tags$link(rel = "icon", type = "image/png", sizes = "16x16", href = "/favicon-16x16.png"),
    tags$style(
      HTML(
        "
        #footerTag {
          padding:10px; 
          padding-top:10px; 
          padding-bottom: 0px; 
          background-color: #b64e01; 
          color: #FFFFFF;
          height:130px;
          border-top: 1px solid #E7E7E7; 
          text-align: center;
        }
        
        .shiny-notification {
              height: 60px;
              width: 400px;
              position:fixed;
              top: calc(50% - 30px);;
              left: calc(50% - 200px);;
        }
        
        .shiny-notification-warning {
          border-radius: 3px;
          opacity: 0.85;
          padding: 10px 8px 10px 10px;
          margin: 2px;
          color: #8a6d3b;
          background-color: #fcf8e3;
          border: 1px solid #faebcc;
          height: 80px;
          width: 400px;
          position:fixed;
          top: calc(50% + 350px);;
          left: calc(50% + 400px);;
        }
        
        .shiny-notification-error {
          border-radius: 3px;
          opacity: 0.85;
          padding: 10px 8px 10px 10px;
          margin: 2px;
          color: #a94442;
          background-color: #f2dede;
          border: 1px solid #ebccd1;
          height: 80px;
          width: 400px;
          position:fixed;
          top: calc(50% + 250px);;
          left: calc(50% + 400px);;
        }
        
        .shiny-notification-message {
          border-radius: 3px;
          opacity: 0.85;
          padding: 10px 8px 10px 10px;
          margin: 2px;
          color: #31708f;
          background-color: #d9edf7;
          border: 1px solid #bce8f1;
          height: 80px;
          width: 400px;
          position:fixed;
          top: calc(50% + 350px);;
          left: calc(50% + 400px);;
        }
            
        "
        
      )
    )
    ),
  navbarPage(
    theme = shinytheme("united"),
    # theme = "mytheme.css",
    title = "Correlation AnalyzeR",
    tabPanel(
      title = "Home",
      value = "homeTab",
      fluidPage(
        br(),
        includeHTML("www/homepage.html")
      )
    ),
    tabPanel(
      title = "About",
      value = "aboutTab",
      fluidPage(
        br(),
        includeHTML("www/about.html")
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
    tabPanel(
      title = "Help",
      value = "helpTab",
      fluidPage(
        br(),
        includeHTML("www/FAQs.html")
      )
    ),
    br(),
    br()
  )
  # tags$div(id="footerTag", p("footer"))
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
