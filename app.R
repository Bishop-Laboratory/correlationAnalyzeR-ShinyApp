library(shiny)
library(shinythemes)
library(shinyjs)
# Helper functions for shiny apps
library(pryr)
library(heatmaply)
library(correlationAnalyzeR)
library(plotly)
library(DT)
library(shinyWidgets)
library(shinyBS)
library(shinycssloaders)
options(spinner.color="#337AB7")
source("scripts/modules.R")
source("scripts/helpers.R")
load("data/GlobalData.RData")

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
        
        #shiny-notification-panel {
          width: 600px;
          font-size: 120%;

        }
        .shiny-notification {
              height: 80px;
              
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
      title = "Single gene",
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
      title = "Gene vs gene",
      value = "geneVsGeneModeTab",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          geneVsGeneModeAnalysisUI("geneVsGeneModeAnalysis")
        ),
        mainPanel(
          width = 9,
          geneVsGeneModePlotsUI("geneVsGeneModePlots")
        )
      )
    ),
    tabPanel(
      title = "Gene vs gene list",
      value = "geneVsGeneListModeTab",
      sidebarLayout(
        sidebarPanel(
          width = 2,
          geneVsGeneListModeAnalysisUI("geneVsGeneListModeAnalysis")
        ),
        mainPanel(
          width = 10,
          geneVsGeneListModePlotsUI("geneVsGeneListModePlots")
        )
      )
    ),
    tabPanel(
      title = "Gene list topology",
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
    )
  ),
  br(),
  br()
)


server <- function(input, output, session) {
  
  # Make session-specific tmp dir. Delete once finished with session. 
  tmp <- paste0("www/tmp/", as.character(session$token)[1])
  dir.create(tmp)
  onStop(function() {
    tmp <- paste0("www/tmp/", as.character(session$token)[1])
    unlink(tmp, force = T, recursive = T)
  })
  

  singleModeData <- reactiveValues(singleModeData = NULL)
  geneVsGeneModeData <- reactiveValues(geneVsGeneModeData = NULL)
  geneVsGeneListModeData <- reactiveValues(geneVsGeneListModeData = NULL)
  topologyModeData <- reactiveValues(topologyModeData = NULL)
  
  
  singleModeData[["singleModeData"]] <- callModule(module = singleModeAnalysis, 
                                                   id = "singleModeAnalysis", 
                                                   parent_session = parent_session, 
                                                   GlobalData = GlobalData)
  geneVsGeneModeData[["geneVsGeneModeData"]] <- callModule(module = geneVsGeneModeAnalysis, 
                                                           id = "geneVsGeneModeAnalysis", 
                                                           parent_session = parent_session, 
                                                           GlobalData = GlobalData)
  geneVsGeneListModeData[["geneVsGeneListModeData"]] <- callModule(module = geneVsGeneListModeAnalysis, 
                                                                   id = "geneVsGeneListModeAnalysis", 
                                                                   parent_session = parent_session, 
                                                                   GlobalData = GlobalData)
  topologyModeData[["topologyModeData"]] <- callModule(module = topologyModeAnalysis, 
                                                       id = "topologyModeAnalysis", 
                                                       parent_session = parent_session, 
                                                       GlobalData = GlobalData)
  callModule(module = singleModePlots,
             id = "singleModePlots",
             dataTables = singleModeData,
             parent_session = session)
  callModule(module = geneVsGeneModePlots,
             id = "geneVsGeneModePlots",
             dataTables = geneVsGeneModeData,
             parent_session = session,
             GlobalData = GlobalData)
  callModule(module = geneVsGeneListModePlots,
             id = "geneVsGeneListModePlots",
             dataTables = geneVsGeneListModeData,
             parent_session = session,
             GlobalData = GlobalData)
  callModule(module = topologyModePlots,
             id = "topologyModePlots",
             dataTables = topologyModeData,
             parent_session = session,
             GlobalData = GlobalData)
  
}

shinyApp(ui, server)
