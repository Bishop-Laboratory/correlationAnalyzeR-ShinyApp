library(shiny)
library(shinythemes)
library(shinyjs)
library(pryr)
library(heatmaply)
library(correlationAnalyzeR)
library(plotly)
library(DT)
library(future)
library(promises)
library(pool)
library(shinyWidgets)
library(shinyBS)
library(shinycssloaders)
options(spinner.color="#337AB7")
options(shiny.sanitize.errors = FALSE)
options(future.globals.maxSize=1e9)
source("scripts/modules.R")
source("scripts/helpers.R")
load("data/GlobalData.RData")

plan(multiprocess, gc = TRUE)
# Get hardware info for load monitoring
if (Sys.info()[['sysname']] == "Windows") {
  print("Windows OS detected!")
  totalMemory <- as.numeric(gsub(system('wmic OS get TotalVisibleMemorySize /Value', intern = TRUE)[3], 
                                 pattern = ".*=([0-9]+).*", replacement = "\\1"))
} else if (Sys.info()[['sysname']] == "Linux") {
  print("Linux OS detected!")
  totalMemory <- benchmarkme::get_ram()
  print(totalMemory)
}
totalCores <- parallel::detectCores(logical = FALSE)
totalThreads <- parallel::detectCores(logical = TRUE)

# Make sure tmp dir exists
if (! dir.exists("www/tmp")) {
  dir.create("www/tmp")
}

# Load connections
pool <- pool::dbPool(
  drv = RMySQL::MySQL(),
  user = "public-rds-user", port = 3306,
  dbname="bishoplabdb",
  password='public-user-password',
  host="bishoplabdb.cyss3bq5juml.us-west-2.rds.amazonaws.com"
)
print("Pool connected")
# lapply( DBI::dbListConnections( DBI::dbDriver( drv = "MySQL")), DBI::dbDisconnect)
# pool::poolClose(pool)
# on.exit({
#   pool::poolClose(pool)
#   warning("Pool disconnected now!")
# })


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
    ),
    tags$script(HTML(paste0("var header = $('.navbar > .container-fluid');
 header.append('<div style=\"float:right; vertical-align: middle;\">", 
                            "<p style = \"padding: 5px; margin: 12px; font-size: x-small;",
                            "background-color: #f5f5f5; border: 1px solid #e3e3e3;",
                            "border-radius: 4px; box-shadow: inset 0 1px 1px rgba(0,0,0,0.05); \">CPU free: ", 
                            uiOutput("CPUHTML", inline = TRUE), " | Memory free: ", uiOutput("MEMHTML", inline = TRUE),
                            "</p></div>');"
    )))
  ),
  # uiOutput("machineLoad"),
  br(),
  br()
)


server <- function(input, output, session) {
  
  # Print the current server workload to the user's screen
  autoInvalidate <- reactiveTimer(10000)
  workOut1 <- reactiveVal("0%")
  workOut2 <- reactiveVal("0%")
  output$CPUHTML <- renderUI({
    workOut1()
  })
  output$MEMHTML <- renderUI({
    workOut2()
  })
  observe({
    # print("Getting machine load!")
    if (Sys.info()[['sysname']] == "Windows") {
      memfree <- as.numeric(gsub(system('wmic OS get FreePhysicalMemory /Value', intern = TRUE)[3], 
                                 pattern = ".*=([0-9]+).*", replacement = "\\1"))
      a <- system("wmic path Win32_PerfFormattedData_PerfProc_Process get Name,PercentProcessorTime", intern = TRUE)
      df <- do.call(rbind, lapply(strsplit(a, " "), function(x) {x <- x[x != ""];data.frame(process = x[1], cpu = x[2])}))
      cpuNow <- sum(as.numeric(as.character(df[grepl("Rgui|rstudio|R|rsession", df$process),2])), na.rm = TRUE)
      cpuNow <- cpuNow/totalCores
      memNow <- round((memfree/totalMemory) * 100)
    } else if (Sys.info()[['sysname']] == "Linux") {
      memfree <- (as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", 
                                   intern=TRUE))/1000000000)
      # From this
      cpuNow <- system("ps -C R -o %cpu", intern = TRUE)
      cpuNow <- (sum(as.numeric(gsub(cpuNow[c(-1)], pattern = " ", replacement = "")))/totalCores)
      memNow <- system("ps -C R -o %mem", intern = TRUE)
      memNow <- sum(as.numeric(gsub(memNow[c(-1)], pattern = " ", replacement = "")))
      memNow <- round(100-memNow)
    }
    cpuNow <- round(100-cpuNow)
    # cpuNow <- round((as.numeric(future::availableCores()/parallel::detectCores())) * 100)
    CpuColor <- ifelse(cpuNow < 25, yes = "red", no = 
                         ifelse(cpuNow < 40, yes = "orange", no = "green"))
    CPUHTML <- strong(span(paste0(cpuNow, "%"), style = paste0("color: ", CpuColor, ";")))
    MemColor <- ifelse(memNow < 25, yes = "red", no = 
                         ifelse(memNow < 40, yes = "orange", no = "green"))
    MEMHTML <- strong(span(paste0(memNow, "%"), style = paste0("color: ", MemColor, ";")))
    workOut1(CPUHTML)
    workOut2(MEMHTML)
    autoInvalidate()
  }, priority = -1)
  
  
  
  # Make session-specific tmp dir. Delete once finished with session. 
  tmp <- paste0("www/tmp/", as.character(session$token)[1])
  dir.create(tmp)
  onStop(function() {
    tmp <- paste0("www/tmp/", as.character(session$token)[1])
    unlink(tmp, force = T, recursive = T)
  })
  
  # Main analysis functions
  singleModeData <- reactiveValues(singleModeData = NULL)
  geneVsGeneModeData <- reactiveValues(geneVsGeneModeData = NULL)
  geneVsGeneListModeData <- reactiveValues(geneVsGeneListModeData = NULL)
  topologyModeData <- reactiveValues(topologyModeData = NULL)
  
  
  singleModeData[["singleModeData"]] <- callModule(module = singleModeAnalysis, 
                                                   id = "singleModeAnalysis", 
                                                   parent_session = parent_session, 
                                                   GlobalData = GlobalData,
                                                   pool = pool)
  geneVsGeneModeData[["geneVsGeneModeData"]] <- callModule(module = geneVsGeneModeAnalysis, 
                                                           id = "geneVsGeneModeAnalysis", 
                                                           parent_session = parent_session, 
                                                           GlobalData = GlobalData,
                                                           pool = pool)
  geneVsGeneListModeData[["geneVsGeneListModeData"]] <- callModule(module = geneVsGeneListModeAnalysis, 
                                                                   id = "geneVsGeneListModeAnalysis", 
                                                                   parent_session = parent_session, 
                                                                   GlobalData = GlobalData,
                                                                   pool = pool)
  topologyModeData[["topologyModeData"]] <- callModule(module = topologyModeAnalysis, 
                                                       id = "topologyModeAnalysis", 
                                                       parent_session = parent_session, 
                                                       GlobalData = GlobalData,
                                                       pool = pool)
  
  
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
