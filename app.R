library(shiny)
library(shinythemes)
library(shinyjs)
library(pryr)
library(heatmaply)
library(correlationAnalyzeR)
library(plotly)
library(DT)
library(shinymanager)
library(future)
library(promises)
library(ggpubr)
library(pool)
library(shinyWidgets)
library(shinyBS)
library(shinycssloaders)
options(spinner.color="#337AB7")
options(shiny.sanitize.errors = FALSE)
options(future.globals.maxSize=1e9)
source("scripts/modules_old.R")
source("scripts/helpers.R")
load("data/GlobalData.RData")

# credentials <- read.csv("accessKeys.csv", col.names = c("user", "password"),
#                          stringsAsFactors = FALSE)


# Get hardware info for load monitoring
if (Sys.info()[['sysname']] == "Windows") {
  print("Windows OS detected!")
  # plan(multiprocess, workers = 4, gc = TRUE)
  # plan(sequential)
  totalMemory <- 5
  # totalMemory <- as.numeric(gsub(system('wmic OS get TotalVisibleMemorySize /Value', intern = TRUE)[3], 
  #                                pattern = ".*=([0-9]+).*", replacement = "\\1"))
} else if (Sys.info()[['sysname']] == "Linux") {
  print("Linux OS detected!")
  # plan(multiprocess, gc = TRUE)
  totalMemory <- benchmarkme::get_ram()
  print(totalMemory)
}
# plan(sequential)
plan(multiprocess)
totalCores <- parallel::detectCores(logical = FALSE)
totalThreads <- parallel::detectCores(logical = TRUE)

# Make sure tmp dir exists
if (! dir.exists("www/tmp")) {
  dir.create("www/tmp")
}

# Load connections
pool <- pool::dbPool(
  drv = RMySQL::MySQL(),
  user = "public-rds-user@m2600az-db01p.mysql.database.azure.com", port = 3306,
  dbname="correlation_analyzer",
  password='public-user-password',
  host="m2600az-db01p.mysql.database.azure.com"
)
print("Pool connected")
# lapply( DBI::dbListConnections( DBI::dbDriver( drv = "MySQL")), DBI::dbDisconnect)
# pool::poolClose(pool)
# on.exit({
#   pool::poolClose(pool)
#   warning("Pool disconnected now!")
# })
onStop(function() {
  print("Disconnecting pool now!")
  pool::poolClose(pool)
  print("Open connections:")
  print(DBI::dbListConnections( DBI::dbDriver( drv = "MySQL")))
})
# DBI::dbListConnections( DBI::dbDriver( drv = "MySQL"))

ui <- tagList(
  # # authentication module
  # auth_ui(
  #   id = "auth", 
  #   tag_img = tags$img(
  #     src = "android-chrome-256x256.png", width = 100
  #   ),
  #   tag_div = tags$div(
  #     tags$p(
  #       "For any questions, please  contact ",
  #       tags$a(
  #         href = "mailto:millerh1@livemail.uthscsa.edu?Subject=Correlation%20AnalyzeR",
  #         target="_top", "administrator"
  #       )
  #     )
  #   )
  # ),
  
  ## result of authentication
  # verbatimTextOutput(outputId = "res_auth"),
  tags$head(
    tags$script(src="https://kit.fontawesome.com/5071e31d65.js", crossorigin="anonymous"),
    tags$link(rel = "shortcut icon", href = "favicon.ico"),
    tags$link(rel = "apple-touch-icon", sizes = "180x180", href = "favicon.ico"),
    tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "/favicons-32x32.png"),
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
        .selectize-dropdown .optgroup-header {
          font-size: 16px;
          font-weight: bold;
        }
        .selectize-control.multi .selectize-input > div.active {
          background: #efefef;
          color: #333333;
        }
        .selectize-control.multi .selectize-input > div {
          cursor: default;
        }
        
        "
        
      )
    )
    ),
  useShinyjs(),
  navbarPage(
    theme = shinytheme("united"),
    # theme = "mytheme.css",
    title = "Correlation AnalyzeR",
    tabPanel(
      title = "Home",
      value = "homeTab",
      fluidPage(
        br(),
        # verbatimTextOutput("res_auth"),
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
 #    tags$script(HTML(paste0("var header = $('.navbar > .container-fluid');
 # header.append('<div style=\"float:right; vertical-align: middle;\">", 
 #                            "<p style = \"padding: 5px; margin: 12px; font-size: x-small;",
 #                            "background-color: #f5f5f5; border: 1px solid #e3e3e3;",
 #                            "border-radius: 4px; box-shadow: inset 0 1px 1px rgba(0,0,0,0.05); \">CPU free: ", 
 #                            uiOutput("CPUHTML", inline = TRUE), " | Memory free: ", uiOutput("MEMHTML", inline = TRUE),
 #                            "</p></div>');"
 #    )))
  ),
  # uiOutput("machineLoad"),
  br(),
  br()
)


server <- function(input, output, session) {
  
  
  # # authentication module
  # auth <- callModule(
  #   module = auth_server,
  #   id = "auth",
  #   check_credentials = check_credentials(credentials)
  # )
  # 
  # output$res_auth <- renderPrint({
  #   reactiveValuesToList(auth)
  # })
  
  
  # # Print the current server workload to the user's screen
  # autoInvalidate <- reactiveTimer(10000)
  # workOut1 <- reactiveVal("0%")
  # workOut2 <- reactiveVal("0%")
  # output$CPUHTML <- renderUI({
  #   workOut1()
  # })
  # output$MEMHTML <- renderUI({
  #   workOut2()
  # })
  # observe({
  #   # print("Getting machine load!")
  #   if (Sys.info()[['sysname']] == "Windows") {
  #     memfree <- as.numeric(gsub(system('wmic OS get FreePhysicalMemory /Value', intern = TRUE)[3], 
  #                                pattern = ".*=([0-9]+).*", replacement = "\\1"))
  #     a <- system("wmic path Win32_PerfFormattedData_PerfProc_Process get Name,PercentProcessorTime", intern = TRUE)
  #     df <- do.call(rbind, lapply(strsplit(a, " "), function(x) {x <- x[x != ""];data.frame(process = x[1], cpu = x[2])}))
  #     cpuNow <- sum(as.numeric(as.character(df[grepl("Rgui|rstudio|R|rsession", df$process),2])), na.rm = TRUE)
  #     cpuNow <- cpuNow/totalCores
  #     memNow <- round((memfree/totalMemory) * 100)
  #   } else if (Sys.info()[['sysname']] == "Linux") {
  #     memfree <- (as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", 
  #                                  intern=TRUE))/1000000000)
  #     # From this
  #     cpuNow <- system("ps -C R -o %cpu", intern = TRUE)
  #     cpuNow <- (sum(as.numeric(gsub(cpuNow[c(-1)], pattern = " ", replacement = "")))/totalCores)
  #     memNow <- system("ps -C R -o %mem", intern = TRUE)
  #     memNow <- sum(as.numeric(gsub(memNow[c(-1)], pattern = " ", replacement = "")))
  #     memNow <- round(100-memNow)
  #   }
  #   cpuNow <- round(100-cpuNow)
  #   # cpuNow <- round((as.numeric(future::availableCores()/parallel::detectCores())) * 100)
  #   CpuColor <- ifelse(cpuNow < 25, yes = "red", no = 
  #                        ifelse(cpuNow < 40, yes = "orange", no = "green"))
  #   CPUHTML <- strong(span(paste0(cpuNow, "%"), style = paste0("color: ", CpuColor, ";")))
  #   MemColor <- ifelse(memNow < 25, yes = "red", no = 
  #                        ifelse(memNow < 40, yes = "orange", no = "green"))
  #   MEMHTML <- strong(span(paste0(memNow, "%"), style = paste0("color: ", MemColor, ";")))
  #   workOut1(CPUHTML)
  #   workOut2(MEMHTML)
  #   autoInvalidate()
  # }, priority = -1)
  
  
  # autoInvalidate <- reactiveTimer(3.6E3)
  # systemtime <- reactiveVal()
  # observe({
  #   print(system.time())
  #   autoInvalidate()
  #   dd <- Sys.time()
  #   tt <- unlist(strsplit(x = as.character(Sys.time()), split = " "))[[2]]
  #   tt2 <- as.numeric(unlist(strsplit(x = tt, split = ":"))[[1]])
  #   if (tt2 == 0) {
  #     lapply( DBI::dbListConnections( DBI::dbDriver( drv = "MySQL")), DBI::dbDisconnect)
  #   }
  # }, priority = -1)
  
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
                                                   pool = pool#, auth = auth
                                                   )
  geneVsGeneModeData[["geneVsGeneModeData"]] <- callModule(module = geneVsGeneModeAnalysis, 
                                                           id = "geneVsGeneModeAnalysis", 
                                                           parent_session = parent_session, 
                                                           GlobalData = GlobalData,
                                                           pool = pool#, auth = auth
                                                           )
  geneVsGeneListModeData[["geneVsGeneListModeData"]] <- callModule(module = geneVsGeneListModeAnalysis, 
                                                                   id = "geneVsGeneListModeAnalysis", 
                                                                   parent_session = parent_session, 
                                                                   GlobalData = GlobalData,
                                                                   pool = pool#, auth = auth
                                                                   )
  topologyModeData[["topologyModeData"]] <- callModule(module = topologyModeAnalysis, 
                                                       id = "topologyModeAnalysis", 
                                                       parent_session = parent_session, 
                                                       GlobalData = GlobalData,
                                                       pool = pool#, auth = auth
                                                       )
  
  
  callModule(module = singleModePlots,
             id = "singleModePlots",
             dataTables = singleModeData,
             parent_session = session#, auth = auth
             )
  callModule(module = geneVsGeneModePlots,
             id = "geneVsGeneModePlots",
             dataTables = geneVsGeneModeData,
             parent_session = session,
             GlobalData = GlobalData#, auth = auth
             )
  callModule(module = geneVsGeneListModePlots,
             id = "geneVsGeneListModePlots",
             dataTables = geneVsGeneListModeData,
             parent_session = session,
             GlobalData = GlobalData#, auth = auth
             )
  callModule(module = topologyModePlots,
             id = "topologyModePlots",
             dataTables = topologyModeData,
             parent_session = session,
             GlobalData = GlobalData#, auth = auth
             )
  
}

shinyApp(ui, server)
