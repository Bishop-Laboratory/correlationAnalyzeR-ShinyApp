library(shiny)
library(plotly)
library(heatmaply)
library(DT)
library(shinyBS)
source("scripts/helpers.R")
library(shinyWidgets)

## Generic helper modules ##
# Input types
tissueTypeInputUI <- function(id) {
  ns <- NS(id)
  popify(
    selectizeInput(inputId = ns("tissueType"), label = "Select tissue type",
                   choices = c("All"), 
                   multiple = F),
    placement = "right", 
    title = "Select tissue type", options=list(container="body"),
    content = paste0('Correlations were processed separately by tissue type. ',
                     'Select a tissue to extract tissue-specific correlations, ',
                     'or select "all" to use correlations from all tissues.')
  )
}
tissueTypeInput <- function(input, output, session, 
                            parent_session,
                            GlobalData, 
                            species, 
                            crossComparisonMode = FALSE) {
  # Get data objects
  mouseTissueOptions <- GlobalData$mouseTissueOptions
  humanTissueOptions <- GlobalData$humanTissueOptions
  # Observe for the selected species
  # If the selected species changes to mouse, update the selectize options
  observe({
    speciesSelected <- species()
    crossComparisonMode <- crossComparisonMode
    if (crossComparisonMode) {
      updateSelectizeInput(session = session,
                           inputId = 'tissueType',
                           choices = c("-"),
                           server = TRUE, selected = "-")
      return("-")
    }
    if (speciesSelected == "Mouse") {
      mTisOpt <- names(mouseTissueOptions)[order(names(mouseTissueOptions))]
      mTisOpt <- gsub(mTisOpt, pattern = "0", replacement = " ")
      mTisOpt <- stringr::str_to_title(mTisOpt)
      mTisOpt[which(mTisOpt == "all")] <- "All"
      updateSelectizeInput(session = session,
                           inputId = 'tissueType',
                           choices = mTisOpt,
                           server = TRUE, selected = "All")
      
    } else if (speciesSelected == "Human") {
      hTisOpt <- names(humanTissueOptions)[order(names(humanTissueOptions))]
      hTisOpt <- gsub(hTisOpt, pattern = "0", replacement = " ")
      hTisOpt <- stringr::str_to_title(hTisOpt)
      hTisOpt[which(hTisOpt == "all")] <- "All"
      updateSelectizeInput(session = session,
                           inputId = 'tissueType',
                           choices = hTisOpt,
                           server = TRUE, selected = "All")
    }
  })
  # Pass the value of 'tissueType' backwards to the parent module
  tissueType <- reactive({input$tissueType})
  return(tissueType)
}
sampleTypeInputUI <- function(id) {
  ns <- NS(id)
  popify(
    selectizeInput(inputId = ns("sampleType"), label = "Select sample type",
                   choices = c("Normal", "Cancer"), selected = "Normal",
                   multiple = F),
    placement = "right", 
    title = "Select sample type", options=list(container="body"),
    content = paste0('Disease status was defined effectively for "Cancer"-derived',
                     ' samples and non-cancerous "Normal" samples.')
                     
  )
}
sampleTypeInput <- function(input, output, session, 
                            parent_session,
                            GlobalData, 
                            species, tissueType) {
  # Get data objects
  mouseTissueOptions <- GlobalData$mouseTissueOptions
  humanTissueOptions <- GlobalData$humanTissueOptions
  # Observe for the selected species
  # If the selected species changes to mouse, update the selectize options
  observe({
    speciesSelected <- species()
    tissueSelected <- tissueType()
    if (tissueSelected == "-") {
      updateSelectizeInput(session = session,
                           inputId = 'sampleType',
                           choices = "-",
                           server = TRUE, selected = "-")
      return("-")
    }
    tissueSelected <- tolower(tissueSelected)
    tissueSelected <- gsub(tissueSelected, pattern = " ", replacement = "0")
    if (speciesSelected == "Mouse") {
      updateSelectizeInput(session = session,
                           inputId = 'sampleType',
                           choices = stringr::str_to_title(as.character(mouseTissueOptions[[tissueSelected]])),
                           server = TRUE, selected = "Normal")
      
    } else if (speciesSelected == "Human") {
      updateSelectizeInput(session = session,
                           inputId = 'sampleType',
                           choices = stringr::str_to_title(as.character(humanTissueOptions[[tissueSelected]])),
                           server = TRUE, selected = "Normal")
    }
  })
  # Pass the value of 'primaryGene' backwards to the parent module
  sampleType <- reactive({input$sampleType})
  return(sampleType)
}



singleGeneInputUI <- function(id, label) {
  ns <- NS(id)
  popify(
    selectizeInput(inputId = ns("primaryGene"), label = label,
                   choices = c("Loading ..."), selected = "Loading ...",
                   multiple = F, options = list(maxOptions = 100)),
    placement = "right", 
    title = paste0(label, " input"), options=list(container="body"),
    content = paste0("Enter a gene of interest. ",
                     "Valid gene names will be displayed below as ",
                     "you type.")
  )
}




singleGeneInput <- function(input, output, session, 
                            parent_session,
                            GlobalData, 
                            species) {
  # Get data objects
  mouseGeneOptions <- GlobalData$mouseGeneOptions
  humanGeneOptions <- GlobalData$humanGeneOptions
  # Observe for the selected species
  # If the selected species changes to mouse, update the selectize options
  observe({
    speciesSelected <- species()
    if (speciesSelected == "Mouse") {
      updateSelectizeInput(session = session,
                           inputId = 'primaryGene',
                           choices = mouseGeneOptions[order(mouseGeneOptions)],
                           server = TRUE, selected = "Brca1",
                           options = list(maxOptions = 100))
      
    } else if (speciesSelected == "Human") {
      updateSelectizeInput(session = session,
                           inputId = 'primaryGene',
                           choices = humanGeneOptions[order(humanGeneOptions)],
                           server = TRUE, selected = "BRCA1",
                           options = list(maxOptions = 100))
    }
  })
  # Pass the value of 'primaryGene' backwards to the parent module
  primaryGene <- reactive({input$primaryGene})
  return(primaryGene)
}

multiGeneInputUI <- function(id, label, content) {
  ns <- NS(id)
  popify(
    textAreaInput(inputId = ns("secondaryGenes"), resize = "none",
                  label = label, height = "150px", 
                  placeholder = paste("ATM\nBRCA1\nTP53\nFANCA\nATR\n...",
                                      sep="")), placement = "right", 
    title = paste0(label, " input"), options=list(container="body"),
    content = content
  )
  
}

multiGeneInput <- function(input, output, session) {
  secondaryGenes <- reactive({input$secondaryGenes})
  return(secondaryGenes)
}
# Downloads
downloadDataUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("downloadBox"))
}

downloadData <- function(input, output, session, 
                         primaryName, downloadsListReact) {
  
  downloadsList <- reactiveValuesToList(downloadsListReact)
  
  
  output$downloadBox <- renderUI({
    ns <- session$ns
    downloadBoxUI <- tagList()
    downloadBoxUI <- lapply(1:length(names(downloadsList)), function(i) {
      downloadBoxUI <- tagList(
        downloadBoxUI,
        fluidRow(
          column(
            width = 6,
            h4(downloadsList[[i]][["uiName"]])
          ),
          column(
            width = 6,
            downloadButton(outputId = ns(paste0("download", i)))
          )
        ),
        br()
      )
      return(downloadBoxUI)
    })
    
    tagList(
      hr(),
      h3("Downloads"),
      hr(),
      column(width = 6, offset = 3,
             downloadBoxUI
      ),
      br(),
      br()
    )
  })
  
  # Put handlers here
  observe({
    lapply(1:length(names(downloadsList)), function(i) {
      output[[paste0("download", i)]] <- downloadHandler(
        filename = function() {
          paste0(primaryName, "_", names(downloadsList)[i], downloadsList[[i]]$file)
        },
        content = function(file) {
          if (downloadsList[[i]]$file == ".tsv" ) {
            data.table::fwrite(x = downloadsList[[i]]$content,
                               file = file,
                               sep = "\t", quote = F, row.names = F)
          } else {
            ggplot2::ggsave(filename = file, # device = "png", # put this back to re-activate
                            plot = downloadsList[[i]]$content)
          }
        }
      )
    })
  })
}


## Single-Mode modules ##
# Analysis
singleModeAnalysisUI <- function(id) {
  ns <- NS(id)
  tagList(
    singleGeneInputUI(ns("singleGeneInput"), "Select gene"),
    popify(
      selectInput(inputId = ns("species"), label = "Select species",
                  choices = c("Human", "Mouse"), selected = "Human"),
      placement = "right", 
      title = "Select species", options=list(container="body"),
      content = paste0('Correlations were processed separately by species. ',
                       'Specify which correlations to analyze by choosing "Human" or "Mouse".')
    ),
    tissueTypeInputUI(ns("tissueTypeInput")),
    sampleTypeInputUI(ns("sampleTypeInput")),
    popify(
      radioButtons(inputId = ns("gseaType"), label = "corGSEA type",
                   choices = c("Simple", "Complex", "None")),
      placement = "right", 
      title = 'Select corGSEA type', options=list(container="body"),
      content = paste0('Use corGSEA to discover pathways that correlate ',
                       'with your gene of interest. ',
                       'Choose "Simple" to rapidly enrich for common genesets',
                       ' (MSIGDB genesets "H", "C1", "C2", "C5", and "C6").',
                       ' Choose "Complex" to consider all MSIGDB genesets, or choose "None" ',
                       'to skip corGSEA entirely.')
    ), 
    popify(title = "Cross comparison mode", 
           placement = "right", options=list(container="body"),
      switchInput(ns("crossComparisonMode"), value = FALSE, label = "Group mode"),
      content = paste0('Cross comparison mode is a new analysis style for ',
                       'viewing correlations for a gene across tissue/disease groups.',
                       ' Select whether to examine differences across "Normal", "Cancer", ',
                       'or "All" tissue groups. Only "Normal" is available for mouse due to ',
                       'concerns regarding sample number for mouse cancer conditions.')
    ),
    conditionalPanel(condition = "input.crossComparisonMode && input.species == 'Human'", 
                     ns = ns,
                     radioButtons(inputId = ns("crossComparisonModeTypeHuman"), 
                                  label = "Group mode type", selected = "All",
                                  choices = c("All", "Normal", "Cancer"))),
    conditionalPanel(condition = "input.crossComparisonMode && input.species == 'Mouse'", 
                     ns = ns,
                     radioButtons(inputId = ns("crossComparisonModeTypeMouse"), 
                                  label = "Group mode type",  
                                  selected = "Normal",
                                  choices = c("Normal"))),
    fluidRow(
      column(4, actionButton(ns("do"), "Analyze"))
    )
  )
}

singleModeAnalysis <- function(input, output, session, 
                               parent_session, GlobalData) {
  
  species <- reactive({input$species})
  observe({
    crossComparisonMode <- input$crossComparisonMode
  })
  do <- reactive({input$do})
  primaryGene <- callModule(singleGeneInput, "singleGeneInput",
                            parent_session = parent_session,
                            GlobalData = GlobalData,
                            species = species)
  tissueType <- callModule(tissueTypeInput, "tissueTypeInput",
                           parent_session = parent_session,
                           GlobalData = GlobalData,
                           species = species,
                           crossComparisonMode = input$crossComparisonMode)
  sampleType <- callModule(sampleTypeInput, "sampleTypeInput",
                           parent_session = parent_session,
                           GlobalData = GlobalData,
                           species = species, 
                           tissueType = tissueType)

  observe({
    crossComparisonMode <- input$crossComparisonMode
    speciesSelected <- input$species
    if (crossComparisonMode) {
      updateRadioButtons(
        inputId = "gseaType", session = session, 
        choices = c("None"), selected = "None"
      )
    } else {
      updateRadioButtons(inputId = "gseaType", label = "corGSEA type",
                         choices = c("Simple", "Complex", "None"),
                         selected = "Simple", session = session)
    }
    tissueType <- callModule(tissueTypeInput, "tissueTypeInput",
                             parent_session = parent_session,
                             GlobalData = GlobalData,
                             species = species,
                             crossComparisonMode = input$crossComparisonMode)
    sampleType <- callModule(sampleTypeInput, "sampleTypeInput",
                             parent_session = parent_session,
                             GlobalData = GlobalData,
                             species = species, 
                             tissueType = tissueType)
  })
  
  data <- reactiveVal()
  
  # Observe the value of 'do' and run the analysis if it's above 1
  data <- eventReactive(eventExpr = input$do, {
    primaryGene <- primaryGene()
    sampleType <- sampleType()
    sampleType <- tolower(sampleType)
    tissueType <- tissueType()
    tissueType <- tolower(tissueType)
    tissueType <- gsub(tissueType, pattern = " ", replacement = "0")
    species <- input$species
    gseaType <- input$gseaType
    pval <- input$pval
    
    # # Bug testing
    # primaryGene <- "BRCA1"
    # species <- "Human"
    # sampleType <- "Normal_Tissues"
    # gseaType <- "Simple"
    # pval <- .05
    # GlobalData <- GlobalData
    
    # Validate inputs
    shiny::validate(
      need(primaryGene != "", "Please select a gene")
    )
    
    # Initialize progress object
    progress <- shiny::Progress$new()
    progress$set(message = "Validating inputs ... ", value = .1)
    # on.exit(progress$close())
    
    
    cleanRes <- cleanInputs(primaryGene = primaryGene,
                            selectedSpecies = species,
                            sampleType = sampleType,
                            tissueType = tissueType,
                            GlobalData = GlobalData,
                            session = session)
    
    
    if (input$crossComparisonMode) {
      progress$inc(.2, message = paste0("Starting cross comparisons for ", 
                                        primaryGene, " ... "))
      if (input$species == "Human") {
        whichCompareGroups <- input$crossComparisonModeTypeHuman
        print("Human cross")
      } else {
        whichCompareGroups <- input$crossComparisonModeTypeMouse
        print("mouse cross")
      }
      print("which compare")
      print(whichCompareGroups)
      
      resList <- correlationAnalyzeR::analyzeSingleGenes(
        Species = cleanRes$selectedSpecies,
        runGSEA = F, crossCompareMode = T,
        returnDataOnly = T, 
        whichCompareGroups = tolower(whichCompareGroups),
        Sample_Type = cleanRes$sampleType,
        Tissue = cleanRes$tissueType,
        genesOfInterest = cleanRes$primaryGene
      )
      data <- resList[[1]][["correlations"]]
      data$Variance <- matrixStats::rowVars(as.matrix(data))
      data <- cbind(rownames(data), data)
      colnames(data)[1] <- "geneName"
      data <- data[which(data[,1] != cleanRes$primaryGene),]
      rownames(data) <- NULL
      data <- merge(x = cleanRes$basicGeneInfo, y = data, by = "geneName")
      data <- data[order(data$Variance, decreasing = T),]
      resList[[1]][["correlations"]] <- data
      res <- list("correlationData" = resList,
                  "species" = species,
                  "gseaType" = gseaType,
                  "pval" = pval,
                  "primaryGene" = primaryGene,
                  "sampleType" = sampleType,
                  "tissueType" = tissueType,
                  "progress" = progress,
                  "whichCompareGroups" = whichCompareGroups)
      res
      
    } else {
      progress$inc(.2, message = paste0("Gathering correlations for ", 
                                        primaryGene, " ... "))
      
      data <- correlationAnalyzeR::getCorrelationData(Species = cleanRes$selectedSpecies,
                                                      Sample_Type = cleanRes$sampleType,
                                                      Tissue = cleanRes$tissueType,
                                                      geneList = cleanRes$primaryGene)
      data <- cbind(rownames(data), data)
      colnames(data)[1] <- "geneName"
      data <- data[which(data[,1] != cleanRes$primaryGene),]
      rownames(data) <- NULL
      data <- merge(x = cleanRes$basicGeneInfo, y = data, by = "geneName")
      # Enable the download button
      res <- list("correlationData" = data,
                  "species" = species,
                  "gseaType" = gseaType,
                  "pval" = pval,
                  "primaryGene" = primaryGene,
                  "sampleType" = sampleType,
                  "tissueType" = tissueType,
                  "progress" = progress)
      res
    }
    
    
  })
  
  return(data)
}
# Plotting/reporting
singleModePlotsUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      uiOutput(outputId = ns("correlationsUI")),
      uiOutput(outputId = ns("gseaUI")),
      downloadDataUI(ns("singleModeDownloads")),
      br(),
      br()
    )
  )
}

singleModePlots <- function(input, output, session,
                            parent_session, 
                            GlobalData, dataTables) {
  
  # # Bug testing
  # dataTables <- list("singleModeData" = res)
  
  observe({
    dataListReact <- dataTables[["singleModeData"]]
    dataList <- dataListReact()
    species <- dataList[["species"]]
    gseaType <- dataList[['gseaType']]
    pval <- dataList[['pval']]
    sampleType <- dataList[['sampleType']]
    tissueType <- dataList[['tissueType']]
    correlationData <- dataList[["correlationData"]]
    primaryGene <- dataList[["primaryGene"]]
    
    observeEvent(eventExpr = (! is.null(dataList)), {
      
      progress <- dataList[['progress']]
      if ("whichCompareGroups" %in% names(dataList)) {
        whichCompareGroups <- dataList[["whichCompareGroups"]]
        resList <- dataList[["correlationData"]][[1]]
        downloadsList <- reactiveValues()
        correlations <- resList[["correlations"]]
        
        uiName <- paste0(primaryGene," ", tolower(whichCompareGroups),
                         " groups")
        fileName <- paste0(primaryGene, "_",
                           tolower(whichCompareGroups), "_",
                           "groupMode")
        downloadsList[["correlationData"]] <- list("content" = correlations,
                                                   "uiName" = paste0(uiName,
                                                                     " correlation data"),
                                                   "file" = ".tsv")
        heatMapDat <- resList[["heatmapBigData"]]
        heatMapSmall <- resList[["heatmapSmall"]]
        geneTPMBoxplot <- resList[["TPM_boxPlot"]]
        geneTPMBoxplotData <- resList[["TPM_DF"]]
        downloadsList[["TPM"]] <- list("content" = geneTPMBoxplotData,
                                       "uiName" = paste0(uiName,
                                                         " normalized expression (TPM)"),
                                       "file" = ".tsv")
        output$heatMap <- renderPlotly({
          progress$inc(.3, message = "Rendering interactive heatmap ... ")
          
          plt_dat <- heatMapDat
          # Center the scale
          n <- length(colnames(plt_dat))# Get number of samples
          width <- n*50
          if (width < 800) {
            width <- 800
          } else if (width > 6000) {
            width <- 6000
          }
          newNames <- colnames(plt_dat)
          if (whichCompareGroups != "All") {
            newNames <- strsplit(newNames, split = "_")
            newNames <- sapply(newNames, "[[", 2)
            newNames <- gsub(newNames, pattern = "0", replacement = " ")
            newNames <- stringr::str_to_title(newNames)
          } else {
            newNames <- strsplit(newNames, split = "_")
            newNames1 <- sapply(newNames, "[[", 2)
            newNames1 <- gsub(newNames1, pattern = "0", replacement = " ")
            newNames1 <- stringr::str_to_title(newNames1)
            newNames2 <- sapply(newNames, "[[", 3)
            newNames2 <- stringr::str_to_title(newNames2)
            newNames <- paste0(newNames1, " - ", newNames2)
          }
          plt_dat <- plt_dat[c(1:500),]
          mini <- min(plt_dat)
          maxi <- max(plt_dat)
          newVal <- max(c(abs(mini), maxi))
          
          p <- heatmaply(plt_dat, hide_colorbar = TRUE, 
                         limits = c(-1*newVal, newVal), 
                         colors =  colorRampPalette(rev(RColorBrewer::brewer.pal(n = 7, name =
                                                                     "RdYlBu")))(100),
                         labCol = newNames,
                         showticklabels = c(T, F)) %>% layout(height=500)
          p <- p %>%
            config(plot_ly(),
                   toImageButtonOptions = list(
                     filename = paste0(fileName, "_heatMap.png"),
                     format = "png",
                     width = width,
                     height = 500))
          p
        })
        output$heatMapSmall <- renderPlot(height = 500, {
          heatMapSmall
        })
        output$geneTPMBoxplot <- renderPlot(height = 500, {
          geneTPMBoxplot
        })
        
        output$correlationsUI <- renderUI({
          ns <- session$ns
          tagList(
            hr(),
            h3("Group mode correlation heatmap"),
            hr(),
            fluidRow( style = "height:500px;",
              column(width = 12, #title = "Correlation Histogram",
                     plotlyOutput(ns("heatMap")))
            )
          )
        })
        output$gseaUI <- renderUI({
          ns <- session$ns
          tagList(
            hr(),
            h3("Top variable genes across groups"),
            hr(),
            fluidRow( style = "height:500px;",
              column(width = 12,
                     plotOutput(ns("heatMapSmall")))
            ),
            hr(),
            h3("Gene expression across groups"),
            hr(),
            fluidRow(style = "height:500px;",
                     column(width = 12,
                            plotOutput(ns("geneTPMBoxplot"))))
          )
        })

        progress$close()
        primaryName <- primaryGene
        
        callModule(module = downloadData, id = "singleModeDownloads", 
                   primaryName = uiName, downloadsListReact = downloadsList)
      } else {
        tissueType <- gsub(tissueType, pattern = "0", replacement = " ")
        downloadsList <- reactiveValues()
        uiName <- paste0(primaryGene, " (",
                         stringr::str_to_title(tissueType), " - ",
                         stringr::str_to_title(sampleType), ")")
        fileName <- paste0(primaryGene, "_",
                           tissueType, "_",
                           sampleType)
        correlationData <- correlationData[order(correlationData[,4], decreasing = T),]
        downloadsList[["correlationData"]] <- list("content" = correlationData,
                                                   "uiName" = paste0(uiName,
                                                                     " correlation data"),
                                                   "file" = ".tsv")
        
        output$geneHist <- renderPlotly({
          colnames(correlationData)[4] <- "vals"
          p <- ggpubr::gghistogram(data = correlationData, x = "vals", y = "..count..",
                                   bins = 100, ylab = "Frequency\n",
                                   title = uiName, 
                                   xlab = paste0(primaryGene, " correlation values")) +
            ggplot2::expand_limits(x = 0, y = 0)
          
          p <- plotly::ggplotly(p, source = "singleSelect") %>%
            # plotly::layout(dragmode = "select") %>%
            config(plot_ly(),
                   toImageButtonOptions= list(filename = paste0(fileName,
                                                                "_correlationHistogram.png")))
          s <- input$correlationData_rows_selected
          if (length(s)) {
            d2 <- correlationData[,c(1,3,4)]
            d2 <- unique(d2)
            colnames(d2)[3] <- "vals"
            # correlationData$vals[s]
            p <- p %>%
              plotly::add_segments(x = d2$vals[s], xend = d2$vals[s], y = 0,
                                   yend = 3000, color = "FF0404")
          } 
          p
        })
        
        # Make correlations datatable
        output$correlationData <- DT::renderDataTable(server = T,{
          correlationData$geneName <- createGeneInfoLink(correlationData$geneName)
          d2 <- correlationData
          d2 <- d2[,c(1,3,4)]
          d2 <- unique(d2)
          datatable(d2, selection = "single", rownames = F, escape = F,
                    options = list(
                      pageLength = 6,
                      dom = "ftprl",
                      scrollX = TRUE
                    ),
                    colnames = c("Gene Name", 
                                 "Description", "Correlation Value"))
        }, escape = FALSE)
        
        output$correlationsUI <- renderUI({
          ns <- session$ns
          tagList(
            hr(),
            h3("Correlation data"),
            hr(),
            fluidRow(
              column(width = 6, #title = "Correlation Histogram",
                     plotlyOutput(ns("geneHist"))
              ),
              column(width =  6, #title = "Correlation Data",
                     DT::dataTableOutput(ns("correlationData"))
              )
            )
          )
        })
        
        if (gseaType != 'None') {
          pval <- .05
          progress$inc(.1, message = "Calculating corGSEA ... ")
          progress$inc(.1, detail = paste("This may take ~1 minute to complete."))
          
          output$gseaUI <- renderUI({
            ns <- session$ns
            tagList(
              hr(),
              h3("corGSEA results"),
              hr(),
              fluidRow(
                column(width = 6, 
                       plotOutput(ns("plotGSEA"))
                ),
                column(width = 6, 
                       DT::dataTableOutput(ns("gseaData"))
                )
              )
            )
          })
          
          if (gseaType == "Simple") {
            if (species == "Human") {
              TERM2GENE <- correlationAnalyzeR::hsapiens_simple_TERM2GENE
            } else {
              TERM2GENE <- correlationAnalyzeR::mmusculus_simple_TERM2GENE
            }
          } else {
            if (species == "Human") {
              TERM2GENE <- correlationAnalyzeR::hsapiens_complex_TERM2GENE
            } else {
              TERM2GENE <- correlationAnalyzeR::mmusculus_complex_TERM2GENE
            }
          }
          ranks <- correlationData[,4]
          names(ranks) <- correlationData[,1]
          set.seed(1) # Reproducible
          gseaData <- correlationAnalyzeR::myGSEA(ranks = ranks, 
                                                  nperm = 2000,
                                                  TERM2GENE = TERM2GENE, 
                                                  padjustedCutoff = pval,
                                                  returnDataOnly = T,
                                                  topPlots = F)
          # Check if p cutoff value was raised 
          eres <- gseaData$eres
          minp <- min(eres$p.adjust)
          if (minp > pval) {
            msg <- paste0("No results returned at p value of ", pval, 
                          ". corGSEA p value cutoff was raised to accommodate.")
            shiny::showNotification(ui = msg, type = "warning")
          }
          downloadsList[['gseaData']] <- list("content" = eres,
                                              "file" = ".tsv",
                                              "uiName" = paste0(uiName, 
                                                                " corGSEA data"))
          eres <- eres[,c(2, 5, 6, 7)]
          eres <- eres[which(abs(eres[,2]) > 2),]
          eres <- eres[order(eres[,2], decreasing = T),]
          eres[,c(2,3,4)] <- apply(eres[,c(2,3,4)], 1:2, round, digits = 7)
          eres <- eres[order(eres[,3], decreasing = F),]
          
          eresTitles <- eres$Description
          eresTitles <- correlationAnalyzeR::fixStrings(eresTitles)
          eresTitles[which(nchar(eresTitles) > 45)] <- paste0(substr(eresTitles[which(nchar(eresTitles) > 45)], 1, 41), "...")
          # Output gseaData
          output$gseaData <- renderDataTable(server = T, {
            eresDT <- eres
            eresDT$Description <- createGSEAInfoLink(eresDT$Description, eresTitles)
            datatable(eresDT, selection = list(mode = "single", selected = 1), 
                      rownames = F, escape = F,
                      options = list(
                        pageLength = 6,
                        dom = "ftprl",
                        scrollX = TRUE
                      ),
                      colnames = c("Pathway", "Enrichment (normalized)",
                                   "Pval", "Padj"))
          }, escape = F)
          plotGSEA <- reactive({
            s <- input$gseaData_rows_selected
            if (length(s)) {
              id <- eres[s,1]
              titleID <- eresTitles[s]
            } else{
              id <- eres[1,1]
              titleID <- eresTitles[1]
            }
            p <- clusterProfiler::gseaplot(gseaData$EGMT, 
                                           geneSetID = id, 
                                           title = titleID)
            p
          })
          
          output$plotGSEA <- renderPlot({
            plotGSEA()
          })
          progress$close()
        } else {
          output$gseaUI <- renderUI({
            tagList(
              hr(),
              h4(em("Choose a 'corGSEA type' to analyze correlated pathways.")),
              hr()
            )
          })
          progress$close()
        }
        
        primaryName <- primaryGene
        
        callModule(module = downloadData, id = "singleModeDownloads", 
                   primaryName = uiName, downloadsListReact = downloadsList)
      }
    })
  })
}

## geneVsgene-mode analysis ##
geneVsGeneModeAnalysisUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 6,
        singleGeneInputUI(ns("singleGeneInputOne"), 
                          "Gene A"),
        tissueTypeInputUI(ns("tissueTypeInputOne")),
        sampleTypeInputUI(ns("sampleTypeInputOne"))
      ),
      column(width = 6,
        singleGeneInputUI(ns("singleGeneInputTwo"), 
                          "Gene B"),
        tissueTypeInputUI(ns("tissueTypeInputTwo")),
        sampleTypeInputUI(ns("sampleTypeInputTwo"))
      )
    ),
    
    popify(
      selectInput(inputId = ns("species"), label = "Select species",
                  choices = c("Human", "Mouse"), selected = "Human"),
      placement = "right", 
      title = "Select species", options=list(container="body"),
      content = paste0('Correlations were processed separately by species. ',
                       'Specify which correlations to analyze by choosing "Human" or "Mouse".')
    ),
    popify(
      radioButtons(inputId = ns("gseaType"), label = "corGSEA type",
                   choices = c("Simple", "Complex")), 
      placement = "right", 
      title = 'Select corGSEA type', options=list(container="body"),
      content = paste0('Use corGSEA to discover pathways that correlate ',
                       'with your gene of interest. ',
                       'Choose "Simple" to rapidly enrich for common genesets',
                       ' (MSIGDB genesets "H", "C2", "C5", and "C6")',
                       ' Choose "Complex" to consider all MSIGDB genesets.')
    ),
    popify(title = "Cross comparison mode", 
           placement = "right", options=list(container="body"),
           switchInput(ns("crossComparisonMode"), value = FALSE, label = "Group mode"),
           content = paste0('Cross comparison mode is a new analysis style for ',
                            'viewing correlations for a gene across tissue/disease groups.',
                            ' Select whether to examine differences across "Normal", "Cancer", ',
                            'or "All" tissue groups.')
    ),
    fluidRow(
      column(4, actionButton(ns("do"), "Analyze"))
    )
  )
}
geneVsGeneModeAnalysis <- function(input, output, session, 
                                       parent_session, GlobalData) {
  
  crossComparisonMode <- reactive({input$crossComparisonMode})
  observe({
    crossComparisonMode <- input$crossComparisonMode
  })
  do <- reactive({input$do})
  species <- reactive({input$species})
  geneOne <- callModule(singleGeneInput, "singleGeneInputOne",
                        parent_session = parent_session,
                        GlobalData = GlobalData,
                        species = species)
  tissueTypeOne <- callModule(tissueTypeInput, "tissueTypeInputOne",
                              parent_session = parent_session,
                              GlobalData = GlobalData,
                              species = species)
  sampleTypeOne <- callModule(sampleTypeInput, "sampleTypeInputOne",
                              parent_session = parent_session,
                              GlobalData = GlobalData,
                              species = species, 
                              tissueType = tissueTypeOne)
  
  geneTwo <- callModule(singleGeneInput, "singleGeneInputTwo",
                        parent_session = parent_session,
                        GlobalData = GlobalData,
                        species = species)
  tissueTypeTwo <- callModule(tissueTypeInput, "tissueTypeInputTwo",
                              parent_session = parent_session,
                              GlobalData = GlobalData,
                              species = species)
  sampleTypeTwo <- callModule(sampleTypeInput, "sampleTypeInputTwo",
                              parent_session = parent_session,
                              GlobalData = GlobalData,
                              species = species, 
                              tissueType = tissueTypeTwo)

  
  observe({
    crossComparisonMode <- input$crossComparisonMode
    speciesSelected <- input$species
    if (crossComparisonMode) {
      updateRadioButtons(
        inputId = "gseaType", session = session, 
        choices = c("None"), selected = "None"
      )
    } else {
      updateRadioButtons(inputId = "gseaType", label = "corGSEA type",
                         choices = c("Simple", "Complex"),
                         selected = "Simple", session = session)
    }
    # Force input boxes to update if user chooses group mode
    tissueType <- callModule(tissueTypeInput, "tissueTypeInputOne",
                             parent_session = parent_session,
                             GlobalData = GlobalData,
                             species = species,
                             crossComparisonMode = input$crossComparisonMode)
    sampleType <- callModule(sampleTypeInput, "tissueTypeInputOne",
                             parent_session = parent_session,
                             GlobalData = GlobalData,
                             species = species, 
                             tissueType = tissueType)
    tissueType <- callModule(tissueTypeInput, "tissueTypeInputTwo",
                             parent_session = parent_session,
                             GlobalData = GlobalData,
                             species = species,
                             crossComparisonMode = input$crossComparisonMode)
    sampleType <- callModule(sampleTypeInput, "tissueTypeInputTwo",
                             parent_session = parent_session,
                             GlobalData = GlobalData,
                             species = species, 
                             tissueType = tissueType)
  })
  
  
  
  data <- reactiveVal()
  data <- eventReactive(eventExpr = input$do, {
    geneOne <- geneOne()
    tissueTypeOne <- tissueTypeOne()
    tissueTypeOne <- tolower(tissueTypeOne)
    tissueTypeOne <- gsub(tissueTypeOne, pattern = " ",replacement = "0")
    sampleTypeOne <- sampleTypeOne()
    sampleTypeOne <- tolower(sampleTypeOne)
    geneTwo <- geneTwo()
    tissueTypeTwo <- tissueTypeTwo()
    tissueTypeTwo <- tolower(tissueTypeTwo)
    tissueTypeTwo <- gsub(tissueTypeTwo, pattern = " ",replacement = "0")
    sampleTypeTwo <- sampleTypeTwo()
    sampleTypeTwo <- tolower(sampleTypeTwo)
    species <- input$species
    gseaType <- input$gseaType
    pval <- input$pval
    # Validate inputs
    shiny::validate(
      need(geneOne != "", "Please select Gene A")
    )
    shiny::validate(
      need(geneTwo != "", "Please select Gene B")
    )
    
    if (geneOne == geneTwo &
        sampleTypeOne == sampleTypeTwo & 
        tissueTypeOne == tissueTypeTwo & 
        sampleTypeOne != "-") {
      showNotification(paste0("Please select two different genes, tissue types,",
                              " and/or disease states to compare.",
                              " Alternatively, select 'Group mode'."),
                       type = "error", duration = 12)
    }
    
    shiny::validate(
      need(expr = {geneOne != geneTwo |
             sampleTypeOne != sampleTypeTwo |
             tissueTypeOne != tissueTypeTwo |
          sampleTypeOne == "-"}, 
          message = 
            "Please select two different genes, tissue types, and/or disease states.")
    )
    
    # Initialize progress object
    progress <- shiny::Progress$new()
    progress$set(message = "Validating inputs ... ", value = .1)
    # on.exit(progress$close())
    
    # # Bug testing
    # geneOne <- "BRCA1"
    # tissueTypeOne <- "all"
    # sampleTypeOne <- "normal"
    # geneTwo <- "BRCA1"
    # tissueTypeTwo <- "female0reproductive"
    # sampleTypeTwo <- "normal"
    # species <- "Human"
    # gseaType <- "Simple"
    
    cleanResOne <- cleanInputs(primaryGene = geneOne,
                               selectedSpecies = species,
                               sampleType = sampleTypeOne,
                               tissueType = tissueTypeOne,
                               GlobalData = GlobalData,
                               session = session)

    cleanResTwo <- cleanInputs(primaryGene = geneTwo,
                               selectedSpecies = species,
                               sampleType = sampleTypeTwo,
                               tissueType = tissueTypeTwo,
                               GlobalData = GlobalData,
                               session = session)
    
    genesOfInterest <- c(cleanResOne$primaryGene, cleanResTwo$primaryGene)
    
    if (input$crossComparisonMode) {
      if (cleanResOne$selectedSpecies == "mmusculus" &
          length(unique(genesOfInterest)) == 1) {
        progress$close()
        
        showNotification(paste0("Normal vs Cancer group mode unavailable for",
                         " 'Mouse' due to sample number disparity. ",
                         "Select two different genes or select 'Human' samples."), 
                         type = "error", duration = 15)
        # Force stall with validate
        shiny::validate(
          need(expr = {geneOne != geneTwo}, 
              message = 
                "This message should not be visible to user.")
        )
        
      }
      # # Bug testing
      # genesOfInterest <- c("ATM", "SLC3A2")
      progress$inc(.1, message = "Running group mode ... ")
      progress$inc(.1, detail = "This may take ~1 minute to complete.")
      pairedRes <- correlationAnalyzeR::analyzeGenePairs(genesOfInterest = genesOfInterest, 
                                                         Species = cleanResOne$selectedSpecies,
                                                         returnDataOnly = T, 
                                                         crossCompareMode = T)
      dataOrig <- pairedRes$Correlations
      data <- cbind(rownames(dataOrig), dataOrig)
      colnames(data)[1] <- "geneName"
      # data <- data[which(data[,1] != cleanRes$primaryGene),]
      rownames(data) <- NULL
      data <- merge(x = cleanResOne$basicGeneInfo, y = data, by = "geneName")
      pairedRes[["Correlations"]] <- data
      res <- list("geneVsGeneResults" = pairedRes,
                  "species" = species,
                  "gseaType" = gseaType,
                  "pval" = pval,
                  "geneOne" = geneOne,
                  "sampleTypeOne" = sampleTypeOne,
                  "tissueTypeOne" = tissueTypeOne,
                  "geneTwo" = geneTwo,
                  "sampleTypeTwo" = sampleTypeTwo,
                  "tissueTypeTwo" = tissueTypeTwo,
                  "crossCompareMode" = TRUE,
                  "progress" = progress)
      
      res
    } else {
      progress$inc(.1, message = paste0("Calculating corGSEA for ", geneOne,
                                        " and ", geneTwo, " ... "))
      progress$inc(.1, detail = paste("This may take 1-2 minutes to complete."))
      
      genesOfInterest <- c(cleanResOne$primaryGene, cleanResTwo$primaryGene)
      Sample_Type <- c(cleanResOne$sampleType, cleanResTwo$sampleType)
      Tissue <- c(cleanResOne$tissueType, cleanResTwo$tissueType)
      gseaType <- tolower(gseaType)
      
      pairedRes <- correlationAnalyzeR::analyzeGenePairs(genesOfInterest = genesOfInterest, 
                                                         Sample_Type = Sample_Type,
                                                         Tissue = Tissue, 
                                                         Species = cleanResOne$selectedSpecies,
                                                         GSEA_Type = gseaType, 
                                                         returnDataOnly = T,
                                                         topPlots = F, 
                                                         runGSEA = T)
      dataOrig <- pairedRes$compared$correlations
      data <- cbind(rownames(dataOrig), dataOrig)
      colnames(data)[1] <- "geneName"
      # data <- data[which(data[,1] != cleanRes$primaryGene),]
      rownames(data) <- NULL
      data <- merge(x = cleanResOne$basicGeneInfo, y = data, by = "geneName")
      colnames(data)[c(4:7)] <- colnames(dataOrig)
      pairedRes[["processedCorrelationsFrame"]] <- data
      res <- list("geneVsGeneResults" = pairedRes,
                  "species" = species,
                  "gseaType" = gseaType,
                  "pval" = pval,
                  "geneOne" = geneOne,
                  "sampleTypeOne" = sampleTypeOne,
                  "tissueTypeOne" = tissueTypeOne,
                  "geneTwo" = geneTwo,
                  "sampleTypeTwo" = sampleTypeTwo,
                  "tissueTypeTwo" = tissueTypeTwo,
                  "progress" = progress)
      
      res
    }
  })
  return(data)
}
# Plotting/reporting
geneVsGeneModePlotsUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      uiOutput(outputId = ns("correlationsUI")),
      uiOutput(outputId = ns("comparedPathsUI")),
      downloadDataUI(ns("geneVsGeneModeDownloads")),
      br()
    )
  )
}


geneVsGeneModePlots <- function(input, output, session, 
                                    parent_session,
                                    GlobalData, dataTables) {
  
  observe({
    
    dataListReact <- dataTables[["geneVsGeneModeData"]]
    
    dataList <- dataListReact()
    
    species <- dataList[["species"]]
    geneOne <- dataList[["geneOne"]]
    tissueTypeOne <- dataList[["tissueTypeOne"]]
    sampleTypeOne <- dataList[["sampleTypeOne"]]
    geneTwo <- dataList[["geneTwo"]]
    tissueTypeTwo <- dataList[["tissueTypeTwo"]]
    sampleTypeTwo <- dataList[["sampleTypeTwo"]]
    progress <- dataList[["progress"]]
    pairedRes <- dataList[["geneVsGeneResults"]]
    # # Bug testing
    # pairedRes <- pairedRes
    if ("crossCompareMode" %in% names(dataList)) {
      progress$inc(.4, message = "Preparing group mode result files ... ")
      on.exit(progress$close())
      dataList <- pairedRes[["pairResList"]]
      mode <- pairedRes[["mode"]]
      correlations <- pairedRes[["Correlations"]]
      
      if (mode == "cross_geneVsGene") {
        uiName <- paste0(geneOne, " vs ", geneTwo)
        ht <- 30
      } else {
        uiName <- paste0(geneOne, " - normal vs. cancer")
        ht <- 14
      }
      downloadDataPairsCor <- list(
        "content" = correlations,
        "uiName" = paste0(uiName, " correlation data"),
        "file" = ".tsv"
      )
      plotListHeat <- list()
      plotListScatter <- list()
      for (i in 1:length(dataList)) {
        name <- names(dataList)[i]
        plotListScatter[[i]] <- dataList[[i]][["scatterPlot"]]
        plotListHeat[[i]] <- dataList[[i]][["heatMap"]][[4]]
        
      }
      
      ga <- ggpubr::ggarrange(plotlist = plotListScatter, ncol = 3)
      # output$scatterGrid <- renderPlot({
      #   ga
      # }, height = 1000)
      
      ge <- gridExtra::arrangeGrob(grobs = plotListHeat, ncol = 7)
      tmp <- paste0("www/tmp/", as.character(session$token)[1])
      tmpHeatFile <- file.path(tmp, paste0(uiName, " heatMap.pdf"))
      tmpHeatFile <- gsub(tmpHeatFile, pattern = " ", replacement = "_")
      ggplot2::ggsave(ge, filename = tmpHeatFile, height = ht, width = 40)
      tmpHeatFile <- gsub(tmpHeatFile, pattern = "www/", replacement = "")
      tmpscatterFile <- file.path(tmp, paste0(uiName, " scatterMap.pdf"))
      tmpscatterFile <- gsub(tmpscatterFile, pattern = " ", replacement = "_")
      ggpubr::ggexport(plotlist = plotListScatter, ncol = 4, 
                       filename = tmpscatterFile, height = 5, width = 20)
      tmpscatterFile <- gsub(tmpscatterFile, pattern = "www/", replacement = "")
      
      output$correlationsUI <- renderUI({
        ns <- session$ns
        tagList(
          hr(),
          h3("Compared correlation scatter plots"),
          hr(),
          fluidRow(
            column(12,
                tags$iframe(style="height:600px; width:100%",
                            src=tmpscatterFile))
          )
        )
      })
      
      
      compResTPM <- pairedRes[["singleGeneCrossCompareResults"]]
      if (mode == "cross_geneVsGene") {
        output$comparedPathsUI <- renderUI({
          ns <- session$ns
          tagList(
            hr(),
            h3("Compared correlation heatmaps"),
            hr(),
            fluidRow(
              column(12,
                     tags$iframe(style="height:600px; width:100%",
                                 src=tmpHeatFile))
            ),
            hr(),
            h3("Gene expression across groups"),
            hr(),
            fluidRow(style = "height:500px;",
                     column(width = 12,
                            plotOutput(ns("geneTPMBoxplot1")))),
            br(),
            fluidRow(style = "height:500px;",
                     column(width = 12,
                            plotOutput(ns("geneTPMBoxplot2"))))
          )
        })
        geneTPMBoxplot1 <- compResTPM[[1]][["TPM_boxPlot"]]
        geneTPMBoxplotData1 <- compResTPM[[1]][["TPM_DF"]]
        downloadsListTPM1 <- list("content" = geneTPMBoxplotData1,
                                  "uiName" = paste0(geneOne,
                                                    " normalized expression (TPM)"),
                                  "file" = ".tsv")
        
        output$geneTPMBoxplot1 <- renderPlot(height = 500, {
          geneTPMBoxplot1
        })
        geneTPMBoxplot2 <- compResTPM[[2]][["TPM_boxPlot"]]
        geneTPMBoxplotData2 <- compResTPM[[2]][["TPM_DF"]]
        downloadsListTPM2 <- list("content" = geneTPMBoxplotData2,
                                  "uiName" = paste0(geneTwo,
                                                    " normalized expression (TPM)"),
                                  "file" = ".tsv")
        
        output$geneTPMBoxplot2 <- renderPlot(height = 500, {
          geneTPMBoxplot2
        })
        downloadsList <- reactiveValues("correlationData" = downloadDataPairsCor,
                                        "geneOneTPM" = downloadsListTPM1,
                                        "geneTwoTPM" = downloadsListTPM2)
      } else {
        output$comparedPathsUI <- renderUI({
          ns <- session$ns
          tagList(
            hr(),
            h3("Compared correlation heatmaps"),
            hr(),
            fluidRow(
              column(12,
                     tags$iframe(style="height:600px; width:100%",
                                 src=tmpHeatFile))
            ),
            hr(),
            h3("Gene expression across groups"),
            hr(),
            fluidRow(style = "height:500px;",
                     column(width = 12,
                            plotOutput(ns("geneTPMBoxplot"))))
          )
        })
        geneTPMBoxplot <- compResTPM[[1]][["TPM_boxPlot"]]
        output$geneTPMBoxplot <- renderPlot(height = 500, {
          geneTPMBoxplot
        })
        geneTPMBoxplotData <- compResTPM[[1]][["TPM_DF"]]
        downloadsListTPM <- list("content" = geneTPMBoxplotData,
                                  "uiName" = paste0(uiName,
                                                    " normalized expression (TPM)"),
                                  "file" = ".tsv")
        
        downloadsList <- reactiveValues("correlationData" = downloadDataPairsCor,
                                        "geneTMP" = downloadsListTPM)
      }
      
      
      
      callModule(module = downloadData, id = "geneVsGeneModeDownloads", 
                 primaryName = uiName, downloadsListReact = downloadsList)
      
    } else {
      correlations <- pairedRes[["processedCorrelationsFrame"]]
      corrPlot <- pairedRes[["compared"]][["correlationPlot"]]
      heatGenes <- pairedRes[["compared"]][["correlationVarianceHeatmap"]]
      heatPaths <- pairedRes[["compared"]][["correlatedPathwaysHeatmap"]]
      correlatedPathwaysDF <- pairedRes[["compared"]][["correlatedPathwaysDataFrame"]]
      
      
      if (species == "Human") {
        GeneInfo <- GlobalData$HS_basicGeneInfo
      } else {
        GeneInfo <- GlobalData$MM_basicGeneInfo
      }
      tissueTypeOne <- gsub(tissueTypeOne, pattern = "0", replacement = " ")
      tissueTypeTwo <- gsub(tissueTypeTwo, pattern = "0", replacement = " ")
      longName <- ifelse((tissueTypeOne != tissueTypeTwo | 
                            sampleTypeOne != sampleTypeTwo),
                         yes = T, no = F)
      uiNameOne <- paste0(geneOne, " (",
                          tissueTypeOne, " - ",
                          sampleTypeOne, ")")
      fileNameOne <- paste0(geneOne, "_",
                            tissueTypeOne, "_",
                            sampleTypeOne)
      uiNameTwo <- paste0(geneTwo, " (",
                          tissueTypeTwo, " - ",
                          sampleTypeTwo, ")")
      fileNameTwo <- paste0(geneTwo, "_",
                            tissueTypeTwo, "_",
                            sampleTypeTwo)
      if (longName) {
        
        uiName <- paste0(uiNameOne, " vs ", uiNameTwo)
        fileName <- paste0(fileNameOne, "_vs_", fileNameTwo)
        
      } else {
        uiName <- paste0(geneOne, " vs ", geneTwo, "(",
                         tissueTypeTwo, " - ",
                         sampleTypeTwo, ")")
        fileName <- paste0(geneOne, "_vs_", geneTwo, "_",
                           tissueTypeTwo, "_",
                           sampleTypeTwo)
      }
      
      on.exit(progress$close())
      progress$inc(.4, message = "Returning results ... ")
      # Make a plot
      output$corrScatter <- renderPlot({
        corrPlot
      })
      corrPathDFReact <- reactive({
        # Setup dataframe to match plotly data
        correlatedPathwaysDT <- correlatedPathwaysDF[,c(1, 2, 6, 10, 11)]
        correlatedPathwaysDT <- correlatedPathwaysDT[order(correlatedPathwaysDT$NES_variance, 
                                                           decreasing = T),]
        
        colnames(correlatedPathwaysDT)[1] <- c("Pathway")
        rownames(correlatedPathwaysDT) <- NULL
        correlatedPathwaysDT
      })
      
      
      
      corrValDFReact <- reactive({
        # Setup dataframe to match plotly data
        corrValDF <- correlations
        corrValDF <- corrValDF[,c(1, 3:7)]
        corrValDF <- unique(corrValDF)
        corrValDF <- corrValDF[order(corrValDF$variance, decreasing = T),]
        corrValDF <- corrValDF[which(! corrValDF$geneName %in% colnames(corrValDF)),]
        rownames(corrValDF) <- NULL
        colnames(corrValDF)[c(3:6)] <- colnames(correlations)[c(4:7)]
        colnames(corrValDF)[3] <- convertToColnames(uiNameOne)
        colnames(corrValDF)[4] <- convertToColnames(uiNameTwo)
        corrValDF
      })
      
      downloadDataPairsCor <- list(
        "content" = corrValDFReact(),
        "uiName" = "Gene correlation values",
        "file" = ".tsv"
      )
      downloadDataPairsPath <- list(
        "content" = correlatedPathwaysDF,
        "uiName" = "Pathway correlation values",
        "file" = ".tsv"
      )
      
      output$correlationData <- renderDataTable(server = T, {
        
        corrValDF <- corrValDFReact()
        corrValDF[,c(3:6)] <- apply(corrValDF[,c(3:6)], 2, FUN = signif, digits = 3)
        
        cols <- colnames(corrValDF)
        cols[1] <- "Gene Name"
        cols[2] <- "Description"
        cols[3] <- uiNameOne
        cols[4] <- uiNameTwo
        cols[c(5:6)] <- stringr::str_to_title(cols[c(5:6)])
        
        # Replace gene name with HTML to call gene info modal
        corrValDF$geneName <- createGeneInfoLink(corrValDF$geneName)
        # Construct datatable
        DT_out <- datatable(corrValDF, selection = "single",
                            rownames = F, escape = F,  
                            colnames = cols,
                            options = list(dom = "ftprl",
                                           scrollX = TRUE,
                                           pageLength = 6)
        )
        DT_out
      }, escape = F)
      
      output$heatGenes <- renderPlot({
        heatGenes
      })
      
      output$correlationsUI <- renderUI({
        ns <- session$ns
        tagList(
          hr(),
          h3("Compared correlations"),
          hr(),
          fluidRow(
            column(width = 6, #title = "Correlation Histogram",
                   plotOutput(ns("corrScatter"))
            ),
            column(width = 6, #title = "Correlation Histogram",
                   plotOutput(ns("heatGenes"))
            )
          ),
          br(),
          br(),
          fluidRow(
            column(width =  12, #offset = 2, #title = "Correlation Data",
                   DT::dataTableOutput(ns("correlationData"))
            )
          )
        )
      })
      
      
      output$heatPaths <- renderPlot({
        heatPaths
      })
      
      output$correlatedPathwaysDF <- renderDataTable(server = T, {
        
        eres <- corrPathDFReact()
        cols <- colnames(eres)
        cols[1] <- "Pathway"
        cols <- gsub(x = cols, pattern = "_", replacement = " ")
        cols <- gsub(x = cols, pattern = "NES ", replacement = "")
        cols[2] <- uiNameOne
        cols[3] <- uiNameTwo
        cols[c(4:5)] <- stringr::str_to_title(cols[c(4:5)])
        
        eres <- eres[order(eres[,5], decreasing = T),]
        eres[,c(2:5)] <- apply(eres[,c(2:5)], 1:2, round, digits = 3)
        eresTitles <- eres[,1]
        eresTitles <- correlationAnalyzeR::fixStrings(eresTitles)
        eresTitles[which(nchar(eresTitles) > 45)] <- paste0(substr(eresTitles[which(nchar(eresTitles) > 45)],
                                                                   1, 41), "...")
        # Replace gene name with HTML to call gene info modal
        eres[,1] <- createGSEAInfoLink(val = eres[,1],
                                       valTitle = eresTitles)  
        rownames(eres) <- NULL
        # Construct datatable
        DT_out <- datatable(eres, 
                            rownames = F, escape = F,  
                            colnames = cols,
                            options = list(dom = "ftprl",
                                           scrollX = TRUE,
                                           pageLength = 6)
        )
        DT_out
      }, escape = F)

      output$comparedPathsUI <- renderUI({
        ns <- session$ns
        tagList(
          hr(),
          h3("Compared corGSEA results"),
          hr(),
          fluidRow(
            
            column(width = 8, offset = 2, #title = "Correlation Histogram",
                   plotOutput(ns("heatPaths"))
                   
            )
          ),
          br(),
          br(),
          fluidRow(
            column(width =  12, #offset = 3, #title = "Correlation Data",
                   DT::dataTableOutput(ns("correlatedPathwaysDF"))
            )
          )
          
        )
      })
      
      
      downloadsList <- reactiveValues("correlationData" = downloadDataPairsCor,
                                      "pathwayData" = downloadDataPairsPath
                                      )
      
      callModule(module = downloadData, id = "geneVsGeneModeDownloads", 
                 primaryName = uiName, downloadsListReact = downloadsList)
      
    }

    
  })
}


## geneVsGeneList-mode analysis ##
geneVsGeneListModeAnalysisUI <- function(id) {
  ns <- NS(id)
  tagList(
    singleGeneInputUI(ns("singleGeneInput"), 
                      "Primary gene"),
    multiGeneInputUI(ns("multiGeneInput"), 
                     "Secondary gene list", 
                     content = paste0("Enter a list of interesting genes (or an official MSIGDB geneset name) to",
                                      "  see how they correlate with the primary gene. Use significance testing to ",
                                      "see whether the correlation (absolute) is greater than random.")),
    popify(
      selectInput(inputId = ns("species"), label = "Select species",
                  choices = c("Human", "Mouse"), selected = "Human"),
      placement = "right", 
      title = "Select species", options=list(container="body"),
      content = paste0('Correlations were processed separately by species. ',
                       'Specify which correlations to analyze by choosing "Human" or "Mouse".')
    ),
    tissueTypeInputUI(ns("tissueTypeInput")),
    sampleTypeInputUI(ns("sampleTypeInput")),
    popify(
      checkboxInput(inputId = ns("sigTest"), label = "Test significance", value = TRUE),
      placement = "right", 
      title = "Significance testing", options=list(container="body"),
      content = paste0('If selected, permutation tests will be performed to determine whether ',
                       'secondary genes are significantly correlated with the primary gene ',
                       ' compared to random. NOTE: This test considers only absolute correlation values',
                       ' -- it does not consider positive or negative correlations differently.')
    ),
    fluidRow(
      column(4, actionButton(ns("do"), "Analyze"))
    )
  )
}
geneVsGeneListModeAnalysis <- function(input, output, session, 
                               parent_session, GlobalData) {
  
  species <- reactive({input$species})
  do <- reactive({input$do})
  
  primaryGene <- callModule(singleGeneInput, "singleGeneInput",
                            parent_session = parent_session,
                            GlobalData = GlobalData,
                            species = species)
  tissueType <- callModule(tissueTypeInput, "tissueTypeInput",
                           parent_session = parent_session,
                           GlobalData = GlobalData,
                           species = species)
  sampleType <- callModule(sampleTypeInput, "sampleTypeInput",
                           parent_session = parent_session,
                           GlobalData = GlobalData,
                           species = species, 
                           tissueType = tissueType)
  
  secondaryGenes <- callModule(multiGeneInput, "multiGeneInput")
  
  data <- reactiveVal()
  data <- eventReactive(eventExpr = input$do, {
    primaryGene <- primaryGene()
    secondaryGenes <- secondaryGenes()
    species <- input$species
    sampleType <- sampleType()
    sampleType <- tolower(sampleType)
    tissueType <- tissueType()
    tissueType <- tolower(tissueType)
    tissueType <- gsub(tissueType, pattern = " ", replacement = "0")
    sigTest <- input$sigTest
    
    # Clean secondaryGenes input
    secondaryGenes <- strsplit(secondaryGenes, split = "\n")
    secondaryGenes <- unlist(secondaryGenes, use.names = F)
    # Validate inputs
    shiny::validate(
      need(primaryGene != "", "Please select a gene")
    )
    shiny::validate(
      need(secondaryGenes != "", "Please provide secondary genes to compare against")
    )
    
    # Initialize progress object
    progress <- shiny::Progress$new()
    progress$set(message = "Validating inputs ... ", value = .1)
    
    cleanRes <- cleanInputs(primaryGene = primaryGene,
                            secondaryGenes = secondaryGenes,
                            selectedSpecies = species,
                            sampleType = sampleType,
                            tissueType = tissueType,
                            GlobalData = GlobalData,
                            session = session)
    pass <- 1
    if (is.null(cleanRes$secondaryGenes)) {
      print("FAIL")
      showNotification(ui = "No valid secondary genes provided.", 
                       duration = 8, type = 'error')
      pass <- 0
      progress$close()
    } else if (sigTest & length(unique(cleanRes$secondaryGenes)) < 2) {
      if (length(cleanRes$genesetInputs) != 1) {
        showNotification(ui = "Significance testing requires 2+ secondary genes", 
                         duration = 8, type = 'error')
        sigTest <- FALSE
      } 
    }
    shiny::validate(
      need(pass == 1, message = "No valid genes provided. ")
    )
    geneVsGeneListGenesList <- list(cleanRes$secondaryGenes)
    names(geneVsGeneListGenesList) <- cleanRes$primaryGene
    
    progress$inc(.2, message = "Analyzing correlations ... ")
    
    # # BugTesting
    # geneVsGeneListGenesList <- list("BRCA1" = c("ATM", "BRCA2", "BRCC3"))
    # cleanRes <- list("selectedSpecies" = "hsapiens",
    #                  "sampleType" = "Normal_Tissues")
    # sigTest <- F
    set.seed(1) #Reproducible
    data <- correlationAnalyzeR::geneVsGeneListAnalyze(pairedGenesList = geneVsGeneListGenesList, 
                                                       Tissue = cleanRes$tissueType,
                                                       Species = cleanRes$selectedSpecies, 
                                                       Sample_Type = cleanRes$sampleType, 
                                                       plotLabels = F, plotMaxMinCorr = T, 
                                                       sigTest = sigTest, returnDataOnly = T,
                                                       autoRug = T, plotTitle = F, onlyTop = F)
    data <- data[[1]]
    res <- list("geneVsGeneListModeData" = data,
                "primaryGene" = cleanRes$primaryGene,
                "tissueType" = cleanRes$tissueType,
                "sampleType" = cleanRes$sampleType,
                "species" = species,
                "sigTest" = sigTest,
                "progress" = progress)
    res
  })
  return(data)
}
# Plotting/reporting
geneVsGeneListModePlotsUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      uiOutput(outputId = ns("correlationsUI")),
      uiOutput(outputId = ns("sigTestPlotsUI")),
      downloadDataUI(ns("geneVsGeneListModeDownloads")),
      br()
    )
  )
}


geneVsGeneListModePlots <- function(input, output, session, 
                            parent_session,
                            GlobalData, dataTables) {
  
  observe({
    
    dataListReact <- dataTables[["geneVsGeneListModeData"]]
    dataList <- dataListReact()
    data <- dataList[[1]]
    species <- dataList[["species"]]
    primaryGene <- dataList[["primaryGene"]]
    tissueType <- dataList[["tissueType"]]
    sampleType <- dataList[["sampleType"]]
    if (species == "Human") {
      GeneInfo <- GlobalData$HS_basicGeneInfo
    } else {
      GeneInfo <- GlobalData$MM_basicGeneInfo
    }
    
    tissueType <- gsub(tissueType, pattern = "0", replacement = " ")
    uiName <- paste0(primaryGene, " (",
                     tissueType, " - ",
                     sampleType, ")")
    fileName <- paste0(primaryGene, "_",
                       tissueType, "_",
                       sampleType)
    
    progress <- dataList[["progress"]]
    on.exit(progress$close())
    progress$inc(.4, message = "Returning results ... ")
    # Make a plot
    output$geneHist <- renderPlotly({
      corrDataDF <- data$Correlation_histogram$data
      colnames(corrDataDF)[2] <- "vals"
      corrDataDF <- corrDataDF[order(corrDataDF$vals, decreasing = T),]
      a <- list(
        title = "Correlation values"
      )
      p <- ggpubr::gghistogram(data = corrDataDF, x = "vals", y = "..count..",
                               bins = 50, ylab = "Frequency\n",
                               title = uiName, 
                               xlab = paste0(primaryGene, " correlation values")) +
        ggplot2::scale_y_continuous(expand = c(0, 0))
      p <- ggplotly(p)
      corrDataDF2 <- corrDataDF[which(corrDataDF$secondaryGene),]
      p <- p %>% 
        add_segments(data = corrDataDF2, x = ~vals, 
                     xend = ~vals, y = 0, 
                     yend = 500, color = I('blue'),
                     text = ~paste('Gene: ', geneName, " - Value: ", round(vals, 3)),
                     opacity = .7) %>%
        layout(showlegend = FALSE, xaxis = a) %>%
        config(plot_ly(),
               toImageButtonOptions = list(
                 filename = paste0(fileName, 
                                   "_geneVsGeneListMode_correlationHistogram"),
                 format = "png",
                 width = 1000,
                 height = 600))
      s <- input$correlationData_rows_selected
      if (length(s)) {
        corrDataDF3 <- corrDataDF2[s,]
        p <- p %>% 
          add_segments(data = corrDataDF3, x = ~vals, 
                       text = ~paste('Gene: ', geneName, " - Value: ", round(vals, 3)),
                       xend = ~vals, y = 0, yend = 500) 
      } else {
        p <- p %>% highlight("plotly_selected", color = I('green'))
      }
      p
    })
    
    corrValDFReact <- reactive({
      # Setup dataframe to match plotly data
      corrValDF <- data$Correlation_Values
      corrValDF <- data.frame(geneName = names(corrValDF), 
                              Values = corrValDF, row.names = NULL)
      corrValDF <- merge(x = corrValDF, y = GeneInfo, all.x = T, by = "geneName")
      corrValDF <- corrValDF[,c(1, 4, 2)]
      corrValDF <- unique(corrValDF)
      corrValDF <- corrValDF[order(corrValDF$Values, decreasing = T),]
      rownames(corrValDF) <- NULL
      corrValDF <- unique(corrValDF)
      corrValDF
    })
    
    downloadDataPairs <- list(
      "content" = corrValDFReact(),
      "uiName" = "Correlation values",
      "file" = ".tsv"
    )
    downloadsList <- reactiveValues("correlationData" = downloadDataPairs)
    
    callModule(module = downloadData, id = "geneVsGeneListModeDownloads", 
               primaryName = uiName, downloadsListReact = downloadsList)
    
    # Make correlation values datatable
    output$correlationData <- renderDataTable(server = T, {
      
      corrValDF <- corrValDFReact()
      # Replace gene name with HTML to call gene info modal
      corrValDF$geneName <- createGeneInfoLink(corrValDF$geneName)
      # Construct datatable
      DT_out <- datatable(corrValDF, selection = "single",
                          rownames = F, escape = F,  
                          colnames = c("Gene Name", "Description",
                                       "Correlation Value"),
                          options = list(dom = "ftprl",
                                         scrollX = TRUE,
                                         pageLength = 6)
      )
      DT_out
    }, escape = F)
    # Render correlation data UI
    output$correlationsUI <- renderUI({
      ns <- session$ns
      tagList(
        hr(),
        h3("Correlation data"),
        hr(),
        fluidRow(
          column(width = 6, #title = "Correlation Histogram",
                 plotlyOutput(ns("geneHist"))
          ),
          column(width =  6, #title = "Correlation Data",
                 DT::dataTableOutput(ns("correlationData"))
          )
        )
      )
    })
    # Make a plot of significance tests
    if (length(data$sigTest)) {
      
      output$sigTestPlotsUI <- renderUI({
        ns <- session$ns
        tagList(
          
          fluidRow(
            
            hr(),
            h3("Significance test plot"),
            hr(),
            column(width = 8, 
                   plotlyOutput(ns("sigTestPlots"),
                                height = "100%")
            )
          )
        )
      })
      
      output$sigTestPlots <- renderPlotly({
        m <- list(
          l = 50,
          r = 50,
          b = 50,
          t = 50,
          pad = 4
        )
        
        p3 <- data$sigTest$tTest_pvalsPlot
        p3 <- ggplotly(p3)
        p3 <- p3 %>% layout(title = paste0(primaryGene, 
                                           " correlation with secondary genes"),
                            xaxis = list(title = 't.test p value')) %>%
          layout(autosize = F, margin = m) %>%
          config(plot_ly(), 
                 toImageButtonOptions = list(
                   filename = paste0(fileName, 
                                     "_correlation_sigTest"),
                   format = "png",
                   width = 1000,
                   height = 600))
        p3
      })
      
    } else {
      output$sigTestPlotsUI <- renderUI({
        ns <- session$ns
        tagList(
          fluidRow(
            hr(),
            h4(em("Select 'Test significance' to run permutation tests")),
            hr()
            )
          )
      })
    }
  })
}

## Topology-Mode modules ##
# Analysis
topologyModeAnalysisUI <- function(id) {
  ns <- NS(id)
  tagList(
    multiGeneInputUI(ns("multiGeneInput"), "Gene list",
                     content = paste0("Enter a list of interesting genes (or an official MSIGDB geneset name) to",
                                      " find functional domains, clusters, and enriched pathways.")),
    popify(
      selectInput(inputId = ns("species"), label = "Select species",
                  choices = c("Human", "Mouse"), selected = "Human"),
      placement = "right", 
      title = "Select species", options=list(container="body"),
      content = paste0('Correlations were processed separately by species. ',
                       'Specify which correlations to analyze by choosing "Human" or "Mouse".')
    ),
    tissueTypeInputUI(ns("tissueTypeInput")),
    sampleTypeInputUI(ns("sampleTypeInput")),
    popify(
      checkboxGroupInput(inputId = ns("crossComparisonType"), selected = c("PCA",
                                                                           "variantGenes",
                                                                           "pathwayEnrich"),
                         choiceNames = c("Dimension reduction",
                                         "Variant genes",
                                         "Pathway enrichment"),
                         label = "Choose analyses", choiceValues = c("PCA",
                                                                     "variantGenes",
                                                                     "pathwayEnrich")),
      placement = "right", 
      title = "Select analyses", options=list(container="body"),
      content = paste0('See "Topology analysis explained" for details.')
    ),
    fluidRow(
      column(4, actionButton(ns("do"), "Analyze"))
    )
  )
}

topologyModeAnalysis <- function(input, output, session, 
                                 parent_session, GlobalData) {
  
  species <- reactive({input$species})
  secondaryGenes <- callModule(multiGeneInput, "multiGeneInput")
  tissueType <- callModule(tissueTypeInput, "tissueTypeInput",
                           parent_session = parent_session,
                           GlobalData = GlobalData,
                           species = species)
  sampleType <- callModule(sampleTypeInput, "sampleTypeInput",
                           parent_session = parent_session,
                           GlobalData = GlobalData,
                           species = species, 
                           tissueType = tissueType)
  
  data <- reactiveVal()
  data <- eventReactive(eventExpr = input$do, {
    secondaryGenes <- secondaryGenes()
    species <- input$species
    tissueType <- tissueType()
    tissueType <- tolower(tissueType)
    tissueType <- gsub(tissueType, pattern = " ", replacement = "0")
    sampleType <- sampleType()
    sampleType <- tolower(sampleType)
    crossComparisonType <- input$crossComparisonType
    # Clean secondaryGenes input
    secondaryGenes <- strsplit(secondaryGenes, split = "\n")
    secondaryGenes <- unlist(secondaryGenes, use.names = F)
    # # Bug testing
    # species <- "Human"
    # sampleType <- "Normal_Tissues"
    # secondaryGenes <- c("ATM", "RIF1", "NFE2L2", "ATMIN", "BRCA1", "BRCA2")
    # GlobalData <- GlobalData
    # crossComparisonType <- c("PCA",
    #                          "variantGenes",
    #                          "pathwayEnrich")
    
    # Validate inputs
    shiny::validate(
      need(secondaryGenes != "", "Please select secondary genes/pathway")
    )
    progress <- shiny::Progress$new()
    progress$set(message = "Validating inputs ... ", value = .1)
    cleanRes <- cleanInputs(secondaryGenes = secondaryGenes,
                            selectedSpecies = species,
                            sampleType = sampleType,
                            tissueType = tissueType,
                            GlobalData = GlobalData,
                            session = session)
    # Return plots + correlation data
    progress$inc(.2, message = "Analyzing geneset topology ... ")
    # Warn if using less than 10 genes for pathway enrichment
    pass <- 1
    if (! cleanRes$geneSetInputType & length(cleanRes$secondaryGenes) < 3) {
      msg <- "Topology analysis requires 3+ valid genes."
      print("FAIL")
      showNotification(ui = msg, 
                       duration = 8, type = 'error')
      pass <- 0
      progress$close()
    } else if (! cleanRes$geneSetInputType & 
               length(cleanRes$secondaryGenes) < 10 & 
               "pathwayEnrich" %in% crossComparisonType) {
      msg <- "Pathway enrichment is recommended with at least 10 genes, otherwise results may not be informative."
      showNotification(msg, type = "warning", duration = 8)
    }
    shiny::validate(
      need(pass == 1, message = "No valid genes provided. ")
    )
    set.seed(1) # Reproducible
    data <- correlationAnalyzeR::analyzeGenesetTopology(genesOfInterest = cleanRes$secondaryGenes, 
                                                        Tissue = cleanRes$tissueType, 
                                                        alternativeTSNE = T, 
                                                        returnDataOnly = T, 
                                                        pathwayEnrichment = F, 
                                                        crossComparisonType = crossComparisonType,
                                                        Sample_Type = cleanRes$sampleType, 
                                                        Species = cleanRes$selectedSpecies)
    res <- list("topologyModeData" = data,
                "species" = species,
                "crossComparisonType" = crossComparisonType,
                "tissueType" = cleanRes$tissueType,
                "sampleType" = cleanRes$sampleType,
                "progress" = progress,
                "do" = reactive(input$do))
    res
  })
  return(data)
}
# Plotting/reporting
topologyModePlotsUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      br(),
      tabsetPanel(#type = "pills",
        id = ns("topologyNavs"), 
        tabPanel(
          title = "Topology analysis",
          value = ns("aboutTab"),
          fluidPage(
            br(),
            includeHTML("www/topologyAbout.html")
          )
        ),
        tabPanel(
          title = "Dimension reduction", 
          value = "dimReduction",
          fluidRow(
            br(),
            column(width = 6, 
                plotlyOutput(ns("PCAPlot"), width = "600px")
            ),
            column(width =  6, 
                DT::dataTableOutput(ns("PCADT"), width = "600px")
            )
          )
        ),
        tabPanel(
          title = "Variant genes", 
          value = "varGenes",
          fluidRow(
            br(),
            plotlyOutput(ns("varHeat"), width = "1200px")
            
          ),
          fluidRow(
            br(),
            column(width = 5, offset = 4,
                   sliderInput(inputId = ns("varHeatSelectSize"), 
                               label = "# shown genes above and below click", 
                               min = 0, max = 150, step = 1, value = 15))
          ),
          fluidRow(
            br(),
            column(width = 8, offset = 2,
                   DT::dataTableOutput(ns("varDT"))
            )
          )
        ),
        tabPanel(
          title = "Pathway enrichment", 
          value = "pathwayEnrich",
          fluidRow(
            br(),
            column(width = 10, offset = 1,
                   plotOutput(ns("pathwayEnrichPlot")))
          ),
          fluidRow(
            br(),
            column( width = 10, offset = 1,
                    DT::dataTableOutput(ns("pathwayEnrichDT"))
            )
          )
        ),
        tabPanel(
          title = "Downloads", 
          value = "downloadsTab",
          fluidRow(
            br(),
            downloadDataUI(ns("topologyModeDownloads"))
          )
        )
      )
    )
  )
}

topologyModePlots <- function(input, output, session,
                              parent_session,
                              GlobalData, dataTables) {
  
  # # Bug testing
  # dataTables <- list("singleModeData" = res)
  
  hideTab(inputId = "topologyNavs", target = "dimReduction", 
          session = session)
  hideTab(inputId = "topologyNavs", target = "varGenes", 
          session = session)
  hideTab(inputId = "topologyNavs", target = "pathwayEnrich", 
          session = session)
  hideTab(inputId = "topologyNavs", target = "downloadsTab", 
          session = session)
  
  observe({
    
    dataListReact <- dataTables[["topologyModeData"]]
    dataList <- dataListReact()
    do <- dataList$do
    observeEvent(eventExpr = dataList, {

      selectedSpecies <- dataList$species
      data <- dataList$topologyModeData
      tissueType <- dataList[["tissueType"]]
      sampleType <- dataList[["sampleType"]]
      tissueType <- gsub(tissueType, pattern = "0", replacement = " ")
      
      uiName <- paste0(" (",
                       tissueType, " - ",
                       sampleType, ")")
      fileName <- paste0(
                         tissueType, "_",
                         sampleType)
      
      if (selectedSpecies == "Human") {
        basicGeneInfo <- GlobalData$HS_basicGeneInfo
      } else {
        basicGeneInfo <- GlobalData$MM_basicGeneInfo
      }
      
      progress <- dataList$progress
      progress$inc(.3, message = "Returning results ... ")
      
      
      ##########################################################
      
      ##########################################################
      
      downloadsList <- reactiveValues()
      corrData <- data$Correlation_Data
      corrData$geneName <- rownames(corrData)
      n <- length(colnames(corrData))
      corrData <- corrData[,c(n, 1:(n-1))]
      downloadsList[["correlationData"]] <- list(
        "content" = corrData,
        "file" = ".tsv",
        "uiName" = paste0("Correlation values ", uiName)
      )
      if (length(names(data))) {
        showTab(inputId = "topologyNavs", 
                target = "downloadsTab", 
                session = session, select = F)
      }
      if ("PCA_data" %in% names(data)) {
        downloadsList[["pcaData"]] <- list(
          "content" = data$PCA_data,
          "file" = ".tsv",
          "uiName" = "PCA Data"
        )
      } else if ("TSNE_data" %in% names(data)) {
        downloadsList[["tsneData"]] <- list(
          "content" = data$TSNE_data,
          "file" = ".tsv",
          "uiName" = "TSNE Data"
        )
      }
      if ("variantGenesHeatmap" %in% names(data)) {
        ddata <- as.data.frame(data$variantGenesHeatmap_MAT)
        ddata$geneName <- rownames(ddata)
        n <- length(colnames(ddata))
        m <- n-1
        ddata <- ddata[,c(n, 1:m)]
        downloadsList[["variantGenesHeatmapMatrix"]] <- list(
          "content" = ddata,
          "file" = ".tsv",
          "uiName" = "Heatmap data matrix"
        )
      }
      if ("inputGenes_pathwayEnrich_data" %in% names(data)) {
        downloadsList[["pathwayEnrichmentData"]] <- list(
          "content" = as.data.frame(data$inputGenes_pathwayEnrich),
          "file" = ".tsv",
          "uiName" = "Pathway enrichment data"
        )
      }
      
      callModule(module = downloadData, id = "topologyModeDownloads", 
                 primaryName = "topologyMode", downloadsListReact = downloadsList)
      ###########################################################
      
      
      if ("PCA_plot" %in% names(data) | "TSNE_plot" %in% names(data)) {
        
        # Make the dimension reduction plot
        PCAPlot <- reactive({
          # If a PCA was run
          if ("PCA_plot" %in% names(data)) {
            if (data$clustered) {
              xaxistext <- data$PCA_plot$labels$x
              yaxistext <- data$PCA_plot$labels$y
              PCADat <- data$PCA_data
              p <- plot_ly(PCADat, x = ~PC1 , y = ~PC2, text = ~ geneNames,
                           mode = "markers", color = ~clusters, marker = list(size = 7)) %>% 
                layout(title = "PCA with clustering",
                       xaxis = list(title = xaxistext),
                       yaxis = list(title = yaxistext)) %>%
                config(plot_ly(),
                       toImageButtonOptions = list(filename = paste0(fileName, "_topologyMode_clusteredPCA.png")))
            } else {
              plt <- data$PCA_plot
              p <- ggplotly(plt)
              p <- p %>%
                config(plot_ly(), 
                       toImageButtonOptions = list(
                         filename = paste0(fileName, "_topologyMode_PCA.png"),
                         format = "png",
                         width = 800,
                         height = 600))
            }
          } else if ("TSNE_plot" %in% names(data)) {
            TSNEDat <- data$TSNE_data
            xaxistext <- data$TSNE_plot$labels$x
            yaxistext <- data$TSNE_plot$labels$y
            p <- plot_ly(TSNEDat, x = ~tsne1 , y = ~tsne2, text =~ geneNames,
                         mode = "markers", color = ~hclust, marker = list(size = 7)) 
            p <- layout(p, title = "TSNE with clustering", 
                        xaxis = list(title = xaxistext),
                        yaxis = list(title = yaxistext))
            p <- p %>%
              config(plot_ly(),
                     toImageButtonOptions = list(filename = paste0("topologyMode_TSNE.png")))
          }
          p
        })
        # Do not suspend when hidden by tab
        observeEvent(PCAPlot(), {
          output$PCAPlot <- renderPlotly({
            l <- PCAPlot()
            l$x$layout$width <- NULL
            l$x$layout$height <- NULL
            l$width <- NULL
            l$height <- NULL
            l
          })
          outputOptions(output, 'PCAPlot', suspendWhenHidden = FALSE)
        })
        
        PCADT <- reactive({
          # Get appropriate data
          if ("PCA_data" %in% names(data)) {
            if (data$clustered) {
              dt_data <- data$PCA_data
              colnames(dt_data)[1] <- "geneName"
              # Setup dataframe to match plotly data
              dt_data <- merge(y = dt_data, x = basicGeneInfo, all.y = T, by = "geneName")
              dt_data <- dt_data[order(dt_data$clusters, -dt_data$PC1),]
              rownames(dt_data) <- NULL
              dt_data <- dt_data[,c(-2)]
              # Replace gene name with HTML to call gene info modal
              dt_data$geneName <- createGeneInfoLink(dt_data$geneName)
              dt_data[,c(3,4)] <- apply(dt_data[,c(3,4)], 1:2, signif, 3)
              # Construct datatable
              dt_dataList <- list("data" = dt_data,
                                  "type" = "PCA",
                                  "cluster" = TRUE)
            } else {
              dt_data <- data$PCA_data
              dt_data <- dt_data[,c(3,1,2)]
              colnames(dt_data)[1] <- "geneName"
              # Setup dataframe to match plotly data
              dt_data <- merge(y = dt_data, x = basicGeneInfo, all.y = T, by = "geneName")
              dt_data <- dt_data[order(-dt_data$PC1),]
              dt_data <- dt_data[,c(-2)]
              dt_data[,c(3,4)] <- apply(dt_data[,c(3,4)], 1:2, signif, 3)
              # Replace gene name with HTML to call gene info modal
              dt_data$geneName <- createGeneInfoLink(dt_data$geneName)
              dt_data <- unique(dt_data)
              # Construct datatable
              dt_dataList <- list("data" = dt_data,
                                  "type" = "PCA",
                                  "cluster" = FALSE)
              
            }
            
          } else if ("TSNE_data" %in% names(data)) {
            dt_data <- data$TSNE_data
            colnames(dt_data)[1] <- "geneName"
            # Setup dataframe to match plotly data
            dt_data <- merge(y = dt_data, x = basicGeneInfo, all.y = T, by = "geneName")
            dt_data <- dt_data[order(dt_data$hclust, -dt_data$tsne1),]
            dt_data <- dt_data[,c(-2)]
            dt_data[,c(3,4)] <- apply(dt_data[,c(3,4)], 1:2, signif, 3)
            # Replace gene name with HTML to call gene info modal
            dt_data$geneName <- createGeneInfoLink(dt_data$geneName)
            dt_data <- unique(dt_data)
            dt_dataList <- list("data" = dt_data,
                                "type" = "TSNE",
                                "cluster" = TRUE)
          }
          dt_dataList
        })
        
        output$PCADT <- DT::renderDataTable(server = T, {
          # Make dimension reduction datatables output
          PCADT <- PCADT()
          dt_data <- PCADT$data
          type <- PCADT$type
          cluster <- PCADT$cluster

          if (type == "PCA" & ! cluster) {
            DT_out <- datatable(dt_data, selection = "single",
                                rownames = F, escape = F,
                                colnames = c("Gene Name",
                                             "Description", "PC1", "PC2"),
                                options = list(dom = "ftprl",
                                               scrollX = TRUE,
                                               pageLength = 6))
          } else if (type == "PCA" & cluster) {
            DT_out <- datatable(dt_data, selection = "single",
                                rownames = F, escape = F,  filter = 'top',
                                colnames = c("Gene Name",
                                             "Description", "PC1", "PC2", "Cluster"),
                                options = list(dom = "tprl",
                                               scrollX = TRUE,
                                               pageLength = 6))
          } else {
            DT_out <- datatable(dt_data, selection = "single",
                                rownames = F, escape = F,  filter = 'top',
                                colnames = c("Gene Name",
                                             "Description", "TSNE 1", "TSNE 2", "Cluster"),
                                options = list(dom = "tprl",
                                               scrollX = TRUE,
                                               pageLength = 6))
          }

        }, escape = F)
        outputOptions(output, 'PCADT', suspendWhenHidden = FALSE)
        
        # This observer is needed because of a bug/feature of shiny DT::
        # It will look for changes in PCADT and force the datatable to reload
        observeEvent(PCADT(), {
          # Make dimension reduction datatables output
          PCADT <- PCADT()
          dt_data <- PCADT$data
          type <- PCADT$type
          cluster <- PCADT$cluster
          PCADT_proxy <- dataTableProxy("PCADT", session = session,
                                          deferUntilFlush = TRUE)
          reloadData(PCADT_proxy)
          
        })
        
        
        showTab(inputId = "topologyNavs", 
                target = "dimReduction", 
                session = session, select = F)
        
      } else {
        hideTab(inputId = "topologyNavs", target = "dimReduction", 
                session = session)
      }
      
      if ("variantGenesHeatmap" %in% names(data)) {
        
        
        
        varHeat <- reactive({
          plt_dat <- data$variantGenesHeatmap_MAT
          # Center the scale
          n <- length(colnames(plt_dat))# Get number of samples
          width <- n*50
          if (width < 800) {
            width <- 800
          } else if (width > 6000) {
            width <- 6000
          }
          mini <- min(plt_dat)
          maxi <- max(plt_dat)
          newVal <- max(c(abs(mini), maxi))
          p <- heatmaply(plt_dat, hide_colorbar = TRUE, 
                         limits = c(-1*newVal, newVal), 
                         colors = gplots::greenred(100), showticklabels = c(T, F))
          p <- p %>%
            config(plot_ly(),
                   toImageButtonOptions = list(
                     filename = paste0(fileName, "_topologyMode_variantGenesHeatmap.png"),
                     format = "png",
                     width = width,
                     height = 500))
          p
        })
        output$varHeat <- renderPlotly({
          withProgress(value = .4, message = "Rendering interactive heatmap ... ", {
            varHeat()
            
          })
        })
        outputOptions(output, 'varHeat', suspendWhenHidden = FALSE)
        
        # Get indices of plotly-selected genes
        selection_varHeat <- reactive({
          s <- event_data("plotly_click")
          df <- data.frame(s)
          df
        })
        
        varDT <- reactive({
          p <- varHeat()
          plot_text <- p$x$data[[4]]$text
          plot_coord_x <- p$x$data[[4]]$x
          plot_coord_y <- p$x$data[[4]]$y
          plot_coord_z <- p$x$data[[4]]$z
          plot_text_df <- as.data.frame(plot_text)
          genes <- strsplit(substr(as.character(plot_text_df[,1]), 6, 100), "<")
          genes <- sapply(genes, "[[", 1)
          genesDF <- data.frame(geneName = genes, y_coord = plot_coord_y)
          
          dt_data <- unique(basicGeneInfo[,c(3,2)])
          dt_data <- merge(x = dt_data, y = genesDF, by = "geneName")
          dt_data <- dt_data[order(dt_data$y_coord),]
          s <- selection_varHeat()
          if (length(s)) {
            x <- s$x[1]
            y <- s$y[1]
            z <- s$z[1]
            
            plot_pinpoint <- as.character(plot_text_df[y,x])
            selectSize <- input$varHeatSelectSize
            y_pos_vec <- c(y-selectSize, y + selectSize)
            y_pos_vec[which(y_pos_vec < 1)] <- 1
            y_pos_vec[which(y_pos_vec > 1500)] <- 1500
            plot_pinpoints <- as.character(plot_text_df[c(y_pos_vec[1]:y_pos_vec[2]),x])
            genes <- strsplit(substr(plot_pinpoints, 6, 100), "<")
            genes <- sapply(genes, "[[", 1)
            dt_data <- dt_data[which(dt_data$geneName %in% genes),]
          }
          dt_data <- unique(dt_data)
          dt_data
        })
        # Make dimension reduction datatables output
        output$varDT <- renderDataTable(server = T, {
          # Data inputs
          # Varheat data
          dt_data <- varDT()
          DT_out <- datatable(dt_data, selection = "single",
                              rownames = F, escape = F,  
                              colnames = c("Gene Name", 
                                           "Description",
                                           "Heatmap Y Position"),
                              options = list(dom = "tprl",
                                             scrollX = F,
                                             pageLength = 6))
          
          
          DT_out
          
        }, escape = F)
        
        showTab(inputId = "topologyNavs", 
                target = "varGenes", select = F,
                session = session)
        
        observeEvent(varDT(), {
          varHeat_proxy <- dataTableProxy("varDT", session = session,
                                                  deferUntilFlush = TRUE)
          reloadData(varHeat_proxy)
          
        })
      } else {
        hideTab(inputId = "topologyNavs", target = "varGenes", 
                session = session)
      }
      
      if ("inputGenes_pathwayEnrich" %in% names(data)) {
        # Get the pathway enrichment results and display dotplot
        output$pathwayEnrichPlot <- renderPlot({
          eres <- data$inputGenes_pathwayEnrich_data
          shiny::validate(need(length(eres),
                               "No results returned at selected p value cutoff. Please increase it."))
          dp <- data$inputGenes_pathwayEnrich_dotplot
          dp
        })
        
        pathwayEnrichDT <- reactive({
          eres <- data$inputGenes_pathwayEnrich_data
          eres <- eres[,c(2, 3, 5, 6, 8)]
          eres <- eres[order(eres[,3], decreasing = F),]
          if (length(eres$Description) > 100) {
            eres <- eres[c(1:100),]
          }
          eres[,c(3,4)] <- apply(eres[,c(3,4)], 1:2, round, digits = 4)
          eresTitles <- eres$Description
          eresTitles <- correlationAnalyzeR::fixStrings(eresTitles)
          eresTitles[which(nchar(eresTitles) > 60)] <- paste0(substr(eresTitles[which(nchar(eresTitles) > 60)], 1, 57), "...")
          eres$Description <- createGSEAInfoLink(eres$Description, eresTitles)
          eres
        })
        # Display the pathway enrichment data
        output$pathwayEnrichDT <- renderDataTable(server = T, {
          eres <- pathwayEnrichDT()
          datatable(eres, rownames = F, escape = F, 
                    options = list(
                      pageLength = 8,
                      autoWidth = TRUE,
                      columnDefs = list(list(width = '200px', targets = c(4)),
                                        list(width = '100px', targets = c(1,2,3))),
                      dom = "ftprl",
                      scrollX = TRUE
                    ),
                    colnames = c("Pathway", "Gene ratio",
                                 "Pval", "Padj", "Genes included"))
        }, escape = F)
        outputOptions(output, 'pathwayEnrichDT', suspendWhenHidden = FALSE)
        
        
        showTab(inputId = "topologyNavs", 
                target = "pathwayEnrich", select = F,
                session = session)
        
        # This observer is needed because of a bug/feature of shiny DT::
        # It will look for changes in PCADT and force the datatable to reload
        observeEvent(pathwayEnrichDT(), {
          pathwayEnrichDT_proxy <- dataTableProxy("pathwayEnrichDT", session = session,
                                                  deferUntilFlush = TRUE)
          reloadData(pathwayEnrichDT_proxy)
          
        })
        
      } else {
        hideTab(inputId = "topologyNavs", target = "pathwayEnrich", 
                session = session)
      }
      
      progress$close()
    })
    
  })
}




