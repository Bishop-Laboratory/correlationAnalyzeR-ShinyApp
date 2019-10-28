
## Generic helper modules ##
# Input types
tissueTypeInputUI <- function(id) {
  ns <- NS(id)
  selectizeInput(inputId = ns("tissueType"), label = "Select tissue type",
                 choices = c("All"), 
                 multiple = F)
}
tissueTypeInput <- function(input, output, session, 
                            parent_session,
                            mouseTissueOptions,
                            humanTissueOptions,
                            species, 
                            crossComparisonMode = FALSE) {
  # Get data objects
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
      # mTisOpt <- gsub(mTisOpt, pattern = "repiratory", replacement = "respiratory")
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
  selectizeInput(inputId = ns("sampleType"), label = "Select sample type",
                 choices = c("Normal", "Cancer"), selected = "Normal",
                 multiple = F)
}
sampleTypeInput <- function(input, output, session, 
                            parent_session,
                            mouseTissueOptions,
                            humanTissueOptions,
                            species, tissueType) {
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
# Input function for single genes
singleGeneInputUI <- function(id, label) {
  ns <- NS(id)
  selectizeInput(inputId = ns("primaryGene"), label = label,
                 choices = c("Loading ..."), selected = "Loading ...",
                 multiple = F, options = list(maxOptions = 100))
}

singleGeneInput <- function(input, output, session, 
                            parent_session,
                            mouseGeneOptions, 
                            humanGeneOptions, 
                            species, 
                            selected = NULL) {
  # Observe for the selected species
  # If the selected species changes to mouse, update the selectize options
  observe({
    speciesSelected <- species()
    if (is.null(selected)) {
      selected <- c("BRCA1", "Brca1")
    }
    if (speciesSelected == "Mouse") {
      updateSelectizeInput(session = session,
                           inputId = 'primaryGene',
                           choices = mouseGeneOptions,
                           server = TRUE, selected = selected[2],
                           options = list(maxOptions = 100))
      
    } else if (speciesSelected == "Human") {
      updateSelectizeInput(session = session,
                           inputId = 'primaryGene',
                           choices = humanGeneOptions,
                           server = TRUE, selected = selected[1],
                           options = list(maxOptions = 100))
    }
  })
  # Pass the value of 'primaryGene' backwards to the parent module
  primaryGene <- reactive({input$primaryGene})
  return(primaryGene)
}

multiGeneInputUI <- function(id, label) {
  ns <- NS(id)
  textAreaInput(inputId = ns("secondaryGenes"), resize = "none",
                label = label, height = "150px", 
                placeholder = paste("ATM\nBRCA1\nTP53\nFANCA\nATR\n...",
                                    sep=""))
}

multiGeneInput <- function(input, output, session) {
  secondaryGenes <- reactive({input$secondaryGenes})
  return(secondaryGenes)
}
# Downloads module
downloadDataUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("downloadBox"))
}

downloadData <- function(input, output, session, invalidater = FALSE,
                         primaryName, downloadsListReact) {
  
  downloadBoxUIReact <- reactive({
    req(downloadsListReact)
    downloadsList <- reactiveValuesToList(downloadsListReact)
    if ("init" %in% names(downloadsList)) {
      req((! downloadsListReact$init))
      req((downloadsListReact$ready))
    }
    ns <- session$ns
    goodInd <- which(! names(downloadsList) %in% c("init", "ready") &
                       lengths(downloadsList) != 0)
    downloadsList <- downloadsList[goodInd]
    downloadBoxUI <- tagList()
    downloadBoxUI <- lapply(1:length(names(downloadsList)), function(i) {
      downloadBoxUI <- tagList(
        downloadBoxUI,
        fluidRow(
          column(
            width = 10,
            h4(HTML(downloadsList[[i]][["uiName"]]))
          ),
          column(
            width = 2,
            downloadButton(outputId = ns(paste0("download", i)))
          )
        ),
        hr(style = "margin-top: 10px; margin-bottom: 10px; border-top: 1px dashed #eeeeee;")
      )
      return(downloadBoxUI)
    })
    if (invalidater) {
      # print("Downloads invalidating")
      invalidateLater(250)
    }
    downloadBoxUI
  })
  
  output$downloadBox <- renderUI({
    ns <- session$ns
    tagList(
      br(),
      br(),
      br(),
      fluidRow(
        column( style = paste0("background-color: #F5F5F5; border-radius:",
                               " 50px; font-weight: bold; border: 1px solid #e3e3e3;",
                               "padding-top: 20px; ", "padding-left: 80px;",
                               "box-shadow: inset 0 1px 1px rgba(0,0,0,0.05);",
                               " padding-bottom: 20px; ", "padding-right: 80px;"),
                width = 10, offset = 1,
                h2("Downloads"),
                hr(),
                fluidRow(
                  column(width = 12,
                         downloadBoxUIReact())
                ))
      
      ),      
      br()
    )
  })
  
  observe({
    req(downloadsListReact)
    req(downloadBoxUIReact)
    req(downloadsListReact)
    downloadsList <- reactiveValuesToList(downloadsListReact)
    if ("init" %in% names(downloadsList)) {
      req((! downloadsListReact$init))
      req((downloadsListReact$ready))
    }
    goodInd <- which(! names(downloadsList) %in% c("init", "ready") &
                       lengths(downloadsList) != 0)
    downloadsList <- downloadsList[goodInd]
    lapply(1:length(names(downloadsList)), function(i) {
      if ("filePrefix" %in% names(downloadsList[[i]])) {
        fileName <- paste0(primaryName, "_", 
                           downloadsList[[i]]$filePrefix,
                           downloadsList[[i]]$file)
      } else {
        fileName <- paste0(primaryName, "_", 
                           names(downloadsList)[i],
                           downloadsList[[i]]$file)
      }
      output[[paste0("download", i)]] <- downloadHandler(
        filename = function() {
          fileName
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

## Single-Mode analysis ##
singleModeAnalysisUI <- function(id) {
  ns <- NS(id)
  tagList(
    singleGeneInputUI(ns("singleGeneInput"), "Select gene"),
    selectInput(inputId = ns("species"), label = "Select species",
                choices = c("Human", "Mouse"), selected = "Human"),
    tissueTypeInputUI(ns("tissueTypeInput")),
    sampleTypeInputUI(ns("sampleTypeInput")),
    popify(#trigger = "focus",
      radioButtons(inputId = ns("gseaType"), label = "corGSEA type",
                   choices = c("Simple", "Complex", "None")),
      placement = "right", 
      title = 'Select corGSEA type', options=list(container="body"),
      content = paste0('"Simple" contains commonly-used genesets',
                       ' (MSIGDB genesets "H", "C1", "C2", "C5", and "C6").',
                       ' "Complex" uses all MSIGDB genesets.')
    ), 
    popify(title = "Group mode", #trigger = "focus",
           placement = "right", options=list(container="body"),
           switchInput(ns("crossComparisonMode"), value = FALSE, label = "Group mode"),
           content = paste0('Shows correlations across multiple tissue-disease groups.')
    ),
    conditionalPanel(condition = "input.crossComparisonMode && input.species == 'Human'", 
                     ns = ns,
                     radioButtons(inputId = ns("crossComparisonModeTypeHuman"), 
                                  label = "Group mode type", selected = "Normal",
                                  choices = c("Normal", "Cancer", "All"))),
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
                               parent_session, GlobalData, pool) {
  
  species <- reactive({input$species})
  primaryGene <- callModule(singleGeneInput, "singleGeneInput",
                            parent_session = parent_session,
                            humanGeneOptions = GlobalData$humanGeneOptions,
                            mouseGeneOptions = GlobalData$mouseGeneOptions,
                            species = species)
  tissueType <- callModule(tissueTypeInput, "tissueTypeInput",
                           parent_session = parent_session,
                           mouseTissueOptions = GlobalData$mouseTissueOptions,
                           humanTissueOptions = GlobalData$humanTissueOptions,
                           species = species,
                           crossComparisonMode = input$crossComparisonMode)
  sampleType <- callModule(sampleTypeInput, "sampleTypeInput",
                           parent_session = parent_session,
                           mouseTissueOptions = GlobalData$mouseTissueOptions,
                           humanTissueOptions = GlobalData$humanTissueOptions,
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
                             mouseTissueOptions = GlobalData$mouseTissueOptions,
                             humanTissueOptions = GlobalData$humanTissueOptions,
                             species = species,
                             crossComparisonMode = input$crossComparisonMode)
    sampleType <- callModule(sampleTypeInput, "sampleTypeInput",
                             parent_session = parent_session,
                             mouseTissueOptions = GlobalData$mouseTissueOptions,
                             humanTissueOptions = GlobalData$humanTissueOptions,
                             species = species, 
                             tissueType = tissueType)
  })
  
  data <- eventReactive(eventExpr = input$do, {
    primaryGene <- primaryGene()
    shiny::validate(need(primaryGene != "Loading ...", 
                         label = "Please select a gene."))
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
    # sampleType <- "normal"
    # gseaType <- "Simple"
    # tissueType <- "all"
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
                            session = session,
                            pool = pool)
    
    if (input$crossComparisonMode) {
      progress$inc(.2, message = paste0("Starting group comparisons for ", 
                                        primaryGene, " ... "))
      if (input$species == "Human") {
        whichCompareGroups <- input$crossComparisonModeTypeHuman
      } else {
        whichCompareGroups <- input$crossComparisonModeTypeMouse
      }
      
      resList <- correlationAnalyzeR::analyzeSingleGenes(
        Species = cleanRes$selectedSpecies,
        runGSEA = F, crossCompareMode = T,
        returnDataOnly = T, pool = pool,
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
      print("End of single mode analysis memory (cross compare): ")
      print(pryr::mem_used())
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
      runGSEA <- ifelse(gseaType != "None", T, F)
      
      if (runGSEA) {
        progress$inc(.1, message = paste0("Calculating corGSEA for ", 
                                          primaryGene, " ... "))
        progress$inc(.1, detail = "This may take ~1 minute.")
      } else {
        progress$inc(.1, message = paste0("Gathering correlations for ", 
                                          primaryGene, " ... "))
      }
      
      resList <- correlationAnalyzeR::analyzeSingleGenes(pool = pool,
        genesOfInterest = cleanRes$primaryGene, Species = cleanRes$selectedSpecies,
        Sample_Type = cleanRes$sampleType, GSEA_Type = tolower(gseaType), 
        Tissue = cleanRes$tissueType,crossCompareMode = FALSE, 
        # nperm = 500, sampler = T,
        runGSEA = runGSEA, topPlots = F, returnDataOnly = T
      )
      progress$inc(.3, detail = "Returning results.")
      data <- resList[["correlations"]]
      data <- cbind(rownames(data), data)
      colnames(data)[1] <- "geneName"
      # data <- data[which(data[,1] != "BRCA1"),]
      data <- data[which(data[,1] != cleanRes$primaryGene),]
      rownames(data) <- NULL
      # data <- merge(x = GlobalData$HS_basicGeneInfo, y = data, by = "geneName")
      data <- merge(x = cleanRes$basicGeneInfo, y = data, by = "geneName")
      data <- data[order(data[,4], decreasing = T),]
      resList[["correlations"]] <- data
      print("End of single mode analysis memory: ")
      print(pryr::mem_used())
      # Enable the download button
      res <- list("correlationData" = resList,
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
      downloadDataUI(ns("singleModeDownloads"))
    )
  )
}

singleModePlots <- function(input, output, session, 
                            parent_session, dataTables) {
  
  # For group and normal mode
  primaryGene <- reactiveVal()
  uiName <- reactiveVal()
  fileName <- reactiveVal()
  correlations <- reactiveVal()
  species <- reactiveVal()
  downloadsList <- reactiveValues()
  processed <- reactiveVal()
  preprocessed <- reactiveVal()
  corrReady <- reactiveVal()
  
  # Only for group mode
  groupMode <- reactiveVal()
  whichCompareGroups <- reactiveVal()
  heatMapDat <- reactiveVal()
  heatMapSmall <- reactiveVal()
  geneTPMBoxplot <- reactiveVal()
  geneTPMBoxplotData <- reactiveVal()
  heatMap <- reactiveVal()
  
  # Only for normal mode
  gseaRun <- reactiveVal()
  sampleType <- reactiveVal()
  eresDownload <- reactiveVal()
  eresCor <- reactiveVal()
  eresCorDownload <- reactiveVal()
  tissueType <- reactiveVal()
  EGMT <- reactiveVal()
  gseaDTReady <- reactiveVal()
  
  observeEvent(eventExpr = dataTables$singleModeData(), {
    dataList <- dataTables$singleModeData()
    processed(FALSE)
    heatMapDat(FALSE)
    uiName(NULL)
    fileName(NULL)
    corrReady(FALSE)
    preprocessed(FALSE)
    gseaDTReady(FALSE)
    
    resList <- dataList[[1]]
    ns <- session$ns
    downloadsList$init <- FALSE
    downloadsList$ready <- FALSE
    downloadsList$gseaData <- NULL
    downloadsList$correlationsGSEA <- NULL
    downloadsList$correlationData <- NULL
    downloadsList$groupModeTPM <- NULL
    
    # Assign values relevant to both modes
    
    primaryGene(dataList[["primaryGene"]])
    
    species(dataList[["species"]])
    # Assign values in a group-specific manner
    if ("whichCompareGroups" %in% names(dataList)) {
      groupMode(TRUE)
      whichCompareGroups(dataList[["whichCompareGroups"]])
      resList <- resList[[1]]
      correlations(resList[["correlations"]])
      # Assign Group-mode-only values
      uiNameRaw <- paste0(dataList[["primaryGene"]]," ",
                          tolower(dataList[["whichCompareGroups"]]),
                          " groups")
      fileNameRaw <- paste0(dataList[["primaryGene"]], "_",
                            tolower(dataList[["whichCompareGroups"]]), "_",
                            "groupMode")
      heatMapDat(resList[["heatmapBigData"]])
      # heatMap(resList[["heatmapBig"]])
      heatMapSmall(resList[["heatmapSmall"]])
      tpmBP <- resList[["TPM_boxPlot"]]
      tpmBP <- tpmBP + ggpubr::rotate_x_text(45, vjust = 1)
      geneTPMBoxplot(tpmBP)
      geneTPMBoxplotData(resList[["TPM_DF"]])
      dataTables$singleModeData()$progress$inc(.2,
                                               message = "Rendering interactive heatmap ... ")
      uiName(uiNameRaw)
      fileName(fileNameRaw)
      plt_dat <- heatMapDat()[c(1:200),]
      # Center the scale
      n <- length(colnames(plt_dat))# Get number of samples
      width <- n*50
      if (width < 800) {
        width <- 800
      } else if (width > 6000) {
        width <- 6000
      }
      newNamesRaw <- colnames(plt_dat)
      newNamesRaw <- strsplit(newNamesRaw, split = "_")
      newNames <- sapply(newNamesRaw, "[[", 2)
      newNames <- gsub(newNames, pattern = "0", replacement = " ")
      newNames <- stringr::str_to_title(newNames)
      if (whichCompareGroups() == "All") {
        newNames2 <- sapply(newNamesRaw, "[[", 3)
        newNames2 <- stringr::str_to_title(newNames2)
        newNames <- paste0(newNames, " - ", newNames2)
      }
      mini <- min(plt_dat)
      maxi <- max(plt_dat)
      newVal <- max(c(abs(mini), maxi))
      plt <- heatmaply(plt_dat, hide_colorbar = TRUE,
                     limits = c(-1*newVal, newVal),# plot_method = "plotly",
                     branches_lwd = .3,
                     colors =  colorRampPalette(rev(RColorBrewer::brewer.pal(n = 7, name =
                                                                               "RdYlBu")))(100),
                     labCol = newNames,
                     showticklabels = c(T, F)) %>%
        layout(height=500)
      plt <- plt %>%
        config(plot_ly(), displaylogo = F,
               modeBarButtonsToRemove = list(
                 'hoverCompareCartesian',
                 'hoverClosestCartesian',
                 'zoomIn2d', 'zoomOut2d',
                 'lasso2d'),
               toImageButtonOptions= list(filename = paste0(fileName(),
                                                            "_heatMap.png"),
                                          format = "png",
                                          width = width,
                                          height = 500))
      heatMap(plt)
    } else {
      # Assign normal-mode only values
      groupMode(FALSE)
      correlations(resList[["correlations"]])
      sampleType(dataList[['sampleType']])
      tissueTypeRaw <- dataList[['tissueType']]
      tissueTypeRaw <- gsub(tissueTypeRaw, pattern = "0", replacement = " ")
      uiNameRaw <- paste0(dataList[["primaryGene"]], " (",
                          stringr::str_to_title(tissueTypeRaw), " - ",
                          stringr::str_to_title(dataList[['sampleType']]), ")")
      fileNameRaw <- paste0(dataList[["primaryGene"]], "_",
                            tissueTypeRaw, "_",
                            dataList[['sampleType']])
      tissueType(tissueTypeRaw)
      if ("GSEA" %in% names(resList[[1]])) {
        gseaRun(T)
        eres <- resList[[1]]$GSEA$eres
        EGMT(resList[[1]]$GSEA$EGMT)
        minp <- min(eres$p.adjust)
        if (minp > .05) {
          msg <- paste0("No results returned at p value of .05", 
                        ". corGSEA p value cutoff was raised to accommodate.")
          shiny::showNotification(ui = msg, type = "warning")
        }
        # eres <- eres[which(abs(eres[,5]) > 2),]
        eres <- eres[order(eres[,5], decreasing = T),]
        eresDownload(eres) # Set download eres value
        eres[,c(5,6,7)] <- apply(eres[,c(5,6,7)], 1:2, round, digits = 5)
        eresCor(eres) # Set frame for correlation download
      } else {
        gseaRun(F)
        eresCor(NULL)
        eresDownload(NULL)
        eresCorDownload(NULL)
        EGMT(NULL)
        downloadsList$gseaData <- NULL
        downloadsList$correlationsGSEA <- NULL
        
      }
      heatMap(NULL)
      uiName(uiNameRaw)
      fileName(fileNameRaw)
    }
    preprocessed(TRUE)
    print("End of pre-processing")
    
  })
  
  # Render UI functions
  output$correlationsUI <- renderUI({
    req(dataTables$singleModeData())
    req(! is.null(groupMode()))
    ns <- session$ns
    if (! groupMode()) {
      req(! groupMode())
      if(! isolate({corrReady()})) {
        invalidateLater(500)
      }
      tagList(
        h1("Single Gene Results"),
        br(),
        hr(),
        h2("Co-expression correlations"),
        hr(),
        fluidRow(
          column(width = 6, 
                 plotOutput(ns("geneHist"))
          ),
          column(width =  6, 
                 DT::dataTableOutput(ns("correlationData"))
          )
        )
      )
    } else {
      req(groupMode())
      
      tagList(
        h1("Single Gene Results"),
        br(),
        hr(),
        h2("Variant correlations across groups"),
        hr(),
        fluidRow( style = "height: 500px;",
                  column(width = 12, #title = "Correlation Histogram",
                         withSpinner(plotlyOutput(ns("heatMap")),
                                     type = 7))
        )
      )
    }
  })
  output$gseaUI <- renderUI({
    req(preprocessed())
    ns <- session$ns
    if (! groupMode()) {
      if (gseaRun()) {
        req(primaryGene())
        req(! is.null(eresCor()))
        tagList(
          hr(),
          h2("corGSEA results"),
          hr(),
          fluidRow(
            column(width = 6, 
                   withSpinner(plotOutput(ns("plotGSEA")), type = 7)
            ),
            column(width = 6, 
                   DT::dataTableOutput(ns("gseaData"))
            )
          )
        )
      } else {
        invalidateLater(100)
        tagList(
          hr(),
          h4(em("Choose a 'corGSEA type' to analyze correlated pathways.")),
          hr()
        )
      }
    } else {
      tagList(
        hr(),
        h2("Top variant correlations"),
        hr(),
        fluidRow( #style = "height:500px;",
                  column(width = 12,
                         plotOutput(ns("heatMapSmall")))
        ),
        hr(),
        h2("Gene expression across groups"),
        hr(),
        fluidRow(#style = "height:500px;",
                 column(width = 12,
                        plotOutput(ns("geneTPMBoxplot"))))
      )
    }
  })
  output$heatMap <- renderPlotly({
    req(processed())
    req(groupMode())
    heatMap()
  })

  
  output$heatMapSmall <- renderPlot({heatMapSmall()})
  output$geneTPMBoxplot <- renderPlot({geneTPMBoxplot()})
  
  # Normal-mode UI elements
  output$correlationData <- DT::renderDataTable({
    corrReady(TRUE)
    req((! groupMode()))
    req(correlations())
    req(processed())
    correlationData <- correlations()
    colnames(correlationData)[4] <- "vals"
    correlationData$geneName <- createGeneInfoLink(correlationData$geneName)
    d2 <- correlationData
    d2 <- d2[,c(1,3,4)]
    d2 <- unique(d2)
    d2
  }, server = T, 
  selection = list(mode = "single", selected = 1),
  rownames = F, escape = F,
  options = list(
    pageLength = 6,
    dom = "ftprl",
    scrollX = TRUE),  colnames = c("Gene Name", 
                                   "Description", "Correlation Value"))
  geneHist <- reactive({
    req((! groupMode()))
    req(processed())
    correlationData <- correlations()
    colnames(correlationData)[4] <- "vals"
    d2 <- correlationData[,c(1,3,4)]
    d2 <- unique(d2)
    colnames(d2)[3] <- "vals"
    s <- input$correlationData_rows_selected
    p <- ggpubr::gghistogram(data = correlationData, x = "vals", y = "..count..",
                             bins = 60, ylab = "Frequency\n",
                             title = uiName(), 
                             xlab = paste0(primaryGene(), " correlation values")) +
      ggplot2::scale_y_continuous(expand = c(0,0))
    
    if (length(s)) {
      p <- p + ggplot2::geom_vline(xintercept = d2$vals[s],
                                   show.legend = F, color = "red")
      p
    } else {
      p
    }
  })
  output$geneHist <- renderPlot({
    invalidateLater(5000)
    req(processed())
    geneHist()
  })
  output$gseaData <- renderDataTable({
    req(eresCor())
    eres <- eresCor()
    eres <- eres[,c(2, 5, 6, 7)]
    eresTitles <- eres$Description
    eresTitles <- correlationAnalyzeR::fixStrings(eresTitles)
    eresTitles[which(nchar(eresTitles) > 45)] <- paste0(
      substr(eresTitles[which(nchar(eresTitles) > 45)], 1, 41), "..."
    )
    eresDT <- eres
    eresDT$Description <- createGSEAInfoLink(eresDT$Description, eresTitles)
    gseaDTReady(TRUE)
    eresDT
  }, selection = list(mode = "single", selected = 1), 
  rownames = F, server = T, escape = F, 
  options = list(
    pageLength = 6,
    dom = "ftprl",
    scrollX = TRUE
  ),
  colnames = c("Pathway", "Enrichment (normalized)",
               "Pval", "Padj"))
  
  # Datatables proxy
  observeEvent(processed(), {
    # req(downloadsList$ready)
    req(correlations())
    req(processed())
    req((! groupMode()))
    proxyCor <- dataTableProxy("correlationData")
    correlationData <- correlations()
    colnames(correlationData)[4] <- "vals"
    correlationData$geneName <- createGeneInfoLink(correlationData$geneName)
    d2 <- correlationData
    d2 <- d2[,c(1,3,4)]
    d2 <- unique(d2)
    replaceData(proxyCor, d2, rownames = FALSE)
    selectRows(proxyCor, 1)
    reloadData(proxyCor)
  })
  
  observe({
    req(eresCor())
    proxy <- dataTableProxy('gseaData')
    eres <- eresCor()
    eres <- eres[,c(2, 5, 6, 7)]
    eresTitles <- eres$Description
    eresTitles <- correlationAnalyzeR::fixStrings(eresTitles)
    eresTitles[which(nchar(eresTitles) > 45)] <- paste0(
      substr(eresTitles[which(nchar(eresTitles) > 45)], 1, 41), "..."
    )
    eresDT <- eres
    eresDT$Description <- createGSEAInfoLink(eresDT$Description, eresTitles)
    replaceData(proxy, eresDT, rownames = FALSE)
    selectRows(proxy, 1)
  })
  output$plotGSEA <- renderPlot({
    req(EGMT())
    req((! groupMode()))
    req(gseaDTReady())
    eresRaw <- eresCor()
    eres <- eresRaw[,c(2, 5, 6, 7)]
    eresTitles <- eres$Description
    eresTitles <- correlationAnalyzeR::fixStrings(eresTitles)
    eresTitles[which(nchar(eresTitles) > 45)] <- paste0(
      substr(eresTitles[which(nchar(eresTitles) > 45)], 1, 41), "..."
    )
    s <- input$gseaData_rows_selected
    if (length(s)) {
      id <- eres[s,1]
      titleID <- eresTitles[s]
    } else{
      id <- eres[1,1]
      titleID <- eresTitles[1]
    }
    # Get the downloadable correlation-term frame
    correlationData <- correlations()
    genes <- unlist(strsplit(eresRaw$core_enrichment[
      which(eresRaw$ID == id)], split = "/"))
    corrDFNow <- correlationData[which(correlationData$geneName %in% genes),]
    corrDFNow2 <- cbind(rep(id, length(corrDFNow$geneName)), corrDFNow)
    colnames(corrDFNow2)[1] <- "geneset_ID"
    colnames(corrDFNow2)[5] <- paste0(colnames(corrDFNow2)[5], "_correlation_value")
    eresCorDownload(corrDFNow2)
    # Return the plot
    plt <- clusterProfiler::gseaplot(EGMT(), 
                                     geneSetID = id, 
                                     title = titleID)
    # Fix issue with plot margins cutting off the 30,000
    if (species() == "Human") {
      plt + theme(plot.margin = margin(t = 0, r = 20, b = 0, l = 0, unit = "pt"))
    } else {
      plt
    }
  })
  
  # Make downloads for group mode
  observe({
    req(dataTables$singleModeData())
    req(preprocessed())
    req(groupMode())
    req((! downloadsList$ready))
    req((! downloadsList$init))
    downloadsList$init <- TRUE
    
    downloadsList[["correlationData"]] <- list("content" = correlations(),
                                               "uiName" = paste0(
                                                 strong("Group-mode correlations: "), 
                                                 uiName()),
                                               "file" = ".tsv")
    req(geneTPMBoxplotData())
    downloadsList[["groupModeTPM"]] <- list("content" = geneTPMBoxplotData(),
                                            "uiName" = paste0(strong("Group-mode expression: "),
                                                              uiName()),
                                            "file" = ".tsv")
    downloadsList$init <- FALSE
    downloadsList$ready <- TRUE
    
    
  })
  # Change downloads init and ready when observe row select
  observe({
    s <- input$gseaData_rows_selected
    downloadsList$init <- FALSE
    downloadsList$ready <- FALSE
  })
  # Make downloads for normal mode
  observe({
    req(dataTables$singleModeData())
    req(preprocessed())
    req((! groupMode()))
    req((! downloadsList$ready))
    req((! downloadsList$init))
    if (gseaRun()) {
      req(eresCorDownload())
    } else {
      req(is.null(eresCorDownload()))
    }
    downloadsList$init <- TRUE
    downloadsList[["correlationData"]] <- list("content" = correlations(),
                                               "uiName" = paste0(strong("Correlations: "),
                                                                 uiName()),
                                               "file" = ".tsv")
    if (! gseaRun()) {
      downloadsList$init <- FALSE
      downloadsList$ready <- TRUE
    }
    req(eresDownload())
    req(eresCorDownload())
    req((! downloadsList$ready))
    req((downloadsList$init))
    downloadsList[["gseaData"]] <-  list("content" = eresDownload(),
                                         "uiName" = paste0(strong("GSEA results: "),
                                                           uiName()),
                                         "file" = ".tsv")
    termDownRaw <- unique(eresCorDownload()$geneset_ID)
    termDown <- correlationAnalyzeR::fixStrings(termDownRaw)
    downloadsList[["correlationsGSEA"]] <- list("content" = eresCorDownload(),
                                                "uiName" = paste0(strong("GSEA correlations: "), 
                                                                  termDown),
                                                "filePrefix" = termDownRaw,
                                                "file" = ".tsv")
    downloadsList$init <- FALSE
    downloadsList$ready <- TRUE
  })
  
  # Call downloads modeule
  observe({
    req(downloadsList$correlationData)
    req((! downloadsList$init))
    req(downloadsList$ready)
    if (is.null(downloadsList$gseaData) & ! groupMode()) {
      downloadsList2 <- reactiveValues(correlationData = downloadsList$correlationData)
      callModule(module = downloadData, id = "singleModeDownloads", invalidater = TRUE,
                 primaryName = fileName(), downloadsListReact = downloadsList2)
    } else {
      callModule(module = downloadData, id = "singleModeDownloads", 
                 primaryName = fileName(), downloadsListReact = downloadsList)
    }
    
    print("End of single mode plots memory: ")
    print(pryr::mem_used())
    dataTables$singleModeData()$progress$close()
    req((! processed()))
    print("Processed!")
    processed(TRUE)
    
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
    selectInput(inputId = ns("species"), label = "Select species",
                choices = c("Human", "Mouse"), selected = "Human"),
    popify(#trigger = "focus",
      radioButtons(inputId = ns("gseaType"), label = "corGSEA type",
                   choices = c("Simple", "Complex")), 
      placement = "right", 
      title = 'Select corGSEA type', options=list(container="body"),
      content = paste0('"Simple" includes commonly-used genesets',
                       ' (MSIGDB genesets "H", "C2", "C5", and "C6")',
                       '. "Complex" considers all MSIGDB genesets.')
    ),
    popify(title = "Group mode", # trigger = "focus",
           placement = "right", options=list(container="body"),
           switchInput(ns("crossComparisonMode"), value = FALSE, label = "Group mode"),
           content = paste0('Group comparison shows correlations across multiple tissue-disease groups.')
    ),
    fluidRow(
      column(4, actionButton(ns("do"), "Analyze"))
    )
  )
}
geneVsGeneModeAnalysis <- function(input, output, session, 
                                   parent_session, GlobalData, pool) {
  
  crossComparisonMode <- reactive({input$crossComparisonMode})
  observe({
    crossComparisonMode <- input$crossComparisonMode
  })
  do <- reactive({input$do})
  species <- reactive({input$species})
  geneOne <- callModule(singleGeneInput, "singleGeneInputOne",
                        parent_session = parent_session,
                        humanGeneOptions = GlobalData$humanGeneOptions,
                        mouseGeneOptions = GlobalData$mouseGeneOptions,
                        species = species)
  tissueTypeOne <- callModule(tissueTypeInput, "tissueTypeInputOne",
                              parent_session = parent_session,
                              mouseTissueOptions = GlobalData$mouseTissueOptions,
                              humanTissueOptions = GlobalData$humanTissueOptions,
                              species = species)
  sampleTypeOne <- callModule(sampleTypeInput, "sampleTypeInputOne",
                              parent_session = parent_session,
                              mouseTissueOptions = GlobalData$mouseTissueOptions,
                              humanTissueOptions = GlobalData$humanTissueOptions,
                              species = species, 
                              tissueType = tissueTypeOne)
  
  geneTwo <- callModule(singleGeneInput, "singleGeneInputTwo",
                        parent_session = parent_session,
                        humanGeneOptions = GlobalData$humanGeneOptions,
                        mouseGeneOptions = GlobalData$mouseGeneOptions,
                        species = species, selected = c("TP53", "Tp53"))
  tissueTypeTwo <- callModule(tissueTypeInput, "tissueTypeInputTwo",
                              parent_session = parent_session,
                              mouseTissueOptions = GlobalData$mouseTissueOptions,
                              humanTissueOptions = GlobalData$humanTissueOptions,
                              species = species)
  sampleTypeTwo <- callModule(sampleTypeInput, "sampleTypeInputTwo",
                              parent_session = parent_session,
                              mouseTissueOptions = GlobalData$mouseTissueOptions,
                              humanTissueOptions = GlobalData$humanTissueOptions,
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
                             mouseTissueOptions = GlobalData$mouseTissueOptions,
                             humanTissueOptions = GlobalData$humanTissueOptions,
                             species = species,
                             crossComparisonMode = input$crossComparisonMode)
    sampleType <- callModule(sampleTypeInput, "tissueTypeInputOne",
                             parent_session = parent_session,
                             mouseTissueOptions = GlobalData$mouseTissueOptions,
                             humanTissueOptions = GlobalData$humanTissueOptions,
                             species = species, 
                             tissueType = tissueType)
    tissueType <- callModule(tissueTypeInput, "tissueTypeInputTwo",
                             parent_session = parent_session,
                             mouseTissueOptions = GlobalData$mouseTissueOptions,
                             humanTissueOptions = GlobalData$humanTissueOptions,
                             species = species,
                             crossComparisonMode = input$crossComparisonMode)
    sampleType <- callModule(sampleTypeInput, "tissueTypeInputTwo",
                             parent_session = parent_session,
                             mouseTissueOptions = GlobalData$mouseTissueOptions,
                             humanTissueOptions = GlobalData$humanTissueOptions,
                             species = species, 
                             tissueType = tissueType)
  })
  
  
  
  data <- reactiveVal()
  data <- eventReactive(eventExpr = input$do, {
    geneOne <- geneOne()
    shiny::validate(need(geneOne != "Loading ...", 
                         label = "Please select a gene one."))
    tissueTypeOne <- tissueTypeOne()
    tissueTypeOne <- tolower(tissueTypeOne)
    tissueTypeOne <- gsub(tissueTypeOne, pattern = " ",replacement = "0")
    sampleTypeOne <- sampleTypeOne()
    sampleTypeOne <- tolower(sampleTypeOne)
    geneTwo <- geneTwo()
    shiny::validate(need(geneTwo != "Loading ...", 
                         label = "Please select a gene two"))
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
                       type = "error", duration = 8)
    }
    
    shiny::validate(
      need(expr = {geneOne != geneTwo |
          sampleTypeOne != sampleTypeTwo |
          tissueTypeOne != tissueTypeTwo |
          sampleTypeOne == "-"}, 
          message = 
            "")
    )
    
    # Initialize progress object
    progress <- shiny::Progress$new()
    progress$set(message = "Validating inputs ... ", value = .1)
    # on.exit(progress$close())
    
    # # Bug testing
    # geneOne <- "Atmin"
    # tissueTypeOne <- "all"
    # sampleTypeOne <- "normal"
    # geneTwo <- "Slc7a2"
    # tissueTypeTwo <- "female0reproductive"
    # sampleTypeTwo <- "normal"
    # species <- "Mouse"
    # gseaType <- "Simple"
    cleanResOne <- cleanInputs(primaryGene = geneOne,
                               selectedSpecies = species,
                               sampleType = sampleTypeOne,
                               tissueType = tissueTypeOne,
                               GlobalData = GlobalData,
                               session = session,
                               pool = pool)
    cleanResTwo <- cleanInputs(primaryGene = geneTwo,
                               selectedSpecies = species,
                               sampleType = sampleTypeTwo,
                               tissueType = tissueTypeTwo,
                               GlobalData = GlobalData,
                               session = session,
                               pool = pool)
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
                 "")
        )
        
      }
      # # Bug testing
      # genesOfInterest <- c("ATM", "SLC3A2")
      progress$inc(.1, message = "Running group mode ... ")
      progress$inc(.1, detail = "This may take ~1 minute to complete.")
      pairedRes <- correlationAnalyzeR::analyzeGenePairs(genesOfInterest = genesOfInterest, 
                                                         Species = cleanResOne$selectedSpecies,
                                                         returnDataOnly = T, runGSEA = F,
                                                         # nperm = 500, sampler = TRUE,
                                                         crossCompareMode = T, pool = pool)
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
                                                         topPlots = F, pool = pool,
                                                         # nperm = 500, sampler = T,
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
      br(),
      uiOutput(outputId = ns("comparedPathsUI")),
      downloadDataUI(ns("geneVsGeneModeDownloads")),
      br()
    )
  )
}
geneVsGeneModePlots <- function(input, output, session, 
                                parent_session,
                                GlobalData, dataTables) {
  
  # For group and normal mode
  primaryGene <- reactiveVal()
  uiName <- reactiveVal()
  fileName <- reactiveVal()
  geneOne <- reactiveVal()
  geneTwo <- reactiveVal()
  correlations <- reactiveVal()
  species <- reactiveVal()
  tissueTypeOne <- reactiveVal()
  tissueTypeTwo <- reactiveVal()
  sampleTypeOne <- reactiveVal()
  sampleTypeTwo <- reactiveVal()
  downloadsList <- reactiveValues()
  preprocessed <- reactiveVal()
  processed <- reactiveVal()
  heatReady <- reactiveVal()
  
  # Only for group mode
  groupMode <- reactiveVal()
  whichCompareGroups <- reactiveVal()
  ht <- reactiveVal()
  tmpscatterFile <- reactiveVal()
  # tmpHeatFile <- reactiveVal()
  geneTPMBoxplot1 <- reactiveVal()
  geneTPMBoxplot2 <- reactiveVal()
  geneTPMData <- reactiveVal()
  
  # Only for normal mode
  TPMData <- reactiveVal()
  # corrPlot <- reactiveVal()
  heatGenesVar <- reactiveVal()
  heatPathsVar <- reactiveVal()
  heatGenesSim <- reactiveVal()
  heatPathsSim <- reactiveVal()
  correlatedPathwaysDF <- reactiveVal()
  uiNameOne <- reactiveVal()
  uiNameTwo <- reactiveVal()
  fileNameOne <- reactiveVal()
  fileNameTwo <- reactiveVal()
  specName <- reactiveVal()
  corrPlot <- reactiveVal()

  observeEvent(eventExpr = dataTables$geneVsGeneModeData(), {
    req(dataTables$geneVsGeneModeData())
    preprocessed(FALSE)
    processed(FALSE)
    correlations(NULL)
    correlatedPathwaysDF(NULL)
    corrPlot(NULL)
    heatReady(FALSE)
    heatPathsVar(NULL)
    heatPathsSim(NULL)
    uiName(NULL)
    
    downloadsList$init <- FALSE
    downloadsList$ready <- FALSE
    downloadsList$downloadDataPairsPath <- NULL
    downloadsList$correlationData <- NULL
    downloadsList$downloadDataTPM <- NULL
    dataList <- dataTables$geneVsGeneModeData()
    pairedRes <- dataList[["geneVsGeneResults"]]
    species(dataList[["species"]])
    geneOne(dataList[["geneOne"]])
    tissueTypeOne(gsub(dataList[["tissueTypeOne"]],
                       pattern = "0", replacement = " "))
    sampleTypeOne(dataList[["sampleTypeOne"]])
    geneTwo(dataList[["geneTwo"]])
    tissueTypeTwo(gsub(dataList[["tissueTypeTwo"]],
                       pattern = "0", replacement = " "))
    sampleTypeTwo(dataList[["sampleTypeTwo"]])
    if ("crossCompareMode" %in% names(dataList)) {
      groupMode(TRUE)
      correlations(pairedRes[["Correlations"]])
      dataList$progress$inc(
                  .4,
                  message = "Preparing group mode result files ... ")
      on.exit(dataList$progress$close())
      dataListNow <- pairedRes[["pairResList"]]
      whichCompareGroups(pairedRes[["mode"]])
      if (pairedRes[["mode"]] == "cross_geneVsGene") {
        uiName(paste0(geneOne(), " vs ", geneTwo()))
        ht(30)
      } else {
        uiName(paste0(geneOne(), " - normal vs. cancer"))
        ht(14)
      }
      # plotListHeat <- list()
      plotListScatter <- list()
      for (i in 1:length(names(dataListNow))) {
        name <- names(dataListNow)[i]
        plotListScatter[[i]] <- dataListNow[[i]][["scatterPlot"]]
        # plotListHeat[[i]] <- dataListNow[[i]][["heatMap"]][[4]]
      }
      ga <- ggpubr::ggarrange(plotlist = plotListScatter, ncol = 3)
      # ge <- gridExtra::arrangeGrob(grobs = plotListHeat, ncol = 7)
      tmp <- paste0("www/tmp/", as.character(session$token)[1])
      # tmpHeatFileRaw <- file.path(tmp, paste0(uiName(), " heatMap.pdf"))
      # tmpHeatFileRaw <- gsub(tmpHeatFileRaw, pattern = " ", replacement = "_")
      # ggplot2::ggsave(ge, filename = tmpHeatFileRaw, height = ht(), width = 40)
      # tmpHeatFile(gsub(tmpHeatFileRaw, pattern = "www/", replacement = ""))
      tmpscatterFileRaw <- file.path(tmp, paste0(uiName(), " scatterMap.pdf"))
      tmpscatterFileRaw <- gsub(tmpscatterFileRaw, pattern = " ", replacement = "_")
      ggpubr::ggexport(plotlist = plotListScatter, ncol = 4, 
                       filename = tmpscatterFileRaw, height = 5, width = 20)
      tmpscatterFile(gsub(tmpscatterFileRaw, pattern = "www/", replacement = ""))
      # Get TPM
      geneTPMData(pairedRes$crossCompareTPM[["TPM_DF"]])
      if (whichCompareGroups() == "cross_geneVsGene") {
        geneTPMBoxplot1(pairedRes$crossCompareTPM[["TPM_boxPlotOne"]])
        geneTPMBoxplot2(pairedRes$crossCompareTPM[["TPM_boxPlotTwo"]])
      } else {
        geneTPMBoxplot1(pairedRes$crossCompareTPM[["TPM_boxPlot"]])
        geneTPMBoxplot2(NULL)
      }
    } else {
      groupMode(FALSE)
      uiNameOne(paste0(geneOne(), " (",
                       tissueTypeOne(), " - ",
                       sampleTypeOne(), ")"))
      fileNameOne(paste0(geneOne(), "_",
                         tissueTypeOne(), "_",
                         sampleTypeOne()))
      uiNameTwo(paste0(geneTwo(), " (",
                       tissueTypeTwo(), " - ",
                       sampleTypeTwo(), ")"))
      fileNameTwo(paste0(geneTwo(), "_",
                         tissueTypeTwo(), "_",
                         sampleTypeTwo()))
      longName <- ifelse((tissueTypeOne() != tissueTypeTwo() | 
                            sampleTypeOne() != sampleTypeTwo()),
                         yes = T, no = F)
      if (longName) {
        uiName(paste0(uiNameOne(), " vs ", uiNameTwo()))
        fileName(paste0(fileNameOne(), "_vs_", fileNameTwo()))
      } else {
        uiName(paste0(geneOne(), " vs ", geneTwo(), "(",
                      tissueTypeTwo(), " - ",
                      sampleTypeTwo(), ")"))
        fileName(paste0(geneOne(), "_vs_", geneTwo(), "_",
                        tissueTypeTwo(), "_",
                        sampleTypeTwo()))
      }
      dataList$progress$inc(.4, message = "Returning results ... ")
      on.exit(dataList$progress$close())
      correlations(pairedRes[["processedCorrelationsFrame"]])
      corrPlot(pairedRes[["compared"]][["correlationPlotBin"]])
      TPMData(pairedRes[["compared"]][["TPM_Data"]])
      geneTPMBoxplot1(pairedRes[["compared"]][["TPM_boxPlot"]])
      geneTPMBoxplot2(NULL)
      heatGenesVar(pairedRes[["compared"]][["correlationVarianceHeatmap"]])
      heatGenesSim(pairedRes[["compared"]][["correlationSimilarityHeatmap"]])
      heatPathsVar(pairedRes[["compared"]][["pathwayVarianceHeatmap"]])
      heatPathsSim(pairedRes[["compared"]][["pathwaySimilarityHeatmap"]])
      correlatedPathwaysDT <- pairedRes[["compared"]][[
        "correlatedPathwaysDataFrame"]][,c(1, 2, 6, 10, 11)]
      correlatedPathwaysDT <- correlatedPathwaysDT[order(correlatedPathwaysDT$NES_variance, 
                                                         decreasing = T),]
      colnames(correlatedPathwaysDT)[1] <- c("Pathway")
      rownames(correlatedPathwaysDT) <- NULL
      correlatedPathwaysDF(correlatedPathwaysDT)
      if (species() == "Human") {
        GeneInfo <- GlobalData$HS_basicGeneInfo
      } else {
        GeneInfo <- GlobalData$MM_basicGeneInfo
      }
      
      if (geneOne() != geneTwo()) {
        specName(paste0(geneOne(), " vs ", geneTwo()))
      } else {
        specName(paste0(geneOne(), " (", tissueTypeOne(), "-", sampleTypeOne(), " vs ",
                        tissueTypeTwo(), "-", sampleTypeTwo(), ")"))
      }
    }
    preprocessed(TRUE)
  })
  
  # Render UI
  output$correlationsUI <- renderUI({
    req(dataTables$geneVsGeneModeData())
    req(processed())
    ns <- session$ns
    if (groupMode()) {
      req(groupMode())
      req(tmpscatterFile())
      tagList(
        h1("Gene vs Gene Results"),
        br(),
        hr(),
        h2("Compared correlation scatter plots"),
        hr(),
        fluidRow(
          column(12,
                 tags$iframe(style="height:600px; width:100%",
                             src=tmpscatterFile()))
        )
      )
    } else {
      req((! groupMode()))
      # print("Invalidating gene vs gene")
      invalidateLater(1000)
      tagList(
        h1("Gene vs Gene Results"),
        br(),
        hr(),
        h2("Compared correlations"),
        hr(),
        fluidRow(
          column(width = 7, offset = 2, 
                 tagList(
                   plotOutput(ns("corrScatter")),
                   hr()
                 )
          )
        ),
        br(),
        fluidRow(
          column(width = 5, offset = 1, 
                 plotOutput(ns("heatGenesVar"))),
          column(width = 5, 
                 plotOutput(ns("heatGenesSim")))
        )
      )
    }
  })
  output$comparedPathsUI <- renderUI({
    req(dataTables$geneVsGeneModeData())
    req(processed())
    ns <- session$ns
    if(! isolate({heatReady()})) {
      invalidateLater(500)
    }
    if (! groupMode()) {
      req((! groupMode()))
      tagList(
        hr(),
        h2("Compared corGSEA results"),
        hr(),
        fluidRow(style = "height:400px;",
                 column(width = 6,  
                        plotOutput(ns("heatPathsVar"))
                 ),
                 column(width = 6, 
                        plotOutput(ns("heatPathsSim"))
                 )
        ),
        hr(),
        fluidRow(style = "height:300px;",
                 column(width =  12, #offset = 3, #title = "Correlation Data",
                        DT::dataTableOutput(ns("correlatedPathwaysDF"))
                 )
        ),
        br(),
        br(),
        br(),
        hr(),
        h2("Compared gene expression"),
        hr(),
        fluidRow(
                 column(width = 10, offset = 1,
                        plotOutput(ns("geneTPMBoxplot1"))))
        
      )
    } else {
      req((groupMode()))
      # invalidateLater(1000)
      if (whichCompareGroups() == "cross_geneVsGene") {
        tagsTPMNow <- tagList(
          br(),
          fluidRow(
                   column(width = 12, 
                          plotOutput(ns("geneTPMBoxplot2"))))
        )
      } else {
        tagsTPMNow <- br()
      }
      
      tagList(
        hr(),
        h2("Gene expression across groups"),
        hr(),
        fluidRow(
                 column(width = 12, 
                        plotOutput(ns("geneTPMBoxplot1")))),
        tagsTPMNow
      )
    }
    
  })
  
  # Build shared UI elements
  output$geneTPMBoxplot1 <- renderPlot({
    req(geneTPMBoxplot1())
    heatReady(TRUE)
    geneTPMBoxplot1()
  })
  
  # Builg UI elements for group mode
  output$geneTPMBoxplot2 <- renderPlot({
    req(geneTPMBoxplot2())
    geneTPMBoxplot2()
  })
  
  # Build UI elements for normal mode
  corrValDFReact <- reactive({
    req((! groupMode()))
    req(correlations())
    # Setup dataframe to match plotly data
    corrValDF <- correlations()
    corrValDF <- corrValDF[,c(1, 3:7)]
    corrValDF <- unique(corrValDF)
    corrValDF <- corrValDF[order(corrValDF$variance, decreasing = T),]
    corrValDF <- corrValDF[which(! corrValDF$geneName %in% colnames(corrValDF)),]
    rownames(corrValDF) <- NULL
    colnames(corrValDF)[c(3:6)] <- colnames(correlations())[c(4:7)]
    colnames(corrValDF)[3] <- convertToColnames(uiNameOne())
    colnames(corrValDF)[4] <- convertToColnames(uiNameTwo())
    corrValDF
  })
  output$corrScatter <- renderPlot({
    req((! groupMode()))
    invalidateLater(1000)
    print(corrPlot())
    
  })
  output$heatGenesVar <- renderPlot({
    req(downloadsList$downloadDataTPM)
    invalidateLater(1000)
    heatGenesVar()
  })
  output$heatGenesSim <- renderPlot({
    req(downloadsList$downloadDataTPM)
    invalidateLater(1000)
    heatGenesSim()
  })
  
  output$heatPathsVar <- renderPlot({
    req((! groupMode()))
    print(heatPathsVar())
  })
  
  output$heatPathsSim <- renderPlot({
    req((! groupMode()))
    invalidateLater(1000)
    print(heatPathsSim())
  })
  
  output$correlatedPathwaysDF <- renderDataTable(server = T, {
    req((! groupMode()))
    eres <- correlatedPathwaysDF()
    cols <- colnames(eres)
    cols[1] <- "Pathway"
    cols <- gsub(x = cols, pattern = "_", replacement = " ")
    cols <- gsub(x = cols, pattern = "NES ", replacement = "")
    cols[2] <- uiNameOne()
    cols[3] <- uiNameTwo()
    cols[c(4:5)] <- stringr::str_to_title(cols[c(4:5)])
    eres <- eres[order(as.numeric(eres[,5]), decreasing = T),]
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
  
  # Group mode downloads
  observe({
    req(dataTables$geneVsGeneModeData())
    req(preprocessed())
    req(groupMode())
    req((! downloadsList$ready))
    req((! downloadsList$init))
    downloadsList$init <- TRUE
    downloadsList[["correlationData"]] <- list(
      "content" = correlations(),
      "uiName" = paste0(strong("Correlation data: "), uiName()),
      "file" = ".tsv"
    )
    downloadsList[["downloadDataTPM"]] <- list("content" = geneTPMData(),
                                            "uiName" = paste0(strong("Expression: "),
                                                              uiName()),
                                            "file" = ".tsv")
    downloadsList$init <- FALSE
    downloadsList$ready <- TRUE
  })

  # Normal mode downloads
  observe({
    req(dataTables$geneVsGeneModeData())
    req(preprocessed())
    req((! groupMode()))
    req((! downloadsList$ready))
    req((! downloadsList$init))
    downloadsList$init <- TRUE
    downloadsList[['correlationData']] <- list(
      "content" = corrValDFReact(),
      "uiName" = paste0(strong("Correlations: "), specName()),
      "file" = ".tsv"
    )
    downloadsList[['downloadDataPairsPath']] <- list(
      "content" = correlatedPathwaysDF(),
      "uiName" = paste0(strong("GSEA results: "), specName()),
      "file" = ".tsv"
    )
    downloadsList[['downloadDataTPM']] <- list(
      "content" = TPMData(),
      "uiName" = paste0(strong("Expression: "), specName()),
      "file" = ".tsv"
    )
    downloadsList$init <- FALSE
    downloadsList$ready <- TRUE
  })

  # Call downloads modeule
  observe({
    req(dataTables$geneVsGeneModeData())
    req(preprocessed())    
    req(! processed())
    req(downloadsList$downloadDataTPM)
    req((! downloadsList$init))
    req(downloadsList$ready)
    if (! groupMode()) {
      callModule(module = downloadData, id = "geneVsGeneModeDownloads", invalidater = T,
                 primaryName = uiName(), downloadsListReact = downloadsList)
    } else {
      callModule(module = downloadData, id = "geneVsGeneModeDownloads",
                 primaryName = uiName(), downloadsListReact = downloadsList)
    }
    print("End of gene vs gene plots memory: ")
    print(pryr::mem_used())
    processed(TRUE)
  })
  
}

## geneVsGeneList-mode analysis ##
geneVsGeneListModeAnalysisUI <- function(id) {
  ns <- NS(id)
  tagList(
    singleGeneInputUI(ns("singleGeneInput"), 
                      "Primary gene"),
    multiGeneInputUI(ns("multiGeneInput"), 
                     "Secondary gene list"),
    selectInput(inputId = ns("species"), label = "Select species",
                choices = c("Human", "Mouse"), selected = "Human"),
    tissueTypeInputUI(ns("tissueTypeInput")),
    sampleTypeInputUI(ns("sampleTypeInput")),
    popify( #trigger = "focus",
      checkboxInput(inputId = ns("sigTest"), label = "Test significance", value = TRUE),
      placement = "right", 
      title = "Significance testing", options=list(container="body"),
      content = paste0('If selected, permutation tests will be performed to determine whether ',
                       'secondary genes are significantly correlated with the primary gene ',
                       ' compared to random.')
    ),
    fluidRow(
      column(4, actionButton(ns("do"), "Analyze"))
    )
  )
}
geneVsGeneListModeAnalysis <- function(input, output, session, 
                                       parent_session, GlobalData, pool) {
  
  species <- reactive({input$species})
  do <- reactive({input$do})
  
  primaryGene <- callModule(singleGeneInput, "singleGeneInput",
                            parent_session = parent_session,
                            humanGeneOptions = GlobalData$humanGeneOptions,
                            mouseGeneOptions = GlobalData$mouseGeneOptions,
                            species = species)
  tissueType <- callModule(tissueTypeInput, "tissueTypeInput",
                           parent_session = parent_session,
                           mouseTissueOptions = GlobalData$mouseTissueOptions,
                           humanTissueOptions = GlobalData$humanTissueOptions,
                           species = species)
  sampleType <- callModule(sampleTypeInput, "sampleTypeInput",
                           parent_session = parent_session,
                           mouseTissueOptions = GlobalData$mouseTissueOptions,
                           humanTissueOptions = GlobalData$humanTissueOptions,
                           species = species, 
                           tissueType = tissueType)
  
  secondaryGenes <- callModule(multiGeneInput, "multiGeneInput")
  
  data <- reactiveVal()
  data <- eventReactive(eventExpr = input$do, {
    primaryGene <- primaryGene()
    shiny::validate(need(primaryGene != "Loading ...", 
                         label = "Please select a primary gene."))
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
                            session = session,
                            pool = pool)
    pass <- 1
    
    if (is.null(cleanRes$secondaryGenes)) {
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
    if (cleanRes$geneSetInputType) {
      TERM2GENE <- correlationAnalyzeR::getTERM2GENE(GSEA_Type = "complex",
                                                     Species = cleanRes$selectedSpecies)
      geneVsGeneListGenesList[[1]] <- TERM2GENE$gene_symbol[TERM2GENE$gs_name ==
                                                              geneVsGeneListGenesList[[1]]]
    }
    if (length(geneVsGeneListGenesList[[1]]) > 500) {
      msg <- paste0("Gene vs gene list mode cannot process more than 500 genes. If you would ",
                    "like to test more, please use the R-package of correlationAnalyzeR.")
      showNotification(ui = msg, 
                       duration = 8, type = 'error')
      progress$close()
    }
    shiny::validate(need(length(geneVsGeneListGenesList[[1]]) < 501, 
                         label = "Use may not enter > 500 genes"))
    progress$inc(.2, message = "Analyzing correlations ... ")
    
    # # BugTesting
    # geneVsGeneListGenesList <- list("BRCA1" = c("ATM", "BRCA2", "BRCC3"))
    # cleanRes <- list("selectedSpecies" = "hsapiens",
    #                  "sampleType" = "Normal_Tissues")
    # sigTest <- F
    set.seed(1) #Reproducible
    data <- correlationAnalyzeR::geneVsGeneListAnalyze(pairedGenesList = geneVsGeneListGenesList, 
                                                       Tissue = cleanRes$tissueType, pool = pool,
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
  downloadsList <- reactiveValues()
  uiName <- reactiveVal()
  fileName <- reactiveVal()
  primaryGene <- reactiveVal()
  corrValDF <- reactiveVal()
  tTest_pvalsPlot <- reactiveVal()
  processed <- reactiveVal()
  sigTest <- reactiveVal()
  secondaryGenes <- reactiveVal()
  tissueType <- reactiveVal()
  sampleType <- reactiveVal()
  
  observeEvent(dataTables$geneVsGeneListModeData(), {
    req(dataTables$geneVsGeneListModeData())
    processed(FALSE)
    tissueType(NULL)
    primaryGene(NULL)
    sampleType(NULL)
    dataListReact <- dataTables[["geneVsGeneListModeData"]]
    dataList <- dataListReact()
    data <- dataList[[1]]
    species <- dataList[["species"]]
    primaryGene(dataList[["primaryGene"]])
    tissueType <- gsub(tissueType(), pattern = "0", replacement = " ")
    tissueType(dataList[["tissueType"]])
    sampleType(dataList[["sampleType"]])
    if (species == "Human") {
      GeneInfo <- GlobalData$HS_basicGeneInfo
    } else {
      GeneInfo <- GlobalData$MM_basicGeneInfo
    }
    
    uiName(paste0(primaryGene(), " (",
                     tissueType(), " - ",
                     sampleType(), ")"))
    fileName(paste0(primaryGene(), "_",
                       tissueType(), "_",
                       sampleType()))
    downloadsList[["correlationData"]] <- NULL
    progress <- dataList[["progress"]]
    progress$inc(.4, message = "Returning results ... ")
    on.exit(progress$close())
    corrDataDF <- data$Correlation_histogram$data
    secondaryGenes(unique(corrDataDF$geneName[which(corrDataDF$secondaryGene)]))
    corrDataDF <- corrDataDF[,c(1,2)]
    colnames(corrDataDF)[2] <- "Values"
    corrDataDF <- merge(x = corrDataDF, y = GeneInfo, all.x = T, by = "geneName")
    corrDataDF <- corrDataDF[,c(1, 4, 2)]
    corrDataDF <- corrDataDF[order(corrDataDF$Values, decreasing = T),]
    rownames(corrDataDF) <- NULL
    corrDataDF <- unique(corrDataDF)
    corrValDF(corrDataDF)
    
    if ("sigTest" %in% names(data)) {
      sigTest(TRUE)
      tTest_pvalsPlot(data$sigTest$tTest_pvalsPlot)
    } else {
      sigTest(FALSE)
      tTest_pvalsPlot(NULL)
    }
    
    processed(TRUE)
    print("End of gene vs gene list plots memory: ")
    print(pryr::mem_used())
  })
  
  output$geneHist <- renderPlotly({
    req(processed())
    a <- list(
      title = "Correlation values"
    )
    plt <- ggpubr::gghistogram(data = corrValDF(), x = "Values", y = "..count..",
                             bins = 50, ylab = "Frequency\n",
                             title = uiName(), 
                             xlab = paste0(primaryGene(), " correlation values")) +
      ggplot2::scale_y_continuous(expand = c(0, 0))
    plt <- ggplotly(plt)
    corrDataDF2 <- corrValDF()[which(corrValDF()$geneName %in% secondaryGenes()),]
    plt <- plt %>% 
      add_segments(data = corrDataDF2, x = ~Values, 
                   xend = ~Values, y = 0, 
                   yend = 500, color = I('blue'),
                   text = ~paste('Gene: ', geneName, " - Value: ", round(Values, 3)),
                   opacity = .7) %>%
      layout(showlegend = FALSE, xaxis = a) %>%
      config(plot_ly(), displaylogo = F,
             modeBarButtonsToRemove = list('zoom2d', 'pan2d', 
                                           'autoScale2d', 
                                           'hoverCompareCartesian',
                                           'hoverClosestCartesian',
                                           'zoomIn2d', 'zoomOut2d',
                                           'select2d', 'lasso2d'),
             toImageButtonOptions= list(filename = paste0(fileName(),
                                                          "_geneVsGeneListMode_correlationHistogram"),
                                        format = "png",
                                        width = 1000,
                                        height = 600))
    
    s <- input$correlationData_rows_selected
    if (length(s)) {
      corrDataDF3 <- corrDataDF2[s,]
      plt <- plt %>% 
        add_segments(data = corrDataDF3, x = ~Values, 
                     text = ~paste('Gene: ', geneName, " - Value: ", round(Values, 3)),
                     xend = ~Values, y = 0, yend = 500) 
    } else {
      plt <- plt %>% highlight("plotly_selected", color = I('green'))
    }
    plt
  })
  

  
  # Make correlation values datatable
  output$correlationData <- renderDataTable(server = T, {
    req(processed())
    corrValDFNow <- corrValDF()[which(corrValDF()$geneName %in% secondaryGenes()),]
    # Replace gene name with HTML to call gene info modal
    corrValDFNow$geneName <- createGeneInfoLink(corrValDFNow$geneName)
    # Construct datatable
    DT_out <- datatable(corrValDFNow, selection = "single",
                        rownames = F, escape = F,  
                        colnames = c("Gene Name", "Description",
                                     "Correlation Value"),
                        options = list(dom = "ftprl",
                                       scrollX = TRUE,
                                       pageLength = 6)
    )
    DT_out
  }, escape = F)
  observe({
    req(corrValDF())
    proxy <- dataTableProxy('correlationData')
    reloadData(proxy)
    selectRows(proxy, 1)
  })
  
  # Render correlation data UI
  output$correlationsUI <- renderUI({
    req(processed())
    ns <- session$ns
    tagList(
      h1("Gene vs Gene List Results"),
      br(),
      hr(),
      h2("Correlation data"),
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
  
  
  output$sigTestPlotsUI <- renderUI({
    ns <- session$ns
    req(processed())
    if (sigTest()) {
      req(sigTest())
      tagList(
        fluidRow(
          hr(),
          h2("Significance test plot"),
          hr(),
          column(width = 8, 
                 plotlyOutput(ns("sigTestPlots"),
                              height = "100%")
          )
        )
      )
    } else {
      req((! sigTest()))
      tagList(
        fluidRow(
          hr(),
          h4(em("Select 'Test significance' to run permutation tests")),
          hr()
        )
      )
    }
  })
  
  output$sigTestPlots <- renderPlotly({
    req(processed())
    req(sigTest())
    m <- list(
      l = 50,
      r = 50,
      b = 50,
      t = 50,
      pad = 4
    )
    p3 <- tTest_pvalsPlot()
    p3 <- ggplotly(p3)
    p3 <- p3 %>% layout(title = paste0(primaryGene(), 
                                       " correlation with secondary genes"),
                        xaxis = list(title = 't.test p value')) %>%
      layout(autosize = F, margin = m) %>%
      config(plot_ly(), displaylogo = F,
             modeBarButtonsToRemove = list('zoom2d', 'pan2d', 
                                           'autoScale2d', 
                                           'hoverCompareCartesian',
                                           'hoverClosestCartesian',
                                           'zoomIn2d', 'zoomOut2d',
                                           'select2d', 'lasso2d'),
             toImageButtonOptions= list(filename = paste0(fileName(),
                                                          "_correlation_sigTest"),
                                        format = "png",
                                        width = 1000,
                                        height = 600))
    p3
  })
  

  observe({
    req(processed())
    invalidateLater(500)
    downloadsList[["correlationData"]] <- list(
      "content" = corrValDF()[which(corrValDF()$geneName %in% secondaryGenes()),],
      "uiName" = paste0(strong("Correlations: "), uiName()),
      "file" = ".tsv"
    )
    callModule(module = downloadData, id = "geneVsGeneListModeDownloads", 
               primaryName = uiName(), downloadsListReact = downloadsList)
  })
  
  
}

## Topology-Mode analysis ##
# Analysis
topologyModeAnalysisUI <- function(id) {
  ns <- NS(id)
  tagList(
    multiGeneInputUI(ns("multiGeneInput"), "Gene list"),
    selectInput(inputId = ns("species"), label = "Select species",
                choices = c("Human", "Mouse"), selected = "Human"),
    tissueTypeInputUI(ns("tissueTypeInput")),
    sampleTypeInputUI(ns("sampleTypeInput")),
    checkboxGroupInput(inputId = ns("crossComparisonType"), selected = c("PCA",
                                                                         "variantGenes",
                                                                         "pathwayEnrich"),
                       choiceNames = c("Dimension reduction",
                                       "Variant genes",
                                       "Pathway enrichment"),
                       label = "Choose analyses", choiceValues = c("PCA",
                                                                   "variantGenes",
                                                                   "pathwayEnrich")),
    fluidRow(
      column(4, actionButton(ns("do"), "Analyze"))
    )
  )
}
topologyModeAnalysis <- function(input, output, session, 
                                 parent_session, GlobalData, pool) {
  
  species <- reactive({input$species})
  secondaryGenes <- callModule(multiGeneInput, "multiGeneInput")
  tissueType <- callModule(tissueTypeInput, "tissueTypeInput",
                           parent_session = parent_session,
                           mouseTissueOptions = GlobalData$mouseTissueOptions,
                           humanTissueOptions = GlobalData$humanTissueOptions,
                           species = species)
  sampleType <- callModule(sampleTypeInput, "sampleTypeInput",
                           parent_session = parent_session,
                           mouseTissueOptions = GlobalData$mouseTissueOptions,
                           humanTissueOptions = GlobalData$humanTissueOptions,
                           species = species, 
                           tissueType = tissueType)
  
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
    if (length(secondaryGenes) > 500) {
      msg <- paste0("Topology mode cannot process more than 500 genes. If you would ",
                    "like to test more, please use the R-package.")
      showNotification(ui = msg, 
                       duration = 8, type = 'error')
    }
    shiny::validate(need(length(secondaryGenes) < 501, 
                         label = "Use may not enter > 500 genes"))
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
                            session = session,
                            pool = pool)
    # Return plots + correlation data
    progress$inc(.1, message = "Analyzing geneset topology ... ")
    progress$inc(.1, detail = "Based on list size, this may take ~1-2 minutes.")
    # Warn if using less than 10 genes for pathway enrichment
    pass <- 1
    if (! cleanRes$geneSetInputType & length(cleanRes$secondaryGenes) < 3) {
      msg <- "Topology analysis requires 3+ valid genes."
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
    if (cleanRes$geneSetInputType) {
      TERM2GENE <- correlationAnalyzeR::getTERM2GENE(GSEA_Type = "complex",
                                                     Species = cleanRes$selectedSpecies )
      cleanRes$secondaryGenes <- TERM2GENE$gene_symbol[TERM2GENE$gs_name ==
                                                         cleanRes$secondaryGenes]
    }
    if (length(cleanRes$secondaryGenes) > 500) {
      msg <- paste0("Topology mode cannot process more than 500 genes. If you would ",
                    "like to test more, please use the R-package of correlationAnalyzeR.")
      showNotification(ui = msg, 
                       duration = 8, type = 'error')
      progress$close()
    }
    shiny::validate(need(length(cleanRes$secondaryGenes) < 501, 
                         label = "Use may not enter > 500 genes"))
    progress$inc(.2, message = "Analyzing correlations ... ")
    data <- correlationAnalyzeR::analyzeGenesetTopology(genesOfInterest = cleanRes$secondaryGenes, 
                                                        Tissue = cleanRes$tissueType, 
                                                        alternativeTSNE = T,
                                                        pool = pool,
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
                "progress" = progress)
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
            withSpinner(plotlyOutput(ns("varHeat"), width = "1200px"), type = 7)
            
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
  
  hideTab(inputId = "topologyNavs", target = "dimReduction", 
          session = session)
  hideTab(inputId = "topologyNavs", target = "varGenes", 
          session = session)
  hideTab(inputId = "topologyNavs", target = "pathwayEnrich", 
          session = session)
  hideTab(inputId = "topologyNavs", target = "downloadsTab", 
          session = session)
  
  
  # for all modes
  tissueType <- reactiveVal()
  uiName <- reactiveVal()
  fileName <- reactiveVal()
  correlations <- reactiveVal()
  species <- reactiveVal()
  sampleType <- reactiveVal()
  downloadsList <- reactiveValues()
  processed <- reactiveVal()
  basicGeneInfo <- reactiveVal()
  dataNow <- reactiveVal()
  
  # Mode types
  dimReduce <- reactiveVal()
  dimMode <- reactiveVal()
  varHeatMode <- reactiveVal()
  pathEnrich <- reactiveVal()
  
  # Dim reduce
  PCADT <- reactiveVal()
  PCAPlot <- reactiveVal()
  
  # Var heat
  variantGenesHeatmap_MAT <- reactiveVal()
  
  # Path enrich
  inputGenes_pathwayEnrich_dotplot <- reactiveVal()
  pathwayEnrichDT <- reactiveVal()
  
  # Initialize values for output
  observeEvent(eventExpr = dataTables$topologyModeData(), {
    req(dataTables$topologyModeData())
    dataListReact <- dataTables[["topologyModeData"]]
    dataList <- dataListReact()
    dataList$progress$inc(.3, message = "Returning results ... ")
    on.exit(dataList$progress$close())
    processed(FALSE)
    do(dataList$do)
    species(dataList$species)
    dataRaw <- dataList$topologyModeData
    corrData <- dataRaw$Correlation_Data
    corrData$geneName <- rownames(corrData)
    n <- length(colnames(corrData))
    corrData <- corrData[,c(n, 1:(n-1))]
    dataRaw$Correlation_Data <- corrData
    if (species() == "Human") {
      basicGeneInfo(GlobalData$HS_basicGeneInfo)
    } else {
      basicGeneInfo(GlobalData$MM_basicGeneInfo)
    }
    
    tissueTypeRaw <- dataList[["tissueType"]]
    sampleType(dataList[["sampleType"]])
    tissueType(gsub(tissueTypeRaw, pattern = "0", replacement = " "))
    uiName(paste0(" (",
                     tissueType(), " - ",
                     sampleType(), ")"))
    fileName(paste0(
      tissueType(), "_",
      sampleType()))
    downloadsList[["correlationData"]] <- list(
      "content" = corrData,
      "file" = ".tsv",
      "uiName" = paste0(strong("Correlations: "), stringr::str_to_title(gsub(uiName(), 
                                                       pattern = "\\(|\\)", 
                                                       replacement = "")))
    )
    callModule(module = downloadData, id = "topologyModeDownloads", 
               primaryName = "topologyMode", downloadsListReact = downloadsList)
    
    showTab(inputId = "topologyNavs", 
            target = "downloadsTab", 
            session = session, select = F)
    
    # Mode types
    if ("PCA_plot" %in% names(dataRaw) | "TSNE_plot" %in% names(dataRaw)) {
      dimReduce(TRUE)
      if (length(dataRaw$TSNE_data)) {
        dimMode("TSNE")
        dt_data <- dataRaw$TSNE_data
      } else {
        dimMode("PCA")
        dt_data <- dataRaw$PCA_data
      }
      if(dimMode() == "PCA") {
        if (! dataRaw$clustered) {
          dt_data <- dt_data[,c(3,1,2)]
          colnames(dt_data)[1] <- "geneName"
          dt_data <- merge(y = dt_data, x = basicGeneInfo(), all.y = T, by = "geneName")
          dt_data <- dt_data[order(dt_data$PC1, decreasing = T),]
          plt <- dataRaw$PCA_plot
          plt <- ggplotly(plt)
          plt <- plt %>%
            config(plot_ly(), 
                   toImageButtonOptions = list(
                     filename = paste0(fileName(), "_topologyMode_PCA.png"),
                     format = "png",
                     width = 800,
                     height = 600))
        } else {
          colnames(dt_data)[1] <- "geneName"
          dt_data <- merge(y = dt_data, x = basicGeneInfo(), all.y = T, by = "geneName")
          dt_data <- dt_data[order(dt_data$clusters, -dt_data$PC1),]
          xaxistext <- dataRaw$PCA_plot$labels$x
          yaxistext <- dataRaw$PCA_plot$labels$y
          plt <- plot_ly(dt_data, x = ~PC1 , y = ~PC2, text = ~ geneName,
                         mode = "markers", color = ~clusters, marker = list(size = 7)) %>% 
            layout(title = "PCA with clustering",
                   xaxis = list(title = xaxistext),
                   yaxis = list(title = yaxistext)) %>%
            config(plot_ly(),
                   toImageButtonOptions = list(
                     filename = paste0(fileName(),
                                       "_topologyMode_clusteredPCA.png")))
        }
      } else if (dimMode() == "TSNE") {
        colnames(dt_data)[1] <- "geneName"
        dt_data <- merge(y = dt_data, x = basicGeneInfo(), all.y = T, by = "geneName")
        dt_data <- dt_data[order(dt_data$hclust, dt_data$tsne1),]
        xaxistext <- dataRaw$TSNE_plot$labels$x
        yaxistext <- dataRaw$TSNE_plot$labels$y
        plt <- plot_ly(dt_data, x = ~tsne1 , y = ~tsne2, text =~ geneName,
                     mode = "markers", color = ~hclust, marker = list(size = 7)) 
        plt <- layout(plt, title = "TSNE with clustering", 
                    xaxis = list(title = xaxistext),
                    yaxis = list(title = yaxistext))
        plt <- plt %>%
          config(plot_ly(), displaylogo = F,
                 modeBarButtonsToRemove = list(
                   'hoverCompareCartesian',
                   'hoverClosestCartesian',
                   'zoomIn2d', 'zoomOut2d',
                   'lasso2d'),
                 toImageButtonOptions= list(filename = paste0(fileName(),
                                                              "_topologyMode_TSNE.png"),
                                            format = "png",
                                            width = 1000,
                                            height = 600))
      } 
      PCAPlot(plt)
      rownames(dt_data) <- NULL
      dt_data <- dt_data[,c(-2)]
      # Replace gene name with HTML to call gene info modal
      dt_data[,c(3,4)] <- apply(dt_data[,c(3,4)], 1:2, signif, 3)
      downloadsList[["dimReduceData"]] <- list(
        "content" = dt_data,
        "file" = ".tsv",
        "uiName" = paste0(strong(paste0(dimMode(), " data: ")), stringr::str_to_title(gsub(uiName(), 
                                                                     pattern = "\\(|\\)", 
                                                                     replacement = "")))
      )
      dt_data$geneName <- createGeneInfoLink(dt_data$geneName)
      
      PCADT(list("data" = dt_data,
                 "type" = dimMode(),
                 "cluster" = dataRaw$clustered))
      showTab(inputId = "topologyNavs", 
              target = "dimReduction", 
              session = session, select = F)
    } else {
      dimReduce(FALSE)
      downloadsList[["dimReduceData"]] <- NULL
      hideTab(inputId = "topologyNavs", target = "dimReduction", 
              session = session)
    }
    if ("variantGenesHeatmap" %in% names(dataRaw)) {
      varHeatMode(TRUE)
      variantGenesHeatmap_MAT(dataRaw$variantGenesHeatmap_MAT)
      ddata <- as.data.frame(variantGenesHeatmap_MAT())
      ddata$geneName <- rownames(ddata)
      n <- length(colnames(ddata))
      m <- n-1
      ddata <- ddata[,c(n, 1:m)]
      downloadsList[["variantGenesHeatmapMatrix"]] <- list(
        "content" = ddata,
        "file" = ".tsv",
        "uiName" = paste0(strong("Heatmap data: "), stringr::str_to_title(gsub(uiName(), 
                                                         pattern = "\\(|\\)", 
                                                         replacement = "")))
      )
      showTab(inputId = "topologyNavs", 
              target = "varGenes", select = F,
              session = session)
    } else {
      varHeatMode(FALSE)
      downloadsList[["variantGenesHeatmapMatrix"]] <- NULL
      hideTab(inputId = "topologyNavs", target = "varGenes", 
              session = session)
    }
    if ("inputGenes_pathwayEnrich" %in% names(dataRaw)) {
      pathEnrich(TRUE)
      eres <- dataRaw$inputGenes_pathwayEnrich_data
      shiny::validate(need(length(eres),
                           "No results returned at selected p value cutoff. Please increase it."))
      inputGenes_pathwayEnrich_dotplot(dataRaw$inputGenes_pathwayEnrich_dotplot)
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
      pathwayEnrichDT(eres)
      downloadsList[["pathwayEnrichmentData"]] <- list(
        "content" = as.data.frame(dataRaw$inputGenes_pathwayEnrich),
        "file" = ".tsv",
        "uiName" = paste0(strong("Pathway enrichment: "), stringr::str_to_title(gsub(uiName(), 
                                                               pattern = "\\(|\\)", 
                                                               replacement = "")))
      )
      showTab(inputId = "topologyNavs", 
              target = "pathwayEnrich", select = F,
              session = session)
    } else {
      pathEnrich(FALSE)
      downloadsList[["pathwayEnrichmentData"]] <- NULL
      hideTab(inputId = "topologyNavs", target = "pathwayEnrich", 
              session = session)
    }
    processed(TRUE)
    print("End of topology plots memory: ")
    print(pryr::mem_used())
  })
  
  # Dim reduction
  output$PCAPlot <- renderPlotly({
    req(processed())
    req(dimReduce())
    l <- PCAPlot()
    l$x$layout$width <- NULL
    l$x$layout$height <- NULL
    l$width <- NULL
    l$height <- NULL
    l
  })
  outputOptions(output, 'PCAPlot', suspendWhenHidden = FALSE)
  
  output$PCADT <- DT::renderDataTable({
    # Make dimension reduction datatables output
    req(processed())
    req(dimReduce())
    dt_data <- PCADT()$data
    dt_data <- unique(dt_data)
    if (dimMode() == "TSNE") {
      cols <- c("Gene Name",
                "Description", "TSNE 1", "TSNE 2", "Cluster")
      filterStr <- list(position = "top", plain = TRUE)
    } else {
      cluster <- PCADT()$cluster
      cols <- c("Gene Name",
                "Description", "PC1", "PC2")
      if (cluster) {
        cols <- c(cols, "Cluster")
        filterStr <- list(position = "top", plain = TRUE)
      } else {
        filterStr <- "none"
      }
    }
    DT_out <- datatable(dt_data, selection = "single",
                        rownames = F, escape = F, 
                        colnames = cols, filter = filterStr,
                        options = list(dom = "ftprl",
                                       scrollX = TRUE,
                                       pageLength = 6))
    DT_out
  })
  # outputOptions(output, 'PCADT', suspendWhenHidden = FALSE)
  # observeEvent(PCADT(), {
  #   PCADT_proxy <- dataTableProxy("PCADT", session = session,
  #                                 deferUntilFlush = TRUE)
  #   reloadData(PCADT_proxy)
  # })
  
  # Variant heat genes
  varHeat <- reactive({
    req(processed())
    req(varHeatMode())
    plt_dat <- variantGenesHeatmap_MAT()
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
    plt <- heatmaply(plt_dat, hide_colorbar = TRUE, 
                   limits = c(-1*newVal, newVal), 
                   colors = gplots::greenred(100), showticklabels = c(T, F))
    plt <- plt %>%
      config(plot_ly(), displaylogo = F,
             modeBarButtonsToRemove = list(
               'hoverCompareCartesian',
               'hoverClosestCartesian',
               'zoomIn2d', 'zoomOut2d',
               'lasso2d'),
             toImageButtonOptions= list(filename = paste0(fileName(),
                                                          "_topologyMode_variantGenesHeatmap.png"),
                                        format = "png",
                                        width = width,
                                        height = 500))
    plt
  })
  output$varHeat <- renderPlotly({
    req(processed())
    req(varHeatMode())
    withProgress(value = .4, message = "Rendering interactive heatmap ... ", {
      varHeat()
    })
  })
  outputOptions(output, 'varHeat', suspendWhenHidden = FALSE)
  
  # Pathway enrichment
  output$pathwayEnrichPlot <- renderPlot({
    inputGenes_pathwayEnrich_dotplot()
  })
  output$pathwayEnrichDT <- renderDataTable({
    pathwayEnrichDT()
  }, rownames = F, escape = F, 
  options = list(
    pageLength = 8,
    autoWidth = TRUE,
    columnDefs = list(list(width = '200px', targets = c(4)),
                      list(width = '100px', targets = c(1,2,3))),
    dom = "ftprl",
    scrollX = TRUE
  ), colnames = c("Pathway", "Gene ratio",
                 "Pval", "Padj", "Genes included"))
  outputOptions(output, 'pathwayEnrichDT', suspendWhenHidden = FALSE)
  observeEvent(pathwayEnrichDT(), {
    pathwayEnrichDT_proxy <- dataTableProxy("pathwayEnrichDT", session = session,
                                            deferUntilFlush = TRUE)
    reloadData(pathwayEnrichDT_proxy)
  })
  
}


