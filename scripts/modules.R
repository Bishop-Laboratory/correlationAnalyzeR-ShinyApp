library(shiny)
library(plotly)
library(heatmaply)
library(DT)
library(shinyBS)
source("scripts/helpers.R")

## Generic helper modules ##
# Input types
singleGeneInputUI <- function(id, label) {
  ns <- NS(id)
  popify(
    selectizeInput(inputId = ns("primaryGene"), label = label,
                   choices = NULL, 
                   multiple = F, options = list(maxOptions = 100)),
    placement = "right", 
    title = paste0(label, " input"), options=list(container="body"),
    content = paste0("Enter a primary gene of interest. ",
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
    print(names(downloadsList))
    downloadBoxUI <- tagList()
    downloadBoxUI <- lapply(1:length(names(downloadsList)), function(i) {
      print(i)
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
    print(names(downloadsList))
    lapply(1:length(names(downloadsList)), function(i) {
      print(paste0("download", names(downloadsList)[i]))
      print(paste0(primaryName, "_", names(downloadsList)[i], downloadsList[[i]]$file))
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
    popify(
      selectInput(inputId = ns("sampleType"), label = "Select tissue type",
                  choices = c("Normal", "Tumor")),
      placement = "right", 
      title = "Select tissue type", options=list(container="body"),
      content = paste0('Correlations were processed separately by tissue type. ',
                       'Specify which correlations to analyze by choosing "Normal" or "Tumor"')
    ),
    popify(
      radioButtons(inputId = ns("gseaType"), label = "GSEA type",
                   choices = c("Simple", "Complex", "None")),
      placement = "right", 
      title = 'Select GSEA type', options=list(container="body"),
      content = paste0('Use GSEA to discover pathways that correlate ',
                       'with your gene of interest. ',
                       'Choose "Simple" to rapidly enrich for common genesets',
                       ' (MSIGDB genesets "H", "C1", "C2", "C5", and "C6").',
                       ' Choose "Complex" to consider all MSIGDB genesets, or choose "None" ',
                       'to skip GSEA entirely.')
    ),
    popify(
      numericInput(inputId = ns("pval"), value = .05,
                   label = "GSEA P val Cutoff", max = 1, min = 0, step = .001),
      placement = "right", 
      title = 'Select GSEA P value cutoff', options=list(container="body"),
      content = paste0('Select the maximum p value for GSEA results. Pathways ',
                       'correlated with a p value above this level are not returned.')
    ),
    fluidRow(
      column(4, actionButton(ns("do"), "Analyze"))
    )
  )
}

singleModeAnalysis <- function(input, output, session, 
                               parent_session, GlobalData) {
  
  species <- reactive({input$species})
  do <- reactive({input$do})
  # do <- reactive({input$do})
  print(species)
  print(do)
  # print(do)
  primaryGene <- callModule(singleGeneInput, "singleGeneInput",
                            parent_session = parent_session,
                            GlobalData = GlobalData,
                            species = species)
  print(primaryGene)
  
  observeEvent(input$do, {
    primaryGene <- primaryGene()
    print(primaryGene)
    print("DO")
  })
  
  data <- reactiveVal()
  
  # Observe the value of 'do' and run the analysis if it's above 1
  data <- eventReactive(eventExpr = input$do, {
    primaryGene <- primaryGene()
    species <- input$species
    sampleType <- input$sampleType
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
                            GlobalData = GlobalData,
                            session = session)
    
    progress$inc(.2, message = paste0("Gathering correlations for ", 
                                      primaryGene, " ... "))
    
    data <- correlationAnalyzeR::getCorrelationData(Species = cleanRes$selectedSpecies,
                                                    Sample_Type = cleanRes$sampleType,
                                                    geneList = cleanRes$primaryGene)
    data <- cbind(rownames(data), data)
    colnames(data)[1] <- "geneName"
    data <- data[which(data[,1] != cleanRes$primaryGene),]
    rownames(data) <- NULL
    data <- merge(x = cleanRes$basicGeneInfo, y = data, by = "geneName")
    # Enable the download button
    
    print("inside single mode do: ")
    print(input$do)
    res <- list("correlationData" = data,
                "species" = species,
                "gseaType" = gseaType,
                "pval" = pval,
                "primaryGene" = primaryGene,
                "progress" = progress)
    res
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
    correlationData <- dataList[["correlationData"]]
    primaryGene <- dataList[["primaryGene"]]
    
    print("Switching to single-mode view")
    # showTab(inputId = "tabs", target = "singleModeTab", 
    #         select = TRUE, session = parent_session)
    
    
    
    observeEvent(eventExpr = (! is.null(dataList)), {
      
      progress <- dataList[['progress']]
      progress$inc(.2, message = "Returning results ... ")
      
      downloadsList <- reactiveValues()
      correlationData <- correlationData[order(correlationData[,4], decreasing = T),]
      downloadsList[["correlationData"]] <- list("content" = correlationData,
                                                 "uiName" = paste0(primaryGene, 
                                                                   " correlation data"),
                                                 "file" = ".tsv")
      
      output$geneHist <- renderPlotly({
        colnames(correlationData)[4] <- "vals"
        p <- ggpubr::gghistogram(data = correlationData, x = "vals", y = "..count..",
                                 bins = 100, ylab = "Frequency\n",
                                 title = primaryGene, 
                                 xlab = paste0(primaryGene, " correlation values"))
        
        p <- plotly::ggplotly(p, source = "singleSelect") %>%
          # plotly::layout(dragmode = "select") %>%
          config(plot_ly(),
                 toImageButtonOptions= list(filename = paste0(primaryGene, "_correlationHistogram.png")))
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
      
      # # Get indices of plotly-selected genes
      # selection2 <- reactive({
      #   s <- event_data("plotly_selected", source = "singleSelect")
      #   df <- data.frame(s)
      #   df
      # })
      
      # Make correlations datatable
      output$correlationData <- DT::renderDataTable(server = T,{
        correlationData$geneName <- createGeneInfoLink(correlationData$geneName)
        # s1 <- selection2()
        # s1 <- as.data.frame(s1)
        # if (length(rownames(s1)) > 0) {
        #   maxs <- max(s1$x) + .05
        #   mins <- min(s1$x) - .05
        #   d2 <- correlationData[which(correlationData[,4] < maxs & correlationData[,4] > mins ),]
        # } else {
        #   d2 <- correlationData
        # }
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
        output$gseaUI <- renderUI({
          ns <- session$ns
          tagList(
            hr(),
            h3("GSEA results"),
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
        
        print("Running GSEA ... ")
        print(species)
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
                                                TERM2GENE = TERM2GENE, 
                                                padjustedCutoff = pval,
                                                returnDataOnly = T,
                                                topPlots = F)
        # Check if p cutoff value was raised 
        eres <- gseaData$eres
        minp <- min(eres$p.adjust)
        print("PVALUE COMPA")
        print(minp)
        print(pval)
        if (minp > pval) {
          msg <- paste0("No results returned at p value of ", pval, 
                        ". GSEA p value cutoff was raised to accommodate.")
          shiny::showNotification(ui = msg, type = "warning")
        }
        downloadsList[['gseaData']] <- list("content" = eres,
                                            "file" = ".tsv",
                                            "uiName" = paste0(primaryGene, 
                                                              " GSEA data"))
        eres <- eres[,c(2, 5, 6, 7)]
        eres <- eres[order(eres[,2], decreasing = T),]
        eres[,c(2,3,4)] <- apply(eres[,c(2,3,4)], 1:2, round, digits = 4)
        eresTitles <- eres$Description
        eresTitles <- gsub(eresTitles, pattern = "_", replacement = " ")
        eresTitles <- tolower(eresTitles)
        eresTitles <- tools::toTitleCase(text = eresTitles)
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
          print("inside plot")
          s <- input$gseaData_rows_selected
          if (length(s)) {
            id <- eres[s,1]
            print(id)
            titleID <- eresTitles[s]
            print(titleID)
          } else{
            print("NO ID Selected ----- showing top")
            id <- eres[1,1]
            titleID <- eresTitles[1]
          }
          
          p <- clusterProfiler::gseaplot(gseaData$EGMT, 
                                         geneSetID = id, 
                                         title = titleID)
          p
        })
        
        # # This isn't producing the selected plot, only the first one -- for future release?
        # downloadsList[['gseaPlot']] <- list("content" = plotGSEA(),
        #                                     "file" = ".png",
        #                                     "uiName" = "Selected GSEA plot")
        
        output$plotGSEA <- renderPlot({
          plotGSEA()
        })
        progress$close()
      } else {
        output$gseaUI <- renderUI({
          tagList(
            hr(),
            h4(em("Choose a 'GSEA type' to analyze correlated pathways.")),
            hr()
          )
        })
        progress$close()
      }
      
      
      primaryName <- primaryGene
      
      callModule(module = downloadData, id = "singleModeDownloads", 
                 primaryName = primaryName, downloadsListReact = downloadsList)
      
    })
  })
}

## Paired-mode analysis ##
pairedModeAnalysisUI <- function(id) {
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
    popify(
      selectInput(inputId = ns("sampleType"), label = "Select tissue type",
                  choices = c("Normal", "Tumor")),
      placement = "right", 
      title = "Select tissue type", options=list(container="body"),
      content = paste0('Correlations were processed separately by tissue type. ',
                       'Specify which correlations to analyze by choosing "Normal" or "Tumor"')
    ),
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
pairedModeAnalysis <- function(input, output, session, 
                               parent_session, GlobalData) {
  
  species <- reactive({input$species})
  do <- reactive({input$do})
  # do <- reactive({input$do})
  print(species)
  print(do)
  
  
  primaryGene <- callModule(singleGeneInput, "singleGeneInput",
                            parent_session = parent_session,
                            GlobalData = GlobalData,
                            species = species)
  print(primaryGene)
  secondaryGenes <- callModule(multiGeneInput, "multiGeneInput")
  print(secondaryGenes)
  
  data <- reactiveVal()
  data <- eventReactive(eventExpr = input$do, {
    primaryGene <- primaryGene()
    secondaryGenes <- secondaryGenes()
    species <- input$species
    sampleType <- input$sampleType
    sigTest <- input$sigTest
    print(sigTest)
    print(secondaryGenes)
    # Clean secondaryGenes input
    secondaryGenes <- strsplit(secondaryGenes, split = "\n")
    secondaryGenes <- unlist(secondaryGenes, use.names = F)
    print(secondaryGenes)
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
                            GlobalData = GlobalData,
                            session = session)
    pass <- 1
    # print(cleanRes)
    if (is.null(cleanRes$secondaryGenes)) {
      print("FAIL")
      showNotification(ui = "No valid secondary genes provided.", 
                       duration = 8, type = 'error')
      pass <- 0
      progress$close()
    } else if (sigTest & length(unique(cleanRes$secondaryGenes)) < 2) {
      showNotification(ui = "Significance testing requires 2+ secondary genes", 
                       duration = 8, type = 'error')
      sigTest <- FALSE
      
    }
    shiny::validate(
      need(pass == 1, message = "No valid genes provided. ")
    )
    pairedGenesList <- list(cleanRes$secondaryGenes)
    names(pairedGenesList) <- cleanRes$primaryGene
    
    progress$inc(.2, message = "Analyzing correlations ... ")
    
    # # BugTesting
    # pairedGenesList <- list("BRCA1" = c("ATM", "BRCA2", "BRCC3"))
    # cleanRes <- list("selectedSpecies" = "hsapiens",
    #                  "sampleType" = "Normal_Tissues")
    # sigTest <- F
    set.seed(1) #Reproducible
    data <- correlationAnalyzeR::pairedGenesAnalyzeR(pairedGenesList = pairedGenesList, 
                                                     Species = cleanRes$selectedSpecies, 
                                                     Sample_Type = cleanRes$sampleType, 
                                                     plotLabels = F, plotMaxMinCorr = T, 
                                                     sigTest = sigTest, returnDataOnly = T,
                                                     autoRug = T, plotTitle = F, onlyTop = F)
    
    data <- data[[1]]
    res <- list("pairedModeData" = data,
                "primaryGene" = cleanRes$primaryGene,
                "species" = species,
                "sigTest" = sigTest,
                "progress" = progress)
    res
  })
  return(data)
}
# Plotting/reporting
pairedModePlotsUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      uiOutput(outputId = ns("correlationsUI")),
      uiOutput(outputId = ns("sigTestPlotsUI")),
      downloadDataUI(ns("pairedModeDownloads")),
      br()
    )
  )
}


pairedModePlots <- function(input, output, session, 
                            parent_session,
                            GlobalData, dataTables) {
  
  observe({
    
    dataListReact <- dataTables[["pairedModeData"]]
    dataList <- dataListReact()
    
    # # Switch to paired mode view when called
    # print("switching to paired mode view")
    # showTab(inputId = "tabs", target = "pairedModeTab", 
    #         select = TRUE, session = parent_session)
    
    # dataList <- data
 
    data <- dataList[[1]]
    species <- dataList[["species"]]
    primaryGene <- dataList[["primaryGene"]]
    if (species == "Human") {
      GeneInfo <- GlobalData$HS_basicGeneInfo
    } else {
      GeneInfo <- GlobalData$MM_basicGeneInfo
    }
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
                               title = primaryGene, 
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
                 filename = paste0(primaryGene, 
                                   "_pairedMode_correlationHistogram.png"),
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
      
      
      # p <- plot_ly(corrDataDF, x = ~vals, type = 'histogram', 
      #              text = ~paste('Gene: ', geneName, " - Value: ", round(vals, 3)))  %>%
      #   layout(title = primaryGene) %>%
      #   config(plot_ly(),
      #          toImageButtonOptions = list(filename = paste0(primaryGene, 
      #                                                        "_pairedMode_correlationHistogram.png")))
      # corrDataDF2 <- corrDataDF[which(corrDataDF$secondaryGene),]
      # p <- p %>% 
      #   add_segments(data = corrDataDF2, x = ~vals, 
      #                xend = ~vals, y = 0, 
      #                yend = 150, 
      #                opacity = .5) %>%
      #   layout(showlegend = FALSE, xaxis = a)
      # s <- input$correlationData_rows_selected
      # if (length(s)) {
      #   corrDataDF3 <- corrDataDF2[s,]
      #   p <- p %>% 
      #     add_segments(data = corrDataDF3, x = ~vals, 
      #                  xend = ~vals, y = 0, yend = 150) 
      # } else {
      #   p <- p %>% highlight("plotly_selected", color = I('green'))
      # }
      p
    })
    
    corrValDFReact <- reactive({
      # Setup dataframe to match plotly data
      corrValDF <- data$Correlation_Values
      corrValDF <- data.frame(geneName = names(corrValDF), Values = corrValDF, row.names = NULL)
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
    
    
    callModule(module = downloadData, id = "pairedModeDownloads", 
               primaryName = "pairedMode", downloadsListReact = downloadsList)
    
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
        
        # Get plots and convert to plotly
        # p1 <- data$sigTest$meansPlot
        # p1 <- ggplotly(p1)
        # p2 <- data$sigTest$mediansPlot
        # p2 <- ggplotly(p2)
        p3 <- data$sigTest$tTest_pvalsPlot
        p3 <- ggplotly(p3)
        # p4 <- subplot(p1, p2, p3, titleX = T, widths = c(.3, .3, .4))
        p3 <- p3 %>% layout(title = paste0(primaryGene, " correlation with secondary genes vs random"),
                            xaxis = list(title = 't.test p value')) %>%
          layout(autosize = F, margin = m) %>%
          config(plot_ly(), 
                 toImageButtonOptions = list(
                   filename = paste0(primaryGene, 
                                     "_correlation_sigTest.png"),
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
    popify(
      selectInput(inputId = ns("sampleType"), label = "Select tissue type",
                  choices = c("Normal", "Tumor")),
      placement = "right", 
      title = "Select tissue type", options=list(container="body"),
      content = paste0('Correlations were processed separately by tissue type. ',
                       'Specify which correlations to analyze by choosing "Normal" or "Tumor"')
    ),
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
  
  secondaryGenes <- callModule(multiGeneInput, "multiGeneInput")
  
  observeEvent(input$do, {
    secondaryGenes <- secondaryGenes()
    print(secondaryGenes)
    print("DO")
  })
  
  data <- reactiveVal()
  data <- eventReactive(eventExpr = input$do, {
    secondaryGenes <- secondaryGenes()
    species <- input$species
    sampleType <- input$sampleType
    crossComparisonType <- input$crossComparisonType
    print(crossComparisonType)
    print(secondaryGenes)
    # Clean secondaryGenes input
    secondaryGenes <- strsplit(secondaryGenes, split = "\n")
    secondaryGenes <- unlist(secondaryGenes, use.names = F)
    print(secondaryGenes)
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
                            GlobalData = GlobalData,
                            session = session)
    # Return plots + correlation data
    progress$inc(.2, message = "Analyzing geneset topology ... ")
    # Warn if using less than 10 genes for pathway enrichment
    pass <- 1
    print(cleanRes$secondaryGenes)
    print(cleanRes$geneSetInputType)
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
                                                        alternativeTSNE = T, 
                                                        returnDataOnly = T, 
                                                        pathwayEnrichment = F, 
                                                        crossComparisonType = crossComparisonType,
                                                        Sample_Type = cleanRes$sampleType, 
                                                        Species = cleanRes$selectedSpecies)
    res <- list("topologyModeData" = data,
                "species" = species,
                "crossComparisonType" = crossComparisonType,
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
    print(names(dataList))
    
    # print("switching to topology mode view")
    # showTab(inputId = "tabs", target = "topologyModeTab", 
    #         select = TRUE, session = parent_session)
    
    do <- dataList$do
    print(do)
    observeEvent(eventExpr = dataList, {

      selectedSpecies <- dataList$species
      data <- dataList$topologyModeData
      print(names(data))
      print(selectedSpecies)
      
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
        "uiName" = "Correlation values"
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
                       toImageButtonOptions = list(filename = paste0("topologyMode_clusteredPCA.png")))
            } else {
              plt <- data$PCA_plot
              p <- ggplotly(plt)
              p <- p %>%
                config(plot_ly(), 
                       toImageButtonOptions = list(
                         filename = paste0("topologyMode_PCA.png"),
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
          print("DT Observed!")
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
                     filename = paste0("topologyMode_variantGenesHeatmap.png"),
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
          print("PLOTLY SELECTED")
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
            print("SELECTION VARHEAT RENDERING IN DT")
            x <- s$x[1]
            y <- s$y[1]
            z <- s$z[1]
            print(x)
            print(y)
            print(z)
            
            str(x)
            str(y)
            
            plot_pinpoint <- as.character(plot_text_df[y,x])
            selectSize <- input$varHeatSelectSize
            print("SELECT SIZE")
            print(selectSize)
            y_pos_vec <- c(y-selectSize, y + selectSize)
            y_pos_vec[which(y_pos_vec < 1)] <- 1
            y_pos_vec[which(y_pos_vec > 1500)] <- 1500
            plot_pinpoints <- as.character(plot_text_df[c(y_pos_vec[1]:y_pos_vec[2]),x])
            print(plot_pinpoints)
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
          print("DT varHeat Observed!")
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
          shiny::validate(need(length(eres), "No results returned at selected p value cutoff. Please increase it."))
          dp <- data$inputGenes_pathwayEnrich_dotplot
          dp
        })
        
        pathwayEnrichDT <- reactive({
          eres <- data$inputGenes_pathwayEnrich_data
          eres <- eres[,c(2, 3, 5, 6, 8)]
          eres <- eres[order(eres[,2], decreasing = T),]
          if (length(eres$Description) > 100) {
            eres <- eres[c(1:100),]
          }
          eres[,c(3,4)] <- apply(eres[,c(3,4)], 1:2, round, digits = 4)
          eresTitles <- eres$Description
          eresTitles <- gsub(eresTitles, pattern = "_", replacement = " ")
          eresTitles <- tolower(eresTitles)
          eresTitles <- tools::toTitleCase(text = eresTitles)
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
          print("DT Observed!")
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




