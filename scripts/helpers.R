# Removes temporary files from www directory
removeTmp <- function() {
  unlink(x = "www/tmp")
  dir.create("www/tmp")
}


# UI function to make ? button
helpButton <- function(message) {
  return(
    tipify(
      span(
          HTML('<i class="fa fa-question-circle"></i>')), 
       title = message, placement = "right",
      options=list(container="body")
    )
  )
  
}

# Get TERM2GENE from MDF object
MDFtoTERM2GENE <- function(MDF, GSEA_Type, species) {
  
  # # Bug testing
  # GSEA_Type <- "Basic"
  # species <- "hsapiens"
  
  if (species == "hsapiens") {
    toGrab <- 3
  } else {
    toGrab <- 4
  }
  print(GSEA_Type)
  # Filter for pathways of interest
  optionsNow <- c("Basic", "All", unique(MDF$gs_cat))
  GSEA_Type <- gsub(GSEA_Type, pattern = "miRNAs", 
                    replacement = "miRNA targets", perl = TRUE)
  GSEA_Type <- gsub(GSEA_Type, pattern = "Transcription factors", 
                    replacement = "TF targets", perl = TRUE)
  GSEA_Type <- gsub(GSEA_Type, pattern = "Biological process", 
                    replacement = "GO:BP", perl = TRUE)
  GSEA_Type <- gsub(GSEA_Type, pattern = "Cellular component", 
                    replacement = "GO:CC", perl = TRUE)
  GSEA_Type <- gsub(GSEA_Type, pattern = "Molecular function", 
                    replacement = "GO:MF", perl = TRUE)
  GSEA_Type <- gsub(GSEA_Type, pattern = "Molecular perturbations", 
                    replacement = "Perturbations", perl = TRUE)
  GSEA_Type <- gsub(GSEA_Type, pattern = "Immuno signatures", 
                    replacement = "Immunological signatures", perl = TRUE)
  GSEA_Type <- gsub(GSEA_Type, pattern = "Onco signatures",
                    replacement = "Oncogenic signatures", perl = TRUE)
  GSEA_Type <- gsub(GSEA_Type, pattern = "Oncogene neighborhoods",
                    replacement = "Cancer gene neighborhoods", perl = TRUE)
  
  
  if (! all(GSEA_Type %in% optionsNow)) {
    stop("\nPlease enter a valid GSEA_Type. Use ?getTERM2GENE to see available options.\n")
  }
  categories <- c()
  if ("Basic" %in% GSEA_Type) {
    categories <- c(categories, "Hallmark", "Perturbations", "BioCarta",
                    "GO:BP", "KEGG", "Canonical pathways", "Reactome", "GO:MF", "GO:CC", "PID")
  }
  if ("All" %in% GSEA_Type) {
    categories <- c(categories, optionsNow)
  }
  categories <- unique(c(categories, GSEA_Type))
  colnames(MDF)[toGrab] <- "gene_symbol"
  TERM2GENE <- MDF %>%
    filter(.data$gs_cat %in% categories) %>%
    select(.data$gs_name, .data$gene_symbol) %>%
    filter(! is.na(.data$gene_symbol)) %>%
    distinct()
  
  return(TERM2GENE)
}

# Fixes uiNames into colnames
convertToColnames <- function(uiNameRaw) {
  colName <- gsub(uiNameRaw, pattern = " - ", replacement = "_")
  colName <- gsub(colName, pattern = " ", replacement = "_")
  colName <- gsub(colName, pattern = "\\(|\\)", replacement = "")
  return(colName)
}

# Create links in datatabels for gene info modal
createGeneInfoLink <- function(val) {
  sprintf(paste0('<a href="https://www.genecards.org/cgi-bin/carddisp.pl?gene=', val ,'" target="_blank" class="tooltip-test" onClick="gene_click(this.id)" title="', val, '">', val, '</a>'))
  
}

# Create links in datatables for GSEA info from MSIGDB
createGSEAInfoLink <- function(val, valTitle) {
  sprintf(paste0('<a href="http://software.broadinstitute.org/gsea/msigdb/cards/', val,'" id="', val ,'" target="_blank" class="tooltip-test" onClick="gsea_click(this.id)" title="', val, '">', valTitle, '</a>'))
}

# Get heatmap breaks
getPhBreaks <- function(mat, palette = NULL) {
  # From https://stackoverflow.com/questions/31677923/set-0-point-for-pheatmap-in-r
  if (is.null(palette)) {
    palette <- grDevices::colorRampPalette(rev(
      RColorBrewer::brewer.pal(n = 7, name =
                                 "RdYlBu")))(100)
  }
  n <- length(palette)
  breaks <- c(seq(min(mat), 0, length.out=ceiling(n/2) + 1),
              seq(max(mat)/n, max(mat), length.out=floor(n/2)))
  return(list(palette, breaks))
}

# Pretty pathEnrich plot
prettyPathEnrich <- function(eres) {
  eres$GeneRatio <- sapply(eres$GeneRatio, function(x) eval(parse(text=x)))
  eresTitles <- eres$Description
  eresTitles <- correlationAnalyzeR::fixStrings(eresTitles)
  eresTitles[which(nchar(eresTitles) > 45)] <- paste0(
    substr(eresTitles[which(nchar(eresTitles) > 45)], 1, 41), "..."
  )
  eres$Description <- eresTitles
  eres$p.adjust <- -log10(eres$p.adjust)
  eres <- eres[order(eres$GeneRatio, decreasing = TRUE),]
  eres <- eres[c(1:12),]
  order <- rev(eres$Description)
  bp <- ggpubr::ggbarplot(data = eres, x = "Description",
                          y = "GeneRatio", ylab = "\nGene Ratio",
                          fill = "p.adjust", order = order,
                          legend.title = "-log10(pAdj)",
                          legend = "right") +
    ggpubr::rremove("ylab") +
    ggpubr::rotate() 
  return(bp)
}

# Function to convert a vector of old gene symbols to new ones
symbolConverter <- function(symbolVec, species, pool) {
  # Provide a vector of gene symbols to check and convert
  
  ## Bug testing
  # species <- "mmusculus"
  # symbolVec <- c("ATM", "BRCA1", "ATF4")
  ## symbolVec <- selectedGenes
  
  n <- length(symbolVec)
  if (species == "mmusculus") {
    dbCon <- org.Mm.eg.db::org.Mm.eg_dbconn()
    
  } else if (species == "hsapiens") {
    dbCon <- org.Hs.eg.db::org.Hs.eg_dbconn()
    
  }
  sqlQuery <- 'SELECT * FROM alias, gene_info WHERE alias._id == gene_info._id;'
  aliasSymbol <- DBI::dbGetQuery(dbCon, sqlQuery)
  aliasSymbol <- as.data.frame(aliasSymbol)
  avgenes <- correlationAnalyzeR::getAvailableGenes(Species = species, pool = pool)
  if (is.null(avgenes)) {
    stop("Unable to connect to database. Please contact package maintainer",
         " if you believe this is in error. ")
  }
  aliasSymbol <- aliasSymbol[which(aliasSymbol$symbol %in% avgenes),]
  
  checkTheGene <- function(geneToCheck, aliasSymbol) {
    # symbolVec <- c("NFKB", "NRF2", "ASDNasd")
    # geneToCheck <- symbolVec[1]
    # Check official symbols
    newGene <- aliasSymbol$symbol[grep(aliasSymbol$symbol, pattern = geneToCheck, ignore.case = T)][1]
    # Make sure length is matching for inclusion errors.  e.g. 'NRF2' returns 'LONRF2' erroneously
    if (! is.na(newGene) & nchar(newGene) != nchar(geneToCheck)) {
      newGene <- NA
    }
    if (is.na(newGene)) {
      # Check aliases
      aliases <- aliasSymbol[grep(aliasSymbol$alias_symbol, 
                                  pattern = geneToCheck,
                                  ignore.case = T),]
      
      if (! length(aliases$alias_symbol)) {
        # unresolvableGenes <- c(unresolvableGenes, geneToCheck)
        resDF <- data.frame("category" = rep("unresolvableGenes", length(geneToCheck)),
                            "geneNames" = geneToCheck)
      } else {
        # Check for multimapped genes
        aliases <- aliasSymbol[grep(aliasSymbol$alias_symbol,
                                    pattern = geneToCheck, 
                                    ignore.case = T),]
        aliases <- aliases[which(nchar(aliases$alias_symbol) == nchar(geneToCheck)),]
        # resGenes <- c(resGenes, unique(aliases$symbol))
        if (length(unique(aliases$symbol)) > 1) {
          resDF1 <- data.frame("category" = rep("multiMappedGenes", length(unique(aliases$symbol))),
                              "geneNames" = unique(aliases$symbol))
          resDF2 <- data.frame("category" = rep("resGenes", length(unique(aliases$symbol))),
                               "geneNames" = unique(aliases$symbol))
          resDF <- rbind(resDF1, resDF2)
        } else {
          resDF <- data.frame("category" = rep("resGenes", length(unique(aliases$symbol))),
                              "geneNames" = unique(aliases$symbol))
        }
      }
    } else {
      resDF <- data.frame("category" = rep("resGenes", length(newGene)),
                          "geneNames" = newGene)
    }
    return(resDF)
  }
  if (length(symbolVec) > 1) {
    resList <- lapply(symbolVec, checkTheGene, aliasSymbol = aliasSymbol)
    resDF <- data.table::rbindlist(resList)
    resList <- split(resDF, resDF$category)
    resGenes <- as.character(resList$resGenes$geneNames)
    confusedGenes <- as.character(resList$multiMappedGenes$geneNames)
    unresolvableGenes <- as.character(resList$unresolvableGenes$geneNames)
    resList <- list("unresolvableGenes" = unresolvableGenes, 
                    "multiMappedGenes" = confusedGenes, 
                    "resGenes" = resGenes)
  } else {
    resList <- lapply(symbolVec, checkTheGene, aliasSymbol = aliasSymbol)
    resDF <- data.table::rbindlist(resList)
    resList <- split(resDF, resDF$category)
    resGenes <- as.character(resList$resGenes$geneNames)
    confusedGenes <- as.character(resList$multiMappedGenes$geneNames)
    unresolvableGenes <- as.character(resList$unresolvableGenes$geneNames)
    resList <- list("unresolvableGenes" = unresolvableGenes, 
                    "multiMappedGenes" = confusedGenes, 
                    "resGenes" = resGenes)
  }
  return(resList)
}

# Script to create the usable gene guide
createGeneGuide <- function() {
  require(correlationAnalyzeR)
  # Human
  dbCon <- org.Hs.eg.db::org.Hs.eg_dbconn()
  sqlQuery <- 'SELECT * FROM alias, gene_info WHERE alias._id == gene_info._id;'
  aliasSymbol <- DBI::dbGetQuery(dbCon, sqlQuery)
  aliasSymbol <- as.data.frame(aliasSymbol)
  hsgenes <- correlationAnalyzeR::getAvailableGenes(Species = 'hsapiens')
  aliasSymbol <- aliasSymbol[which(aliasSymbol$symbol %in% hsgenes$geneName),]
  humanSymbols <- aliasSymbol[,c(2, 5, 4)]
  toAdd <- data.frame(alias_symbol = humanSymbols$symbol, 
                      symbol = humanSymbols$symbol, 
                      gene_name = humanSymbols$gene_name)
  toAdd <- unique(toAdd)
  humanSymbolsFinal <- rbind(humanSymbols, toAdd)
  humanSymbolsFinal$species <- "hsapiens"
  
  dbCon <- org.Mm.eg.db::org.Mm.eg_dbconn()
  sqlQuery <- 'SELECT * FROM alias, gene_info WHERE alias._id == gene_info._id;'
  aliasSymbol <- DBI::dbGetQuery(dbCon, sqlQuery)
  aliasSymbol <- as.data.frame(aliasSymbol)
  mmgenes <- correlationAnalyzeR::getAvailableGenes(Species = 'mmusculus', pool = pool)
  aliasSymbol <- aliasSymbol[which(aliasSymbol$symbol %in% mmgenes$geneName),]
  mouseSymbols <- aliasSymbol[,c(2, 5, 4)]
  toAdd <- data.frame(alias_symbol = mouseSymbols$symbol, 
                      symbol = mouseSymbols$symbol, 
                      gene_name = mouseSymbols$gene_name)
  toAdd <- unique(toAdd)
  mouseSymbolsFinal <- rbind(mouseSymbols, toAdd)
  mouseSymbolsFinal$species <- "mmusculus"
  
  symbolsFinal <- rbind(humanSymbolsFinal, mouseSymbolsFinal)
  symbolsFinal <- symbolsFinal[,c(1, 4)]
  # Remove '('-containing gene aliases
  symbolsFinal <- symbolsFinal[grep(symbolsFinal$alias_symbol, 
                                    pattern = "\\(", 
                                    invert = T),]
  save(symbolsFinal, file = "data/symbol_suggestions.RData")
  
}


cleanInputs <- function(primaryGene = NULL, 
                        secondaryGenes = NULL, 
                        selectedSpecies,
                        sampleType,
                        tissueType,
                        GlobalData,
                        session,
                        pool) {
  

  # # # Bug testing
  # primaryGene <- "BRCA1"
  # selectedSpecies <- "Human"
  # sampleType <- "normal"
  # tissueType <- "all"
  # secondaryGenes <- c("AAACCAC_MIR140")
    
  MM_basicGeneInfo <- GlobalData$MM_basicGeneInfo
  HS_basicGeneInfo <- GlobalData$HS_basicGeneInfo
  # Convert to correlationAnalyzeR inputs
  if (selectedSpecies == "Human") {
    selectedSpecies <- "hsapiens"
    basicGeneInfo <- HS_basicGeneInfo
  } else {
    selectedSpecies <- "mmusculus"
    basicGeneInfo <- MM_basicGeneInfo
  }
  if (sampleType == "normal") {
    sampleType <- "normal"
  } else {
    sampleType <- "cancer"
  }
  
  resList <- list()
  resList[["selectedSpecies"]] <- selectedSpecies
  resList[["sampleType"]] <- sampleType
  resList[["tissueType"]] <- tissueType
  resList[["basicGeneInfo"]] <- basicGeneInfo
  
  
  # Validate primary gene
  if (! is.null(primaryGene)){
    # Handle bad gene name here
    res <- symbolConverter(symbolVec = primaryGene,
                           species = selectedSpecies, pool = pool)
    unresolvableGenes <- res$unresolvableGenes
    if (length(res$resGenes)) {
      multiMappedGenes <- res$multiMappedGenes
      if (length(multiMappedGenes)) {
        inputAlias <- primaryGene
        mappedSymbols <- multiMappedGenes
        msg <- paste0("Input '", inputAlias, 
                      "' returned multiple official gene symbols: '",
                      paste0(mappedSymbols, collapse = "', '"), 
                      "'. Only the first will be used, '", res$resGenes[1], "'.")
        showNotification(id = "multi-mapped-gene-warning", ui = msg, session = session,
                         closeButton = T, type = "warning", duration = 10)
      }
      primaryGeneNow <- res$resGenes[1]
      resList[["primaryGene"]] <- primaryGeneNow
    }
  } 
  # Validate secondary genes
  if (! is.null(secondaryGenes)) {
    possibleGenesets <- correlationAnalyzeR::MSIGDB_Geneset_Names
    # Evaluate user input for secondary genes
    genesetInputs <- toupper(secondaryGenes)[which(toupper(secondaryGenes) %in% possibleGenesets)]
    shiny::validate(
      need((! length(genesetInputs) > 1), message = "Please select only one MSIGDB geneset.")
    )
    if (length(genesetInputs) == 1) {
      resList[['geneSetInputType']] <- T
      secondaryGenes <- genesetInputs
    } else {
      resList[['geneSetInputType']] <- F
      res <- symbolConverter(symbolVec = secondaryGenes, pool = pool,
                             species = selectedSpecies)
      secondaryGenes <- NULL
      unresolvableGenes <- res$unresolvableGenes
      if (length(unresolvableGenes)) {
        msg <- paste0("Input list warning: '", paste0(unique(unresolvableGenes), collapse = "', '"), 
                      "' not found. Skipping...")
        showNotification(id = "unresolvable-gene-warning", ui = msg, session = session,
                         closeButton = T, type = "warning", duration = 8)
      }
      if (length(res$resGenes)) {
        secondaryGenes <- res$resGenes
        resList[["secondaryGenes"]] <- secondaryGenes
        multiMappedGenes <- res$multiMappedGenes
        if (length(multiMappedGenes)) {
          msg <- paste0("Input '", paste0(multiMappedGenes, collapse = ", "), 
                        "' returned multiple official gene symbols.")
          showNotification(id = "multi-mapped-gene-warning", ui = msg, session = session,
                           closeButton = T, type = "warning", duration = 8)
        }
      }
    }
    resList[["genesetInputs"]] <- genesetInputs
  }
  return(resList)
}




