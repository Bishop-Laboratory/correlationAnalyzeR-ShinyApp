# Removes temporary files from www directory
removeTmp <- function() {
  unlink(x = "www/tmp")
  dir.create("www/tmp")
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

# Function to convert a vector of old gene symbols to new ones
symbolConverter <- function(symbolVec, species, pool) {
  # Provide a vector of gene symbols to check and convert
  
  ## Bug testing
  # species <- "hsapiens"
  # symbolVec <- c("Asxl1", "CGAS", "RPA", "SON", "ASDJN", "NRF2", "DHX9", "SRSF2", "SF3B1")
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
    # geneToCheck <- symbolVec[i]
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
    resList <- future({
      lapply(symbolVec, checkTheGene, aliasSymbol = aliasSymbol)
    }, globals = list(symbolVec = symbolVec,
                      checkTheGene = checkTheGene,
                      aliasSymbol = aliasSymbol)) %...>%
      (function(resList) {
        resDF <- data.table::rbindlist(resList)
        resList <- split(resDF, resDF$category)
        resGenes <- as.character(resList$resGenes$geneNames)
        confusedGenes <- as.character(resList$multiMappedGenes$geneNames)
        unresolvableGenes <- as.character(resList$unresolvableGenes$geneNames)
        resList <- list("unresolvableGenes" = unresolvableGenes, 
                        "multiMappedGenes" = confusedGenes, 
                        "resGenes" = resGenes)
        resList
      })
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
  # primaryGene <- "Tp53bp2"
  # selectedSpecies <- "Mouse"
  # sampleType <- "normal"
  # tissueType <- "all"
  # secondaryGenes <- c("NRF2", "rif1", "atM", "xct", 'asdla', 'asdhsad', 'dfkjkn')
    
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
      res %...>% (function(res) {
        secondaryGenes <- NULL
        unresolvableGenes <- res$unresolvableGenes
        if (length(unresolvableGenes)) {
          msg <- paste0("Input list warning: '", paste0(unresolvableGenes, collapse = "', '"), 
                        "' not found. Skipping...")
          showNotification(id = "unresolvable-gene-warning", ui = msg, session = session,
                           closeButton = T, type = "warning", duration = 8)
        }
        if (length(res$resGenes)) {
          secondaryGenes <- res$resGenes
          multiMappedGenes <- res$multiMappedGenes
          if (length(multiMappedGenes)) {
            for (i in 1:length(multiMappedGenes)) {
              inputAlias <- names(multiMappedGenes)[i]
              mappedSymbols <- multiMappedGenes[[i]]
              msg <- paste0("Input '", inputAlias, 
                            "' returned multiple official gene symbols: '",
                            paste0(mappedSymbols, collapse = "', '"), 
                            "'.")
              showNotification(id = "multi-mapped-gene-warning", ui = msg, session = session,
                               closeButton = T, type = "warning", duration = 8)
            }
          }
        }
      })
    }
    resList[["secondaryGenes"]] <- secondaryGenes
    resList[["genesetInputs"]] <- genesetInputs
  }
  return(resList)
}




