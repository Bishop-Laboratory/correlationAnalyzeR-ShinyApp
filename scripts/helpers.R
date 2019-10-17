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
symbolConverter <- function(symbolVec, species) {
  # Provide a vector of gene symbols to check and convert
  
  # # Bug testing
  # species <- "hsapiens"
  # symbolVec <- c("Asxl1", "CGAS", "RPA", "SON", "ASDJN", "NRF2", "DHX9", "SRSF2", "SF3B1")
  # symbolVec <- selectedGenes
  
  n <- length(symbolVec)
  if (species == "mmusculus") {
    dbCon <- org.Mm.eg.db::org.Mm.eg_dbconn()
    
  } else if (species == "hsapiens") {
    dbCon <- org.Hs.eg.db::org.Hs.eg_dbconn()
    
  }
  sqlQuery <- 'SELECT * FROM alias, gene_info WHERE alias._id == gene_info._id;'
  aliasSymbol <- DBI::dbGetQuery(dbCon, sqlQuery)
  aliasSymbol <- as.data.frame(aliasSymbol)
  avgenes <- correlationAnalyzeR::getAvailableGenes(Species = species)
  aliasSymbol <- aliasSymbol[which(aliasSymbol$symbol %in% avgenes$geneName),]
  
  unresolvableGenes <- c() # Cannot find any gene or alias matching this input
  confusedGenes <- list() # Input was an alias for which two valid gene symbols exists e.g. NRF2
  resGenes <- c()
  
  for (i in 1:n) {
    geneToCheck <- symbolVec[i]
    # Check official symbols
    newGene <- aliasSymbol$symbol[grep(aliasSymbol$symbol, pattern = geneToCheck, ignore.case = T)][1]
    # Make sure length is matching for inclusion errors.  e.g. 'NRF2' returns 'LONRF2' erroneously
    if (! is.na(newGene) & nchar(newGene) != nchar(geneToCheck)) {
      newGene <- NA
    }
    if (is.na(newGene)) {
      # Check aliases
      aliases <- aliasSymbol[grep(aliasSymbol$alias_symbol, pattern = geneToCheck, ignore.case = T),]
      
      if (! length(aliases$alias_symbol)) {
        unresolvableGenes <- c(unresolvableGenes, geneToCheck)
      } else {
        # Check for multimapped genes
        aliases <- aliasSymbol[grep(aliasSymbol$alias_symbol, pattern = geneToCheck, ignore.case = T),]
        aliases <- aliases[which(nchar(aliases$alias_symbol) == nchar(geneToCheck)),]
        resGenes <- c(resGenes, unique(aliases$symbol))
        if (length(unique(aliases$symbol)) > 1) {
          confusedGenes[[geneToCheck]] <- unique(aliases$symbol)
          
        }
      }
    } else {
      resGenes <- c(resGenes, newGene)
    }
  }
  resList <- list("unresolvableGenes" = unresolvableGenes, 
                  "multiMappedGenes" = confusedGenes, 
                  "resGenes" = resGenes)
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
  mmgenes <- correlationAnalyzeR::getAvailableGenes(Species = 'mmusculus')
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
                        session) {
  # Get GlobalData inputs
  HS_basicGeneInfo <- GlobalData$HS_basicGeneInfo
  MM_basicGeneInfo <- GlobalData$MM_basicGeneInfo
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
  
  
  # # Bug testing
  # primaryGene <- "BRCA1"
  # secondaryGenes <- c("NRF2", "rif1", "atM", "xct", 'asdla', 'asdhsad', 'dfkjkn')
  
  # Validate primary gene
  if (! is.null(primaryGene)){
    # Handle bad gene name here
    res <- symbolConverter(symbolVec = primaryGene, species = selectedSpecies)
    unresolvableGenes <- res$unresolvableGenes
    if (length(res$resGenes)) {
      primaryGene <- res$resGenes[1]
      multiMappedGenes <- res$multiMappedGenes
      if (length(multiMappedGenes)) {
        inputAlias <- names(multiMappedGenes)[1]
        mappedSymbols <- multiMappedGenes[[1]]
        msg <- paste0("Input '", inputAlias, 
                      "' returned multiple official gene symbols: '",
                      paste0(mappedSymbols, collapse = "', '"), 
                      "'. Only the first will be used, '", primaryGene, "'.")
        showNotification(id = "multi-mapped-gene-warning", ui = msg, session = session,
                         closeButton = T, type = "warning", duration = 8)
      }
      resList[["primaryGene"]] <- primaryGene
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
      res <- symbolConverter(symbolVec = secondaryGenes, species = selectedSpecies)
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
    }
    resList[["secondaryGenes"]] <- secondaryGenes
    resList[["genesetInputs"]] <- genesetInputs
  }
  return(resList)
}




