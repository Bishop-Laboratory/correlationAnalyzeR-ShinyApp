# Script to generate global data for app

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

# Generate tissue option lists
tissues <- unique(
  gsub(
    correlationAnalyzeR::getTissueTypes('hsapiens', pool = pool),
    pattern = " - .+", replacement = ""
  )
)
humanTissueOptions <- list()
for (i in 1:length(tissues)) {
  tissue <- tissues[i]
  ll <- correlationAnalyzeR::getTissueTypes('hsapiens', pool = pool)
  ll <- ll[grep(pattern = tissue, x = ll)]
  ll <- gsub(pattern = ".+ - ", x = ll, replacement = "")
  humanTissueOptions[[i]] <- ll
  names(humanTissueOptions)[i] <- tissue
}
tissues <- unique(
  gsub(
    correlationAnalyzeR::getTissueTypes('mmusculus', pool = pool),
    pattern = " - .+", replacement = ""
  )
)
mouseTissueOptions <- list()
for (i in 1:length(tissues)) {
  tissue <- tissues[i]
  ll <- correlationAnalyzeR::getTissueTypes('mmusculus', pool = pool)
  ll <- ll[grep(pattern = tissue, x = ll)]
  ll <- gsub(pattern = ".+ - ", x = ll, replacement = "")
  mouseTissueOptions[[i]] <- ll
  names(mouseTissueOptions)[i] <- tissue
}

save(humanTissueOptions, file = "data/humanTissueOptions.RData")
save(mouseTissueOptions, file = "data/mouseTissueOptions.RData")

# # Make GSEA objects
# hsapiens_simple_TERM2GENE <- correlationAnalyzeR::getTERM2GENE(GSEA_Type = "simple",
#                                                                Species = "hsapiens")
# mmusculus_simple_TERM2GENE <- correlationAnalyzeR::getTERM2GENE(GSEA_Type = "simple",
#                                                                Species = "mmusculus")
# hsapiens_complex_TERM2GENE <- correlationAnalyzeR::getTERM2GENE(GSEA_Type = "complex",
#                                                                Species = "hsapiens")
# mmusculus_complex_TERM2GENE <- correlationAnalyzeR::getTERM2GENE(GSEA_Type = "complex",
#                                                                Species = "mmusculus")
# TERM2GENEList <- list("complex" = list("hsapiens" = hsapiens_complex_TERM2GENE,
#                                        "mmusculus" = mmusculus_complex_TERM2GENE),
#                       "simple" = list("hsapiens" = hsapiens_simple_TERM2GENE,
#                                       "mmusculus" = mmusculus_simple_TERM2GENE))
# save(TERM2GENEList,
#      file = "data/TERM2GENE_Objects.RData")

# Load data for the global environment
load("data/geneInfo/HS_basicGeneInfo.RData")
load("data/geneInfo/MM_basicGeneInfo.RData")
load("data/humanTissueOptions.RData")
load("data/mouseTissueOptions.RData")
load("data/mouseGeneOptions.RData")
load("data/humanGeneOptions.RData")
load("data/symbol_suggestions.RData")
humanGeneOptions <- symbolsFinal$alias_symbol[which(symbolsFinal$species == "hsapiens")]
mouseGeneOptions <- symbolsFinal$alias_symbol[which(symbolsFinal$species == "mmusculus")]
humanGeneOptions <- humanGeneOptions[order(humanGeneOptions)]
mouseGeneOptions <- mouseGeneOptions[order(mouseGeneOptions)]
save(humanGeneOptions, file = "data/humanGeneOptions.RData")
save(mouseGeneOptions, file = "data/mouseGeneOptions.RData")

# Create global data object
GlobalData <- list("HS_basicGeneInfo" = HS_basicGeneInfo,
                   'MM_basicGeneInfo' = MM_basicGeneInfo,
                   'humanGeneOptions' = humanGeneOptions,
                   'mouseGeneOptions' = mouseGeneOptions,
                   'humanTissueOptions' = humanTissueOptions,
                   'mouseTissueOptions' = mouseTissueOptions,
                   'TERM2GENEList' = TERM2GENEList)
save(GlobalData, file = "data/GlobalData.RData")



