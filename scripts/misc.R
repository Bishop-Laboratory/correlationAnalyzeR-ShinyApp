# Script to generate global data for app

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

# Generate tissue option lists
tissues <- unique(
  gsub(
    correlationAnalyzeR::getTissueTypes(#'hsapiens',
                                        pool = pool),
    pattern = " - .+", replacement = ""
  )
)
humanTissueOptions <- list()
for (i in 1:length(tissues)) {
  tissue <- tissues[i]
  ll <- correlationAnalyzeR::getTissueTypes(# 'hsapiens',
                                            pool = pool)
  ll <- ll[grep(pattern = tissue, x = ll)]
  ll <- gsub(pattern = ".+ - ", x = ll, replacement = "")
  humanTissueOptions[[i]] <- ll
  names(humanTissueOptions)[i] <- tissue
}
# tissues <- unique(
#   gsub(
#     correlationAnalyzeR::getTissueTypes('mmusculus', pool = pool),
#     pattern = " - .+", replacement = ""
#   )
# )
# mouseTissueOptions <- list()
# for (i in 1:length(tissues)) {
#   tissue <- tissues[i]
#   ll <- correlationAnalyzeR::getTissueTypes('mmusculus', pool = pool)
#   ll <- ll[grep(pattern = tissue, x = ll)]
#   ll <- gsub(pattern = ".+ - ", x = ll, replacement = "")
#   mouseTissueOptions[[i]] <- ll
#   names(mouseTissueOptions)[i] <- tissue
# }

save(humanTissueOptions, file = "data/humanTissueOptions.RData")
# save(mouseTissueOptions, file = "data/mouseTissueOptions.RData")

# # Make GSEA objects
# Get data object
Species <- "hsapiens"
if (Species == "hsapiens") {
  msigSpec <- "Homo sapiens"
} else {
  msigSpec <- "Mus musculus"
}
MDF <- msigdbr::msigdbr(species = msigSpec)
MDF$gs_subcat <- gsub(MDF$gs_subcat, pattern = "CP:", replacement = "", perl = TRUE)
MDF$gs_cat <- paste0(MDF$gs_cat, ":", MDF$gs_subcat)
MDF$gs_cat <- gsub(MDF$gs_cat, pattern = ":$", replacement = "", perl = TRUE)
MDF$gs_cat <- gsub(MDF$gs_cat, pattern = "C1", replacement = "Cytogenic bands", perl = TRUE)
MDF$gs_cat <- gsub(MDF$gs_cat, pattern = "C6", replacement = "Oncogenic signatures", perl = TRUE)
MDF$gs_cat <- gsub(MDF$gs_cat, pattern = "C7", replacement = "Immunological signatures", perl = TRUE)
MDF$gs_cat <- gsub(MDF$gs_cat, pattern = "C2:", replacement = "", perl = TRUE)
MDF$gs_cat <- gsub(MDF$gs_cat, pattern = "C5", replacement = "GO", perl = TRUE)
MDF$gs_cat <- gsub(MDF$gs_cat, pattern = "H", replacement = "Hallmark", perl = TRUE)
MDF$gs_cat <- gsub(MDF$gs_cat, pattern = "CP", replacement = "Canonical pathways", perl = TRUE)
MDF$gs_cat <- gsub(MDF$gs_cat, pattern = "CGP", replacement = "Perturbations", perl = TRUE)
MDF$gs_cat <- gsub(MDF$gs_cat, pattern = "C4:CGN", replacement = "Cancer gene neighborhoods", perl = TRUE)
MDF$gs_cat <- gsub(MDF$gs_cat, pattern = "C4:CM", replacement = "Cancer modules", perl = TRUE)
MDF$gs_cat <- gsub(MDF$gs_cat, pattern = "C3:MIR", replacement = "miRNA targets", perl = TRUE)
MDF$gs_cat <- gsub(MDF$gs_cat, pattern = "C3:TFT", replacement = "TF targets", perl = TRUE)
MDF$gs_cat <- gsub(MDF$gs_cat, pattern = "BIOCARTA", replacement = "BioCarta", perl = TRUE)
MDF$gs_cat <- gsub(MDF$gs_cat, pattern = "REACTOME", replacement = "Reactome", perl = TRUE)
MDFHuman <- MDF %>% 
  select(gs_name, gs_cat, human_gene_symbol) 
MDF <- MDF %>% distinct()
MDF <- MDF[,c(1, 3, 5)]


# Load data for the global environment
load("data/geneInfo/HS_basicGeneInfo.RData")
# load("data/geneInfo/MM_basicGeneInfo.RData")
load("data/humanTissueOptions.RData")
# load("data/mouseTissueOptions.RData")
# load("data/mouseGeneOptions.RData")
load("data/humanGeneOptions.RData")
load("data/symbol_suggestions.RData")
humanGeneOptions <- symbolsFinal$alias_symbol[which(symbolsFinal$species == "hsapiens")]
# mouseGeneOptions <- symbolsFinal$alias_symbol[which(symbolsFinal$species == "mmusculus")]
humanGeneOptions <- humanGeneOptions[order(humanGeneOptions)]
# mouseGeneOptions <- mouseGeneOptions[order(mouseGeneOptions)]
save(humanGeneOptions, file = "data/humanGeneOptions.RData")
# save(mouseGeneOptions, file = "data/mouseGeneOptions.RData")

# Create global data object
GlobalData <- list("HS_basicGeneInfo" = HS_basicGeneInfo,
                   # 'MM_basicGeneInfo' = MM_basicGeneInfo,
                   'humanGeneOptions' = humanGeneOptions,
                   # 'mouseGeneOptions' = mouseGeneOptions,
                   'humanTissueOptions' = humanTissueOptions,
                   # 'mouseTissueOptions' = mouseTissueOptions,
                   'MDF' = MDF)
save(GlobalData, file = "data/GlobalData.RData")
load("data/GlobalData.RData")
# GlobalData <- GlobalData[-8]
# save(GlobalData, file = "data/GlobalData.RData")

# term2geneCount <- as.data.frame(table(GlobalData$MDF$gs_name), stringsAsFactors = FALSE)
# MSIGDB_Geneset_Small_Names <- term2geneCount$Var1[which(term2geneCount$Freq < 501)]
# save(MSIGDB_Geneset_Small_Names, file = "MSIGDB_Geneset_Small_Names.rda")



