# Generate tissue option lists
tissues <- unique(
  gsub(
    correlationAnalyzeR::getTissueTypes('hsapiens'),
    pattern = " - .+", replacement = ""
  )
)
humanTissueOptions <- list()
for (i in 1:length(tissues)) {
  tissue <- tissues[i]
  ll <- correlationAnalyzeR::getTissueTypes('hsapiens')
  ll <- ll[grep(pattern = tissue, x = ll)]
  ll <- gsub(pattern = ".+ - ", x = ll, replacement = "")
  humanTissueOptions[[i]] <- ll
  names(humanTissueOptions)[i] <- tissue
}
tissues <- unique(
  gsub(
    correlationAnalyzeR::getTissueTypes('mmusculus'),
    pattern = " - .+", replacement = ""
  )
)
mouseTissueOptions <- list()
for (i in 1:length(tissues)) {
  tissue <- tissues[i]
  ll <- correlationAnalyzeR::getTissueTypes('mmusculus')
  ll <- ll[grep(pattern = tissue, x = ll)]
  ll <- gsub(pattern = ".+ - ", x = ll, replacement = "")
  mouseTissueOptions[[i]] <- ll
  names(mouseTissueOptions)[i] <- tissue
}

save(humanTissueOptions, file = "data/humanTissueOptions.RData")
save(mouseTissueOptions, file = "data/mouseTissueOptions.RData")

# Make GSEA objects
hsapiens_simple_TERM2GENE <- correlationAnalyzeR::getTERM2GENE(GSEA_Type = "simple",
                                                               Species = "hsapiens")
mmusculus_simple_TERM2GENE <- correlationAnalyzeR::getTERM2GENE(GSEA_Type = "simple",
                                                               Species = "mmusculus")
hsapiens_complex_TERM2GENE <- correlationAnalyzeR::getTERM2GENE(GSEA_Type = "complex",
                                                               Species = "hsapiens")
mmusculus_complex_TERM2GENE <- correlationAnalyzeR::getTERM2GENE(GSEA_Type = "complex",
                                                               Species = "mmusculus")
save(hsapiens_simple_TERM2GENE, mmusculus_simple_TERM2GENE, 
     hsapiens_complex_TERM2GENE, mmusculus_complex_TERM2GENE,
     file = "data/TERM2GENE_Objects.RData")





