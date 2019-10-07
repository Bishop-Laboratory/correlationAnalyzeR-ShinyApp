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


