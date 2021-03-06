library(shiny)
# getCorrelationData(geneList = "ATM", Sample_Type = "Normal_Tissues", Species = "hsapiens")
# options(shiny.reactlog = TRUE)

runApp(display.mode = "normal", port = 80,
       host = "0.0.0.0", launch.browser = FALSE)
# runApp(launch.browser = TRUE)


# resList <- correlationAnalyzeR::analyzeSingleGenes(genesOfInterest = c("Atm"), Species = "mmusculus",
#                                                    returnDataOnly = T, topPlots = F)
# ranks <- resList$correlations$Atm
# names(ranks) <- rownames(resList$correlations)
# ranks <- ranks[which(! duplicated(names(ranks)))]
# ranks <- ranks[which(! is.na(ranks))]
# ranks <- ranks[order(ranks, decreasing = TRUE)]
# EGMT <- clusterProfiler::GSEA(ranks, TERM2GENE=correlationAnalyzeR::getTERM2GENE(Species = "mmusculus"),
#                               maxGSSize = 500, seed = TRUE,
#                               minGSSize = 15,
#                               nPerm = 1000, pvalueCutoff = .05)
# clusterProfiler::gseaplot(EGMT,
#                           geneSetID = "HALLMARK_OXIDATIVE_PHOSPHORYLATION")

# profvis(prof_output = "profFile.html",{
#   runApp(launch.browser = T)
# 
# })






