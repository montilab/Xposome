
# Parse results
info <- K2info(K2summary); infoMat <- as.matrix(info) # Format information
K2res <- K2results(K2summary) # Format K2 results
dataMatrix <- K2data(K2summary) # Format dataMatrix
genesets <- K2genesets(K2summary) # Get geneset lists
gene2Pathway <- K2gene2Pathway(K2summary) # Get gene2pathway matching
eSet <- K2eSet(K2summary) # Get expression set
gSet <- K2gSet(K2summary) # Get gene set projection expression set
meta <- K2meta(K2summary) # Get meta data
K2dendrogram <- K2dendro(K2summary) # Create static dendrogram

# Create interactive dendrogram ####
vNetOut <- K2visNetwork(K2summary)

# Remove K2summary to save space
rm(K2summary)

# If too many observations in terminal labels, unlabel them
if (max(
  lengths(regmatches(vNetOut$x$nodes$label, gregexpr("\n", vNetOut$x$nodes$label)))
) > 20 ) {
  
  # Fix font size
  vNetOut$x$nodes$font.size <- 25
  vNetOut$x$nodes$font.size[vNetOut$x$nodes$shape == "box"] <- 0
  
  # Change shape
  vNetOut$x$nodes$shape[vNetOut$x$nodes$shape == "box"] <- "square"
}

# Format enrichment table
enrTable <- getEnrichmentTable(K2res)

# Remove unnecessary columns
enrTable <- enrTable[, !colnames(enrTable) %in% c("B", "ntot", "t")]

# Remove gse from K2res
K2res <- lapply(K2res, function(x) { x$gse <- NULL; return(x) })

# Add aliases for plotting and looking up
enrTable$Plot <- paste0("<label for='PlotRow",
                        seq(nrow(enrTable)),
                        "'>&#128202;</label>")
enrTable$Send <- paste0("<label for='SendRow",
                        seq(nrow(enrTable)),
                        "'>&#9992;</label>")

# Add links to gene sets
get_enrTablelink <- function(geneset){
  sprintf('<a href="http://software.broadinstitute.org/gsea/msigdb/cards/%s" style="text-decoration:none" target="_blank">&#128269;</a>', geneset, geneset)
}

enrTable$Link <- sapply(as.character(enrTable$category), get_enrTablelink)

# Format numbers to fit in table
for (i in c("pval_hyper", "fdr_hyper", "pval_limma", "fdr_limma")) {
  enrTable[,i] <- signif(enrTable[,i], digits = 2)
}

# Format numbers to fit in table
for (i in c("coef", "mean")) {
  enrTable[,i] <- round(enrTable[,i], digits = 2)
}

colnames(enrTable) <- c("Gene Set", "Node", "Group", "Direction", "P Value_Hyper", "FDR_Hyper", "N_Overlap", "N_Sig. Genes", "N_Gene Set", "P Value_ssGSEA", "FDR_ssGSEA", "Diff_ssGSEA", "Mean_ssGSEA", "Hits", "Plot", "Send", "Link")

# Get differential gene expression results
dgeTable <- getDGETable(K2res)

# Remove gse from K2res
K2res <- lapply(K2res, function(x) {x$dge <- NULL; return(x)} )

# Add aliases for plotting and looking up
dgeTable$Plot <- paste0("<label for='PlotRow",
                        seq(nrow(dgeTable)),
                        "'>&#128202;</label>")
dgeTable$Send <- paste0("<label for='SendRow",
                        seq(nrow(dgeTable)),
                        "'>&#9992;</label>")

## Add links to genes
get_dgeTable_link <- function(genesymbol){
  sprintf('<a href="http://www.genecards.org/cgi-bin/carddisp.pl?gene=%s&keywords=%s" style="text-decoration:none" target="_blank">&#128269;</a>', genesymbol, genesymbol)
}

dgeTable$Link <- sapply(as.character(dgeTable$gene), get_dgeTable_link)

# Reorder columns
dgeTable <- dgeTable[,c("gene", "split", "mod", "direction", "pval", "fdr", "coef", "mean", "Plot", "Send", "Link")]

# Format numbers to fit in table
for (i in c("pval", "fdr")) {
  dgeTable[,i] <- signif(dgeTable[,i], digits = 2)
}

# Format numbers to fit in table
for (i in c("coef", "mean")) {
  dgeTable[,i] <- round(dgeTable[,i], digits = 2)
}

# Rename columns
colnames(dgeTable) <- c("Gene", "Node", "Group", "Direction", "P Value", "FDR", "Diff", "Mean", "Plot", "Send", "Link")

## Set select input options for annotations bar
varOptions <- sort(colnames(info))
names(varOptions) <- varOptions

if(!is.null(meta$cohorts)) {
  varOptions <- varOptions[varOptions != "meta$cohorts"]
} else {
  varOptions <- varOptions[varOptions != "sampleID"]
}

varOptions <- c("Add Annotation to Heatmap:" = "", "RESET" = "RESET", varOptions)

## Get sample order ####
labs <- get_leaves_attr(K2dendrogram, "label")

# Get IDs of each group ####
obsMap <- unlist(lapply(K2res, function(x) x$obs), recursive = F)

