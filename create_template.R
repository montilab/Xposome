
# R packages
library(tidyverse)

# Read in analysis results
dat <- readRDS("data/HEPG2/data.rds")

#########################################################
# 
# EXPORT THE DATASET
# 
#########################################################

profile_annotation <- dat[["Profile Annotation"]]

chemical_annotation <- dat[["Chemical Annotation"]]

gene_expression <- dat[["Gene Expression"]] 

gene_set_enrichment <- dat[["Gene Set Enrichment"]]

connectivity <- dat[["Connectivity"]]

# Run gene set variation analysis for hallmark
dataset <- c("profile_annotation", "chemical_annotation", "gene_expression", "gene_set_enrichment", "connectivity"); 

for(u in 1:length(dataset)){
  #u=1;
  write_rds(get(paste0(dataset[u])), paste0("data/Template/", dataset[u], ".RDS"))
}

