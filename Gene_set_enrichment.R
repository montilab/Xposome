
#########################################################
# 
# CREATE THE GENE ENRICHMENT SET 
# 
#########################################################
library(limma)
library(GSVA)
library(Biobase)
library(GSEABase)
library(tidyverse)

# Loading data files ####
HEPG2 <- readRDS(paste0("data/HEPG2/data.RDS"))
MCF10A <- readRDS(paste0("data/MCF10A/data.RDS"))
ADIPO <- readRDS(paste0("data/ADIPO/data.RDS"))

# Read in the gene set collection
gsscores_hallmark <- getGmt("data/Enrichment Gene Set/h.all.v7.0.symbols.gmt")
gsscores_c2_reactome <- getGmt("data/Enrichment Gene Set/c2.cp.reactome.v7.0.symbols.gmt")

# Run gene set variation analysis for hallmark
dataset=c("HEPG2", "MCF10A", "ADIPO"); genesetname=c("h.all", "c2.cp.reactome"); geneset=c("gsscores_hallmark", "gsscores_c2_reactome"); method=c("gsva", "ssgsea", "zscore");

for(d in 1:length(dataset)){
  #d=3;
  dat <- get(paste0(dataset[d]))
  
  # Getting the differential gene expression for each sig_id
  differential_expression <- dat[["Gene Expression"]]@assayData[["exprs"]]
  
  # Create a null list 
  gsscores <- list();
  
  for(u in 1:length(genesetname)){
    #u=1;
    for(m in 1:length(method)){
      #m=1;
      gsva_es <- gsva(expr=differential_expression, gset.idx.list=get(paste0(geneset[u])), method=method[m], mx.diff=TRUE)
      pData <- data.frame(sig_id = colnames(gsva_es))
      rownames(pData) <- colnames(gsva_es)
      phenoData <- new("AnnotatedDataFrame", data=pData)
      eSet <- ExpressionSet(assayData=gsva_es, phenoData=phenoData)
      gsscores[[paste0("gsscores_", genesetname[u], ".v7.0_", method[m])]] <- eSet
    }
  }
  
  #########################################################
  # 
  # EXPORT THE DATASET
  # 
  #########################################################
  
  ##Create output data
  dat[["Gene Set Enrichment"]] <- gsscores
  write_rds(dat, paste0("data/", dataset[d], "/data.RDS"))
  
}