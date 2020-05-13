
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
library(cmapR)

# Loading data files ####
HEPG2 <- readRDS(paste0("data/HEPG2/data.RDS"))
MCF10A <- readRDS(paste0("data/MCF10A/data.RDS"))
ADIPO <- readRDS(paste0("data/ADIPO/data.RDS"))

# Run gene set variation analysis for hallmark
dataset=c("HEPG2", "MCF10A", "ADIPO"); 

for(d in 1:length(dataset)){
  #d=1;
  dat <- get(paste0(dataset[d]))
  gsen <- dat[["Gene Set Enrichment"]]
  proann <- data.frame(dat[["Profile Annotation"]], stringsAsFactors = FALSE)
  chemann <- data.frame(dat[["Chemical Annotation"]], stringsAsFactors = FALSE)
  
  # v5 gs 
  #gsv5 <- parse_gctx(paste0("data/", dataset[d], "/gct/gsscores_c2.cp.reactome.v5.0_gsva.gct"))
  
  # get the gene set 
  genesetname <- names(gsen)
  
  GCT <- setClass(
    "GCT",
    slots = c(
      mat = "matrix",
      rid = "character",
      cid = "character",
      rdesc = "data.frame",
      cdesc = "data.frame",
      version = "character",
      src = "character"
    )
  )
  
  #########################################################
  # 
  # EXPORT THE GENE SET
  # 
  #########################################################
  
  for(u in 1:length(genesetname)){
    #u=1;
    ##Create output data
    gs <- exprs(gsen[[u]])
    rdesc <- data.frame(genesets=rownames(gs), id=rownames(gs))
    cdesc <- data.frame(if(d==3){ chemann }else{ proann }, id=colnames(gs))
    colnames(cdesc)[which(colnames(cdesc) %in% c("Chemical.Name", "dose..uM."))] <- c("Chemical Name", "dose (uM)")
    
    gct <- GCT(
      mat=gs,
      rid=rownames(gs),
      cid=colnames(gs),
      rdesc=rdesc,
      cdesc=cdesc
    )
    
    gct@src <- paste0("data/", dataset[d], "/gct/", genesetname[u], ".gct")
    
    print(genesetname[u])
    write_gct(gct, paste0("data/", dataset[d], "/gct/", genesetname[u], ".gct"), appenddim = FALSE)
    
  }
  
}