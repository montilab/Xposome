

# R packages
library(shiny)
library(K2Taxonomer)
library(visNetwork)
library(plotly)
library(heatmaply)
library(RColorBrewer)
library(DT)
library(GSVA)
library(Biobase)
library(GSEABase)
library(limma)
library(dendextend)
library(tidyverse)

# Read in analysis results
K2summary <- readRDS("data/ADIPO/K2results.rds")

# Parse results
chemical <- K2info(K2summary) # chemical info

# The mixture of chemical of 10 ug/ml
chemical$Concentration[which(chemical$Concentration %in% "10 ug/ml")] <- 10/10^6

# Convert the concentration from M to uM
chemical$Concentration <- as.numeric(chemical$Concentration) * 10^6

# Getting the profile annotation
profile <- K2eSet(K2summary)@phenoData@data

# Getting the differential gene expression for each sig_id
profile_differential_expression <- K2eSet(K2summary)@assayData[["exprs"]]

# Get differential gene expression for each chemical
chemical_differential_expression <- K2data(K2summary)

# Define a chemical data frame to store number of replicates and ModZscores
chem_replicate <- data.frame(Chemical = unique(chemical$Chemical), N_replicates = NA, CC = NA, SS=NA, TAS = NA, stringsAsFactors = FALSE)

# Getting the number of replicates for each chemical
for(i in 1:nrow(chem_replicate)){
  #i=1;
  chem_replicate$N_replicates[i] <- nrow(profile %>% filter(Chemical %in% chem_replicate$Chemical[i]))
}

##########################################################################################
#
# CALCULATE MOD Z-SCORES AND REPLICATE CORRELATION (CC) 
# 
##########################################################################################

for(i in 1:nrow(chem_replicate)){
  #i=2;
  rep <- rownames(profile)[which(profile$Chemical %in% chem_replicate$Chemical[i])]
  
  if(length(rep) == 1){
    
    chem_replicate$CC[i] <- 1
    
    w=1 #unweighted
    
    #Calculate the modzscore for the chemical
    modzscore <-  w*profile_differential_expression[,rep]
    
    chemical_differential_expression[,chem_replicate$Chemical[i]] <- modzscore
    
  }else {
    
    # Calculate the spearman correlation between the pairwise replicates 
    spearman_corr <- matrix(NA, nrow=length(rep), ncol=length(rep), byrow=T, dimnames=list(rep, rep))
    
    for(l in 1:nrow(spearman_corr)){
      for(k in 1:nrow(spearman_corr)){
        #l=1, k=1;
        spearman_corr[l,k] <- cor(profile_differential_expression[,rep[l]], profile_differential_expression[,rep[k]], method = c("spearman"))
      }
    }
    
    #CALCULATE THE MODZ-SCORES
    if(length(rep) == 2){
      
      w=0.5 #unweighted
      
      #Calculate the modzscore for the chemical
      modzscore <- 0
      
      for(m in 1:length(rep)){
        #m=2;
        modzscore  <- w*profile_differential_expression[,rep[m]] + modzscore
      }
      
      chemical_differential_expression[,chem_replicate$Chemical[i]] <- modzscore
      
    }else{
      
      rep_corr <- NULL;
      
      for(g in 1:length(rep)){
        #g=3;
        corr <- spearman_corr[rep[g],rep[which(!rep %in% rep[g])]]
        rep_corr <- c(rep_corr, abs(sum(corr, na.rm=T)));
      }
      
      w <- rep_corr/sum(rep_corr, na.rm=TRUE)
      
      if(1-sum(w, na.rm=T) > 0.001) {
        print(rep_corr)
        print(w)
        print('incorrect calculation of w!')
      }
      
      #Calculate the modzscore for the chemical
      modzscore <- 0
      
      for(m in 1:length(rep)){
        #m=2;
        modzscore  <- w[m]*profile_differential_expression[,rep[m]] + modzscore
      }
      
      chemical_differential_expression[,chem_replicate$Chemical[i]] <- modzscore
      
    }
    
    #CALCULATE THE CC
    corr <- spearman_corr[lower.tri(spearman_corr)]
    quan_75 <- quantile(corr, 0.75, na.rm = T)
    chem_replicate$CC[i] <- quan_75
    
  }
}

##########################################################################################
#
#  CALCULATE SIGNATURE STRENGTH AND TAS
# 
##########################################################################################

# Get differential gene expression for each chemical
differential_expression <- K2data(K2summary)

# Calculate signature strength (SS) and TAS
for(i in 1:ncol(differential_expression)){
  #i=1;
  chem_replicate$SS[which(chem_replicate$Chemical %in% colnames(differential_expression)[i])] = length(which(abs(differential_expression[,i]) >= 2))
  chem_replicate$TAS[which(chem_replicate$Chemical %in% colnames(differential_expression)[i])] = sqrt(chem_replicate$SS[which(chem_replicate$Chemical %in% colnames(differential_expression)[i])]*max(c(0,chem_replicate$CC[which(chem_replicate$Chemical %in% colnames(differential_expression)[i])]))/nrow(differential_expression)) 
}

TAS <- chem_replicate[, c("Chemical", "TAS")]

# chemical annotation information
chemical_info <- chemical %>% mutate(BUID=paste0("BUID_", 1:nrow(chemical))) %>% 
  left_join(TAS) %>% 
  filter(!is.na(TAS)) %>% 
  rename(
    "Chemical" := Chemical ,
    "Chemical Name" := Full,
    "BUID" := BUID,
    "CAS" := CAS,
    "Source/Use" := Source,
    "dose (uM)" := Concentration,
    "PPARg_Mod" := PPARg_Mod,
    "TAS" := TAS
  ) %>% 
  select(
    c("Chemical", "Chemical Name", "BUID", "CAS", "Source/Use", "dose (uM)", "PPARg_Mod", "TAS")
  ) 
  
# profile annotation information
profile_info <- data.frame(sig_id=rownames(profile ), profile %>% select(-Concentration)) %>% 
  left_join(chemical %>% mutate(BUID=paste0("BUID_", 1:nrow(chemical)))) %>% 
  left_join(
    TAS
  ) %>% 
  rename(
    "Chemical" = Chemical ,
    "Chemical Name" := Full,
    "BUID" := BUID,
    "CAS" := CAS,
    "Source/Use" := Source,
    "dose (uM)" := Concentration,
    "TAS" := TAS
  ) %>% 
  select(
    c("sig_id", "Chemical", "Chemical Name", "BUID", "CAS", "Source/Use", "NileRed", "Plate", "dose (uM)", "PPARg_Mod", "TAS")
  ) 

#########################################################
# 
# CREATE GENE SET ENRICHMENT  
# 
#########################################################

# Read in the gene set collection
gsscores_hallmark <- getGmt("data/Enrichment Gene Set/h.all.v7.0.symbols.gmt")
gsscores_c2_reactome <- getGmt("data/Enrichment Gene Set/c2.cp.reactome.v7.0.symbols.gmt")
gsscores_nursa <- getGmt("data/Enrichment Gene Set/nursa_consensome_Cbyfdrvalue_0.01.gmt")

# Run gene set variation analysis for hallmark
genesetname <- c("h.all.v7.0", "c2.cp.reactome.v7.0", "nursa_consensome_Cbyfdrvalue_0.01"); 
geneset <- c("gsscores_hallmark", "gsscores_c2_reactome", "gsscores_nursa"); 
method <- c("gsva", "ssgsea", "zscore");

# Getting the differential gene expression for each sig_id
expression_set <- chemical_differential_expression;

# Create a null list 
gsscores <- list();

for(u in 1:length(genesetname)){
  #u=1;
  for(m in 1:length(method)){
    #m=1;
    gsva_es <- gsva(expr=expression_set, gset.idx.list=get(paste0(geneset[u])), method=method[m], mx.diff=TRUE)
    pData <- data.frame(Chemical= colnames(gsva_es))
    rownames(pData) <- colnames(gsva_es)
    phenoData <- new("AnnotatedDataFrame", data=pData)
    eSet <- ExpressionSet(assayData=gsva_es, phenoData=phenoData)
    gsscores[[paste0("gsscores_", genesetname[u], "_", method[m])]] <- eSet
  }
}

#########################################################
# 
# EXPORT THE DATASET
# 
#########################################################

##Create adipogenome data
adipogenome <- list()

#create the annotation data
profile_info <- dat[["Profile Annotation"]]
rownames(profile_info) <- profile_info$sig_id

chemical_info <- dat[["Chemical Annotation"]]
rownames(chemical_info) <- chemical_info$Chemical

adipogenome[["Profile Annotation"]] <- profile_info
adipogenome[["Chemical Annotation"]] <- chemical_info

#create phenotypic data
pData <- data.frame(Chemical = colnames(chemical_differential_expression))
rownames(pData) <- colnames(chemical_differential_expression)
phenoData <- new("AnnotatedDataFrame", data=pData)

#create feature data
fData <- data.frame(Gene = rownames(chemical_differential_expression))
rownames(fData) <- rownames(chemical_differential_expression)
featureData <- new("AnnotatedDataFrame", data=fData)

#create expression set
expressionSet <- chemical_differential_expression
eSet <- ExpressionSet(assayData=expressionSet, phenoData=phenoData, featureData=featureData)
adipogenome[["Gene Expression"]] <-  eSet

#create gene set enrichment and connectivity
adipogenome[["Gene Set Enrichment"]] <- gsscores
adipogenome[["Connectivity"]] <- gsscores
adipogenome[["title"]] <- "ADIPO Portal"
adipogenome[["about page"]] <- "introduction_ADIPO.Rmd"

write_rds(adipogenome, "data/ADIPO/data.RDS")
