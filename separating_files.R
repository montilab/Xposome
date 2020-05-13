
library(tidyverse)
library(Biobase)
library(tools)

# Loading data files ####
HEPG2 <- readRDS(paste0("~/Documents/Internship Project/Exposome app/shinyApps/data/HEPG2/data_old.RDS"))
MCF10A <- readRDS(paste0("~/Documents/Internship Project/Exposome app/shinyApps/data/MCF10A/data_old.RDS"))
ADIPO <- readRDS(paste0("~/Documents/Internship Project/Exposome app/shinyApps/data/ADIPO/data_old.RDS"))

# Run gene set variation analysis for hallmark
dataset=c("HEPG2", "MCF10A", "ADIPO"); 

for(d in 1:length(dataset)){
  #d=3;
  print(d)
  dat <- get(paste0(dataset[d]))
  
  #clean up profile annotation####
  proann <- dat[["Profile Annotation"]]
  colNames <- colnames(proann)
  colNames <- gsub("\\(", "", colNames, fixed=FALSE)
  colNames <- gsub(")", "", colNames, fixed=FALSE)  
  colNames <- gsub(" ", "_", colNames, fixed=FALSE)
  colNames <- gsub("_uM", "", colNames, fixed=FALSE)
  colNames <- gsub("/", "_", colNames, fixed=FALSE)
  colnames(proann) <- toTitleCase(colNames)
  
  l <- grep("Sig_id", colnames(proann), fixed=FALSE)
  
  if(length(l) > 0){ 
    colnames(proann)[l] <- "Sig_Id"
  }
  
  l <- grep("Dose", colnames(proann), fixed=FALSE)
  
  if(length(l) > 0){ 
    w <- which(proann$Dose %in% c(NA, "")); wo <- which(!proann$Dose %in% c(NA, ""));
    proann$Dose[w] <- NA
    proann$Dose[wo] <- paste0(proann$Dose[wo], "uM")
  }
  
  l <- grep("unique_ID_by_chem", colnames(proann), fixed=FALSE)
  
  if(length(l) > 0){ 
    w <- which(proann$unique_ID_by_chem %in% c(NA, "")); wo <- which(!proann$unique_ID_by_chem %in% c(NA, ""));
    proann$unique_ID_by_chem[w] <- NA
    proann$unique_ID_by_chem[wo] <- paste0(proann$unique_ID_by_chem[wo], "uM") 
  }else{
    proann$unique_ID_by_chem <- paste0(proann$Dose) 
  }
  
  l <- which(colnames(proann) %in% "Chemical")
  
  if(length(l) > 0){ 
    colnames(proann)[l] <- "Chemical_Id"
  }
  
  l <- grep("Chemical_Id", colnames(proann), fixed=FALSE)
  
  if(length(l) == 0){ 
    proann$Chemical_Id <- proann$Chemical_Name
  }
  
  proann <- proann %>% select("Sig_Id", "Chemical_Id", "Chemical_Name", "BUID", "CAS", colnames(proann)[which(!colnames(proann) %in% c("Chemical_Id", "Chemical_Name", "BUID", "CAS"))])
  colnames(proann) <- toTitleCase(colnames(proann))
  dat[["Profile Annotation"]] <- proann
  
  ##clean up chemical annotation
  chemann <- dat[["Chemical Annotation"]]  
  colNames <- colnames(chemann)
  
  colNames <- gsub("\\(", "", colNames, fixed=FALSE)
  colNames <- gsub(")", "", colNames, fixed=FALSE)  
  colNames <- gsub(" ", "_", colNames, fixed=FALSE)
  colNames <- gsub("_uM", "", colNames, fixed=FALSE)
  colNames <- gsub("/", "_", colNames, fixed=FALSE)
  colnames(chemann) <- toTitleCase(colNames)
  
  l <- grep("Dose", colnames(chemann), fixed=FALSE)
  
  if(length(l) > 0){ 
    chemann <- chemann[-l]
  }
  
  l <- which(colnames(chemann) %in% "Chemical")
  
  if(length(l) > 0){ 
    colnames(chemann)[l] <- "Chemical_Id"
  }
  
  l <- grep("Chemical_Id", colnames(chemann), fixed=FALSE)
  
  if(length(l) == 0){ 
    chemann$Chemical_Id <- chemann$Chemical_Name
  }
  
  chemann <- chemann %>% select("Chemical_Id", "Chemical_Name", "BUID", "CAS", colnames(chemann)[which(!colnames(chemann) %in% c("Chemical_Id", "Chemical_Name", "BUID", "CAS"))])
  colnames(chemann) <- toTitleCase(colnames(chemann))
  dat[["Chemical Annotation"]] <- chemann
  
  #create expression data
  eset <- dat[["Gene Expression"]]
  expressionData <- exprs(eset)

  #create phenotypic data
  if(all(colnames(expressionData) %in% proann$Sig_Id)){
    pData <- data.frame(Sig_Id=colnames(expressionData))
    rownames(pData) <- colnames(expressionData)
  }else if(all(colnames(expressionData) %in% proann$Chemical_Id)){
    pData <- data.frame(Chemical_Id=colnames(expressionData))
    rownames(pData) <- colnames(expressionData)
  }

  phenoData <- new("AnnotatedDataFrame", data=pData)
  
  #create feature data
  fData <- fData(eset)
  rownames(fData) <- rownames(eset)
  colNames <- colnames(fData) 
  colNames <- gsub("\\(", "", colNames, fixed=FALSE)
  colNames <- gsub(")", "", colNames, fixed=FALSE)  
  colNames <- gsub(" ", "_", colNames, fixed=FALSE)
  colNames <- gsub("_uM", "", colNames, fixed=FALSE)
  colNames <- gsub("/", "_", colNames, fixed=FALSE)
  colnames(fData) <- colNames
  
  if(!dataset[d] %in% "ADIPO"){
    l <- which(colnames(fData) %in% "Gene_Symbol")
    colnames(fData)[l] <- "Gene"
  }
  
  colnames(fData) <- toTitleCase(colnames(fData))
  featureData <- new("AnnotatedDataFrame", data=fData)
  
  #Create the expression set###
  eset <- ExpressionSet(assayData=expressionData, phenoData=phenoData, featureData=featureData)
  dat[["Gene Expression"]] <- eset
  
  #########################################################
  # 
  # EXPORT FILES
  # 
  #########################################################
  write_rds(proann, paste0("~/Documents/Internship Project/Exposome app/shinyApps/data/", dataset[d], "/Profile_Annotation.RDS"))
  write_rds(chemann, paste0("~/Documents/Internship Project/Exposome app/shinyApps/data/", dataset[d], "/Chemical_Annotation.RDS"))
  write_rds(eset, paste0("~/Documents/Internship Project/Exposome app/shinyApps/data/", dataset[d], "/Expression_Set.RDS"))
  
  #########################################################
  # 
  # EXPORT THE CMAP CONNECTIVITY MAP
  # 
  #########################################################
  conn <- dat[["Connectivity"]]
  
  # Create classes for CMap connectivity #####
  connmap <- c("pcl", "pert"); Connectivity <- list()
  
  for(p in 1:length(connmap)){
    #p=1;
    #craate expression data
    eset <- conn[[connmap[p]]]
    expressionData <- exprs(eset)
    
    #create phenotypic data
    if(all(colnames(expressionData) %in% proann$Sig_Id)){
      pData <- data.frame(Sig_Id=colnames(expressionData))
      rownames(pData) <- colnames(expressionData)
    }else if(all(colnames(expressionData) %in% proann$Chemical_Id)){
      pData <- data.frame(Chemical_Id=colnames(expressionData))
      rownames(pData) <- colnames(expressionData)
    }
    
    phenoData <- new("AnnotatedDataFrame", data=pData)
    
    #create feature data
    fData <- fData(eset) %>% rename("Connectivity_Id" := id)
    rownames(fData) <- rownames(eset)
    colNames <- colnames(fData) 
    colNames <- gsub("\\(", "", colNames, fixed=FALSE)
    colNames <- gsub(")", "", colNames, fixed=FALSE)  
    colNames <- gsub(" ", "_", colNames, fixed=FALSE)
    colNames <- gsub("_uM", "", colNames, fixed=FALSE)
    colNames <- gsub("/", "_", colNames, fixed=FALSE)
    colnames(fData) <- colNames
    colnames(fData) <- toTitleCase(colnames(fData))
    
    l <- grep("Pert_id", colnames(fData), fixed=FALSE)
    
    if(length(l) > 0){ 
      fData <- fData[-l]
    }
    
    featureData <- new("AnnotatedDataFrame", data=fData)
    
    #Create the expression set###
    Connectivity[[connmap[p]]] <- ExpressionSet(assayData=expressionData, phenoData=phenoData, featureData=featureData)
    
  }
  
  dat[["Connectivity"]] <- Connectivity
  write_rds(Connectivity, paste0("~/Documents/Internship Project/Exposome app/shinyApps/data/", dataset[d], "/Connectivity.RDS"))
  
  #########################################################
  # 
  # EXPORT THE GENE SET ENRICHMENT
  # 
  #########################################################
  
  # get the gene set 
  gsen <- dat[["Gene Set Enrichment"]]
  genesetname <- names(gsen); GS_Enrichment <- list()
  
  for(u in 1:length(genesetname)){
    #u=1;
    ##Create output data
    eset <- gsen[[genesetname[u]]]
    
    #craate expression data
    expressionData <- exprs(eset)
    
    #create phenotypic data
    if(all(colnames(expressionData) %in% proann$Sig_Id)){
      pData <- data.frame(Sig_Id=colnames(expressionData))
      rownames(pData) <- colnames(expressionData)
    }else if(all(colnames(expressionData) %in% proann$Chemical_Id)){
      pData <- data.frame(Chemical_Id=colnames(expressionData))
      rownames(pData) <- colnames(expressionData)
    }
    
    phenoData <- new("AnnotatedDataFrame", data=pData)
    
    #create feature data
    fData <- data.frame(Geneset=rownames(eset), fData(eset))
    rownames(fData) <- rownames(eset)
    colNames <- colnames(fData) 
    colNames <- gsub("\\(", "", colNames, fixed=FALSE)
    colNames <- gsub(")", "", colNames, fixed=FALSE)  
    colNames <- gsub(" ", "_", colNames, fixed=FALSE)
    colNames <- gsub("_uM", "", colNames, fixed=FALSE)
    colNames <- gsub("/", "_", colNames, fixed=FALSE)
    colnames(fData) <- colNames
    colnames(fData) <- toTitleCase(colnames(fData))
    
    featureData <- new("AnnotatedDataFrame", data=fData)
    
    #Create the expression set###
    GS_Enrichment[[genesetname[u]]] <- ExpressionSet(assayData=expressionData, phenoData=phenoData, featureData=featureData)
    
  }
  
  dat[["Gene Set Enrichment"]] <- GS_Enrichment
  write_rds(GS_Enrichment, paste0("~/Documents/Internship Project/Exposome app/shinyApps/data/", dataset[d], "/GS_Enrichment.RDS"))
  
  #########################################################
  # 
  # EXPORT DATASET
  # 
  #########################################################
  write_rds(dat, paste0("~/Documents/Internship Project/Exposome app/shinyApps/data/", dataset[d], "/data.RDS"))
  
}
