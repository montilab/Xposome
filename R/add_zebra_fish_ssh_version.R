
library(tidyverse)
path = paste0("/home/xposome/Testing")
source(paste0("/home/xposome/Testing/morpheus_heatmap.R"))

dataset=c("HEPG2", "MCF10A", "ADIPO")

for(d in seq_along(dataset)){
  #d=3;
  Portal=dataset[d];
  pro_ann <- readRDS(paste0(path, "/data/", dataset[d], "/Profile_Annotation.RDS"))
  chem_ann <- readRDS(paste0(path, "/data/", dataset[d], "/Chemical_Annotation.RDS"))

  ##Create morpheus heatmap for gene expression####
  gene_expression <- readRDS(paste0(path, "/data/", dataset[d], "/Expression_Set.RDS"))
  var <- ifelse(all(colnames(gene_expression) %in% pro_ann$Sig_Id), "Sig_Id", "Chemical_Id")
  
  eset <- gene_expression
  landmark <- colnames(fData(eset)) %in% "Landmark_Gene"
  
  if(any(landmark %in% TRUE)){
    genesetname <- paste0("landmark_gene")
    inds <- fData(eset)[,"Landmark_Gene"] %in% "Yes"
    eset <- eset[inds,]
    marker <- "Landmark"
    cluster <- FALSE
  }else{
    genesetname <- paste0("gene_expression")
    eset <- eset
    marker <- "Genes"
    cluster <- FALSE
  }    
  
  ge <- exprs(eset)
  feature <- fData(eset)
  cnames <- colnames(ge)
  nr <- nrow(ge); nc <- ncol(ge);
  
  jsonoutputpath=paste0(path, "/www/JSON/", Portal, "/", genesetname, ".json")
  htmloutputpath=paste0(path, "/www/JSON/", Portal, "/", genesetname, ".html")
  
  if(var %in% "Sig_Id"){
    CreateJSON(outputpath=jsonoutputpath, dataset=Portal, genesetname=genesetname, ge=ge, proann=pro_ann, feature=feature, nr=nr, nc=nc, marker=marker)
    CreateHTML(outputpath=htmloutputpath, dataset=Portal, genesetname=genesetname, proann=pro_ann, feature=feature, marker=marker, cluster=cluster)
  }else{
    CreateJSON(outputpath=jsonoutputpath, dataset=Portal, genesetname=genesetname, ge=ge, proann=chem_ann, feature=feature, nr=nr, nc=nc, marker=marker)
    CreateHTML(outputpath=htmloutputpath, dataset=Portal, genesetname=genesetname, proann=chem_ann, feature=feature, marker=marker, cluster=cluster)
  }
  
  ##Create morpheus heatmap for connectivity####
  connectivity <- readRDS(paste0(path, "/data/", dataset[d], "/Connectivity.RDS"))
  conn_pcl <- connectivity[["pcl"]]; conn_pert <- connectivity[["pert"]];
  
  # Create classes for CMap connectivity #####
  connmap <- c("pcl", "pert")
  
  for(p in 1:length(connmap)){
    #p=1;
    eSet <- connectivity[[connmap[p]]]
    var <- ifelse(all(colnames(eSet) %in% pro_ann$Sig_Id), "Sig_Id", "Chemical_Id")
    
    #create morpheus heatmap for connectivity map
    genesetname <- paste0(connmap[p], "_connectivity")
    eset <- eSet
    ge <- exprs(eset)
    feature <- fData(eset)
    cnames <- colnames(ge)
    
    nr <- nrow(ge); nc <- ncol(ge); 
    marker <- "Connectivity"; cluster <- FALSE;
    
    jsonoutputpath=paste0(path, "/www/JSON/", Portal, "/", genesetname, ".json")
    htmloutputpath=paste0(path, "/www/JSON/", Portal, "/", genesetname, ".html")
    
    if(var %in% "Sig_Id"){
      CreateJSON(outputpath=jsonoutputpath, dataset=Portal, genesetname=genesetname, ge=ge, proann=pro_ann, feature=feature, nr=nr, nc=nc, marker=marker)
      CreateHTML(outputpath=htmloutputpath, dataset=Portal, genesetname=genesetname, proann=pro_ann, feature=feature, marker=marker, cluster=cluster)
    }else{
      CreateJSON(outputpath=jsonoutputpath, dataset=Portal, genesetname=genesetname, ge=ge, proann=chem_ann, feature=feature, nr=nr, nc=nc, marker=marker)
      CreateHTML(outputpath=htmloutputpath, dataset=Portal, genesetname=genesetname, proann=chem_ann, feature=feature, marker=marker, cluster=cluster)
    }
  }
  
  # Run gene set enrichment analysis for hallmark, C2, and NURSA
  gs_enrichment <- readRDS(paste0(path, "/data/", dataset[d], "/GS_Enrichment.RDS"))
  Enrichment_Version=7;
  genesetcollection=c(paste0("h.all.v", Enrichment_Version, ".0"), paste0("c2.cp.reactome.v", Enrichment_Version, ".0"), paste0("nursa_consensome_Cbyfdrvalue_0.01"));
  method=c("gsva", "ssgsea", "zscore");
  
  # Getting differential expression
  for(u in 1:length(genesetcollection)){
    #u=1;
    for(m in 1:length(method)){
      #m=1;      
      genesetname <- paste0("gsscores_", genesetcollection[u], "_", method[m])

      #create expression set
      eSet <- gs_enrichment[[genesetname]]
      
      ##Create morpheus heatmap###
      eset <- eSet
      ge <- exprs(eset)
      feature <- fData(eset)
      cnames <- colnames(ge)
      
      nr <- nrow(ge); nc <- ncol(ge); 
      marker <- "Gene Sets"; cluster <- FALSE;
      
      jsonoutputpath=paste0(path, "/www/JSON/", Portal, "/", genesetname, ".json")
      htmloutputpath=paste0(path, "/www/JSON/", Portal, "/", genesetname, ".html")
      
      if(var %in% "Sig_Id"){
        CreateJSON(outputpath=jsonoutputpath, dataset=Portal, genesetname=genesetname, ge=ge, proann=pro_ann, feature=feature, nr=nr, nc=nc, marker=marker)
        CreateHTML(outputpath=htmloutputpath, dataset=Portal, genesetname=genesetname, proann=pro_ann, feature=feature, marker=marker, cluster=cluster)
      }else{
        CreateJSON(outputpath=jsonoutputpath, dataset=Portal, genesetname=genesetname, ge=ge, proann=chem_ann, feature=feature, nr=nr, nc=nc, marker=marker)
        CreateHTML(outputpath=htmloutputpath, dataset=Portal, genesetname=genesetname, proann=chem_ann, feature=feature, marker=marker, cluster=cluster)
      }
      
    }
  }
}

