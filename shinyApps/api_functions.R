
# Increase the memory limit for reading and downloading data from API
library(unix)
unix::rlimit_as(1e12, 1e12)

# Get the 25th percentile ####
Q1 <- function(x){ 
  quantile(x, 0.25, na.rm = T)
}

# Get the 75th percentile ####
Q3 <- function(x){
  quantile(x, 0.75, na.rm = T)
}

##the default gs enrichment version####
gs_enrichment_version <- 7

##Getting the gene set scores for diffrent gsva methods
dsmap <- list(
  Hallmark=paste0("gsscores_h.all.v", gs_enrichment_version, ".0"),
  C2=paste0("gsscores_c2.cp.reactome.v", gs_enrichment_version, ".0"),
  NURSA=paste0("gsscores_nursa_consensome_Cbyfdrvalue_0.01")
)

## Link gene expression to genecards.org####
get_genecard_link <- function(genesymbol){
  sprintf('<a href="http://www.genecards.org/cgi-bin/carddisp.pl?gene=%s&keywords=%s" target="_blank" style="text-decoration:none;">%s</a>', genesymbol, genesymbol, genesymbol)
}

## Get the gene set enrichment hyperlink####
get_geneset_link <- function(geneset){
  label <- gsub("_", " ", geneset)
  geneset <- gsub(" ", "_", geneset)
  sprintf('<a href="http://software.broadinstitute.org/gsea/msigdb/cards/%s" target="_blank" style="text-decoration:none;">%s</a>', geneset, label)
}

# Get the chemical name####
get_chem_description <- function(chemical_dat, chemical_id, data_frame=FALSE){
  
  pos <- lapply(c("Chemical_Id", "Chemical_Name", "BUID", "CAS"), function(x){
    w <- which(chemical_dat[,x] %in% chemical_id)
    if(length(w) > 0){
      return(w)
    }
  }) %>% unlist()
  
  if(length(pos) > 0){
    if(data_frame){
      return(chemical_dat[pos, c("Chemical_Id", "Chemical_Name", "BUID", "CAS")])
    }else{
      return(sort(unique(chemical_dat$Chemical_Id[pos])))
    }
  }else{
    return(NULL)
  }
  
}

##Get gene expression####
get_de <- function(
  chemical_id, annot_var,
  profile_dat, chemical_dat, expression_dat,
  header = "ModZScore",
  summarize.func = "mean",
  landmark = FALSE, 
  do.nmarkers = FALSE, nmarkers = c(1000, 1000),
  do.scorecutoff = FALSE, scorecutoff = c(-2, 2),
  concentration = FALSE){
  
  #getting signature id ####
  profile_dat <- profile_dat[match(colnames(expression_dat), profile_dat[, annot_var]),]
  exposure <- sort(unique(profile_dat$unique_ID_by_chem[which(profile_dat$Chemical_Id %in% chemical_id)]))
  
  #getting expression data####
  eset <- expression_dat
  
  #getting feature data####
  fdat <- fData(eset)
  
  if(nrow(fdat) > 0){
    if(!"Gene" %in% colnames(fdat)){
      fdat <- data.frame(Gene=rownames(eset), fdat)
    }
  }else{
    fdat <- data.frame(Gene=rownames(eset))
  }
  
  #check it landmark is selected
  if(landmark) {
    if("Landmark_Gene" %in% colnames(fdat)){
      eset <- eset[which(toupper(fdat[,"Landmark_Gene"]) %in% "YES"),] 
    }
  }
  
  #summarise the expression set
  eSet <- exprs(eset); mat <- matrix(NA, nrow=nrow(eSet), ncol=length(exposure), byrow=T, dimnames=list(rownames(eSet), paste0(header, " ", exposure)))
  
  for(i in seq_along(exposure)){
    #i=1;
    sig <- unique(profile_dat[which(profile_dat$unique_ID_by_chem %in% exposure[i] & profile_dat$Chemical_Id %in% chemical_id), annot_var])
    if(length(sig) > 1){
      mat[,i] <- rowMeans(eSet[,as.character(sig)], na.rm=T)
    }else{
      mat[,i] <- eSet[,as.character(sig)]
    }
  }
  
  x <- apply(mat, 1, match.fun(summarize.func))
  x <- as.numeric(x)
  n <- length(x)
  
  if(do.nmarkers){
  
    ind_down <- which(x < 0)
    ind_up <- which(x > 0) 
      
    ord_down <- data.frame(
      pos=ind_down,
      x=x[ind_down]
    ) %>% arrange(x)
    
    ord_up <- data.frame(
      pos=ind_up,
      x=x[ind_up]
    ) %>% arrange(desc(x))
    
    n1 = ifelse(as.numeric(nmarkers[1]) > nrow(ord_down), nrow(ord_down), as.numeric(nmarkers[1])); 
    n2 = ifelse(as.numeric(nmarkers[2]) > nrow(ord_up), nrow(ord_up), as.numeric(nmarkers[2]));
    
    if(n1 == 0 & n2 == 0){
      x.ind.nmarkers <- 1:n
    }else if(n2 == 0 & n1 > 0){
      x.ind.nmarkers <- ord_down$pos[1:n1]
    }else if(n1 == 0 & n2 > 0){
      x.ind.nmarkers <- ord_up$pos[1:n2]
    }else if(n1 > 0 & n2 > 0){
      x.ind.nmarkers <- c(ord_down$pos[1:n1], ord_up$pos[1:n2])
    }
    
  }else { 
    
    x.ind.nmarkers <- 1:n 
    
  }
  
  if(do.scorecutoff){
    x.ind.scorecutoff <- c(which(x <= as.numeric(scorecutoff[1])), which(x >= as.numeric(scorecutoff[2])))
  }else {
    x.ind.scorecutoff <- 1:n
  }
  
  inds <- intersect(x.ind.nmarkers, x.ind.scorecutoff)
  inds <- inds[order(x[inds], decreasing = TRUE)]
  
  #get expression results
  res.ind <- inds; res.scores <- x[inds];
  
  #determine whether gene is up or down regulated
  direction <- sapply(res.scores, function(i){
    if(i > 0) return("Up") else return("Down")
  })
  
  #create the summary table 
  table <- cbind(fdat[res.ind,, drop = FALSE], Direction = direction, SummaryScore=res.scores, mat[res.ind,, drop = FALSE])
  colnames(table)[which(colnames(table) %in% "SummaryScore")] <- "Summary Score"
  
  #return hyperlink from genecard.org
  #table$Gene <- sapply(as.character(table$Gene), get_genecard_link)
  
  #return a table with concentration separated
  if(concentration){
    pos <- grep(header, colnames(table), fixed=TRUE)
    table <- table %>% gather(key="Concentration", value="ModZScore", -colnames(table)[-pos]) %>% 
      mutate(Concentration=gsub(paste0(header,"|\\s"), "", Concentration))
  }
  
  return(table)

}

##Summarize gene set enrichment#####
get_gsenrichment <- function(
  chemical_id, annot_var,
  profile_dat, chemical_dat, expression_dat,
  gsname = "Hallmark",
  header = "GS Score",
  summarize.func = "mean",
  concentration = FALSE){
  
  #getting signature id ####
  profile_dat <- profile_dat[match(colnames(expression_dat), profile_dat[, annot_var]),]
  exposure <- sort(unique(profile_dat$unique_ID_by_chem[which(profile_dat$Chemical_Id %in% chemical_id)]))
  
  #getting expression data####
  eset <- expression_dat
  
  #getting feature data####
  fdat <- fData(eset)
  
  if(nrow(fdat) > 0){
    if(!"Geneset" %in% colnames(fdat)){
      fdat <- data.frame(Geneset=rownames(eset), fdat)
    }
  }else{
    fdat <- data.frame(Geneset=rownames(eset))
  }
  
  #summarise the expression set
  eSet <- exprs(eset); mat <- matrix(NA, nrow=nrow(eSet), ncol=length(exposure), byrow=T, dimnames=list(rownames(eSet), paste0(header, " ", exposure)))
  
  for(i in seq_along(exposure)){
    #i=1;
    sig <- unique(profile_dat[which(profile_dat$unique_ID_by_chem %in% exposure[i] & profile_dat$Chemical_Id %in% chemical_id), annot_var])
    if(length(sig) > 1){
      mat[,i] <- rowMeans(eSet[,as.character(sig)], na.rm=T)
    }else{
      mat[,i] <- eSet[,as.character(sig)]
    }
  }
  
  res <- apply(mat, 1, match.fun(summarize.func))
  res <- as.numeric(res)
  
  table <- cbind(fdat, score = res, mat)
  table <- table[order(table$score, decreasing = TRUE),, drop = FALSE]
  colnames(table)[which(colnames(table) %in% "score")] <- "Summary Score"
  
  #return hyperlink to MSigDB genesets
  #if(gsname %in% c("Hallmark", "C2")) table$Geneset <- sapply(as.character(table$Geneset), get_geneset_link)
  
  #return a table with concentration separated
  if(concentration){
    pos <- grep(header, colnames(table), fixed=TRUE)
    table <- table %>% gather(key="Concentration", value="GS Score", -colnames(table)[-pos]) %>% 
      mutate(Concentration=gsub(paste0(header,"|\\s"), "", Concentration))
  }
  
  return(table)
  
}

##Summarize connectivity map#####
get_connectivity <- function(
  chemical_id, annot_var,
  profile_dat, chemical_dat, expression_dat,
  header = "Connectivity Score",
  summarize.func = "mean",
  concentration = FALSE){
  
  #getting signature id ####
  profile_dat <- profile_dat[match(colnames(expression_dat), profile_dat[, annot_var]),]
  exposure <- sort(unique(profile_dat$unique_ID_by_chem[which(profile_dat$Chemical_Id %in% chemical_id)]))
  
  #getting expression data####
  eset <- expression_dat
  
  #getting feature data####
  fdat <- fData(eset)
  
  if(nrow(fdat) > 0){
    if(!"Connectivity_Id" %in% colnames(fdat)){
      fdat <- data.frame(Connectivity_Id=rownames(eset), fdat)
    }
  }else{
    fdat <- data.frame(Connectivity_Id=rownames(eset))
  }
  
  #summarise the expression set
  eSet <- exprs(eset); mat <- matrix(NA, nrow=nrow(eSet), ncol=length(exposure), byrow=T, dimnames=list(rownames(eSet), paste0(header, " ", exposure)))
  
  for(i in seq_along(exposure)){
    #i=1;
    sig <- unique(profile_dat[which(profile_dat$unique_ID_by_chem %in% exposure[i] & profile_dat$Chemical_Id %in% chemical_id), annot_var])
    if(length(sig) > 1){
      mat[,i] <- rowMeans(eSet[,as.character(sig)], na.rm=T)
    }else{
      mat[,i] <- eSet[,as.character(sig)]
    }
  }
  
  res <- apply(mat, 1, match.fun(summarize.func))
  res <- as.numeric(res)
  
  table <- cbind(fdat, score = res, mat)
  table <- table[order(table$score, decreasing = TRUE),, drop = FALSE]
  colnames(table)[colnames(table) %in% "score"] <- "Summary Score"
  
  #return a table with concentration separated
  if(concentration){
    pos <- grep(header, colnames(table), fixed=TRUE)
    table <- table %>% gather(key="Concentration", value="Connectivity Score", -colnames(table)[-pos]) %>% 
      mutate(Concentration=gsub(paste0(header,"|\\s"), "", Concentration))
  }
  
  return(table)
  
}

## Get a list of projects available on GeneHive database
#* @param orderby
#' @get /projects
#' @post /projects
projects <- function(orderby="asc"){

  ##Shiny Packages####
  library(uuidtools)
  library(GeneHive)
  library(tidyverse)
  library(Biobase)
  library(jsonlite)

  # Retrieve list of all PortalDataset entities in hive
  portal_names <- unique(sapply(GeneHive::listEntities("PortalDataset"), slot, "portal"))

  # Sort projects by ascending or descending order
  if(orderby == "asc"){
    portal_names <- sort(portal_names, decreasing = FALSE)
  }else if(orderby == "desc"){
    portal_names <- sort(portal_names, decreasing = TRUE)
  }

  return(toJSON(portal_names, pretty=TRUE))

}

## Get a list of chemical ids/cas ids available on GeneHive database
#* @param projects
#* @param chemical_ids
#' @get /chemicals
#' @post /chemicals
chemicals <- function(res, req, projects="all", chemical_ids="all"){
  
  ##Shiny Packages####
  library(uuidtools)
  library(GeneHive)
  library(tidyverse)
  library(Biobase)
  library(jsonlite)
  
  # string split the project by comma separator
  projects <- strsplit(as.character(projects), ",", perl=TRUE) %>% unlist() %>% gsub("[[:space:]]", "", .)
  chemical_ids <- strsplit(as.character(chemical_ids), ",", perl=TRUE) %>% unlist() %>% gsub("[[:space:]]", "", .)
  
  # Retrieve list of all PortalDataset entities in hive
  portal_names <- unique(sapply(GeneHive::listEntities("PortalDataset"), slot, "portal"))
  
  # Check if the project is available on GeneHive, if not, return warning message
  if(any(toupper(projects) %in% toupper(c(portal_names, "all")))){
    
    all_chemical_list <- NULL;
    
    if(any(toupper(projects) %in% toupper("all"))){
      
      for(i in seq_along(portal_names)){
        
        project <- portal_names[i]
        
        # Retrieve list of all projectDataset entities in hive
        datasets <- listEntities("PortalDataset", portal=project)
        
        # Sort by timestamp and extract most recent dataset to convenience object
        datasets <- datasets[order(sapply(datasets, slot, "timestamp"))]
        dataset <- datasets[[length(datasets)]]
        
        # read in datasets
        chemical_dat <- getWorkFileAsObject(
          hiveWorkFileID(dataset@ChemicalAnnotationRDS)
        )
        
        if(any(toupper(chemical_ids) %in% "ALL")){
          chemical_list <- chemical_dat[, c("Chemical_Id", "Chemical_Name", "BUID", "CAS")]
        }else{
          chemical_list <- get_chem_description(chemical_dat = chemical_dat, chemical_id = chemical_ids, data_frame = TRUE)
        }
        
        if(!is.null(chemical_list)){
          chemical_list <- chemical_list %>% mutate(Project=project)
          all_chemical_list <- rbind(all_chemical_list, chemical_list)
        }
        
      }
      
    }else{
      
      for(i in seq_along(projects)){
        
        if(toupper(projects[i]) %in% toupper(portal_names)){
          
          project <- portal_names[which(toupper(portal_names) == toupper(projects[i]))]
          
          # Retrieve list of all projectDataset entities in hive
          datasets <- listEntities("PortalDataset", portal=project)
          
          # Sort by timestamp and extract most recent dataset to convenience object
          datasets <- datasets[order(sapply(datasets, slot, "timestamp"))]
          dataset <- datasets[[length(datasets)]]
          
          # read in datasets
          chemical_dat <- getWorkFileAsObject(
            hiveWorkFileID(dataset@ChemicalAnnotationRDS)
          )
          
          # get a list of chemical ids/cas ids for a project
          if(any(toupper(chemical_ids) %in% "ALL")){
            chemical_list <- chemical_dat[, c("Chemical_Id", "Chemical_Name", "BUID", "CAS")]
          }else{
            chemical_list <- get_chem_description(chemical_dat = chemical_dat, chemical_id = chemical_ids, data_frame = TRUE)
          }
          
          if(!is.null(chemical_list)){
            chemical_list <- chemical_list %>% mutate(Project=project)
            all_chemical_list <- rbind(all_chemical_list, chemical_list)
          }          
        }
      }
    }
    
    return(toJSON(all_chemical_list, pretty=TRUE))
    
  }else{
    
    res$status <- 404  
    return(list(error=paste0("The ", paste0(projects, collapse=", "), " project(s) is/are not available on the Xposome portal")))
    
  }
  
}

## Get profile annotation
#* @param project
#' @get /profile_annotation
#' @post /profile_annotation
profile_annotation <- function(res, req, project){

  ##Shiny Packages####
  library(uuidtools)
  library(GeneHive)
  library(tidyverse)
  library(Biobase)
  library(jsonlite)

  # Retrieve list of all PortalDataset entities in hive
  portal_names <- unique(sapply(GeneHive::listEntities("PortalDataset"), slot, "portal"))
  
  # Check if the project is available on GeneHive, if not, return warning message
  if(toupper(project) %in% toupper(portal_names)){
  
    # Retrieve list of all projectDataset entities in hive
    datasets <- listEntities("PortalDataset", portal=project)
  
    # Sort by timestamp and extract most recent dataset to convenience object
    datasets <- datasets[order(sapply(datasets, slot, "timestamp"))]
    dataset <- datasets[[length(datasets)]]
  
    # read in datasets
    profile_dat <- getWorkFileAsObject(
      hiveWorkFileID(dataset@ProfileAnnotationRDS)
    )
    
    return(toJSON(profile_dat, pretty=TRUE))
    
  }else{
    
    res$status <- 404  
    return(list(error=paste0("This ", project, " project is not available on the Xposome portal")))
    
  }
  
}

## Get chemical annotation
#* @param project
#' @get /chemical_annotation
#' @post /chemical_annotation
chemical_annotation <- function(res, req, project){

  ##Shiny Packages####
  library(uuidtools)
  library(GeneHive)
  library(tidyverse)
  library(Biobase)
  library(jsonlite)

  # Retrieve list of all PortalDataset entities in hive
  portal_names <- unique(sapply(GeneHive::listEntities("PortalDataset"), slot, "portal"))
  
  # Check if the project is available on GeneHive, if not, return warning message
  if(toupper(project) %in% toupper(portal_names)){
    
    # Retrieve list of all PortalDataset entities in hive
    datasets <- listEntities("PortalDataset", portal=project)
  
    # Sort by timestamp and extract most recent dataset to convenience object
    datasets <- datasets[order(sapply(datasets, slot, "timestamp"))]
    dataset <- datasets[[length(datasets)]]
  
    chemical_dat <- getWorkFileAsObject(
      hiveWorkFileID(dataset@ChemicalAnnotationRDS)
    )
    
    return(toJSON(chemical_dat, pretty=TRUE))
    
  }else{
    
    res$status <- 404  
    return(list(error=paste0("This ", project, " project is not available on the Xposome portal")))
    
  }

}

## Get gene expression set
#* @param project
#' @serializer rds
#' @get /expression_set
#' @post /expression_set
expression_set <- function(res, req, project){

  ##Shiny Packages####
  library(uuidtools)
  library(GeneHive)
  library(tidyverse)
  library(Biobase)
  library(jsonlite)

  # Retrieve list of all PortalDataset entities in hive
  portal_names <- unique(sapply(GeneHive::listEntities("PortalDataset"), slot, "portal"))
  
  # Check if the project is available on GeneHive, if not, return warning message
  if(toupper(project) %in% toupper(portal_names)){
    
    # Retrieve list of all PortalDataset entities in hive
    datasets <- listEntities("PortalDataset", portal=project)
  
    # Sort by timestamp and extract most recent dataset to convenience object
    datasets <- datasets[order(sapply(datasets, slot, "timestamp"))]
    dataset <- datasets[[length(datasets)]]
  
    gene_expression_dat <- getWorkFileAsObject(
      hiveWorkFileID(dataset@GeneExpressionRDS)
    )
  
    return(as_attachment(gene_expression_dat, "Gene_Set_Enrichment.RDS"))
    
  }else{
    
    res$status <- 404  
    return(list(error=paste0("This ", project, " project is not available on the Xposome portal")))

  }
  
}

## Get gene set enrichment
#* @param project
#' @serializer rds
#' @get /enrichment_set
#' @post /enrichment_set
enrichment_set <- function(res, req, project){

  ##Shiny Packages####
  library(uuidtools)
  library(GeneHive)
  library(tidyverse)
  library(Biobase)
  library(jsonlite)
  
  # Retrieve list of all PortalDataset entities in hive
  portal_names <- unique(sapply(GeneHive::listEntities("PortalDataset"), slot, "portal"))
  
  # Check if the project is available on GeneHive, if not, return warning message
  if(toupper(project) %in% toupper(portal_names)){

    # Retrieve list of all PortalDataset entities in hive
    datasets <- listEntities("PortalDataset", portal=project)
  
    # Sort by timestamp and extract most recent dataset to convenience object
    datasets <- datasets[order(sapply(datasets, slot, "timestamp"))]
    dataset <- datasets[[length(datasets)]]
  
    gs_enrichment_dat <- getWorkFileAsObject(
      hiveWorkFileID(dataset@GeneSetEnrichmentRDS)
    )
  
    return(as_attachment(gs_enrichment_dat, "Gene_Set_Enrichment.RDS"))
    
  }else{
    
    res$status <- 404  
    return(list(error=paste0("This ", project, " project is not available on the Xposome portal")))

  }
  
}

## Get the connectivity dataset
#* @param project
#' @serializer rds
#' @get /connectivity_set
#' @post /connectivity_set
connectivity_set <- function(res, req, project){
  
  ##Shiny Packages####
  library(uuidtools)
  library(GeneHive)
  library(tidyverse)
  library(Biobase)
  library(jsonlite)

  # Retrieve list of all PortalDataset entities in hive
  portal_names <- unique(sapply(GeneHive::listEntities("PortalDataset"), slot, "portal"))
  
  # Check if the project is available on GeneHive, if not, return warning message
  if(toupper(project) %in% toupper(portal_names)){
    
    # Retrieve list of all PortalDataset entities in hive
    datasets <- listEntities("PortalDataset", portal=project)
  
    # Sort by timestamp and extract most recent dataset to convenience object
    datasets <- datasets[order(sapply(datasets, slot, "timestamp"))]
    dataset <- datasets[[length(datasets)]]
  
    connectivity_dat <- getWorkFileAsObject(
      hiveWorkFileID(dataset@ConnectivityRDS)
    )
  
    return(as_attachment(connectivity_dat, "Connectivity.RDS"))
    
  }else{
    
    res$status <- 404  
    return(list(error=paste0("This ", project, " project is not available on the Xposome portal")))

  }
  
}

## Get gene expression set
#* @param project
#' @serializer rds
#' @get /k2_taxonomer
#' @post /k2_taxonomer
k2_taxonomer <- function(res, req, project){
  
  ##Shiny Packages####
  library(uuidtools)
  library(GeneHive)
  library(K2Taxonomer)
  library(tidyverse)
  library(Biobase)
  library(jsonlite)
  
  # Retrieve list of all PortalDataset entities in hive
  portal_names <- unique(sapply(GeneHive::listEntities("PortalDataset"), slot, "portal"))
  
  # Check if the project is available on GeneHive, if not, return warning message
  if(toupper(project) %in% toupper(portal_names)){
    
    # Retrieve list of all PortalDataset entities in hive
    datasets <- listEntities("PortalDataset", portal=project)
    
    # Sort by timestamp and extract most recent dataset to convenience object
    datasets <- datasets[order(sapply(datasets, slot, "timestamp"))]
    dataset <- datasets[[length(datasets)]]
    
    K2summary <- getWorkFileAsObject(
      hiveWorkFileID(dataset@K2TaxonomerResultsRDS)
    )
    
    return(as_attachment(K2summary, "K2Taxonomer.RDS"))
    
  }else{
    
    res$status <- 404  
    return(list(error=paste0("This ", project, " project is not available on the Xposome portal")))

  }
    
}

## Get gene expression set
#* @param project
#' @serializer contentType list(type="application/zip")
#' @get /rds_bundle
#' @post /rds_bundle
rds_bundle <- function(res, req, project){
  
  ##Shiny Packages####
  library(uuidtools)
  library(GeneHive)
  library(K2Taxonomer)
  library(tidyverse)
  library(Biobase)
  library(jsonlite)
  
  # Retrieve list of all PortalDataset entities in hive
  portal_names <- unique(sapply(GeneHive::listEntities("PortalDataset"), slot, "portal"))
  
  # Check if the project is available on GeneHive, if not, return warning message
  if(toupper(project) %in% toupper(portal_names)){
    
    # Create a temporary directory to store RDS files
    fs <- c()
    tmpdir <- file.path(tempdir(), "output")
    dir.create(tmpdir)
    
    # Delete the directory after exiting the function
    on.exit({
      if (dir.exists(tmpdir)) {
        unlink(tmpdir, recursive = TRUE)
      }
    }, add = TRUE)
    
    datasets <- listEntities("PortalDataset", portal=project)
    
    # Sort by timestamp and extract most recent dataset to convenience object
    datasets <- datasets[order(sapply(datasets, slot, "timestamp"))]
    dataset <- datasets[[length(datasets)]]
    
    # Read in the profile data ####
    profile_dat <- getWorkFileAsObject(
      hiveWorkFileID(dataset@ProfileAnnotationRDS)
    )
    
    saveRDS(profile_dat, file.path(tmpdir, "Profile_Annotation.RDS"))
    
    # Read in the chemical data ####
    chemical_dat <- getWorkFileAsObject(
      hiveWorkFileID(dataset@ChemicalAnnotationRDS)
    )
    
    saveRDS(chemical_dat, file.path(tmpdir, "Chemical_Annotation.RDS"))
    
    # Read in the expression data ####
    expression_dat <- getWorkFileAsObject(
      hiveWorkFileID(dataset@GeneExpressionRDS)
    )
    
    saveRDS(expression_dat, file.path(tmpdir, "Gene_Expression.RDS"))
    
    # Read in the connectivity data ####
    connectivity_dat <- getWorkFileAsObject(
      hiveWorkFileID(dataset@ConnectivityRDS)
    )
    
    saveRDS(connectivity_dat, file.path(tmpdir, "Connectivity.RDS"))
    
    # Read in the gs enrichment data ####
    gs_enrichment_dat <- getWorkFileAsObject(
      hiveWorkFileID(dataset@GeneSetEnrichmentRDS)
    )
    
    saveRDS(gs_enrichment_dat, file.path(tmpdir, "Gene_Set_Enrichment.RDS"))
    
    # Read K2 Taxonomer Results ####
    K2summary <- getWorkFileAsObject(
      hiveWorkFileID(dataset@K2TaxonomerResultsRDS)
    )
    
    saveRDS(K2summary, file.path(tmpdir, "K2Taxonomer.RDS"))
    
    # zip the files with predefined names
    file_names <- c("Profile_Annotation", "Chemical_Annotation", "Gene_Expression", "Gene_Set_Enrichment", "Connectivity", "K2Taxonomer") 
    
    for(names in file_names){
      path <- file.path(tmpdir, paste0(names, ".RDS"))
      fs <- c(fs, path)
    } 
    
    zip_file = file.path(tmpdir, paste0(project, "-Dataset.zip"))
    
    zip(zipfile=zip_file, files=fs, flags = "-r9Xj")
    
    readBin(zip_file, what="raw", n=file.info(zip_file)$size)
    
  }else{
    
    res$status <- 404  
    return(list(error=paste0("This ", project, " project is not available on the Xposome portal")))

  }
  
}

## Get the gene expression statistics for each chemical
#* @param project
#* @param chemical_id
#* @param summarize.func
#* @param landmark
#* @param do.nmarkers
#* @param nmarkers_up
#* @param nmarkers_down
#* @param do.scorecutoff
#* @param scorecutoff_lb
#* @param scorecutoff_ub
#* @param concentration
#' @get /gene_expression
#' @post /gene_expression
gene_expression <- function(res, req, project, chemical_id, summarize.func="median", landmark=FALSE, do.nmarkers=TRUE, nmarkers_up=1000, nmarkers_down=1000, do.scorecutoff=TRUE, scorecutoff_lb=-2, scorecutoff_ub=2, concentration=FALSE){
  
  ##Shiny Packages####
  library(uuidtools)
  library(GeneHive)
  library(tidyverse)
  library(Biobase)
  library(jsonlite)

  # Retrieve list of all PortalDataset entities in hive
  portal_names <- unique(sapply(GeneHive::listEntities("PortalDataset"), slot, "portal"))
  
  # Check if the project is available on GeneHive, if not, return warning message
  if(toupper(project) %in% toupper(portal_names)){
    
    # Retrieve list of all PortalDataset entities in hive
    datasets <- listEntities("PortalDataset", portal=project)
      
    # Sort by timestamp and extract most recent dataset to convenience object
    datasets <- datasets[order(sapply(datasets, slot, "timestamp"))]
    dataset <- datasets[[length(datasets)]]
  
    # read in datasets
    profile_dat <- getWorkFileAsObject(
      hiveWorkFileID(dataset@ProfileAnnotationRDS)
    )
    
    chemical_dat <- getWorkFileAsObject(
      hiveWorkFileID(dataset@ChemicalAnnotationRDS)
    )
    
    expression_dat <- getWorkFileAsObject(
      hiveWorkFileID(dataset@GeneExpressionRDS)
    )
    
    if(all(profile_dat$Sig_Id %in% colnames(expression_dat))){
      annot_var <- "Sig_Id"
    }else{
      annot_var <- "Chemical_Id"
    }
  
    #getting chemical BUID####
    chemical_id <- get_chem_description(chemical_dat=chemical_dat, chemical_id=chemical_id)
    
    # Check if the chemical id/CAS id available on this project, if not, return warning message
    if(length(chemical_id)==0){
      
      res$status <- 404         
      return(list(error=paste0("This chemical id/CAS id does not available in the ", project, " project")))
      
    }else{
      
      table <- get_de(
        chemical_id=chemical_id,
        annot_var=annot_var,
        profile_dat=profile_dat,
        chemical_dat=chemical_dat,
        expression_dat=expression_dat,
        header = "ModZScore",
        summarize.func = summarize.func,
        landmark = landmark,
        do.nmarkers = do.nmarkers,
        nmarkers = c(nmarkers_down, nmarkers_up),
        do.scorecutoff = do.scorecutoff,
        scorecutoff = c(scorecutoff_lb, scorecutoff_ub),
        concentration = concentration
      )
      
      return(toJSON(table, pretty=TRUE))
      
    }
    
  }else{
    
    res$status <- 404         
    return(list(error=paste0("This ", project, " project is not available on the Xposome portal")))
    
  }

}

## Get the geneset enrichment statistics for each chemical
#* @param project
#* @param chemical_id
#* @param geneset
#* @param gsva
#* @param summarize.func
#* @param concentration
#' @get /gs_enrichment
#' @post /gs_enrichment
gs_enrichment <- function(res, req, project, chemical_id, geneset="Hallmark", gsva="gsva", summarize.func="median", concentration=FALSE){
  
  ##Shiny Packages####
  library(uuidtools)
  library(GeneHive)
  library(tidyverse)
  library(Biobase)
  library(jsonlite)
  
  # Retrieve list of all PortalDataset entities in hive
  portal_names <- unique(sapply(GeneHive::listEntities("PortalDataset"), slot, "portal"))
  
  # Check if the project is available on GeneHive, if not, return warning message
  if(toupper(project) %in% toupper(portal_names)){
    
    # Retrieve list of all PortalDataset entities in hive
    datasets <- listEntities("PortalDataset", portal=project)
    
    # Sort by timestamp and extract most recent dataset to convenience object
    datasets <- datasets[order(sapply(datasets, slot, "timestamp"))]
    dataset <- datasets[[length(datasets)]]
    
    # read in datasets
    profile_dat <- getWorkFileAsObject(
      hiveWorkFileID(dataset@ProfileAnnotationRDS)
    )
    
    chemical_dat <- getWorkFileAsObject(
      hiveWorkFileID(dataset@ChemicalAnnotationRDS)
    )
    
    gs_enrichment_dat <- getWorkFileAsObject(
      hiveWorkFileID(dataset@GeneSetEnrichmentRDS)
    )
    
    gs_enrichment_dat <- gs_enrichment_dat[[paste0(dsmap[[geneset]], "_", gsva)]]
    
    if(all(colnames(gs_enrichment_dat) %in% profile_dat$Sig_Id)){
      annot_var <- "Sig_Id"
    }else{
      annot_var <- "Chemical_Id"
    }
    
    #getting chemical BUID####
    chemical_id <- get_chem_description(chemical_dat=chemical_dat, chemical_id=chemical_id)
    
    # Check if the chemical id/CAS id available on this project, if not, return warning message
    if(length(chemical_id)==0){
      
      res$status <- 404         
      return(list(error=paste0("This chemical id/CAS id does not available in the ", project, " project")))
      
    }else{
      
      table <- get_gsenrichment(
        chemical_id=chemical_id,
        annot_var=annot_var,
        profile_dat=profile_dat,
        chemical_dat=chemical_dat,
        expression_dat=gs_enrichment_dat,
        gsname = geneset,
        header="GS Score",
        summarize.func=summarize.func,
        concentration=concentration
      )
      
      return(toJSON(table, pretty=TRUE))

    }
    
  }else{
    
    res$status <- 404         
    return(list(error=paste0("This ", project, " project is not available on the Xposome portal")))
    
  }
  
}

## Get the connectivity map statistics for each chemical
#* @param project
#* @param chemical_id
#* @param connectivity_classes
#* @param summarize.func
#' @get /connectivity
#' @post /connectivity
connectivity <- function(res, req, project, chemical_id, connectivity_classes="pcl", summarize.func="median", concentration=FALSE){
  
  ##Shiny Packages####
  library(uuidtools)
  library(GeneHive)
  library(tidyverse)
  library(Biobase)
  library(jsonlite)
  
  # Retrieve list of all PortalDataset entities in hive
  portal_names <- unique(sapply(GeneHive::listEntities("PortalDataset"), slot, "portal"))
  
  # Check if the project is available on GeneHive, if not, return warning message
  if(toupper(project) %in% toupper(portal_names)){
    
    if(connectivity_classes %in% c("pcl", "pert")){
      
      # Retrieve list of all PortalDataset entities in hive
      datasets <- listEntities("PortalDataset", portal=project)
      
      # Sort by timestamp and extract most recent dataset to convenience object
      datasets <- datasets[order(sapply(datasets, slot, "timestamp"))]
      dataset <- datasets[[length(datasets)]]
      
      # read in datasets
      profile_dat <- getWorkFileAsObject(
        hiveWorkFileID(dataset@ProfileAnnotationRDS)
      )
      
      chemical_dat <- getWorkFileAsObject(
        hiveWorkFileID(dataset@ChemicalAnnotationRDS)
      )
      
      connectivity_dat <- getWorkFileAsObject(
        hiveWorkFileID(dataset@ConnectivityRDS)
      )
      
      connectivity_dat <- connectivity_dat[[connectivity_classes]]
      
      if(all(colnames(connectivity_dat) %in% profile_dat$Sig_Id)){
        annot_var <- "Sig_Id"
      }else{
        annot_var <- "Chemical_Id"
      }
      
      #getting chemical BUID####
      chemical_id <- get_chem_description(chemical_dat=chemical_dat, chemical_id=chemical_id)
      
      # Check if the chemical id/CAS id available on this project, if not, return warning message
      if(length(chemical_id)==0){
        
        res$status <- 404         
        return(list(error=paste0("This chemical id/CAS id does not available in the ", project, " project")))
        
      }else{
        
        table <- get_connectivity(
          chemical_id=chemical_id,
          annot_var=annot_var,
          profile_dat=profile_dat,
          chemical_dat=chemical_dat,
          expression_dat=connectivity_dat,
          header="Connectivity Score",
          summarize.func=summarize.func,
          concentration=concentration
        )
        
        return(toJSON(table, pretty=TRUE))
        
      }
        
    }else{
      
      res$status <- 404  
      return(list(error=paste0("There is no ", connectivity_classes, " class in the connectivity map")))
      
    }
    
  }else{
    
    res$status <- 404  
    return(list(error=paste0("This ", project, " project is not available on the Xposome portal")))
    
  }
  
}
  
  
  
  