
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

## Get the gene expression statistics for each chemical
#* @param portal
#* @param chemicalId
#* @param summarize.func
#* @param landmark
#* @param do.nmarkers
#* @param nmarkers
#* @param do.scorecutoff
#* @param scorecutoff
#' @get /gene_expression
#' @post /gene_expression
gene_expression <- function(portal, chemical_id, summarize.func="median", landmark=FALSE, do.nmarkers=FALSE, nmarkers=c(100, 100), do.scorecutoff=TRUE, scorecutoff=c(-2, 2)){
  
  ##Shiny Packages####
  library(uuidtools)
  library(GeneHive)
  
  # post body
  profile_dat <- getWorkFileAsObject(
    hiveWorkFileID(WorkFileIDs[[portal]]["Profile_Annotation.RDS"])
  )
  
  chemical_dat <- getWorkFileAsObject(
    hiveWorkFileID(WorkFileIDs[[fname]]["Chemical_Annotation.RDS"])
  )
  
  expression_dat <- getWorkFileAsObject(
    hiveWorkFileID(WorkFileIDs[[portal]]["Expression_Set.RDS"])
  )
  
  if(all(colnames(expression_dat) %in% profile_dat$Sig_Id)){
    annot_var <- "Sig_Id"
  }else{
    annot_var <- "Chemical_Id"
  }
  
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
    sig <- unique(profile_dat[which(profile_dat$unique_ID_by_chem %in% exposure[i]), annot_var])
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
    
    ind0 <- sum(x > 0)
    n1 <- min(nmarkers[1], ind0)
    n2 <- min(nmarkers[2], n-ind0)
    ord <- order(x, decreasing = TRUE)
    n2ind <- n-n2+1
    
    if(n1 == 0 & n2 == 0){
      x.ind.nmarkers <- NULL
    }else if(n2 == 0){
      x.ind.nmarkers <- ord[1:n1]
    }else{
      x.ind.nmarkers <- c(ord[1:n1], ord[n2ind:n])
    }
    
  } else { 
    x.ind.nmarkers <- 1:n 
  }
  
  if(do.scorecutoff){
    #TODO: rank by score here too
    x.ind.scorecutoff <- which(x > scorecutoff[2] | x < scorecutoff[1])
  }else {
    x.ind.scorecutoff <- 1:n
  }
  
  inds <- intersect(x.ind.nmarkers, x.ind.scorecutoff)
  inds <- inds[order(x[inds], decreasing = TRUE)]
  
  #get expression results
  res.ind <- inds; res.scores <- x[inds]
  
  #determine whether gene is up or down regulated
  direction <- sapply(res.scores, function(i){
    if(i > 0) return("Up") else return("Down")
  })
  
  #creata the summary table 
  table <- cbind(fdat[res.ind,, drop = FALSE], Direction = direction, SummaryScore=res.scores, mat[res.ind,, drop = FALSE])
  colnames(table)[which(colnames(table) %in% "SummaryScore")] <- "Summary Score"
  
  #return hyperlink from genecard.org
  table$Gene <- sapply(as.character(table$Gene), get_genecard_link)
  
  return(table)
  
}

## Get the geneset enrichment statistics for each chemical
#* @param portal
#* @param chemicalId
#* @param gsname
#* @param header
#* @param summarize.func
#' @get /gs_enrichment
#' @post /gs_enrichment
gs_enrichment <- function(portal, chemical_id, gsname="Hallmark", header="GS Score", summarize.func="median"){
  
  ##Shiny Packages####
  library(uuidtools)
  library(GeneHive)
  
  # post body
  profile_dat <- getWorkFileAsObject(
    hiveWorkFileID(WorkFileIDs[[portal]]["Profile_Annotation.RDS"])
  )
  
  chemical_dat <- getWorkFileAsObject(
    hiveWorkFileID(WorkFileIDs[[fname]]["Chemical_Annotation.RDS"])
  )
  
  expression_dat <- getWorkFileAsObject(
    hiveWorkFileID(WorkFileIDs[[portal]]["GS_Enrichment.RDS"])
  )
  
  if(all(colnames(expression_dat) %in% profile_dat$Sig_Id)){
    annot_var <- "Sig_Id"
  }else{
    annot_var <- "Chemical_Id"
  }
  
  
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
    sig <- unique(profile_dat[which(profile_dat$unique_ID_by_chem %in% exposure[i]), annot_var])
    if(length(sig) > 1){
      mat[,i] <- rowMeans(eSet[,as.character(sig)], na.rm=T)
    }else{
      mat[,i] <- eSet[,as.character(sig)]
    }
  }
  
  res <- apply(mat, 1, match.fun(summarize.func))
  res <- as.numeric(res)
  
  res <- cbind(fdat, score = res, mat)
  res <- res[order(res$score, decreasing = TRUE),, drop = FALSE]
  colnames(res)[which(colnames(res) %in% "score")] <- "Summary Score"
  
  #return hyperlink to MSigDB genesets
  if(gsname %in% c("Hallmark", "C2")) res$Geneset <- sapply(as.character(res$Geneset), get_geneset_link)
  
  return(res)
  
}

## Get the connectivity map statistics for each chemical
#* @param portal
#* @param chemicalId
#* @param connectivity_classes
#* @param header
#* @param summarize.func
#' @get /connectivity
#' @post /connectivity
connectivity <- function(portal, chemical_id, connectivity_classes="pcl", header="Connectivity Score", summarize.func="median"){
  
  ##Shiny Packages####
  library(uuidtools)
  library(GeneHive)
  
  # post body
  profile_dat <- getWorkFileAsObject(
    hiveWorkFileID(WorkFileIDs[[portal]]["Profile_Annotation.RDS"])
  )
  
  chemical_dat <- getWorkFileAsObject(
    hiveWorkFileID(WorkFileIDs[[fname]]["Chemical_Annotation.RDS"])
  )
  
  expression_dat <- getWorkFileAsObject(
    hiveWorkFileID(WorkFileIDs[[portal]]["Connectivity.RDS"])
  )
  
  if(all(colnames(expression_dat) %in% profile_dat$Sig_Id)){
    annot_var <- "Sig_Id"
  }else{
    annot_var <- "Chemical_Id"
  }
  
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
    sig <- unique(profile_dat[which(profile_dat$unique_ID_by_chem %in% exposure[i]), annot_var])
    if(length(sig) > 1){
      mat[,i] <- rowMeans(eSet[,as.character(sig)], na.rm=T)
    }else{
      mat[,i] <- eSet[,as.character(sig)]
    }
  }
  
  res <- apply(mat, 1, match.fun(summarize.func))
  res <- as.numeric(res)
  
  res <- cbind(fdat, score = res, mat)
  res <- res[order(res$score, decreasing = TRUE),, drop = FALSE]
  colnames(res)[colnames(res) %in% "score"]<- "Summary Score"
  
  return(res)
  
}


