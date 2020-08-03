

##Define the meta variable test for K2Taxonomer
meta_variable_test <- data.frame(
  statistical_test=c("1-sided Fisher test", "2-sided Fisher test", "1-sided Wilcox RS test", "2-sided Wilcox RS test", "1-sided t test", "2-sided t test"),
  method=c("factor1", "factor", "numeric1", "numeric", "normal1", "normal"),
  stringsAsFactors = FALSE
)

# pro_ann = read.csv(paste0(path, "/Profile_Annotation_Testing.csv"), header = TRUE, row.names = 1, check.names = FALSE, stringsAsFactors = FALSE)
# varlist = c("Carcinogenicity", "Genotoxicity"); test <- NULL; 

phenotype_test <- function(pro_ann, varlist){
  
  test <- NULL;
  
  for(v in seq_along(varlist)){ 
    #v=1;
    checkvar <- pro_ann[, varlist[v]]
    
    if(any(!is.na(checkvar))){
      if(all(is.na(as.numeric(checkvar)))){
        if(length(unique(checkvar)) > 1){
          if(length(unique(checkvar)) <= 3 & all(toupper(c("Yes", "No")) %in% toupper(checkvar))){
            test <- c(test, SelectInputFunction(id=varlist[v], label=NULL, choices=c("1-sided Fisher test"), selected="1-sided Fisher test")) 
          }else{
            test <- c(test, SelectInputFunction(id=varlist[v], label=NULL, choices=c("2-sided Fisher test"), selected="2-sided Fisher test")) 
          }
        }else{
          test <- c(test, paste0("<p style='color:red;'>", varlist[v], " only has one factor level. Please choose another variable that has more than one factor/value.</p>"))
        }
      }else{
        if(length(unique(checkvar)) > 1){
          if(length(unique(checkvar)) == 2){
            test <- c(test, SelectInputFunction(id=varlist[v], label=NULL, choices=c("1-sided Fisher test"), selected="1-sided Fisher test")) 
          }else if(length(unique(checkvar)) > 2 & length(unique(checkvar)) <= 10){
            test <- c(test, SelectInputFunction(id=varlist[v], label=NULL, choices=c("2-sided Fisher test"), selected="2-sided Fisher test")) 
          }else{
            test <- c(test, SelectInputFunction(id=varlist[v], label=NULL, choices=c("1-sided Wilcox RS test", "2-sided Wilcox RS test", "1-sided t test", "2-sided t test"), selected=NULL)) 
          }
        }else{
          test <- c(test, paste0("<p style='color:red;'>", varlist[v], " only has one factor level. Please choose another variable that has more than one factor/value.</p>"))
        }
      }
    }else{
      test <- c(test, paste0("<p style='color:red;'>", varlist[v], " variable is empty. Please choose another variable that has more than one factor/value.</p>"))
    }
  }
  
  return(test)
}
  
# Add links to gene sets
get_enrTablelink <- function(geneset){
  geneset <- gsub(" ", "_", geneset)
  sprintf('<a href="http://software.broadinstitute.org/gsea/msigdb/cards/%s" style="text-decoration:none;" target="_blank">&#128269;</a>', geneset)
}

## Add links to genes
get_dgeTable_link <- function(genesymbol){
  sprintf('<a href="http://www.genecards.org/cgi-bin/carddisp.pl?gene=%s&keywords=%s" style="text-decoration: none;" target="_blank">&#128269;</a>', genesymbol, genesymbol)
}

# Function to format covariates string in formula
.formatCov <- function(covariates) if(is.null(covariates)) "" else paste0("+", paste(covariates, collapse = "+"))

# Function to generate differential signature
.signature_wrapper <- function(eSet, cohorts, mods, vehicle = NULL, covariates = NULL){
  
  # Remove vehicle from mods and make a data frame
  mods <- mods[names(mods) != "Vehicle"]
  mods <- data.frame(mods = as.character(mods), GROUP = names(mods), stringsAsFactors = F)
  modStats <- NULL
  
  if(length(unique(mods$mods)) > 1){
    
    # If replicates in data get unique cohorts
    if(is.null(cohorts)){
      cohorts <- "GROUP"
      pData(eSet)[,cohorts] <- colnames(exprs(eSet))
    }
    
    # Get unique groups
    gUnique <- unique(pData(eSet)[,cohorts])
    
    if(!is.null(vehicle)){
      gUnique <- gUnique[gUnique != vehicle]
    }
    
    # Subset data for mods
    eSub <- eSet[,pData(eSet)[,cohorts] %in% c(vehicle, mods$GROUP)]
    
    # Drop levels
    pData(eSub) <- droplevels(pData(eSub))
    
    # Create new variable in pData by merging with mods data.frame
    pData(eSub)$GROUP <- factor(pData(eSub)[,cohorts], levels = c(vehicle, mods$GROUP))
    pData(eSub)$Rownames <- rownames(pData(eSub))
    pD <- merge(pData(eSub), mods, all.x = TRUE)
    rownames(pD) <- pD$Rownames
    pD <- pD[colnames(eSub), , drop = F]
    
    # Add vehicle mod
    pD$mods[is.na(pD$mods)] <- "0"; pD$mods <- as.factor(pD$mods)
    
    # Add back to eSet
    pData(eSub) <- pD
    
    # Need to run different analysis if there are cohorts or not
    if(!is.null(cohorts)){
      
      # Create design matrix
      design <- model.matrix(as.formula(paste0("~ 0 +", "mods", .formatCov(covariates))), data = pData(eSub))
      colnames(design) <- sub("mods", "X", colnames(design))
      
      # Fit model
      fit <- lmFit(eSub, design)
      
      # Fit contrasts
      modFit <- lapply(paste0("X", unique(mods$mods)), function(x, design, fit){
        conString <- paste0(x, " - (", paste(colnames(design)[colnames(design) != x & colnames(design) %in% paste0("X", unique(mods$mods))], collapse = "+"), ")/", sum(colnames(design) != x & colnames(design) %in% paste0("X", unique(mods$mods))))
        contrasts <- makeContrasts(contrasts = conString, levels = design)
        contFit <- suppressWarnings(topTable(eBayes(contrasts.fit(fit, contrasts)), number = Inf, sort.by = "none"))
        contFit <- contFit[, c("logFC", "AveExpr", "t", "P.Value", "adj.P.Val", "B")]
        contFit$mod <- sub("X", "", x)
        return(contFit)
      }, design, fit)
      
    } else {
      
      design <- model.matrix(as.formula(paste0("~ 0 + ", "GROUP",  .formatCov(covariates))), data = pData(eSub))
      colnames(design) <- sub("GROUP", "X", colnames(design))
      
      # Fit model
      fit <- lmFit(eSub, design)
      
      # Create contrasts strings
      modsFull <- unique(pD[,c("GROUP", "mods")])
      modsTable <- table(modsFull$mods)
      cVec <- sapply(names(modsTable), function(x) paste0("(", paste(paste0("X", modsFull$GROUP[modsFull$mods==x], collapse = "+")), ")/", sum(modsFull$mods==x)))
      
      # Run each contrast
      modFit <- lapply(as.character(unique(mods$mods)), function(x, cVec, design, fit){
        conString <- paste0(cVec[x], " - (", paste(cVec[names(cVec) != x], collapse = "+"), ")/", sum(names(cVec) != x))
        contrasts <- makeContrasts(contrasts = conString, levels = design)
        contFit <- topTable(eBayes(contrasts.fit(fit, contrasts)), number = Inf, sort.by = "none")
        contFit <- contFit[, c("logFC", "AveExpr", "t", "P.Value", "adj.P.Val", "B")]
        contFit$mod <- x
        return(contFit)
      }, cVec, design, fit)
      
    }
    
    # Create vector of where to assign result
    if(is.null(vehicle)){
      if(length(modFit) == 2) {
        one2 <- c(1, 2)[as.numeric(modFit[[1]]$t < 0) + 1]
      } else {
        one2 <- sapply(seq(nrow(modFit[[1]])), function(row, modFit){
          which.max(sapply(seq(length(modFit)), function(g, modFit, row){
            modFit[[g]][row,"t"]
          }, modFit, row))
        }, modFit)
      }
    } else {
      one2 <- c(1, 2)[as.numeric(sapply(seq(nrow(modFit[[1]])), function(row, modFit){modFit[[1]]$P.Value[row] > modFit[[2]]$P.Value[row]}, modFit)) + 1]
    }
    
    # Create just one data.frame
    modStats <- as.data.frame(t(vapply(1:nrow(modFit[[1]]), function(row, one2, modFit){
      unlist(as.numeric(modFit[[one2[row]]][row,]))
    }, one2, modFit, FUN.VALUE = double(7))))
    colnames(modStats) <- colnames(modFit[[1]]); rownames(modStats) <- rownames(modFit[[1]])
    
    # Order by p-value
    modStats <- modStats[order(modStats$P.Value),]
    modStats$adj.P.Val <- p.adjust(modStats$P.Value, method = "BH")
    
  }
  
  # Save mods as character
  modStats$mod <- as.character(modStats$mod)
  
  # Change column names
  colnames(modStats) <- c("coef", "mean", "t", "pval", "fdr", "B", "mod")
  
  # Return
  return(modStats)
  
}

# Generete hyperenrichment results
hyperenrichmentClusters <- function(clusterRes, groupList, genesets, qthresh, cthresh, ntotal) {
  
  # Create list of gene signatures
  sigList <- lapply(seq(length(groupList)), function(mod, clusterRes, qthresh) {
    
    # Get subset of the clusters
    cSub <- clusterRes[clusterRes$mod == mod,]
    
    #print(cSub); print(min(cSub$fdr)); print(min(cSub$coef));
    
    # Get genes with sig pvalues
    #genes <- cSub$gene[cSub$fdr < qthresh & cSub$coef > cthresh]
    genes <- cSub$gene
    
    #print(genes)
    
    return(genes)
    
  }, clusterRes, qthresh)
  
  #print(sigList)
  
  names(sigList) <- names(groupList)
  
  # Run hyperenrichment
  gseList <- lapply(sigList, function(sig, genesets, ntotal){
    
    enrichFram <- NULL
    
    if(length(sig) > 0) {
      hits <- sapply(genesets, function(x,y) paste(intersect(x,y),collapse=','), sig)
      nhits <- sapply(genesets, function(x,y) length(intersect(x,y)), sig)
      ndrawn <- length(sig)
      ncats <- sapply(genesets,length)
      nleft <- ntotal-ncats
      pval <- phyper(q=nhits-1,m=ncats,n=nleft,k=ndrawn,lower.tail=F)
      enrichFram <- data.frame(
        category = names(genesets),
        pval = pval,
        nhits = nhits,
        ndrawn = ndrawn,
        ncats = ncats,
        ntot = ntotal,
        hits = hits,
        stringsAsFactors = F
      )
    }
    
    return(enrichFram)
    
  }, genesets, ntotal)
  
  #print(gseList)
  
  # Calculate and merge FDR values
  pValueDF <- data.frame(pval = unlist(lapply(gseList, function(y) y$pval)))
  pValueDF$fdr <- p.adjust(pValueDF$pval, method = "BH")
  pValueDF <- unique(pValueDF)
  
  #print(pValueDF);
  
  gseList <- lapply(gseList, function(y, pValueDF) {
    y <- merge(y, pValueDF)
    if(nrow(y) > 0){
      y <- y[, c("category", "pval", "fdr", "nhits", "ndrawn", "ncats", "ntot", "hits")]
    }
    return(y)
  }, pValueDF)
  
  #print(gseList)
  
  return(gseList)
  
}



