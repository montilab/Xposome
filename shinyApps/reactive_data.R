
# Read in the profile data ####
profile_dat <- reactive({
  
  req(selected_portal())
  
  fname <- selected_portal()
  
  # Retrieve list of all PortalDataset entities in hive matching portal name
  datasets <- listEntities("PortalDataset", portal=fname)
  
  # Sort by timestamp and extract most recent dataset to convenience object
  datasets <- datasets[order(sapply(datasets, slot, "timestamp"))]
  dataset <- datasets[[length(datasets)]]
  
  future({
    getWorkFileAsObject(hiveWorkFileID(dataset@ProfileAnnotationRDS))
  }) %...!% { return(NULL) }
  
})

# Read in the chemical data ####
chemical_dat <- reactive({
  
  req(selected_portal())
  
  fname <- selected_portal()
  
  # Retrieve list of all PortalDataset entities in hive matching portal name
  datasets <- listEntities("PortalDataset", portal=fname)
  
  # Sort by timestamp and extract most recent dataset to convenience object
  datasets <- datasets[order(sapply(datasets, slot, "timestamp"))]
  dataset <- datasets[[length(datasets)]]
  
  future({
    getWorkFileAsObject(hiveWorkFileID(dataset@ChemicalAnnotationRDS))
  }) %...!% { return(NULL) }
  
})

# Read in the expression data ####
expression_dat <- reactive({
  
  req(selected_portal())
  
  fname <- selected_portal();
  
  # Retrieve list of all PortalDataset entities in hive matching portal name
  datasets <- listEntities("PortalDataset", portal=fname)
  
  # Sort by timestamp and extract most recent dataset to convenience object
  datasets <- datasets[order(sapply(datasets, slot, "timestamp"))]
  dataset <- datasets[[length(datasets)]]
  
  future({
    getWorkFileAsObject(hiveWorkFileID(dataset@GeneExpressionRDS))
  }) %...!% { return(NULL) }
  
})

# Read in the connectivity data ####
connectivity_dat <- reactive({
  
  req(selected_portal())
  
  fname <- selected_portal()
  
  # Retrieve list of all PortalDataset entities in hive matching portal name
  datasets <- listEntities("PortalDataset", portal=fname)
  
  # Sort by timestamp and extract most recent dataset to convenience object
  datasets <- datasets[order(sapply(datasets, slot, "timestamp"))]
  dataset <- datasets[[length(datasets)]]
  
  future({
    getWorkFileAsObject(hiveWorkFileID(dataset@ConnectivityRDS))
  }) %...!% { return(NULL) }
  
})

# Read in the gs enrichment data ####
gs_enrichment_dat <- reactive({
  
  req(selected_portal())
  
  fname <- selected_portal()
  
  # Retrieve list of all PortalDataset entities in hive matching portal name
  datasets <- listEntities("PortalDataset", portal=fname)
  
  # Sort by timestamp and extract most recent dataset to convenience object
  datasets <- datasets[order(sapply(datasets, slot, "timestamp"))]
  dataset <- datasets[[length(datasets)]]
  
  future({
    getWorkFileAsObject(hiveWorkFileID(dataset@GeneSetEnrichmentRDS))
  }) %...!% { return(NULL) }
  
})

## Read in K2 Taxonomer data ####
taxonomer_results <- reactive({
  
  req(selected_portal())
  
  fname <- selected_portal()
  
  # Retrieve list of all PortalDataset entities in hive matching portal name
  datasets <- listEntities("PortalDataset", portal=fname)
  
  # Sort by timestamp and extract most recent dataset to convenience object
  datasets <- datasets[order(sapply(datasets, slot, "timestamp"))]
  dataset <- datasets[[length(datasets)]]
  
  future({
    
    ##Shiny Packages####
    require(K2Taxonomer)
    require(visNetwork)
    require(Biobase)
    require(BiocGenerics)

    # Read in K2 Taxonomer data ####
    K2summary <- getWorkFileAsObject(
      hiveWorkFileID(dataset@K2TaxonomerResultsRDS)
    )
    
    # Parse results
    info <- K2info(K2summary)  # Profile information
    infoMat <- as.matrix(info) # Format information
    meta <- K2meta(K2summary) # Get meta data
    #print(head(info))
    
    #Create variable options
    varOptions <- sort(colnames(info))
    names(varOptions) <- varOptions
    
    if(!is.null(meta$cohorts)) {
      varOptions <- varOptions[varOptions != meta$cohorts]
    } else {
      varOptions <- varOptions[varOptions != "sampleID"]
    }
    
    K2res <- K2results(K2summary) # Format K2 results
    #print(head(K2res))
    
    # Get IDs of each group ####
    obsMap <- unlist(lapply(K2res, function(x) x$obs), recursive = F)
    
    dataMatrix <- K2data(K2summary) # Format dataMatrix
    genesets <- K2genesets(K2summary) # Get geneset lists
    gene2Pathway <- K2gene2Pathway(K2summary) # Get gene2pathway matching
    eSet <- K2eSet(K2summary) # Get expression set
    gSet <- K2gSet(K2summary) # Get gene set projection expression set
    
    # Create static dendrogram
    K2dendrogram <- K2dendro(K2summary)
    
    ## Get sample order ####
    labs <- get_leaves_attr(K2dendrogram, "label")
    
    # Create interactive dendrogram ####
    vNetOut <- K2visNetwork(K2summary)
    #print(vNetOut)
    
    # If too many observations in terminal labels, unlabel them
    if (max(lengths(regmatches(vNetOut$x$nodes$label, gregexpr("\n", vNetOut$x$nodes$label)))) > 20 ) {
      
      # Fix font size
      vNetOut$x$nodes$font.size <- 25
      vNetOut$x$nodes$font.size[vNetOut$x$nodes$shape == "box"] <- 0
      
      # Change shape
      vNetOut$x$nodes$shape[vNetOut$x$nodes$shape == "box"] <- "square"
    }
    
    # Get the mod test table
    if(!is.null(K2res[[1]]$modTests)){
      
      # Format table
      K2modTestList <- lapply(K2res, function(x) {
        modTests <- x$modTests
        names(modTests) <- c("1", "2")
        do.call(rbind, modTests)
      })
      
      names(K2modTestList) <- names(K2res)
      K2modTestFram <- do.call(rbind, K2modTestList)[,c("value", "pval", "fdr")]
      
      # Get parent node
      K2modTestFram$Parent <- regmatches(rownames(K2modTestFram), regexpr("^[^.]+", rownames(K2modTestFram)))
      
      # Get direction to chile
      K2modTestFram$Direction <- as.character(gsub("[[:alpha:]]+[.]|[.][[:digit:]]+$", "", rownames(K2modTestFram)))
      
      # Get child
      K2modTestFram$Child <- apply(K2modTestFram[, c("Parent", "Direction")], 1, function(x){
        vSub <- vNetOut$x$edges[vNetOut$x$edges$from == x[1],]
        vSub$to[as.numeric(x[2])]
      })
      
      # Get split
      K2modTestFram$Split <- paste0(K2modTestFram$Parent, K2modTestFram$Direction)
      
      # Format p-values
      K2modTestFram <- K2modTestFram[!is.na(K2modTestFram$pval),]
      K2modTestFram <- K2modTestFram[order(K2modTestFram$pval),]
      
      # Get unrenamed variables of mod test for qvalues formatting
      mvTabSub <- K2modTestFram
      
      K2modTestFram <- K2modTestFram[, c("Split", "Child", "value", "pval", "fdr")]
      colnames(K2modTestFram) <- c("Split", "Node", "Variable", "P Value", "Q Value")
      
      K2modTestFram$`P Value` <- signif(K2modTestFram$`P Value`, 2)
      K2modTestFram$`Q Value` <- signif(K2modTestFram$`Q Value`, 2)
      
      # Color breaks for q values
      breaks <- c(1, 0.25, 0.1, 0.05, 0.01, 0.001, 0)
      breakColors <- brewer.pal(7, "Greens")
      mvTabSub$color <- sapply(mvTabSub$pval, function(pval) {
        breakColors[which.min(breaks >= pval)]
      })
      
      # Size breaks
      breaks <- c(1, 0.1, 0.05, 0.01, 0.001, 0.0001, 0)
      breakSize <- seq(length(breaks)) * 7
      mvTabSub$width <- sapply(mvTabSub$pval, function(pval) {
        breakSize[which.min(breaks >= pval)]
      })
      
      # Add 2 values
      vNetOut_qvalues <- vNetOut
      #print("hello6")
      
      # Change width of edges
      mEdge <- mvTabSub[, c("Parent", "Child", "width")][!duplicated(mvTabSub[, c("Parent", "Child")]),]
      colnames(mEdge) <- c("from", "to", "width")
      edgeFram <- merge(vNetOut_qvalues$x$edges, mEdge, all.x = TRUE, sort = FALSE)
      edgeFram$width[is.na(edgeFram$width)] <- 1
      edgeFram$color.inherit <- 'to'
      vNetOut_qvalues$x$edges <- edgeFram
      
      # Change color of edges
      mNode <- mvTabSub[, c("Child", "color")][!duplicated(mvTabSub[, c("Child")]),]
      colnames(mNode) <- c("id", "color.border")
      nodeFram <- left_join(vNetOut_qvalues$x$nodes, mNode)
      nodeFram$color.border[is.na(nodeFram$color.border)] <- brewer.pal(6, "Greens")[1]
      nodeFram$color.background <- nodeFram$color.border
      nodeFram$color.highlight <- 'red'
      vNetOut_qvalues$x$nodes <- nodeFram
      
    }else{
      
      K2modTestFram <- NULL
      
    }
    
    # Get differential gene expression results
    dgeTable <- getDGETable(K2summary)
    #print("hello7")
    
    ## Get the gene link
    dgeTable$Plot <- paste0("<label for='PlotRow", seq(nrow(dgeTable)), "'>&#128202;</label>")
    dgeTable$Link <- sapply(as.character(dgeTable$gene), get_dgeTable_link)
    
    # Reorder columns
    dgeTable <- dgeTable[,c("gene", "split", "mod", "direction", "pval", "fdr", "coef", "mean", "Plot", "Link")]
    
    # Format numbers to fit in table
    for (i in c("pval", "fdr")) {
      if(i %in% colnames(dgeTable)){
        dgeTable[,i] <- signif(dgeTable[,i], digits = 2)
      }
    }
    
    # Format numbers to fit in table
    for (i in c("coef", "mean")) {
      if(i %in% colnames(dgeTable)){
        dgeTable[,i] <- round(dgeTable[,i], digits = 2)
      }
    }
    
    # Rename columns
    colnames(dgeTable) <- c("Gene", "Node", "Group", "Direction", "P Value", "FDR", "Diff", "Mean", "Plot", "Link")
    
    # Get gene set enrichment results
    enrTable <- getEnrichmentTable(K2summary)
    
    # Remove unnecessary columns
    enrTable <- enrTable[, !colnames(enrTable) %in% c("B", "ntot", "t")]
    
    # Add links to gene sets
    enrTable$Plot <- paste0("<label for='PlotRow", seq(nrow(enrTable)), "'>&#128202;</label>")
    enrTable$Link <- sapply(as.character(enrTable$category), get_enrTablelink)
    
    # Format numbers to fit in table
    for (i in c("pval_hyper", "fdr_hyper", "pval_limma", "fdr_limma")) {
      if(i %in% colnames(enrTable)){
        enrTable[,i] <- signif(enrTable[,i], digits = 2)
      }
    }
    
    # Format numbers to fit in table
    for (i in c("coef", "mean")) {
      if(i %in% colnames(enrTable)){
        enrTable[,i] <- round(enrTable[,i], digits = 2)
      }
    }
    
    colnames(enrTable) <- c("Gene Set", "Node", "Group", "Direction", "P Value_Hyper", "FDR_Hyper", "N_Overlap", "N_Sig. Genes", "N_Gene Set", "P Value_ssGSEA", "FDR_ssGSEA", "Diff_ssGSEA", "Mean_ssGSEA", "Hits", "Plot", "Link")
    #print("hello8")
    
    # return a list of objects
    results <- list(
      info=info,
      infoMat=infoMat,
      meta=meta,
      K2res=K2res,
      dataMatrix=dataMatrix,
      genesets=genesets,
      gene2Pathway=gene2Pathway,
      eSet=eSet,
      gSet=gSet,
      dgeTable=dgeTable,
      enrTable=enrTable,
      K2dendrogram=K2dendrogram,
      vNetOut=vNetOut,
      vNetOut_qvalues=vNetOut_qvalues,
      K2modTestFram=K2modTestFram,
      options=list(
        varOptions=varOptions,
        labs=labs,
        obsMap=obsMap
      )
    )
    
    return(results)
    
  }) %...!% { return(NULL) }
  
})

## Create reactive values####
annot_var <- reactive({
  promise_all(pro_ann=profile_dat(), eset=expression_dat()) %...>% with({
    if(all(colnames(eset) %in% pro_ann$Sig_Id)){
      return("Sig_Id")
    }else{
      return("Chemical_Id")
    }
  })
})



