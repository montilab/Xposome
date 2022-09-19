
# populate inputs
observeEvent({
  input$portal_id
}, {
  
  
  ## Update the chemical list ####
  search_url = isolate({ parseQueryString(session$clientData$url_search) })
  
  if(toupper(names(search_url)) %in% "PAGE" && any(toupper(search_url$page) %in% toupper(projectlist$Portal))){
    fname = projectlist$Portal[which(toupper(projectlist$Portal) == toupper(search_url$page))]
  }else{
    fname = projectlist$Portal[1]
  }
  
  ## Update the chemical list ####
  chemical_list <- readRDS(paste0("www/data/", fname, "/Chemical_Annotation.RDS")) %>% 
    dplyr::transmute(Chemical_Id=as.character(Chemical_Id), CAS=as.character(CAS)) %>% 
    tidyr::drop_na() %>% 
    purrr::flatten_chr()
  
  session$sendCustomMessage("updateChemicalId", chemical_list)
  
  ## Update connectivity option ####
  session$sendCustomMessage("extractConnectivity", "update connectivity options")
  
  ## update portal title and description
  pos <- which(projectlist$Portal %in% input$portal_id)
  
  session$sendCustomMessage("portalTitle", HTML(paste0("<h4 class='highlight-title'><b class='color-title'>PROJECT:</b> ", projectlist$Portal[pos], "; <b class='color-title'>PROJECT-TITLE:</b> ", projectlist$Project[pos], "; <b class='color-title'>CELL-LINE:</b> ", projectlist$Cell_Line[pos], ";&nbsp;<a class='action-button' data-tooltip-toggle='tooltip' data-placement='top' title='Click to view project description' id='sign_in_btn' onclick='ToggleOperation(\"portal-description\")'><i class='fa fa-arrow-alt-circle-down'></i></a></h4>")))
  session$sendCustomMessage("portalDescription", HTML(paste0("<p>", projectlist$Description[pos], "</p>")))
  
  ## update landmark option
  landmark <- projectlist$Landmark_Gene[pos]
  
  # update the chemical explorer selection
  if(landmark){
    shinyjs::enable(id="landmark_de")
  }else{
    shinyjs::disable(id="landmark_de")
  }
  
  updateCheckboxInput(session, inputId="landmark_de", label = "Landmark only", value=landmark)
  
  ## getting genes set collection ####
  geneset_collection <- projectlist$GS_Collection[pos]
  geneset_collection_link <- projectlist$GS_Collection_Link[pos]
  
  ##Getting the helpext for different gene set enrichment
  if(geneset_collection %in% "Default"){
    
    ##Getting the gene set scores for diffrent gsva methods
    dsmap <- gs_default$geneset
    names(dsmap) <- gs_default$name
    
    ##Getting the helptext####
    helptext_geneset <- paste0(
      "<a class=\'tooltip-link\' href=\'https://www.gsea-msigdb.org/gsea/msigdb\'>MSigDB Hallmark Pathways (v7)</a><br>",
      "<a class=\'tooltip-link\' href=\'https://www.gsea-msigdb.org/gsea/msigdb\'>MSigDB C2 Reactome Pathways (v7)</a><br>",
      "<a class=\'tooltip-link\' href=\'https://signalingpathways.org\'>NURSA: Nuclear Receptor Signaling Atlas, consensome data for human</a><br>"
    )
    
  }else{
    
    ##Getting the gene set scores for different gsva methods
    dsmap <- paste0("gsscores_", geneset_collection)
    names(dsmap) <- paste0(geneset_collection)
    
    ##Getting the helptext####
    helptext_geneset <- paste0("<a class=\'tooltip-link\' href=\'", geneset_collection_link, "\'>", geneset_collection, "</a>")
    
  }

  ## update gene expression selection
  updateSelectInput(
    session,
    inputId = "gsname",
    choices = dsmap
  )
  
  ## update marker explorer selection
  updateSelectizeInput(
    session,
    inputId = "marker_gsname",
    choices = dsmap
  )
  
  ## update heatmap explorer selection
  updateSelectizeInput(
    session,
    inputId = "marker_gsname_hm",
    choices = dsmap
  )
  
  # update the helptext description
  session$sendCustomMessage("gsLabel", HTML("Gene set name", paste0('<button type="button" class="tooltip-txt" data-html="true" data-tooltip-toggle="tooltip" data-placement="top" title=\"', helptext_geneset, '\">?</button>')))
  
  # update the k2taxonomer options
  updateTextInput(session, inputId = "mstring", label=NULL, value = "")
  
  updateSelectizeInput(session, inputId = "mVal", label=NULL, choices = NULL)
  
  shinyjs::hide(id = "mVal")
  
  # update the compare multiple options
  groupSelectionValues(NULL)
  
  cluster_compare_results(NULL)
  
  dge_clicked_data(NULL)
  
  gse_clicked_data(NULL)
  
  shinyjs::disable(id = "compareGo")
  
  visNetworkProxy(session, shinyId = "dendroSelect") %>% 
    visOptions(
      autoResize = T,
      nodesIdSelection = FALSE,
      highlightNearest = list(enabled = TRUE, algorithm = "hierarchical", degree = 1E10)
    )
  
})


# Read in the profile data ####
profile_dat <- eventReactive({
  input$portal_id
}, {
  
  fname <- input$portal_id
  
  future_promise({
    readRDS(paste0("www/data/", fname, "/Profile_Annotation.RDS"))
  }) %...!% { return(NULL) }
  
})


# Read in the chemical data ####
chemical_dat <- eventReactive({
  input$portal_id
}, {
  
  fname <- input$portal_id
  
  future_promise({
    
    readRDS(paste0("www/data/", fname, "/Chemical_Annotation.RDS"))
    
  }) %...!% { return(NULL) }
  
})


# Read in the expression data ####
expression_dat <- eventReactive({
  input$portal_id
}, {
  
  fname <- input$portal_id
  
  future_promise({
    readRDS(paste0("www/data/", fname, "/Gene_Expression.RDS"))
  }) %...!% { return(NULL) }
  
})


# Read in the gs enrichment data ####
gs_enrichment_dat <- eventReactive({
  input$portal_id
}, {
  
  fname <- input$portal_id
  
  future_promise({
    readRDS(paste0("www/data/", fname, "/Gene_Set_Enrichment.RDS"))
  }) %...!% { return(NULL) }
  
})


# Read in the connectivity data ####
connectivity_dat <- eventReactive({
  input$portal_id
}, {
  
  fname <- input$portal_id
  
  future_promise({
    readRDS(paste0("www/data/", fname, "/Connectivity.RDS"))
  }) %...!% { return(NULL) }
  
})


## Read in K2 Taxonomer data ####
taxonomer_results <- eventReactive({
  input$portal_id
}, { 
  
  fname <- input$portal_id
  
  future_promise({
    
    ##Shiny Packages####
    require(K2Taxonomer)
    require(visNetwork)
    require(Biobase)
    require(BiocGenerics)
    
    # Read in K2 Taxonomer data ####
    K2summary <- readRDS(paste0("www/data/", fname, "/K2Taxonomer.RDS"))
    
  }) %...!% { return(NULL) }

})
  

## Obtain the visnetwork data #####
vNetOut_data <- reactive({
  
  fname = input$portal_id;
  
  future_promise({
    
    ##Shiny Packages####
    require(K2Taxonomer)
    require(visNetwork)
    require(Biobase)
    require(BiocGenerics)
    
    # Read in K2 Taxonomer data ####
    K2summary <- readRDS(paste0("www/data/", fname, "/K2Taxonomer.RDS"))
    
    # Format K2 results
    K2res <- K2results(K2summary)
    
    # Create interactive dendrogram ####
    vNetOut <- K2visNetwork(K2summary)
    
    # If too many observations in terminal labels, unlabel them
    if(max(lengths(regmatches(vNetOut$x$nodes$label, gregexpr("\n", vNetOut$x$nodes$label)))) > 20){
      # Fix font size
      vNetOut$x$nodes$font.size <- 25
      vNetOut$x$nodes$font.size[vNetOut$x$nodes$shape == "box"] <- 0
      
      # Change shape
      vNetOut$x$nodes$shape[vNetOut$x$nodes$shape == "box"] <- "square"
    }
    
    # Get the mod test table
    if(!is.null(K2res[[1]]$modTests)){
      
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
      
      # Style qvalues with different colors
      vNetOut_qvalues <- vNetOut
      
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
      
      list(
        vNetOut = vNetOut,
        vNetOut_qvalues = vNetOut_qvalues,
        K2modTestFram = K2modTestFram
      )
      
    }else{
      
      list(
        vNetOut = vNetOut,
        vNetOut_qvalues = NULL,
        K2modTestFram = NULL
      )
      
    }
    
  }) %...!% { return(NULL) }
  
}) %>% bindCache(input$portal_id, "-vNetout-K2Taxonomer") %>% 
  bindEvent(input$portal_id)


## Obtain the dge and gse data #####
K2Taxonomer_Tables <- reactive({
  
  fname = input$portal_id; 
  
  ## Exact the dge and gse tables
  future_promise({
    
    ##Shiny Packages####
    require(K2Taxonomer)
    require(Biobase)
    require(BiocGenerics)
    
    # Read in K2 Taxonomer data ####
    K2summary <- readRDS(paste0("www/data/", fname, "/K2Taxonomer.RDS"))
    
    # Get differential gene expression results
    dgeTable <- getDGETable(K2summary)
    colnames(dgeTable) <- c('gene', 'coef', 'mean', 't', 'pval', 'fdr', 'B', 'mod', 'node', 'direction')
    
    ## Get the gene link
    dgeTable$Plot <- paste0("<label for='PlotRow", seq(nrow(dgeTable)), "'>&#128202;&nbsp;&#9992;</label>")
    dgeTable$Link <- sapply(as.character(dgeTable$gene), get_dgeTable_link)
    
    # Reorder columns
    dgeTable <- dgeTable[,c("gene", "node", "mod", "direction", "pval", "fdr", "coef", "mean", "Plot", "Link")]
    
    # Rename columns
    colnames(dgeTable) <- c("Gene", "Node", "Group", "Direction", "P Value", "FDR", "Diff", "Mean", "Plot", "Link")
    
    #print(head(dgeTable))
    
    # Get gene set enrichment results
    enrTable <- getEnrichmentTable(K2summary)
    colnames(enrTable) <- c('category', 'node', 'edge', 'direction', 'pval_hyper', 'fdr_hyper', 'nhits', 'ndrawn', 'ncats', 'ntot', 'pval_limma', 'fdr_limma', 'coef', 'mean', 't', 'B', 'hits')
    
    # Add links to gene sets
    enrTable$Plot <- paste0("<label for='PlotRow", seq(nrow(enrTable)), "'>&#128202;&nbsp;&#9992;</label>")
    enrTable$Link <- sapply(as.character(enrTable$category), get_enrTablelink)
    
    # Reorder columns
    enrTable <- enrTable[,c("category", "node", "edge", "direction", "pval_hyper", "fdr_hyper", "nhits", "ndrawn", "ncats", "pval_limma", "fdr_limma", "coef", "mean", "Plot", "Link")]
    
    # Rename columns
    colnames(enrTable) <- c("Gene Set", "Node", "Group", "Direction", "P Value_Hyper", "FDR_Hyper", "N_Overlap", "N_Sig. Genes", "N_Gene Set", "P Value_ssGSEA", "FDR_ssGSEA", "Diff_ssGSEA", "Mean_ssGSEA", "Plot", "Link")
    
    #print(head(enrTable))
    
    list(
      dgeTable = dgeTable,
      enrTable = enrTable
    )
    
  }) %...!% { return(NULL) }
  
}) %>% bindCache(input$portal_id, "-K2Taxonomer-tables") %>% 
  bindEvent(input$portal_id)
