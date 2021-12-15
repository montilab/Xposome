
# Create reactive values ####
groupSelectionValues <- reactiveVal()
cluster_compare_results <- reactiveVal()
dge_clicked_data <- reactiveVal()
gse_clicked_data <- reactiveVal()

#############################################
#
# DENDROGRAM ####
#
#############################################

# Generate Dendrogram ####
output$dendroSelect <- renderVisNetwork({
  
  vNetOut_data() %...>% (function(vNetOut_data){
    
    vNetOut <- vNetOut_data %>% extract2("vNetOut");
    
    vNetOut %>%
      visOptions(
        autoResize = T,
        nodesIdSelection = FALSE,
        height = "100%",
        highlightNearest = list(enabled = TRUE, algorithm = "hierarchical", degree = 1E10)
      ) %>%
      visNodes(
        font = list(size = 50), 
        size = 40,
        color = list(
          background = "white",
          border = "#2B7CE9",
          highlight = "red"
        )
      ) %>%
      visEdges(width = 11, smooth = T, color = list(inherit = 'to')) %>%
      visPhysics(hierarchicalRepulsion = list(nodeDistance = 200)) %>%
      visHierarchicalLayout(direction = "LR", levelSeparation = 300) %>%
      visInteraction(multiselect = TRUE) %>%
      visEvents(
        type = "on",
        click = "function(nodes) { Shiny.onInputChange('Held', nodes.nodes); }",
        deselectNode = "function(nodes) { Shiny.onInputChange('Held', null); }"
      )
    
  }) %...!% { return(NULL) }
  
})


# Allow multiple selections ####
observeEvent({
  input$Held
}, {

  # Get selection
  nodeMulti = input$Held; 

  if(length(nodeMulti) == 0){
    
    groupSelectionValues(NULL)
    shinyjs::disable(id = "compareGo")
    
  }else{
    
    # If unclicked initialize or reset groupList
    promise_all(K2summary=taxonomer_results(), vNetOut_data=vNetOut_data()) %...>%
      with({
        
        ##Shiny Packages####
        require(K2Taxonomer)
        require(Biobase)
        require(BiocGenerics)
        
        vNetOut <- vNetOut_data %>% extract2("vNetOut");
        
        # Get K2 results
        K2res <- K2results(K2summary)
        
        # Get IDs of each group ####
        obsMap <- unlist(lapply(K2res, function(x) x$obs), recursive = F)
        
        # Create a group list
        groupList <- list(); nodeSel <- NULL
        
        for(i in 1:length(nodeMulti)){
          
          # Get current value
          curVal <- nodeMulti[i]
          
          if(curVal %in% names(K2res)) {
            obs <- unlist(K2res[[curVal]]$obs)
          } else {
            obs <- strsplit(as.character(vNetOut$x$nodes$label[vNetOut$x$nodes$id == curVal]), "\n")[[1]]
          }
          
          obsUsed <- groupList %>% purrr::flatten_chr()
          obs <- obs[!obs %in% obsUsed]
          
          # Get mapped group
          if(length(obs) > 0) {
            
            gMap <- names(obsMap)[which.max(unlist(lapply(
              obsMap,
              function(x) {
                mean(x %in% obs)
              }
            )))]
            
            if(length(gMap) > 0) {
              groupList[[gMap]] <- obs
              nodeSel <- c(nodeSel, curVal)
            }
            
          }
          
        }
        
        return(groupList)
        
      }) %...>% groupSelectionValues()
    
  }
    
}, ignoreNULL=FALSE)


#############################################
#
# SELECTIONS ####
#
#############################################

# Render group selection ####
output$groupSel <- DT::renderDataTable({

  req(groupSelectionValues()) 
  
  groupList <- groupSelectionValues();
  
  #print(groupList)
  
  if(length(groupList) > 1){
    shinyjs::enable(id = "compareGo")
  }else{
    shinyjs::disable(id = "compareGo")
  }
  
  group_df <- names(groupList) %>% 
    map_dfr(
      function(gName){
        pName <- paste0("<b>Node ", substr(gName, 1, nchar(gName) - 1),
                        ", Group ", substr(gName, nchar(gName), nchar(gName)),
                        " (", gName, ")")
        Obs <- paste(groupList[[gName]], collapse = "&ensp;&ensp;")
        paste0(pName, "</b>:&ensp;", Obs)
        data.frame(Selection=paste0(pName, "</b>:&ensp;", Obs))
      }
    )
  
  return(group_df)
  
}, escape = FALSE, extensions = 'Buttons', server = TRUE, colnames = "SELECTION:", rownames = FALSE, selection = "none", options = list(dom='T',   columnDefs = list(list(className = 'dt-left', targets = "_all"))))


# # Reset analysis after compareReset is selected ####
observeEvent({
  input$compareReset
}, {

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


# Run analysis after compare button is selected ####
observeEvent({
  input$compareGo
}, {

  # Reset compared results
  cluster_compare_results(NULL)
  
  # Get the selected groupList
  groupList = groupSelectionValues(); nodeMulti = names(groupList); 

  taxonomer_results() %...>% (function(K2summary){

    ##Create new progress bar
    withProgress(message = "Comparing Selected Groups:", value = 0, {

      ##Shiny Packages####
      require(K2Taxonomer)
      require(Biobase)
      require(BiocGenerics)
      
      eSet <- K2eSet(K2summary) # Get expression set
      gSet <- K2gSet(K2summary) # Get gene set projection expression set
      genesets <- K2genesets(K2summary) # Get geneset lists
      meta <- K2meta(K2summary) # Get meta data
      
      # Create data.frame of mods
      mods <- as.factor(unlist(lapply(seq(length(groupList)), function(x) rep(x, length(groupList[[x]])))))
      names(mods) <- unlist(groupList)

      #print(mods)

      # Genereate gene expression results
      incProgress(1/10, detail = "Differential Analysis")

      clusterRes <- .signature_wrapper(
        eSet=eSet,
        cohorts=meta$cohorts,
        mods=mods,
        vehicle=NULL,
        covariates=meta$covariates
      )

      #print(head(clusterRes)); print("hello1");

      # Add NodeGroup and gene column
      clusterRes$NodeGroup <- names(groupList)[as.numeric(clusterRes$mod)]
      clusterRes$gene <- rownames(clusterRes)

      # Generete hyperenrichment results
      incProgress(1/4, detail = "Pathway Hyperenrichment")

      #print(groupList); print(meta$qthresh); print(meta$cthresh); print(meta$ntotal);

      hyperEnrRes <- hyperenrichmentClusters(
        clusterRes=clusterRes,
        groupList=groupList,
        genesets=genesets,
        qthresh=meta$qthresh,
        cthresh=meta$cthresh,
        ntotal=meta$ntotal
      )

      #print(head(hyperEnrRes)); print("hello2");

      # Add NodeGroup ID
      hyperEnrRes <- do.call(rbind, lapply(names(hyperEnrRes), function(x) {
        # Get GSE tables
        HYPERtab <- hyperEnrRes[[x]]
        if(nrow(HYPERtab) > 0) {
          HYPERtab$NodeGroup <- x
        }
        return(HYPERtab)
      }))

      # Add type of test
      colnames(hyperEnrRes)[colnames(hyperEnrRes) %in% c("pval", "fdr")] <- paste(colnames(hyperEnrRes)[colnames(hyperEnrRes) %in% c("pval", "fdr")], "hyper", sep = "_")

      # Generete hyperenrichment results
      incProgress(1/4, detail = "Pathway Differential Enrichment")

      ssEnrRes <- .signature_wrapper(
        eSet=gSet,
        cohorts = meta$cohorts,
        mods=mods,
        vehicle = NULL,
        covariates = meta$covariates
      )

      #print(head(ssEnrRes)); print("hello3");

      # Add NodeGroup and category
      ssEnrRes$NodeGroup <- names(groupList)[as.numeric(ssEnrRes$mod)]
      ssEnrRes$category <- rownames(ssEnrRes)

      # Add type of test
      colnames(ssEnrRes)[colnames(ssEnrRes) %in% c("pval", "fdr")] <- paste(colnames(ssEnrRes)[colnames(ssEnrRes) %in% c("pval", "fdr")], "limma", sep = "_")

      # Format hyperenrichment table
      incProgress(1/3, detail = "Formatting Results")

      # Merge the two and sort by hyper p-value
      EnrRes <- merge(hyperEnrRes, ssEnrRes, all=TRUE)

      ## Order by pvalue
      EnrRes <- EnrRes[order(EnrRes$pval_hyper),]

      # Sort columns
      EnrRes <- EnrRes[, c("category", "NodeGroup", "pval_hyper", "fdr_hyper", "nhits", "ndrawn", "ncats", "pval_limma", "fdr_limma", "coef", "mean", "hits")]

      # Add links to gene sets
      EnrRes$Plot <- paste0("<label for='PlotRow", seq(nrow(EnrRes)), "'>&#128202;&nbsp;&#9992;</label>")
      EnrRes$Link <- sapply(as.character(EnrRes$category), get_enrTablelink)

      # Format numbers to fit in table
      for (i in c("pval_hyper", "fdr_hyper", "pval_limma", "fdr_limma")) {
        EnrRes[,i] <- signif(EnrRes[,i], digits = 2)
      }

      # Format numbers to fit in table
      for (i in c("coef", "mean")) {
        EnrRes[,i] <- round(EnrRes[,i], digits = 2)
      }

      # Select columns
      EnrRes <- EnrRes[,c("category", "NodeGroup", "pval_hyper", "fdr_hyper", "nhits", "ndrawn", "ncats", "pval_limma", "fdr_limma", "coef", "mean", "Plot", "Link")]
      
      # Change column names
      colnames(EnrRes)  <- c("Gene Set", "NodeGroup", "P Value_Hyper", "FDR_Hyper", "N_Overlap", "N_Sig. Genes", "N_Gene Set", "P Value_ssGSEA", "FDR_ssGSEA", "Diff_ssGSEA", "Mean_ssGSEA", "Plot", "Link")

      ## Order by pvalue
      clusterRes <- clusterRes[order(clusterRes$pval),]

      ## Add links to genes
      clusterRes$Plot <- paste0("<label for='PlotRow", seq(nrow(clusterRes)), "'>&#128202;&nbsp;&#9992;</label>")
      clusterRes$Link <- sapply(as.character(clusterRes$gene), get_dgeTable_link)

      # Select columns
      clusterRes <- clusterRes[, c("gene", "NodeGroup", "pval", "fdr",  "coef", "mean", "Plot", "Link")]

      # Format numbers to fit in table
      for (i in c("pval", "fdr")) {
        clusterRes[,i] <- signif(clusterRes[,i], digits = 2)
      }
      # Format numbers to fit in table
      for (i in c("coef", "mean")) {
        clusterRes[,i] <- round(clusterRes[,i], digits = 2)
      }

      # Rename columns
      colnames(clusterRes) <- c("Gene", "NodeGroup", "P Value", "FDR", "Diff", "Mean", "Plot", "Link")

      list(
        gse_dat = EnrRes,
        dge_dat = clusterRes
      )
      
    })

  }) %...>% cluster_compare_results()

})


observeEvent({
  cluster_compare_results()
}, {
  
  req(cluster_compare_results())
  
  # Get group list
  groupList = groupSelectionValues();
  
  # Initializze gse clicked data
  gse_clicked_data(
    list(
      dge_click = FALSE,
      dge_plot_df = NULL,
      dge_plot_gene = NULL,
      dge_plot_colMan = NULL,
      gsetSelDGE = NULL,
      nodeSelDGE = names(groupList)
    )
  )
  
  # Initializze dge clicked data
  dge_clicked_data(
    list(
      gse_click = FALSE,
      gse_plot_df = NULL,
      cgse_plot_geneSet = NULL,
      gse_plot_colMan = NULL,
      geneSelHE = NULL,
      nodeSelHE = names(groupList)
    )
  )
  
})


###############################################
#
# DIFFERENTIAL ANALYSIS RESULTS  ####
#
#############################################

# Render geneTable from multiple group analysis
output$DGEmulti <- DT::renderDataTable({
  
  # Get differential gene expression results
  DGETABLE = cluster_compare_results() %>% extract2("dge_dat")
  
  if(length(DGETABLE) == 0){
    
    DGETABLE <- data.frame(Warning="\n No nodes selected \n")
    colnames(DGETABLE) <- c("Warning Message")
    
    DGETABLE %>% datatable(
      rownames = FALSE,
      extensions = 'Buttons',
      selection = "none",
      options = list(
        dom = 'T',
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      )
    )
    
  }else{
    
    # Get the node from enrichment table
    geneSelHE = gse_clicked_data() %>% extract("geneSelHE") %>% unlist()
    nodeSelHE = gse_clicked_data() %>% extract("nodeSelHE") %>% unlist()

    # Get exact match for nodeID
    if (length(geneSelHE) > 0 && nchar(geneSelHE) > 0) geneSelHE <- paste0(paste0("^", geneSelHE, "$"), collapse = "|") else geneSelHE <- NULL
    if (length(nodeSelHE) > 0 && nchar(nodeSelHE) > 0) nodeSelHE <- paste0(paste0("^", nodeSelHE, "$"), collapse = "|") else nodeSelHE <- NULL

    # Create data table obect
    DGETABLE %>%
      dplyr::arrange(NodeGroup, Gene) %>% 
      data.table.round() %>%
      datatable(
        rownames = FALSE,
        escape = FALSE,
        extensions = 'Buttons',
        selection = 'single',
        filter = list(position = 'top', clear = TRUE),
        options = list(
          columnDefs = list(
            list(className = 'dt-left', targets = 0),
            list(className = 'dt-center', targets = 1:(ncol(DGETABLE)-1))
          ),
          search = list(regex = TRUE),
          searchCols = list(
            list(search = geneSelHE),
            list(search = nodeSelHE),
            rep(NULL, ncol(DGETABLE)-2)
          ),
          scrollX = TRUE,
          scrollY = '400px',
          dom = 'Brtp',
          paging = TRUE,
          pageLength = 50,
          buttons = list(
            list(
              extend = "collection",
              text = 'Help',
              action = DT::JS(
                paste0(
                  "function ( e, dt, node, config ) {",
                  "Shiny.setInputValue('geneHelpMulti', true, {priority: 'event'});",
                  "}"
                )
              )
            ),
            list(
              extend = "collection",
              text = 'Download All Results',
              action = DT::JS(
                paste0(
                  "function ( e, dt, node, config ) {",
                  "Shiny.setInputValue('geneDLMulti', true, {priority: 'event'});",
                  "}"
                )
              )
            )
          )
        )
      ) %>%
      formatStyle(c("Gene", "NodeGroup", "Mean"), `border-right` = "solid 2px")
    
  }
})


## Pop-up for help ####
observeEvent(input$geneHelpMulti, {
  
  shinyBS::toggleModal(session, modalId = "geneMultiHelpShow", toggle="open")
  
})


## Pop-up for CSV file ####
observeEvent(input$geneDLMulti, {
  
  shinyBS::toggleModal(session, modalId="geneMultiTabDL", toggle="open")
  
})


## Download CSV File ####
output$downloadGeneCSVMulti <- downloadHandler(
  
  filename = function() {
    paste0(input$portal_id, "-gene-expression-multi-compare-results.csv")
  },
  
  content = function(file) {
    write.csv(cluster_compare_results() %>% extract2("dge_dat"), file, row.names=F)
  }
  
)


# Observe when a row is selected####
observeEvent(input$DGEmulti_cell_clicked, {

  req(input$DGEmulti_cell_clicked$value)

  # Get the cell clicked value
  clickedVal = input$DGEmulti_cell_clicked$value;
  
  # Get group list
  groupList = groupSelectionValues();
  
  # Get differential gene expression results
  dgeTable <- cluster_compare_results() %>% extract2("dge_dat");
  
  # Get Value
  dgeVal <- gsub("<label for='|'>&#9992;</label>|'>&#128202;</label>|'>&#128202;&nbsp;&#9992;</label>", "", as.character(clickedVal))
  
  # Check that a link was clicked
  if (grepl("PlotRow|SendRow", dgeVal)) {

    rowNum <- as.numeric(sub("PlotRow|SendRow", "", dgeVal))

    # If plotting send the node to plot, otherwise global
    taxonomer_results() %...>% (function(K2summary){

      ##Shiny Packages####
      require(K2Taxonomer)
      require(Biobase)
      require(BiocGenerics)
      
      gene2Pathway <- K2gene2Pathway(K2summary) # Get gene2pathway matching
      eSet <- K2eSet(K2summary) # Get expression set
      meta <- K2meta(K2summary) # Get meta data
      
      GENERowMulti <- dgeTable[rowNum, , drop = FALSE]
      geneSelDGE <- GENERowMulti[, "Gene"]
      nodeSelDGE <- GENERowMulti[, "NodeGroup"]

      if (geneSelDGE %in% names(gene2Pathway)) {
        gsetSelDGE <- gene2Pathway[[geneSelDGE]]
      } else {
        gsetSelDGE <- "NO GENE SETS FOUND."
      }

      cohorts = meta$cohorts;
      vehicle = meta$vehicle;

      if(geneSelDGE %in% rownames(exprs(eSet))){

        # Format group names
        if(is.null(cohorts)){
          nams <- colnames(exprs(eSet))
        } else {
          nams <- pData(eSet)[,cohorts]
        }
        nams[nams == vehicle] <- "Vehicle"

        # Create data.frame of expression values
        e <- Biobase::exprs(eSet)[geneSelDGE,]
        df <- data.frame(e = e, ch = nams, stringsAsFactors = F)

        # Get clusters
        obs <- unlist(groupList)

        # Subset for obs in groups
        df <- df[df$ch %in% c(obs, "Vehicle"),]
        df$group <- "Vehicle"
        for (i in names(groupList)) {
          df$group[df$ch %in% groupList[[i]]] <- i
        }

        # Get per Observation mean
        dfMeans <- df %>%
          group_by(ch) %>%
          summarise(me = mean(e))
        dfMeans$ch <- as.character(dfMeans$ch)
        dfMeans <- dfMeans[order(dfMeans$me, decreasing = FALSE),]

        # Sort levels by mean expression
        df$ch <- factor(df$ch, levels = dfMeans$ch)
        df <- merge(df, dfMeans)

        # Add levels for boxplots
        df$group2 <- df$group

        # Add rows for boxplots
        df2 <- df
        df2$ch <- df$group
        df2$group2 <- "Comparison"
        df2$e2 <- df2$e
        df2$e <- NA

        # Concatenate
        df$e2 <- NA
        df <- df[df$ch != "Vehicle",]
        df <- rbind(df, df2)

        # Fix levels
        df$ch <- factor(df$ch, levels = c(dfMeans$ch[dfMeans$ch != "Vehicle"], "Vehicle", names(groupList)))
        df$group <- factor(df$group, levels = c("Vehicle", names(groupList)))
        df$group2 <- factor(df$group2, levels = c(names(groupList), "Comparison"))

        # Remove Means from comparison
        df$me[df$group2 == "Comparison"] <- NA

        # Add column names
        colnames(df) <- c("Observation", "Expression", "Group", "Mean", "Group2", "Expression2")

        # Create color manual
        qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
        col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
        colMan <- c("grey", col_vector[seq(length(groupList))])
        names(colMan) <- c("Vehicle", names(groupList))
        
        # update dge data
        list(
          dge_click = grepl("PlotRow", dgeVal),
          dge_plot_df = df,
          dge_plot_gene = geneSelDGE,
          dge_plot_colMan = colMan,
          gsetSelDGE = gsetSelDGE,
          nodeSelDGE = nodeSelDGE
        )
      
      } 
      
    }) %...>% dge_clicked_data()
    
  }
  
})

 
#############################################
#
# GENE EXPRESSION ####
#
#############################################

# Render genePlot for multiple clusters ####
output$genePlotCluster <- renderPlotly({

  dge_click = dge_clicked_data() %>% extract("dge_click") %>% unlist()
  
  if(length(dge_click) > 0 && dge_click == TRUE){

    # Get the plot data
    gene <- dge_clicked_data() %>% extract("dge_plot_gene") %>% unlist()
    df <- dge_clicked_data() %>% extract2("dge_plot_df")
    colMan <- dge_clicked_data() %>% extract2("dge_plot_colMan")

    if(any(df$Group %in% "Vehicle")){ shinyjs::show(id="multiGene-legend") }else{ shinyjs::hide(id="multiGene-legend") }
    
    # Plot
    p <- ggplot(data = df, aes(x = Observation, y = Expression)) +
      geom_boxplot(aes(y = Expression2, fill = Group)) +
      geom_line(aes(group = Observation)) +
      geom_point(aes(colour = Group), size = 3) +
      geom_point(aes(y = Mean), shape = 3, size = 3) +
      facet_grid(~Group2, scales = "free_x") +
      scale_colour_manual(values = colMan) +
      scale_fill_manual(values = colMan) +
      scale_x_discrete() +
      theme_bw() +
      ggtitle(gene) + ylab("Expression") +
      theme(
        plot.margin = margin(5, 5, 0, 0),
        legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust = 0, size = 0, color="white"),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_blank()
      )

    # Fix xaxis due to a bug in plotly
    whXaxis <- which(grepl("xaxis", names(p$x$layout)))

    for (i in whXaxis) {
      ticktext <- p$x$layout[[i]]$ticktext
      if (length(ticktext) == 1) {
        p$x$layout[[i]]$tickvals <- seq(2)
        p$x$layout[[i]]$ticktext <- c(ticktext,"")
      }
    }

    p %>% ggplotly() %>% layout(hoverlabel = list(bgcolor="white"))
    
  } else {

    text = paste("\n Select a gene above \n to show observation-level expression.")

    p <- ggplot() +
      annotate("text", x = 0, y = 0, size = 4, label = text) +
      theme_bw() +
      theme(axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.position="none",
            panel.background=element_blank(),
            panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            plot.background=element_blank())

    p

  }

})


#############################################
#
# ENRICHMENT RESULTS ####
#
#############################################

# Render table of hyperenrichment ####
output$HEmulti <- DT::renderDataTable({

  # Get gene set enrichment results
  ENRTABLE = cluster_compare_results() %>% extract2("gse_dat")

  if(length(ENRTABLE) == 0){

    ENRTABLE <- data.frame(Warning="\n No nodes selected \n", stringsAsFactors = FALSE)
    colnames(ENRTABLE) <- c("Warning Message")

    ENRTABLE %>% datatable(
      rownames = FALSE,
      extensions = 'Buttons',
      selection = "none",
      options = list(
        dom = 'T',
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      )
    )

  }else{

    # Add line breaks to columns
    colnames(ENRTABLE) <- gsub("_", "<br>", colnames(ENRTABLE))
    
    # Get a list of search hits from the node
    gsetSelDGE = dge_clicked_data() %>% extract("gsetSelDGE") %>% unlist()
    nodeSelDGE = dge_clicked_data() %>% extract("nodeSelDGE") %>% unlist()

    # Get exact match for nodeID
    if (length(gsetSelDGE) && nchar(gsetSelDGE) > 0) gsetSelDGE <- paste0("^", gsub("; ", "$|^", gsetSelDGE), "$") else gsetSelDGE <- NULL
    if (length(nodeSelDGE) && nchar(nodeSelDGE) > 0) nodeSelDGE <- paste0(paste0("^", nodeSelDGE, "$"), collapse = "|") else nodeSelDGE <- NULL

    # Create DT object
    ENRTABLE %>%
      mutate(`Gene Set` = gsub("_", " ", !!!syms("Gene Set"))) %>% 
      dplyr::arrange(NodeGroup, `Gene Set`) %>% 
      data.table.round() %>%
      datatable(
        rownames = FALSE,
        escape = FALSE,
        extensions = 'Buttons',
        selection = 'single',
        filter = list(position = 'top', clear = FALSE),
        options = list(
          columnDefs = list(
            list(className = 'dt-left', targets = 0),
            list(className = 'dt-center', targets = 1:(ncol(ENRTABLE)-1))
          ),
          search = list(regex = TRUE),
          searchCols = list(
            list(search = gsetSelDGE),
            list(search = nodeSelDGE),
            rep(NULL, ncol(ENRTABLE)-2)
          ),
          scrollX = TRUE,
          scrollY = '400px',
          dom = 'Brtp',
          paging = TRUE,
          pageLength = 50,
          buttons = list(
            list(
              extend = "collection",
              text = 'Help',
              action = DT::JS(
                "function ( e, dt, node, config ) {",
                  "Shiny.setInputValue('hyperHelpMulti', true, {priority: 'event'});",
                "}"
              )
            ),
            list(
              extend = "collection",
              text = 'Download All Results',
              action = DT::JS(
                "function ( e, dt, node, config ) {",
                  "Shiny.setInputValue('hyperDLMulti', true, {priority: 'event'});",
                "}"
              )
            )
          )
        )
      ) 
  }

})


# Pop-up for help ####
observeEvent(input$hyperHelpMulti, {
  
  shinyBS::toggleModal(session, modalId = "hyperMultiHelpShow", toggle="open")
  
})


# Pop-up for download ####
observeEvent(input$hyperDLMulti, {
  
  shinyBS::toggleModal(session, modalId = "hyperMultiTabDL", toggle="open")
  
})


# Download enrichment results ####
output$downloadHyperCSVMulti <- downloadHandler(
  filename = function() {
    paste0(input$portal_id, "-enrichment-multi-compare-results.csv")
  },
  content = function(file) {
    write.csv(cluster_compare_results() %>% extract2("gse_dat"), file, row.names=F)
  }
)


# Observe row selection ####
observeEvent({
  input$HEmulti_cell_clicked
}, {
  
  req(input$HEmulti_cell_clicked$value)

  # Get the cell clicked value
  clickedVal = input$HEmulti_cell_clicked$value;

  # Get group list
  groupList = groupSelectionValues();

  # Get differential gene expression results
  gseTable = cluster_compare_results() %>% extract2("gse_dat");

  # Get Value
  hyperVal <- gsub("<label for='|'>&#9992;</label>|'>&#128202;</label>|'>&#128202;&nbsp;&#9992;</label>", "", as.character(clickedVal))
  
  # If PlotRow then set nodeSelHE
  if (grepl("PlotRow|SendRow", hyperVal)) {

    rowNum <- as.numeric(sub("PlotRow|SendRow", "", hyperVal))

    # If plotting send the node to plot, otherwise global
    taxonomer_results() %...>% (function(K2summary){

      ##Shiny Packages####
      require(K2Taxonomer)
      require(Biobase)
      require(BiocGenerics)

      genesets <- K2genesets(K2summary) # Get geneset lists
      eSet <- K2gSet(K2summary) # Get gene set projection expression set
      meta <- K2meta(K2summary) # Get meta data

      # If plotting send the node to plot, otherwise global
      HYPERRowMulti <- gseTable[rowNum, , drop = FALSE]

      # get search parameters
      geneSet = HYPERRowMulti[, "Gene Set"]
      geneSelHE = genesets[[geneSet]]
      nodeSelHE = HYPERRowMulti[, "NodeGroup"]

      cohorts = meta$cohorts;
      vehicle = meta$vehicle;

      if(geneSet %in% rownames(exprs(eSet))){

        # Format group names
        if(is.null(cohorts)){
          nams <- colnames(exprs(eSet))
        } else {
          nams <- pData(eSet)[,cohorts]
        }
        nams[nams == vehicle] <- "Vehicle"

        # Create data.frame of expression values
        e <- Biobase::exprs(eSet)[geneSet,]
        df <- data.frame(e = e, ch = nams, stringsAsFactors = F)

        # Get clusters
        obs <- unlist(groupList)

        # Subset for obs in groups
        df <- df[df$ch %in% c(obs, "Vehicle"),]
        df$group <- "Vehicle"
        for (i in names(groupList)) {
          df$group[df$ch %in% groupList[[i]]] <- i
        }

        # Get per Observation mean
        dfMeans <- df %>%
          group_by(ch) %>%
          summarise(me = mean(e))
        dfMeans$ch <- as.character(dfMeans$ch)
        dfMeans <- dfMeans[order(dfMeans$me, decreasing = FALSE),]

        # Sort levels by mean expression
        df$ch <- factor(df$ch, levels = dfMeans$ch)
        df <- merge(df, dfMeans)

        # Add levels for boxplots
        df$group2 <- df$group

        # Add rows for boxplots
        df2 <- df
        df2$ch <- df$group
        df2$group2 <- "Comparison"
        df2$e2 <- df2$e
        df2$e <- NA

        # Concatenate
        df$e2 <- NA
        df <- df[df$ch != "Vehicle",]
        df <- rbind(df, df2)

        # Fix levels
        df$ch <- factor(df$ch, levels = c(dfMeans$ch[dfMeans$ch != "Vehicle"], "Vehicle", names(groupList)))
        df$group <- factor(df$group, levels = c("Vehicle", names(groupList)))
        df$group2 <- factor(df$group2, levels = c(names(groupList), "Comparison"))

        # Remove Means from comparison
        df$me[df$group2 == "Comparison"] <- NA

        # Add column names
        colnames(df) <- c("Observation", "Expression", "Group", "Mean", "Group2", "Expression2")

        # Create color manual
        qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
        col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
        colMan <- c("grey", col_vector[seq(length(groupList))])
        names(colMan) <- c("Vehicle", names(groupList))

        # update the cluster compare results
        list(
          gse_click = grepl("PlotRow", hyperVal),
          gse_plot_df = df,
          cgse_plot_geneSet = geneSet,
          gse_plot_colMan = colMan,
          geneSelHE = geneSelHE,
          nodeSelHE = nodeSelHE
        )
        
      }
      
    }) %...>% gse_clicked_data()
    
  }
  
})

#############################################
#
# SINGLE-SAMPLE ENRICHMENT  ####
#
#############################################

# Render plot of hyperenrichment of multiple clusters ####
output$hePlotCluster <- renderPlotly({

  gse_click = gse_clicked_data() %>% extract2("gse_click")

  if(length(gse_click) > 0 && gse_click == TRUE){

    # Get the plot data
    geneSet <- gse_clicked_data() %>% extract("gse_plot_geneSet") %>% unlist()
    df <- gse_clicked_data() %>% extract2("gse_plot_df")
    colMan <- gse_clicked_data() %>% extract2("gse_plot_colMan")
    
    if(any(df$Group %in% "Vehicle")){ shinyjs::show(id="multiHE-legend") }else{ shinyjs::hide(id="multiHE-legend") }

    # Plot
    p <- ggplot(data = df, aes(x = Observation, y = Expression)) +
      geom_boxplot(aes(y = Expression2, fill = Group)) +
      geom_line(aes(group = Observation)) +
      geom_point(aes(colour = Group), size = 3) +
      geom_point(aes(y = Mean), shape = 3, size = 3) +
      facet_grid(~Group2, scales = "free_x") +
      scale_colour_manual(values = colMan) +
      scale_fill_manual(values = colMan) +
      scale_x_discrete() +
      theme_bw() +
      ggtitle(geneSet) + ylab("Enrichment Score") +
      theme(
        plot.margin = margin(5, 5, 0, 0),
        legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust = 0, size = 0, color="white"),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_blank()
      )

    # Fix xaxis due to a bug in plotly
    whXaxis <- which(grepl("xaxis", names(p$x$layout)))

    for (i in whXaxis) {
      ticktext <- p$x$layout[[i]]$ticktext
      if (length(ticktext) == 1) {
        p$x$layout[[i]]$tickvals <- seq(2)
        p$x$layout[[i]]$ticktext <- c(ticktext,"")
      }
    }

    p %>% ggplotly() %>% layout(hoverlabel = list(bgcolor="white"))


  }else {

    text = paste("\n Select a pathway above \n to show observation-level enrichment. \n")

    p <- ggplot() +
      annotate("text", x = 0, y = 0, size = 4, label = text) +
      theme_bw() +
      theme(axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.position="none",
            panel.background=element_blank(),
            panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            plot.background=element_blank())

    p

  }

})



