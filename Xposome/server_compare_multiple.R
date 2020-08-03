
# Create reactive values ####
values <- reactiveVal(
  list(
    selection="No",
    colSel=NULL, 
    groupList=list(), 
    obsUsed=NULL
  )
)

cluster_compare_results <- reactiveVal(NULL)

dgemulti <- reactiveVal(
  list(
    click="No"
  ) 
)

gsemulti <- reactiveVal(
  list(
    click="No"
  )
)

dendro_reset <- reactiveVal(1)

#############################################
#
# DENDROGRAM ####
#
#############################################

# Generate Dendrogram ####
output$dendroSelect <- renderVisNetwork({
  
  req(dendro_reset() > 0)
  
  taxonomer_results() %...>% extract2("vNetOut") %...>% {
    
    vNetOut <- .
    
    vNetOut %>%
      visOptions(
        autoResize = T,
        nodesIdSelection = FALSE,
        highlightNearest = list(enabled = TRUE, algorithm = "hierarchical", degree = 1E10)
      ) %>%
      visNodes(font = list(size = 25), size = 40) %>%
      visEdges(width = 11, smooth = T, color = list(inherit = 'to')) %>%
      visPhysics(hierarchicalRepulsion = list(nodeDistance = 200)) %>%
      visHierarchicalLayout(direction = "LR", levelSeparation = 300) %>%
      visInteraction(multiselect = TRUE) %>%
      visEvents(
        type = "on",
        click = "function(nodes) { Shiny.onInputChange('Held', nodes.nodes); }",
        deselectNode = "function(nodes) { Shiny.onInputChange('Held', null); }"
      )
    
  }
  
})

# Allow multiple selections ####
observeEvent(input$Held, {
  
  req(input$Held)
  
  # Get selection
  nodeMulti = input$Held;
  colSel = values() %>% extract2("colSel") %>% unlist()
  groupList = values() %>% extract2("groupList")
  obsUsed = values() %>% extract2("obsUsed") %>% unlist()

  # If unclicked initialize or reset groupList
  taxonomer_results() %...>% {
    
    vNetOut <- .[["vNetOut"]]
    K2res <- .[["K2res"]]
    obsMap <- .[["options"]] %>% extract2("obsMap")
  
    colSel <- unique(c(colSel, nodeMulti))
    
    # Get current value
    curVal <- nodeMulti
    
    if(curVal %in% names(K2res)) {
      obs <- unlist(K2res[[curVal]]$obs)
    } else {
      obs <- strsplit(as.character(vNetOut$x$nodes$label[vNetOut$x$nodes$id == curVal]), "\n")[[1]]
    }
    
    # Get unique observations
    obsWhole <- obs
    obs <- obs[!obs %in% obsUsed]
    
    # Get mapped group
    if(length(obs) > 0) {
      
      gMap <- names(obsMap)[which.max(unlist(lapply(
        obsMap,
        function(x) {
          mean(x %in% obs)
        }
      )))]
      
      if( length(gMap) > 0 ) {
        groupList[[gMap]] <- obs
        obsUsed <- unique(c(obsUsed, obs))
      }
    }
    
    list(
      selection="Yes",
      colSel=colSel, 
      groupList=groupList, 
      obsUsed=obsUsed
    )
    
  } %...>% values()

}, ignoreInit = TRUE)


# Render action button ####
output$compare <- renderUI({
  
  req(values()[["selection"]] %in% "Yes")
  
  groupList = values() %>% extract2("groupList")
  
  if(length(groupList) > 1){
    actionButton(inputId = "compareGo", label = strong("Compare Nodes"), class = "mybuttons")
  }
  
})

#############################################
#
# SELECTIONS ####
#
#############################################

# Render group selection ####
output$groupSel <- renderUI({
  
  req(values()[["selection"]] %in% "Yes")
  
  groupList = values() %>% extract2("groupList")
  
  div(style="display: inline-block;",
      HTML(
        paste(unlist(lapply(names(groupList), function(gName) {
          pName <- paste0("<b>Node ", substr(gName, 1, nchar(gName) - 1),
                          ", Group ", substr(gName, nchar(gName), nchar(gName)),
                          " (", gName, ")")
          Obs <- paste(groupList[[gName]], collapse = "&ensp;&ensp;")
          paste0(pName, "</b>:&ensp;", Obs)
        })), collapse = "<br><br><br>")
      )
  )
  
})


# Reset analysis after compareReset is selected ####
observeEvent(input$compareReset, {

  values(
    list(
      selection="No",
      colSel=NULL, 
      groupList=list(), 
      obsUsed=NULL
    )
  )
  
  cluster_compare_results(NULL)
  
  dgemulti(
    list(
      click="No"
    ) 
  )
  
  gsemulti(
    list(
      click="No"
    )
  )
  
  dendro_reset(dendro_reset() + 1)
  
})

# Run analysis after compare button is selected ####
observeEvent(input$compareGo, {

  # Reset the results
  cluster_compare_results(NULL)
  
  # Set clicked groupList
  groupList = values() %>% extract2("groupList")
  
  #print(groupList)
  
  taxonomer_results() %...>% {
    
    ##Create new progress bar
    withProgress(message = "Comparing Selected Groups:", value = 0, {
      
      eSet <- .[["eSet"]]
      gSet <- .[["gSet"]]
      genesets <- .[["genesets"]]
      meta <- .[["meta"]]
      
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
      EnrRes$Plot <- paste0("<label for='PlotRow", seq(nrow(EnrRes)), "'>&#128202;</label>")
      EnrRes$Link <- sapply(as.character(EnrRes$category), get_enrTablelink)
      
      # Format numbers to fit in table
      for (i in c("pval_hyper", "fdr_hyper", "pval_limma", "fdr_limma")) {
        EnrRes[,i] <- signif(EnrRes[,i], digits = 2)
      }
      
      # Format numbers to fit in table
      for (i in c("coef", "mean")) {
        EnrRes[,i] <- round(EnrRes[,i], digits = 2)
      }
      
      # Change column names
      colnames(EnrRes)  <- c("Gene Set", "NodeGroup", "P Value_Hyper", "FDR_Hyper", "N_Overlap", "N_Sig. Genes", "N_Gene Set", "P Value_ssGSEA", "FDR_ssGSEA", "Diff_ssGSEA", "Mean_ssGSEA", "Hits", "Plot", "Link")
      
      ## Order by pvalue
      clusterRes <- clusterRes[order(clusterRes$pval),]
      
      ## Add links to genes
      clusterRes$Plot <- paste0("<label for='PlotRow", seq(nrow(clusterRes)), "'>&#128202;</label>")
      clusterRes$Link <- sapply(as.character(clusterRes$gene), get_dgeTable_link)
      
      clusterRes <- clusterRes[,c("gene", "NodeGroup", "pval", "fdr",  "coef", "mean", "Plot", "Link")]
      
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
        click="Yes",
        dgeTable=clusterRes,
        gseTable=EnrRes
      )
    })
    
  } %...>% cluster_compare_results()
  
}, ignoreInit=TRUE)
    
    
# #############################################
#
# DIFFERENTIAL ANALYSIS RESULTS  ####
#
#############################################

# Render geneTable from multiple group analysis
output$DGEmulti <- DT::renderDataTable({

  if(!is.null(cluster_compare_results())){
    
    gse_click = gsemulti() %>% extract2("click") %>% unlist()
    
    if(gse_click == "Yes"){
      nodegroupID = gsemulti() %>% extract2("nodeSelHEMulti") %>% unlist()
      geneList = gsemulti() %>% extract2("geneList") %>% unlist()
    }else{
      nodegroupID = NULL
      geneList = NULL
    }
  
    DGETABLE <- cluster_compare_results() %>% extract2("dgeTable")
    
    # Get exact match for nodeID
    if (!is.null(nodegroupID)) nodegroupID <- paste0("^", nodegroupID, "$")
    if (!is.null(geneList)) geneList <- paste(paste0("^", geneList, "$"), collapse = "|")
    
    # Create data table obect
    DGETABLE %>% mutate(Gene=gsub("_", " ", Gene)) %>% datatable(
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
          list(search = geneList),
          list(search = nodegroupID),
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
      )) %>%
      formatRound(c("Mean", "Diff"), digits = 2) %>%
      formatSignif(c("P Value", "FDR"), digits = 2) %>%
      formatStyle(c("Gene", "NodeGroup", "Mean"), `border-right` = "solid 2px")
    
  }else{
    
    DGETABLE <- data.frame(Warning="\n No node selected \n")
    colnames(DGETABLE) <- c("Warning Message")
    
    DGETABLE %>%  datatable(
      rownames = FALSE,
      extensions = 'Buttons',
      selection = "none",
      options = list(
        dom = 'T',
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      )
    )
    
  }
})


# Functions for help and download the data
geneHelpShowMulti <- function() {
  div(
    id = "geneHelpMulti",
    modalDialog(
      "Use ^SEARCHTERM$ to filter for exact matches in columns",
      br(),
      easyClose = TRUE, title = "Help"
    )
  )
}

# Functions to download the data ####
geneTabDLMulti <- function() {
  div(
    id = "geneDLMulti",
    modalDialog(
      downloadButton("downloadGeneCSVMulti", "Download Table as CSV file"),
      br(),
      br(),
      easyClose = TRUE, title = "Download Table"
    )
  )
}

# Pop-up for help ####
observeEvent(input$geneHelpMulti, {
  showModal(geneHelpShowMulti())
})

# Download table when prompted ####
observeEvent(input$geneDLMulti, {
  showModal(geneTabDLMulti())
})

# Download gene expression results ####
output$downloadGeneCSVMulti <- downloadHandler(
  filename = function() {
    paste(fname, "-generesultsmulti-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write_csv(cluster_compare_results() %>% extract2("dgeTable"), file)
  }
)

# Observe when a row is selected####
observeEvent(input$DGEmulti_cell_clicked, {

  req(input$DGEmulti_cell_clicked$value)
  
  # Get Value
  dgeVal <- gsub("<label for='|'>&#9992;</label>|'>&#128202;</label>", "", as.character(input$DGEmulti_cell_clicked$value))
  
  # Check that a link was clicked
  if (grepl("PlotRow|SendRow", dgeVal)) {
    
    rowNum <- as.numeric(sub("PlotRow|SendRow", "", dgeVal))
    groupList = values() %>% extract2("groupList")
    dgeTable = cluster_compare_results() %>% extract2("dgeTable")
    
    # If plotting send the node to plot, otherwise global
    taxonomer_results() %...>% {
      
      gene2Pathway = .[["gene2Pathway"]]
      eSet = .[["eSet"]]
      meta = .[["meta"]]
      
      GENERowMulti <- dgeTable[rowNum, , drop = FALSE]
      gene <- GENERowMulti[, "Gene"]
      nodeSelDGEMulti <- GENERowMulti[, "NodeGroup"]
      
      if (gene %in% names(gene2Pathway)) {
        dgeHitsMulti <- gene2Pathway[[gene]]
      } else {
        dgeHitsMulti <- "NO GENE SETS FOUND."
      }
      
      cohorts = meta$cohorts;
      vehicle = meta$vehicle;
      
      if(gene %in% rownames(exprs(eSet))){
        
        # Format group names
        if(is.null(cohorts)){
          nams <- colnames(exprs(eSet))
        } else {
          nams <- pData(eSet)[,cohorts]
        }
        nams[nams == vehicle] <- "Vehicle"
        
        # Create data.frame of expression values
        e <- Biobase::exprs(eSet)[gene,]
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
        
        list(
          click="Yes",
          df=df,
          colMan=colMan,
          gene=gene,
          nodeSelDGEMulti=nodeSelDGEMulti,
          dgeHitsMulti=dgeHitsMulti
        )
        
      }    
    } %...>% dgemulti()
    
  }
})

#############################################
#
# GENE EXPRESSION ####
#
#############################################

# Render genePlot for multiple clusters ####
output$genePlotCluster <- renderPlotly({

  dge_click = dgemulti() %>% extract2("click") %>% unlist()

  if(dge_click == "Yes") {
    
    gene <- dgemulti() %>% extract2("gene")
    df <- dgemulti() %>% extract2("df")
    colMan <- dgemulti() %>% extract2("colMan") 
    
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
    
    p <- p %>% ggplotly(width = input$dimension[1]) %>% layout(hoverlabel = list(bgcolor="white")) 
    
    # Fix xaxis due to a bug in plotly
    whXaxis <- which(grepl("xaxis", names(p$x$layout)))
    
    for (i in whXaxis) {
      ticktext <- p$x$layout[[i]]$ticktext
      if (length(ticktext) == 1) {
        p$x$layout[[i]]$tickvals <- seq(2)
        p$x$layout[[i]]$ticktext <- c(ticktext,"")
      }
    }
    
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
    
    p <- p %>% ggplotly(width = input$dimension[1]) %>% layout(hoverlabel = list(bgcolor="white")) 
    
  }
  
  return(p)
  
})

#############################################
#
# ENRICHMENT RESULTS ####
#
#############################################

# Render table of hyperenrichment ####
output$HEmulti <- DT::renderDataTable({

  dge_click = dgemulti() %>% extract2("click") %>% unlist()
  
  if(dge_click == "Yes"){
    nodegroupID = dgemulti() %>% extract2("nodeSelDGEMulti") %>% unlist()
    dgeHits = dgemulti() %>% extract2("dgeHitsMulti") %>% unlist()
  }else{
    nodegroupID = NULL
    dgeHits = NULL
  }
  
  if(!is.null(cluster_compare_results())){

    ENRTABLE <- cluster_compare_results() %>% extract2("gseTable")
    
    # Get exact match for nodeID
    if (!is.null(nodegroupID)) nodegroupID <- paste0("^", nodegroupID, "$")
    if (!is.null(dgeHits)) dgeHits <- paste0("^", gsub("; ", "$|^", dgeHits), "$")

    # Add line breaks
    ENRTABLE <- ENRTABLE %>% mutate(`Gene Set` = gsub("_", " ", !!!syms("Gene Set"))) 
    colnames(ENRTABLE) <- gsub("_", "<br>", colnames(ENRTABLE))

    # Create DT object
    ENRTABLE %>% datatable(
      rownames = FALSE,
      escape = FALSE,
      extensions = 'Buttons',
      selection = 'single',
      filter = list(position = 'top', clear = FALSE),
      options = list(
        columnDefs = list(
          list(className = 'dt-left', targets = 0),
          list(className = 'dt-center', targets = 1:(ncol(ENRTABLE)-1)),
          list(visible = FALSE, targets = 11)
        ),
        search = list(regex = TRUE),
        searchCols = list(
          list(search = dgeHits),
          list(search = nodegroupID),
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
      )) %>%
      formatRound(c("Mean<br>ssGSEA", "Diff<br>ssGSEA"), digits = 2) %>%
      formatSignif(c("P Value<br>Hyper", "FDR<br>Hyper", "P Value<br>ssGSEA", "FDR<br>ssGSEA"), digits = 2)%>%
      formatStyle(c("Gene Set", "NodeGroup", "N<br>Gene Set", "Mean<br>ssGSEA"), `border-right` = "solid 2px")
    
  }else{
    
    ENRTABLE <- data.frame(Warning="\n No node selected \n", stringsAsFactors = FALSE)
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
    
  }
  
})

# Functions for help and download the data ####
hyperHelpShowMulti <- function() {
  div(
    id = "hyperHelpMulti",
    modalDialog(
      "Use ^SEARCHTERM$ to filter for exact matches in columns",
      br(),
      easyClose = TRUE, title = "Help"
    )
  )
}

# Functions for help and download the data ####
hyperTabDLMulti <- function() {
  div(
    id = "hyperDLMulti",
    modalDialog(
      downloadButton("downloadHyperCSVMulti", "Download Table as CSV file"),
      br(),
      br(),
      easyClose = TRUE, title = "Download Table"
    )
  )
}

# Pop-up for help ####
observeEvent(input$hyperHelpMulti, {
  showModal(hyperHelpShowMulti())
})

# Download data ####
output$downloadHyperCSVMulti <- downloadHandler(
  filename = function() {
    paste(fname, "-enrresultsmulti-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write_csv(cluster_compare_results() %>% extract2("gseTable"), file)
  }
)

# Pop-up for download ####
observeEvent(input$hyperDLMulti, {
  showModal(hyperTabDLMulti())
})

# Observe row selection ####
observeEvent(input$HEmulti_cell_clicked, {
  
  req(input$HEmulti_cell_clicked$value)
  
  # Get Value
  hyperVal <- gsub("<label for='|'>&#9992;</label>|'>&#128202;</label>", "", as.character(input$HEmulti_cell_clicked$value))
  
  # If PlotRow then set nodeSelHE
  if (grepl("PlotRow|SendRow", hyperVal)) {
    
    rowNum <- as.numeric(sub("PlotRow|SendRow", "", hyperVal))
    groupList = values() %>% extract2("groupList")
    gseTable = cluster_compare_results() %>% extract2("gseTable")
    
    # If plotting send the node to plot, otherwise global
    taxonomer_results() %...>% {
      
      genesets = .[["genesets"]]
      eSet = .[["gSet"]]
      meta = .[["meta"]]
      
      # If plotting send the node to plot, otherwise global
      HYPERRowMulti <- gseTable[rowNum, , drop = FALSE]
      geneSet <- HYPERRowMulti[, "Gene Set"]
      nodeSelHEMulti <- HYPERRowMulti[, "NodeGroup"]
      geneList <- genesets[[geneSet]]
      
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
        
        list(
          click="Yes",
          df=df,
          colMan=colMan,
          geneSet=geneSet,
          nodeSelHEMulti=nodeSelHEMulti,
          geneList=geneList
        )
        
      }
    } %...>% gsemulti()
    
  }
})

#############################################
#
# SINGLE-SAMPLE ENRICHMENT  ####
#
#############################################

# Render plot of hyperenrichment of multiple clusters ####
output$hePlotCluster <- renderPlotly({
  
  gse_click = gsemulti() %>% extract2("click") %>% unlist()

  if(gse_click == "Yes") {
    
    geneSet <- gsemulti() %>% extract2("geneSet")
    df <- gsemulti() %>% extract2("df")
    colMan <- gsemulti() %>% extract2("colMan")
    
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
    
    p <- p %>% ggplotly(width = input$dimension[1]) %>% layout(hoverlabel = list(bgcolor="white"))
    
    # Fix xaxis due to a bug in plotly
    whXaxis <- which(grepl("xaxis", names(p$x$layout)))
    
    for (i in whXaxis) {
      ticktext <- p$x$layout[[i]]$ticktext
      if (length(ticktext) == 1) {
        p$x$layout[[i]]$tickvals <- seq(2)
        p$x$layout[[i]]$ticktext <- c(ticktext,"")
      }
    }
    
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
    
    p <- p %>% ggplotly(width = input$dimension[1]) %>% layout(hoverlabel = list(bgcolor="white"))
    
  }
  
  return(p)
  
})



