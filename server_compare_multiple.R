
# Function to format covariates string in formula
.formatCov <- function(covariates) if(is.null(covariates)) "" else paste0("+", paste(covariates, collapse = "+"))

# Function to generate differential signature
.signature_wrapper <- function(eSet, cohorts, mods, vehicle = NULL, covariates = NULL){
  
  # Remove vehicle from mods and make a data frame
  mods <- mods[names(mods) != "Vehicle"]
  mods <- data.frame(mods = as.character(mods), GROUP = names(mods), stringsAsFactors = F)
  
  modStats <- NULL
  if(length(unique(mods$mods)) > 1) {
    
    # If replicates in data get unique cohorts
    if(is.null(cohorts)){
      cohorts <- "GROUP"
      pData(eSet)[,cohorts] <- colnames(eSet)
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
    
    # Get genes with sig pvalues
    genes <- cSub$gene[cSub$fdr < qthresh & cSub$coef > cthresh]
    
    return(genes)
    
  }, clusterRes, qthresh)
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
      enrichFram <- data.frame(category = names(genesets),
                               pval = pval,
                               nhits = nhits,
                               ndrawn = ndrawn,
                               ncats = ncats,
                               ntot = ntotal,
                               hits = hits,
                               stringsAsFactors = F)
    }
    return(enrichFram)
  }, genesets, ntotal)
  
  # Calculate and merge FDR values
  pValueDF <- data.frame(pval = unlist(lapply(gseList, function(y) y$pval)))
  pValueDF$fdr <- p.adjust(pValueDF$pval, method = "BH")
  pValueDF <- unique(pValueDF)
  gseList <- lapply(gseList, function(y, pValueDF) {
    y <- merge(y, pValueDF)
    if(nrow(y) > 0){
      y <- y[, c("category", "pval", "fdr", "nhits", "ndrawn", "ncats", "ntot", "hits")]
    }
    return(y)
  }, pValueDF)
  
  return(gseList)
}


#############################################
#
# DENDROGRAM ####
#
#############################################

# Generate Dendrogram ####
output$dendroSelect <- renderVisNetwork({
  
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
  
})

# Allow multiple selections ####
observeEvent(input$Held, {
  
  # Get selection
  nodeMulti <- input$Held
  
  # If unclicked initialize or reset values$groupList
  if (is.null(nodeMulti)) {
    values$colSel <- c()
    values$groupList <- list()
    values$obsUsed <- c()
  } else {
    values$colSel <- unique(c(values$colSel, nodeMulti))
  }
  
  # Get current value
  curVal <- values$colSel[length(values$colSel)]
  
  if (!is.null(curVal)) {
    
    if(curVal %in% names(K2res)) {
      obs <- unlist(K2res[[curVal]]$obs)
    } else {
      obs <- strsplit(as.character(vNetOut$x$nodes$label[vNetOut$x$nodes$id == curVal]), "\n")[[1]]
    }
    
    # Get unique observations
    obsWhole <- obs
    obs <- obs[!obs %in% values$obsUsed]
    
    # Get mapped group
    if(length(obs) > 0) {
      gMap <- names(obsMap)[which.max(unlist(lapply(
        obsMap,
        function(x) {
          mean(x %in% obs)
        }
      )))]
      if( length(gMap) > 0 ) {
        values$groupList[[gMap]] <- obs
        values$obsUsed <- unique(c(values$obsUsed, obs))
      }
    }
  }
  
}, ignoreInit = TRUE, ignoreNULL = TRUE)

#############################################
#
# SELECTIONS ####
#
#############################################

# Render action button ####
output$compare <- renderUI({
  
  req(length(values$groupList) > 1)
  
  div(
    actionButton(inputId = "compareGo", label = strong("Compare Nodes"), class = "mybuttons"),
    actionButton(inputId = "compareReset", label = strong("Reset"), class = "mybuttons")
  )
  
})

# Render group selection ####
output$groupSel <- renderUI({
  
  req(length(values$groupList) >= 1)
    
  HTML(paste(unlist(lapply(names(values$groupList), function(gName) {
    pName <- paste0("<b>Node ", substr(gName, 1, nchar(gName) - 1),
                    ", Group ", substr(gName, nchar(gName), nchar(gName)),
                    " (", gName, ")")
    Obs <- paste(values$groupList[[gName]], collapse = "&ensp;&ensp;")
    paste0(pName, "</b>:&ensp;", Obs)
  })), collapse = "<br><br><br>"))
  
})

# Reset analysis after compareReset is selected ####
observeEvent(input$compareReset, {
  
  values$colSel <- c()
  values$groupList <- list()
  values$obsUsed <- c()
  values$geneListMulti <- c()
  values$nodeSelHEMulti <- c()
  values$nodeSelDGEMulti <- c()
  values$dgeHitsMulti <- c()
  
})

# Run analysis after compareGo is selected ####
observeEvent(input$compareGo, {
  
  # Set clicked groupList
  values$groupListClicked <- values$groupList
  
  withProgress(message = "Comparing Selected Groups:", value = 0, {
    
    # Create data.frame of mods
    mods <- as.factor(unlist(lapply(seq(length(values$groupListClicked)), function(x) rep(x, length(values$groupListClicked[[x]])))))
    names(mods) <- unlist(values$groupListClicked)
    
    # Genereate gene expression results
    incProgress(1/10, detail = "Differential Analysis")
    clusterRes <- .signature_wrapper(eSet,
                                     cohorts = meta$cohorts,
                                     mods,
                                     vehicle = meta$vehicle,
                                     covariates = meta$covariates)
    
    # Add NodeGroup and gene column
    clusterRes$NodeGroup <- names(values$groupListClicked)[as.numeric(clusterRes$mod)]
    clusterRes$gene <- rownames(clusterRes)
    
    # Generete hyperenrichment results
    incProgress(1/4, detail = "Pathway Hyperenrichment")
    hyperEnrRes <- hyperenrichmentClusters(clusterRes,
                                           values$groupListClicked,
                                           genesets,
                                           meta$qthresh,
                                           meta$cthresh,
                                           meta$ntotal)
    
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
    colnames(hyperEnrRes)[colnames(hyperEnrRes) %in% c("pval", "fdr")] <-
      paste(colnames(hyperEnrRes)[colnames(hyperEnrRes) %in% c("pval", "fdr")], "hyper", sep = "_")
    
    # Generete hyperenrichment results
    incProgress(1/4, detail = "Pathway Differential Enrichment")
    ssEnrRes <- .signature_wrapper(gSet,
                                   cohorts = meta$cohorts,
                                   mods,
                                   vehicle = NULL,
                                   covariates = meta$covariates)
    
    # Add NodeGroup and category
    ssEnrRes$NodeGroup <- names(values$groupListClicked)[as.numeric(ssEnrRes$mod)]
    ssEnrRes$category <- rownames(ssEnrRes)
    
    # Add type of test
    colnames(ssEnrRes)[colnames(ssEnrRes) %in% c("pval", "fdr")] <-
      paste(colnames(ssEnrRes)[colnames(ssEnrRes) %in% c("pval", "fdr")], "limma", sep = "_")
    
    # Format hyperenrichment table
    incProgress(1/3, detail = "Formatting Results")
    
    # Merge the two and sort by hyper p-value
    EnrRes <- merge(hyperEnrRes, ssEnrRes, all = TRUE)
    
    ## Order by pvalue
    EnrRes <- EnrRes[order(EnrRes$pval_hyper),]
    
    # Sort columns
    EnrRes <- EnrRes[, c("category", "NodeGroup", "pval_hyper", "fdr_hyper", "nhits", "ndrawn", "ncats", "pval_limma", "fdr_limma", "coef", "mean", "hits")]
    
    # Add aliases for plotting and looking up
    EnrRes$Plot <- paste0("<label for='PlotRow",
                          seq(nrow(EnrRes)),
                          "'>&#128202;</label>")
    EnrRes$Send <- paste0("<label for='SendRow",
                          seq(nrow(EnrRes)),
                          "'>&#9992;</label>")
    
    # Add links to gene sets
    EnrRes$Link<- sapply(as.character(EnrRes$category), get_enrTablelink)
    
    # Format numbers to fit in table
    for (i in c("pval_hyper", "fdr_hyper", "pval_limma", "fdr_limma")) {
      EnrRes[,i] <- signif(EnrRes[,i], digits = 2)
    }
    # Format numbers to fit in table
    for (i in c("coef", "mean")) {
      EnrRes[,i] <- round(EnrRes[,i], digits = 2)
    }
    
    # Change column names
    colnames(EnrRes)  <- c("Gene Set", "NodeGroup", "P Value_Hyper", "FDR_Hyper", "N_Overlap", "N_Sig. Genes", "N_Gene Set", "P Value_ssGSEA", "FDR_ssGSEA", "Diff_ssGSEA", "Mean_ssGSEA", "Hits", "Plot", "Send", "Link")
    
    # Assign to reactiveValues
    values$EnrRes <- EnrRes
    
    ## Order by pvalue
    clusterRes <- clusterRes[order(clusterRes$pval),]
    
    # Add aliases for plotting and looking up
    clusterRes$Send <- paste0("<label for='SendRow",
                              seq(nrow(clusterRes)),
                              "'>&#9992;</label>")
    clusterRes$Plot <- paste0("<label for='PlotRow",
                              seq(nrow(clusterRes)),
                              "'>&#128202;</label>")
    
    ## Add links to genes
    clusterRes$Link <- sapply(as.character(clusterRes$gene), get_dgeTable_link)
    
    clusterRes <- clusterRes[,c("gene", "NodeGroup", "pval", "fdr",  "coef", "mean", "Plot", "Send", "Link")]
    
    # Format numbers to fit in table
    for (i in c("pval", "fdr")) {
      clusterRes[,i] <- signif(clusterRes[,i], digits = 2)
    }
    # Format numbers to fit in table
    for (i in c("coef", "mean")) {
      clusterRes[,i] <- round(clusterRes[,i], digits = 2)
    }
    
    # Rename columns
    colnames(clusterRes) <- c("Gene", "NodeGroup", "P Value", "FDR", "Diff", "Mean", "Plot", "Send", "Link")
    
    # Assign to reactiveValues
    values$clusterRes <- clusterRes
    
  })
  
})

#############################################
#
# DIFFERENTIAL ANALYSIS RESULTS  ####
#
#############################################

# Render geneTable from multiple group analysis
output$DGEmulti <- renderDataTable({

  req(values$clusterRes)

  clusterRes = values$clusterRes;
  nodegroupID = values$nodeSelHEMulti;
  geneList = values$geneListMulti;
  
  # Get exact match for nodeID
  if (!is.null(nodegroupID)) nodeID <- paste0("^", nodegroupID, "$")
  if (!is.null(geneList)) geneList <- paste(paste0("^", geneList, "$"), collapse = "|")

  # Create data table obect
  datatable(
    clusterRes,
    rownames = F,
    extensions = 'Buttons',
    escape = F,
    filter = list(position = 'top', clear = FALSE),
    options = list(
      columnDefs = list(
        list(searchable = FALSE,
             orderable = FALSE,
             width = "3px",
             targets = c(6, 7, 8)
        ),
        list(className = 'dt-center', targets = 1:8),
        list(className = 'dt-left', targets = 0)
      ),
      search = list(regex = TRUE),
      searchCols = list(
        list(search = geneList),
        list(search = nodegroupID),
        NULL, NULL, NULL, NULL, NULL
      ),
      scrollX = TRUE,
      scrollY = '400px',
      dom = 'Brtp',
      paging = T,
      pageLength = 50,
      buttons = list(
        list(
          extend = "collection",
          text = 'Help',
          action = DT::JS(
            "function ( e, dt, node, config ) {",
              "Shiny.setInputValue('geneHelpMulti', true, {priority: 'event'});",
            "}"
          )
        ),
        list(
          extend = "collection",
          text = 'Download All Results',
          action = DT::JS(
            "function ( e, dt, node, config ) {",
              "Shiny.setInputValue('geneDLMulti', true, {priority: 'event'});",
            "}"
          )
        )
      )
    ),
    selection = "single") %>%
    formatRound(c("Mean", "Diff"), digits = 2) %>%
    formatSignif(c("P Value", "FDR"), digits = 2) %>%
    formatStyle(c("Gene", "NodeGroup", "Mean"), `border-right` = "solid 2px")

})

# Functions for help and download the data
geneHelpShowMulti <- function() {
  div(
    id = "geneHelpMulti",
    modalDialog(
      "1) Use ^SEARCHTERM$ to filter for exact matches in columns",
      br(),
      HTML("2) Select '&#128202;' to plot gene expression below."),
      br(),
      HTML("3) Select '&#9992;' to send row information to look up pathways which include this gene in hyperenrichment results above."),
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
    paste("generesultsmulti-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(values$clusterRes[,seq(6)], file, row.names = FALSE)
  }
)

# Get output ####
observeEvent(input$DGEmulti_cell_clicked, {
  if(!is.null(input$DGEmulti_cell_clicked$value)){
    
    # Get Value
    dgeVal <- gsub("<label for='|'>&#9992;</label>|'>&#128202;</label>", "", as.character(input$DGEmulti_cell_clicked$value))
    
    # Check that a link was clicked
    if (grepl("PlotRow|SendRow", dgeVal)) {
      rowNum <- as.numeric(sub("PlotRow|SendRow", "", dgeVal))
      GENERowMulti <- values$clusterRes[rowNum, , drop = FALSE]
      
      # If plotting send the node to plot, otherwise global
      if (grepl("PlotRow", dgeVal)) {
        values$GeneMultip <- GENERowMulti[, "Gene"]
      } else {
        values$GeneMulti <- GENERowMulti[, "Gene"]
        values$nodeSelDGEMulti <- GENERowMulti[, "NodeGroup"]
        
        if (values$GeneMulti %in% names(gene2Pathway)) {
          values$dgeHitsMulti <- gene2Pathway[[values$GeneMulti]]
        } else {
          values$dgeHitsMulti <- "NO GENE SETS FOUND."
        }
      }
    }
    
  }
})

#############################################
#
# GENE EXPRESSION ####
#
#############################################

# Render genePlot for multiple clusters ####
output$genePlotCluster <- renderPlotly({

  if (!is.null(values$GeneMultip)) {

    eSet = eSet;
    gene = values$GeneMultip;
    groupList = values$groupListClicked;
    cohorts = meta$cohorts;
    vehicle = meta$vehicle;

    if(gene %in% rownames(eSet)){

      # Format group names
      if(is.null(cohorts)){
        nams <- colnames(eSet)
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
        ggtitle(gene) + 
        theme(
          plot.margin = margin(10, 10, 10, 10),
          legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 0, size = 15),
          axis.text.y = element_text(size = 15),
          axis.title.x = element_blank()
        )

      p <- ggplotly(p)

      # Fix xaxis due to a bug in plotly
      whXaxis <- which(grepl("xaxis", names(p$x$layout)))
      for (i in whXaxis) {
        ticktext <- p$x$layout[[i]]$ticktext
        if (length(ticktext) == 1) {
          p$x$layout[[i]]$tickvals <- seq(2)
          p$x$layout[[i]]$ticktext <- c(ticktext,"")
        }
      }
      return(p)
    }

  } else {

    text = paste("\n Select a gene above \n to show observation-level expression.")
    hm <- ggplot() +
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
    ggplotly(hm)

  }

})

#############################################
#
# ENRICHMENT RESULTS ####
#
#############################################

# Render table of hyperenrichment ####
output$HEmulti <- renderDataTable({

  req(values$EnrRes)

  ENRTABLE = values$EnrRes;
  nodegroupID = values$nodeSelDGEMulti;
  dgeHits = values$dgeHitsMulti;

  # Get exact match for nodeID
  if (!is.null(nodegroupID)) nodeID <- paste0("^", nodegroupID, "$")
  if (!is.null(dgeHits)) dgeHits <- paste0("^", gsub("; ", "$|^", dgeHits), "$")

  # Add line breaks
  colnames(ENRTABLE) <- gsub("_", "<br>", colnames(ENRTABLE))

  # Create DT object
  outDT <- datatable(
    ENRTABLE,
    rownames = F,
    extensions = 'Buttons',
    escape = FALSE,
    filter = list(position = 'top', clear = FALSE),
    options = list(
      columnDefs = list(
        list(
          searchable = FALSE,
          orderable = FALSE,
          width = "3px",
          targets = c(12, 13, 14)
        ),
        list(visible = FALSE, targets = 11),
        list(className = 'dt-center', targets = 1:14),
        list(className = 'dt-left', targets = 0)
      ),
      search = list(regex = TRUE),
      searchCols = list(
        list(search = dgeHits),
        list(search = nodegroupID),
        NULL, NULL, NULL, NULL, NULL,
        NULL, NULL, NULL, NULL, NULL,
        NULL, NULL, NULL
      ),
      scrollX = TRUE,
      scrollY = '400px',
      dom = 'Brtp',
      paging = T,
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
    ),
    selection = "single") %>%
    formatRound(c("Mean<br>ssGSEA", "Diff<br>ssGSEA"), digits = 2) %>%
    formatSignif(c("P Value<br>Hyper", "FDR<br>Hyper", "P Value<br>ssGSEA", "FDR<br>ssGSEA"), digits = 2)%>%
    formatStyle(c("Gene Set", "NodeGroup", "N<br>Gene Set", "Mean<br>ssGSEA"), `border-right` = "solid 2px")

})

# Functions for help and download the data ####
hyperHelpShowMulti <- function() {
  div(
    id = "hyperHelpMulti",
    modalDialog(
      "1) Use ^SEARCHTERM$ to filter for exact matches in columns",
      br(),
      HTML("2) Select '&#128202;' to plot single-sample enrichment below."),
      br(),
      HTML("3) Select '&#9992;' to send row information to look up results for individual genes in this pathway below."),
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
    paste("enrresultsmulti-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(values$EnrRes[,seq(12)], file, row.names = FALSE)
  }
)

# Pop-up for download ####
observeEvent(input$hyperDLMulti, {
  showModal(hyperTabDLMulti())
})

# Reactive row selection ####
observeEvent(input$HEmulti_cell_clicked, {
  if (!is.null(input$HEmulti_cell_clicked$value)) {
    
    # Get Value
    hyperVal <- gsub("<label for='|'>&#9992;</label>|'>&#128202;</label>",
                     "",
                     as.character(input$HEmulti_cell_clicked$value))
    
    # If PlotRow then set nodeSelHE
    if (grepl("PlotRow|SendRow", hyperVal)) {
      rowNum <- as.numeric(sub("PlotRow|SendRow", "", hyperVal))
      HYPERRowMulti <- values$EnrRes[rowNum, , drop = FALSE]
      
      # If plotting send the node to plot, otherwise global
      if (grepl("PlotRow", hyperVal)) {
        values$GeneSetMultip <- HYPERRowMulti[, "Gene Set"]
      } else {
        values$nodeSelHEMulti <- HYPERRowMulti[, "NodeGroup"]
        values$GeneSetMulti <- HYPERRowMulti[, "Gene Set"]
        values$geneListMulti <- genesets[[values$GeneSetMulti]]
      }
    }
    
  }
})

#############################################
#
# SINGLE-SAMPLE ENRICHMENT  ####
#
#############################################

# Render plot of hyperenrichment of multiple clusters ####
output$hePlotCluster <- renderPlotly({

  if (!is.null(values$GeneSetMultip)) {

    eSet = gSet;
    gene = values$GeneSetMultip;
    groupList = values$groupListClicked;
    cohorts = meta$cohorts;
    vehicle = meta$vehicle;

    if(gene %in% rownames(eSet)){

      # Format group names
      if(is.null(cohorts)){
        nams <- colnames(eSet)
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
        ggtitle(gene) + ylab("Enrichment Score") +
        theme(
          plot.margin = margin(10, 10, 10, 10),
          legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 0, size = 15),
          axis.text.y = element_text(size = 15),
          axis.title.x = element_blank()
        )

      p <- ggplotly(p)

      # Fix xaxis due to a bug in plotly
      whXaxis <- which(grepl("xaxis", names(p$x$layout)))
      for (i in whXaxis) {
        ticktext <- p$x$layout[[i]]$ticktext
        if (length(ticktext) == 1) {
          p$x$layout[[i]]$tickvals <- seq(2)
          p$x$layout[[i]]$ticktext <- c(ticktext,"")
        }
      }
      return(p)
    }

  } else {

    text = paste("\n Select a pathway above \n to show observation-level enrichment. \n")
    hm <- ggplot() +
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
    ggplotly(hm)

  }

})



