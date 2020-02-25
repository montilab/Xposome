
#############################################
#
# REACTIVE VALUES ####
#
#############################################
values <- reactiveValues(nodeSel = "A", Held = NULL, mvTabSub = NULL)

#############################################
#
# MEMBER SEARCH SECTION ####
#
#############################################

# Show matches for the search string#######
output$Match <- renderUI({
  
  req(input$mstring)
  
  # Find matches in the terminal leafs
  infoInd <- unique(which(`dim<-`(grepl(input$mstring, infoMat, ignore.case = T), dim(infoMat)), arr.ind=TRUE)[,1])
  
  # Generate string to show
  showString <- sapply(infoInd, function(row) paste0(paste(paste0(colnames(infoMat), ": ", infoMat[row,]), collapse = ";\n"), "\n"))
  
  # Get number of matches
  nMatches <- paste0(length(infoInd), " matche(s)")
  valueString <- c("A", rownames(infoMat)[infoInd])
  names(valueString) <- c(nMatches, showString)
  
  selectInput(inputId = "mVal", label = "Select a Match:", choices = valueString, selectize = TRUE)
  
})


# Help dialog box####
HelpShow <- function() {
  div(
    id = "globalHelp",
    modalDialog(
      HTML("<u>K2 Taxonomer Results</u>"),
      br(),
      easyClose = TRUE, title = "Help"
    )
  )
}

# When help button is clicked, show help dialog box####
observeEvent(input$Helpgo, {
  showModal(HelpShow())
}, ignoreInit=TRUE)


#############################################
#
# DENDROGRAM ####
#
#############################################
# Create reactive values ####
pathSel <- reactiveVal("A")

# Observe the selections of dendrogram#####
observeEvent(input$mVal, {
  
  sel <- as.character(vNetOut$x$nodes$id[grep(paste0("^", input$mVal, "<br>|<br>", input$mVal, "<br>|", input$mVal, "$"), vNetOut$x$nodes$title)])
  
  if(!is.null(input$dendro_selected) & input$dendro_selected != ""){
    
    # Select path to a specific member by searching
    if(!is.null(input$mVal) & input$mVal %in% rownames(info) & !input$dendro_selected %in% sel){
      pathSel(sel)
    }
    
  }else{
    
    pathSel(sel)
    
  }
  
}, ignoreInit=TRUE)

# Render Dendrogram #####
output$dendro <- renderVisNetwork({
  
  if(!is.null(values$mvTabSub)) {
    if(nrow(values$mvTabSub) > 0) {
      
      # Change width of edges
      mEdge <- values$mvTabSub[, c("Parent", "Child", "width")]
      colnames(mEdge) <- c("from", "to", "width")
      edgeFram <- merge(vNetOut$x$edges, mEdge, all.x = TRUE, sort = FALSE)
      edgeFram$width[is.na(edgeFram$width)] <- 1
      edgeFram$color.inherit <- 'to'
      vNetOut$x$edges <- edgeFram
      
      # Change color of edges
      mNode <- values$mvTabSub[, c("Child", "color")]
      colnames(mNode) <- c("id", "color.border")
      nodeFram <- left_join(vNetOut$x$nodes, mNode)
      nodeFram$color.border[is.na(nodeFram$color.border)] <- brewer.pal(6, "Greens")[1]
      nodeFram$color.background <- nodeFram$color.border
      nodeFram$color.highlight <- 'red'
      vNetOut$x$nodes <- nodeFram
      
    }
  }
  
  vNetOut %>%
    visOptions(
      autoResize = T,
      height = "100%",
      nodesIdSelection = list(
        enabled = TRUE,
        main = "Node ID",
        style = 'width: 100px; height: 25px;',
        selected = pathSel()
      ),
      highlightNearest = list(
        enabled = TRUE,
        algorithm = "hierarchical",
        degree = 1E10
      )
    ) %>%
    visNodes(
      font = list(size = 25),
      size = 40,
      color = list(
        background = "white",
        border = "#2B7CE9",
        highlight = "red"
      )
    ) %>%
    visEdges(width = 11, smooth = T) %>%
    visPhysics(hierarchicalRepulsion = list(nodeDistance = 200)) %>%
    visHierarchicalLayout(direction = "LR", levelSeparation = 300) %>%
    visInteraction(dragNodes = FALSE)
  
})

# Observe the selections of dendrogram#####
observeEvent(input$dendro_selected, {
  
  if(is.null(input$dendro_selected) | input$dendro_selected == ""){
    
    updateTextInput(session, inputId = "mstring", value = NA)

  }else{
    
    node = input$dendro_selected
    sel <- as.character(vNetOut$x$nodes$id[grep(paste0("^", input$mVal, "<br>|<br>", input$mVal, "<br>|", input$mVal, "$"), vNetOut$x$nodes$title)])
    
    if(!is.null(input$mVal) && !node %in% sel){
      updateTextInput(session, inputId = "mstring", value = NA)
      pathSel(node)
    }
    
    if (node %in% names(K2res) & node != values$nodeSel) {
      
      values$nodeSel <- node
      values$nodeSelDGE <- node
      values$nodeSelHE <- node
      
      values$geneList <-
        values$groupDGE <-
        values$dgeHits <-
        values$Hits <-
        values$nodeSelDGE <-
        values$groupDGE <-
        NULL
      
    }
  }
  
}, ignoreInit = TRUE)

#############################################
#
# CLUSTER SECTION - STABILITY TAB #####
#
#############################################

# Render heatmap of cluster stability ######
output$heatmapPlot <- renderPlotly({

  # Get values to add
  if(!is.null(input$selCov) && input$selCov %in% colnames(info)) {
    values$selCov <- unique(c(values$selCov, input$selCov))
  } else {
    values$selCov <- NULL
  }

  if(values$nodeSel %in% names(K2res)){

    # Get matrix and sort
    samp_stab <- as.matrix(K2res[[values$nodeSel]]$stability$samples)
    ord <- order(match(colnames(samp_stab), labs))
    samp_stab <- as.matrix(samp_stab)[ord, ord]

    # Get sample groups
    modList <- K2res[[values$nodeSel]]$obs

    # Create column annotation
    colrowAnnot <- data.frame(Group = c(rep("Group:1", length(modList[[1]])),
                                        rep("Group:2", length(modList[[2]]))),
                              row.names = c(modList[[1]], modList[[2]])
    )[colnames(samp_stab), , drop = FALSE]

    # If no selections just color groups
    colSidePalette <- c("#000000", "#808080")
    names(colSidePalette) <- c("Group:1", "Group:2")

    # If selections are made add colors
    if(!is.null(values$selCov)){

      # Initialize
      colValues <- c()
      colSidePalette <- c()

      # Get info for these samples
      infoSub <- info[colnames(samp_stab), values$selCov]
      colrowAnnot <- cbind(infoSub, colrowAnnot)
      colnames(colrowAnnot) <- c(values$selCov, "Group")

      # SET VALUES AND COLOR PALLETES
      for (i in values$selCov) {
        colrowAnnot[,i] <- paste(i, colrowAnnot[,i], sep = ":")
        addValues <- info[,i]; addValues <- addValues[!is.na(addValues)]
        addValuesUnique <- unique(addValues)
        colValues <- c(colValues, paste(i, addValuesUnique, sep = ":"))

        # If a factor or character add unique values for each unique value
        if ( class(addValues) %in% c("character", "factor") ) {
          colSidePalette <- c(colSidePalette, heatmaply:::default_side_colors(length(addValuesUnique)))

          # Otherwise use a color gradient based on z-scored quantile
        } else {
          addValuesNorm <- unique(qnorm(rank(addValues)/(length(addValues)+1)))
          addValuesCut <- rep(NA, length(addValuesNorm))
          cuts <- c(-Inf, -2, -1.5, -1, -0.5, 0.5, 1, 1.5, 2)

          for (j in seq(length(cuts))) {
            addValuesCut[addValuesNorm > cuts[j]] <- j
          }

          contPallete <- brewer.pal(9, "Greens")[addValuesCut]
          colSidePalette <- c(colSidePalette, contPallete)
        }
      }

      # Create pallette of all possible factors
      colSidePalette <- c(colSidePalette, "#000000", "#808080", rep("#D3D3D3", length(values$selCov)))
      names(colSidePalette) <- c(colValues, "Group:1", "Group:2", paste0(values$selCov, ":NA"))

    }

    hm <- heatmaply(
      x = samp_stab,
      color = rev(RdBu(n = 256)),
      limits = c(-1, 1),
      col_side_colors = colrowAnnot,
      col_side_palette = colSidePalette,
      hide_colorbar = TRUE,
      margins = c(0,25,50,25),
      key.title = NULL,
      dendrogram = FALSE,
      showticklabels = FALSE
    )

    # Remove legend
    hm$x$layout$showlegend <- FALSE

    # Change value to cosine similarity
    whHeatmap <- which(unlist(lapply(hm$x$data, function(x) x$type == "heatmap"))) # Get heatmap index
    hm$x$data[[whHeatmap]]$text <- sub("value:", "cos similarity:", hm$x$data[[whHeatmap]]$text)

    return(hm)

  } else {

    text = paste("\n No node selected. \n")
    hm <- ggplot() +
      annotate("text", x = 0, y = 0, size=8, label = text) +
      theme_bw() +
      theme(
        axis.line=element_blank(),
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
        plot.background=element_blank()
      )

    return(ggplotly(hm))
  }

})

# Render stability statistics####
output$stabStats <- renderUI({
  if(values$nodeSel %in% names(K2res)){
    bootProb <- K2res[[values$nodeSel]]$bootP
    nodeStab <- K2res[[values$nodeSel]]$stability$node
    sampStab <- K2res[[values$nodeSel]]$stability$clusters
    outBoot <- paste0("<br> Bootstrap Probability: <b>", bootProb, "</b>")
    outNode <- paste0("<br> Node Stability: <b>", signif(nodeStab, 2), "</b> <br>")
    outStab <- paste0("Cluster Stability: <br> &emsp; Group 1: <b>", signif(sampStab[1], 2), "</b> &emsp; Group 2: <b>",  signif(sampStab[2], 2))
    HTML(paste(outBoot, outNode, outStab))
  } else {
    HTML("")
  }
})

#############################################
#
# CLUSTER SECTION - GROUP MEMBER TAB #####
#
#############################################

# Render table of information for each node####
output$infoTab <- renderDataTable({

  req(values$nodeSel != "No Selection")

  # Get observations
  obs1 <- K2res[[values$nodeSel]]$obs[[1]]
  obs2 <- K2res[[values$nodeSel]]$obs[[2]]

  # Format Cluster information
  infoSub <- info[c(obs1, obs2), , drop = FALSE]
  infoSub$Group <- "1"
  infoSub$Group[rownames(infoSub) %in% obs2] <- "2"
  infoSub <- infoSub[ , c(ncol(infoSub), seq(ncol(infoSub) - 1)) ]

  datatable(
    infoSub,
    rownames = FALSE,
    extensions = 'Buttons',
    selection = "none",
    options = list(
      dom = 'T<"clear">Blfrtip',
      search = list(regex = TRUE, caseInsensitive = FALSE),
      scrollX = TRUE,
      scrollY = "400px",
      paging = FALSE,
      searching = TRUE,
      columnDefs = list(
        list(className = 'dt-center', targets = "_all")
      )
    )
  )

})

#############################################
#
# CLUSTER SECTION - META-VARIABLE TAB #####
#
#############################################

# Generate table of meta-variable tests####
if(!is.null(K2res[[1]]$modTests)) {

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

  output$metaVarTab <- renderDataTable({

    values$mvTab <- K2modTestFram

    K2modTestFram <- K2modTestFram[, c("Split", "Child", "value", "pval", "fdr")]
    colnames(K2modTestFram) <- c("Split", "Node", "Variable", "P Value", "Q Value")

    K2modTestFram$`P Value` <- signif(K2modTestFram$`P Value`, 2)
    K2modTestFram$`Q Value` <- signif(K2modTestFram$`Q Value`, 2)

    datatable(
      K2modTestFram,
      rownames = FALSE,
      extensions = 'Buttons',
      escape = FALSE,
      options = list(
        deferRender = FALSE,
        dom = 'T<"clear">Blfrtip',
        buttons=c('copy','csv','print'),
        search = list(regex = TRUE, caseInsensitive = FALSE),
        scrollX = TRUE,
        scrollY = "400px",
        paging = FALSE,
        searching = TRUE,
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      ), selection = "none")

  })

} else {

  output$metaVarTab <- renderDataTable({

    # Set null data table
    values$mvTab <- NULL
    K2modTestFramNULL <- data.frame("No meta-variable results.");
    colnames(K2modTestFramNULL) <- NULL

    datatable(
      K2modTestFramNULL,
      rownames = F,
      extensions = 'Buttons',
      escape = FALSE,
      options = list(
        deferRender = FALSE,
        dom = 'T<"clear">Blfrtip',
        buttons=c('copy','csv','print'),
        search = list(regex = TRUE, caseInsensitive = FALSE),
        paging = FALSE,
        searching = TRUE,
        columnDefs = list(list(className = 'dt-left', targets = "_all"))
      ), selection = "single")

  })

}

# Clicks to visualize dendrogram by Q-values####
observeEvent(input$visualizeQvalues,  {

  if(!is.null(values$mvTab)) {

    mvTabSub <- values$mvTab

    # Color breaks
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
    values$mvTabSub <- mvTabSub
  }

}, ignoreInit = TRUE)

# Reset the dendrogram ####
observeEvent(input$resetQvalues,  {

  values$mvTabSub <- NULL

})

#############################################
#
# DIFFERENTIAL ANALYSIS RESULTS #####
#
#############################################

# Render genetable results####
output$DGE <- DT::renderDataTable({

  DGETABLE=dgeTable; nodeID = values$nodeSelHE; geneList = values$geneList;

  # Get exact match for nodeID
  if (!is.null(nodeID)) nodeID <- paste0("^", nodeID, "$")
  if (!is.null(geneList)) geneList <- paste(paste0("^", geneList, "$"), collapse = "|")

  # Create data table obect
  datatable(
    DGETABLE,
    rownames = FALSE,
    extensions = 'Buttons',
    escape = FALSE,
    filter = list(position = 'top', clear = FALSE),
    options = list(
      columnDefs = list(
        list(searchable = FALSE,
             orderable = FALSE,
             width = "3px",
             targets = c(8, 9, 10)),
        list(className = 'dt-center', targets = 1:10),
        list(className = 'dt-left', targets = 0)
      ),
      search = list(regex = TRUE),
      searchCols = list(
        list(search = geneList),
        list(search = nodeID),
        NULL, NULL, NULL, NULL, NULL,
        NULL, NULL, NULL, NULL
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
                "Shiny.setInputValue('geneHelp', true, {priority: 'event'});",
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
                "Shiny.setInputValue('geneDL', true, {priority: 'event'});",
              "}"
            )
          )
        )
      )
    ), selection = 'single') %>%
    formatRound(c("Mean", "Diff"), digits = 2) %>%
    formatSignif(c("P Value", "FDR"), digits = 2) %>%
    formatStyle(c("Gene", "Direction", "Mean"), `border-right` = "solid 2px")

})

# Functions for help and download the data ####
geneHelpShow <- function() {
  div(
    id = "geneHelp",
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

# Functions to download the data #####
geneTabDL <- function() {
  div(
    id = "geneDL",
    modalDialog(
      downloadButton("downloadGeneCSV", "Download Table as CSV file"),
      br(),
      br(),
      easyClose = TRUE, title = "Download Table"
    )
  )
}

# Pop-up for help ####
observeEvent(input$geneHelp, {
  showModal(geneHelpShow())
}, ignoreInit = TRUE)

# Download CSV File ####
output$downloadGeneCSV <- downloadHandler(
  filename = function() {
    paste("generesults-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(dgeTable[,seq(8)], file, row.names = FALSE)
  }
)

# Pop-up for CSV file ####
observeEvent(input$geneDL, {
  showModal(geneTabDL())
}, ignoreInit = TRUE)

# Get output when a cell value is clicked ####
observeEvent(input$DGE_cell_clicked, {
  if(!is.null(input$DGE_cell_clicked$value)){

    # Get Value
    dgeVal <- gsub("<label for='|'>&#9992;</label>|'>&#128202;</label>", "", as.character(input$DGE_cell_clicked$value))

    # Check that a link was clicked
    if (grepl("PlotRow|SendRow", dgeVal)) {
      rowNum <- as.numeric(sub("PlotRow|SendRow", "", dgeVal))
      GENERow <- dgeTable[rowNum, , drop = FALSE]

      # If plotting send the node to plot, otherwise global
      if (grepl("PlotRow", dgeVal)) {
        values$Genep <- GENERow[, "Gene"]
        values$nodeSelDGEp <- GENERow[, "Node"]
      } else {
        values$Gene <- GENERow[, "Gene"]
        values$nodeSelDGE <- GENERow[, "Node"]
        values$groupDGE <- GENERow[, "Group"]
        if(values$Gene %in% names(gene2Pathway)) {
          values$dgeHits <- gene2Pathway[[values$Gene]]
        } else {
          values$dgeHits <- "NO GENE SETS FOUND."
        }
      }
    }
  }
}, ignoreInit = TRUE)

#############################################
#
# GENE EXPRESSION #####
#
#############################################

output$genePlot <- renderPlotly({

  if(!is.null(values$nodeSelDGEp)){

    eSet = eSet;
    gene = values$Genep;
    obs1 = K2res[[values$nodeSelDGEp]]$obs[[1]];
    obs2 = K2res[[values$nodeSelDGEp]]$obs[[2]];
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

      # Subset for obs in groups
      df <- df[df$ch %in% c(obs1, obs2, "Vehicle"),]
      df$group <- "Group 1"; df$group[df$ch %in% obs2] <- "Group 2"; df$group[df$ch == "Vehicle"] <- "Vehicle"

      # Get per Observation mean
      dfMeans <- df %>%
        group_by(ch) %>%
        summarise(me = mean(e))
      dfMeans$ch <- as.character(dfMeans$ch)
      dfMeans <- dfMeans[order(dfMeans$me, decreasing = T),]

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
      df$ch <- factor(df$ch, levels = c(dfMeans$ch[dfMeans$ch != "Vehicle"], "Group 1", "Vehicle", "Group 2"))
      df$group <- factor(df$group, levels = c("Group 1", "Vehicle", "Group 2"))
      df$group2 <- factor(df$group2, levels = c("Group 1", "Comparison", "Group 2"))

      # Remove Means from comparison
      df$me[df$group2 == "Comparison"] <- NA

      # Add column names
      colnames(df) <- c("Observation", "Expression", "Group", "Mean", "Group2", "Expression2")

      # Plot
      p <- ggplot(data = df, aes(x = Observation, y = Expression)) +
        geom_boxplot(aes(y = Expression2, fill = Group)) +
        geom_line(aes(group = Observation)) +
        geom_point(aes(colour = Group), size = 3) +
        geom_point(aes(y = Mean), shape = 3, size = 3) +
        facet_grid(~Group2, scales = "free_x") +
        scale_colour_manual(values = c("Group 1" = "darkorange",
                                       "Vehicle" = "grey",
                                       "Group 2" = "darkorchid1")) +
        scale_fill_manual(values = c("Group 1" = "darkorange",
                                     "Vehicle" = "grey",
                                     "Group 2" = "darkorchid1")) +
        scale_x_discrete() +
        theme_bw() +
        ggtitle(paste0(gene)) +
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
      annotate("text", x = 0, y = 0, size=4, label = text) +
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
# HYPERENRICHMENT TABLE #####
#
#############################################

# Render hyperenrichment table#####
output$HE <- renderDataTable({

  ENRTABLE = enrTable; nodeID = values$nodeSelDGE; groupID = values$groupDGE; dgeHits = values$dgeHits;

  # Get exact match for nodeID
  if (!is.null(nodeID)) nodeID <- paste0("^", nodeID, "$")
  if (!is.null(dgeHits)) dgeHits <- paste0("^", gsub("; ", "$|^", dgeHits), "$")
  if (!is.null(groupID)) groupID <- paste0("^", groupID, "$")

  # Add line breaks
  colnames(ENRTABLE) <- gsub("_", "<br>", colnames(ENRTABLE))

  # Create DT object
  outDT <- datatable(
    ENRTABLE,
    rownames = FALSE,
    extensions = 'Buttons',
    escape = FALSE,
    filter = list(position = 'top', clear = FALSE),
    options = list(
      columnDefs = list(
        list(
          searchable = FALSE,
          orderable = FALSE,
          width = "3px",
          targets = c(14, 15, 16)
        ),
        list(visible = FALSE, targets=c(12, 13)),
        list(className = 'dt-center', targets = 1:16),
        list(className = 'dt-left', targets = 0)
      ),
      search = list(regex = TRUE),
      searchCols = list(
        list(search = dgeHits),
        list(search = nodeID),
        list(search = groupID),
        NULL, NULL, NULL, NULL, NULL,
        NULL, NULL, NULL, NULL, NULL,
        NULL, NULL, NULL, NULL
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
            "Shiny.setInputValue('hyperHelp', true, {priority: 'event'});",
          "}"
          )
        ),
        list(
          extend = "collection",
          text = 'Download All Results',
          action = DT::JS(
          "function ( e, dt, node, config ) {",
            "Shiny.setInputValue('hyperDL', true, {priority: 'event'});",
          "}"
          )
        )
      )
    ),
    selection = 'single') %>%
    formatRound(c("Mean<br>ssGSEA", "Diff<br>ssGSEA"), digits = 2) %>%
    formatSignif(c("P Value<br>Hyper", "FDR<br>Hyper", "P Value<br>ssGSEA", "FDR<br>ssGSEA"), digits = 2)%>%
    formatStyle(c("Gene Set", "Direction", "N<br>Gene Set", "Diff<br>ssGSEA"), `border-right` = "solid 2px")

})

# Functions for help
hyperHelpShow <- function() {
  div(
    id = "hyperHelp",
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

# Functions for download the data ####
hyperTabDL <- function() {
  div(
    id = "hyperDL",
    modalDialog(
      downloadButton("downloadHyperCSV", "Download Table as CSV file"),
      br(),
      br(),
      easyClose = TRUE, title = "Download Table"
    )
  )
}

# Pop-up for help ####
observeEvent(input$hyperHelp, {
  showModal(hyperHelpShow())
}, ignoreInit = TRUE)

# Download CSV File ####
output$downloadHyperCSV <- downloadHandler(
  filename = function() {
    paste("enrresults-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(enrTable[,seq(10)], file, row.names = FALSE)
  }
)

# Pop-up for download ####
observeEvent(input$hyperDL, {
  showModal(hyperTabDL())
}, ignoreInit = TRUE)

# Reactive row selection ####
observeEvent(input$HE_cell_clicked, {
  if (!is.null(input$HE_cell_clicked$value)) {

    # Get Value
    hyperVal <- gsub("<label for='|'>&#9992;</label>|'>&#128202;</label>", "", as.character(input$HE_cell_clicked$value))

    # If PlotRow then set nodeSelHE
    if (grepl("PlotRow|SendRow", hyperVal)) {
      rowNum <- as.numeric(sub("PlotRow|SendRow", "", hyperVal))
      HYPERRow <- enrTable[rowNum, , drop = FALSE]

      # If plotting send the node to plot, otherwise global
      if (grepl("PlotRow", hyperVal)) {
        values$GeneSetp <- HYPERRow[, "Gene Set"]
        values$nodeSelHEp <- HYPERRow[, "Node"]
        values$groupSelHEp <-HYPERRow[, "Group"]
        values$dirSelHEp <-HYPERRow[, "Direction"]
      } else {
        values$nodeSelHE <- HYPERRow[, "Node"]
        values$Hits <- strsplit(HYPERRow[, "Hits"], ",")[[1]]
        values$GeneSet <- HYPERRow[, "Gene Set"]
        values$geneList <- genesets[[values$GeneSet]]
      }
    }
  }
}, ignoreInit = TRUE)

#############################################
#
# SINGLE-SAMPLE ENRICHEMENT #####
#
#############################################

# Render pathwayPlot####
output$pathwayPlot <- renderPlotly({
  if(!is.null(values$nodeSelHEp)){

    # Plot Pathway
    eSet = gSet;
    gene = values$GeneSetp;
    obs1 = K2res[[values$nodeSelHEp]]$obs[[1]];
    obs2 = K2res[[values$nodeSelHEp]]$obs[[2]];
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

      # Subset for obs in groups
      df <- df[df$ch %in% c(obs1, obs2, "Vehicle"),]
      df$group <- "Group 1"; df$group[df$ch %in% obs2] <- "Group 2"; df$group[df$ch == "Vehicle"] <- "Vehicle"

      # Get per Observation mean
      dfMeans <- df %>%
        group_by(ch) %>%
        summarise(me = mean(e))
      dfMeans$ch <- as.character(dfMeans$ch)
      dfMeans <- dfMeans[order(dfMeans$me, decreasing = T),]

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
      df$ch <- factor(df$ch, levels = c(dfMeans$ch[dfMeans$ch != "Vehicle"], "Group 1", "Vehicle", "Group 2"))
      df$group <- factor(df$group, levels = c("Group 1", "Vehicle", "Group 2"))
      df$group2 <- factor(df$group2, levels = c("Group 1", "Comparison", "Group 2"))

      # Remove Means from comparison
      df$me[df$group2 == "Comparison"] <- NA

      # Add column names
      colnames(df) <- c("Observation", "Expression", "Group", "Mean", "Group2", "Expression2")

      # Plot
      p <- ggplot(data = df, aes(x = Observation, y = Expression)) +
        geom_boxplot(aes(y = Expression2, fill = Group)) +
        geom_line(aes(group = Observation)) +
        geom_point(aes(colour = Group), size = 3) +
        geom_point(aes(y = Mean), shape = 3, size = 3) +
        facet_grid(~Group2, scales = "free_x") +
        scale_colour_manual(values = c("Group 1" = "darkorange",
                                       "Vehicle" = "grey",
                                       "Group 2" = "darkorchid1")) +
        scale_fill_manual(values = c("Group 1" = "darkorange",
                                     "Vehicle" = "grey",
                                     "Group 2" = "darkorchid1")) +
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

    text <- paste("\n Select a pathway above \n to show observation-level enrichment. \n")
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

