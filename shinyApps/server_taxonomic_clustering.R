
# Create reative values
selCov <- reactiveVal()
vNetOut <- reactiveVal()
dge_dat <- reactiveVal()
gse_dat <- reactiveVal()

#############################################
#
# MEMBER SEARCH SECTION ####
#
#############################################

## Update the search string results when the search button is clicked on
observeEvent({
  input$search_string
}, {

  req(input$mstring)

  mstring = input$mstring; 

  promise_all(K2summary=taxonomer_results(), profile_dat=profile_dat()) %...>%
    with({

      ##Shiny Packages####
      require(K2Taxonomer)
      require(Biobase)
      require(BiocGenerics)

      # Parse results
      info <- K2info(K2summary)  # Profile information
      infoMat <- as.matrix(info)
      chemicalMat <- infoMat[,which(colnames(infoMat) %in% colnames(profile_dat))]

      # Find matches in the terminal leafs
      infoInd <- unique(which(`dim<-`(grepl(mstring, chemicalMat, ignore.case = T), dim(chemicalMat)), arr.ind=TRUE)[,1])

      # Generate string to show
      showString <- sapply(infoInd, function(row) paste0(paste(paste0(colnames(chemicalMat), ": ", chemicalMat[row,]), collapse = ";\n"), "\n"))

      # Get number of matches
      nMatches <- paste0(length(infoInd), " matche(s)")
      valueString <- c("", rownames(chemicalMat)[infoInd])
      names(valueString) <- c(nMatches, showString)

      updateSelectizeInput(session, inputId = "mVal", label = "Results", choices = valueString)

      shinyjs::show(id = "mVal")

    }) %...!% { return(NULL) }

})

## Update the search string results when the search button is clicked on
observeEvent({
  input$reset_string
}, {

  shinyjs::hide(id = "mVal")
  
  updateTextInput(session, inputId = "mstring", value = "")
  
  visNetworkProxy(session, shinyId = "dendro") %>% 
    visOptions(
      nodesIdSelection = list(
        main = "Node ID",
        selected=NULL
      )
    )
      
})


# Observe the selection of search string results#####
observeEvent({
  input$mVal
}, {

  req(input$mVal)

  mVal = input$mVal;
  
  vNetOut_data() %...>% (function(vNetOut_data){
    
    vNetOut <- vNetOut_data %>% extract2("vNetOut")
    
    selected <- vNetOut$x$nodes$id[grep(paste0("^", mVal, "<br>|<br>", mVal, "<br>|", mVal, "$"), vNetOut$x$nodes$title)]
    
    visNetworkProxy(session, shinyId = "dendro") %>% 
      visOptions(
        nodesIdSelection = list(
          main = "Node ID",
          selected=selected
        )
      )
    
  }) %...!% { return(NULL) }

})


observeEvent({
  input$visualizeQvalues
}, {
  
  node = input$dendro_selected; visualizeQvalues = input$visualizeQvalues;
  
  vNetOut_data() %...>% (function(vNetOut_data){
    
    if(visualizeQvalues){
      vNetOut = vNetOut_data %>% extract2("vNetOut_qvalues")
    }else{
      vNetOut = vNetOut_data %>% extract2("vNetOut")
    }
    
    list(
      vNetOut = vNetOut,
      node = node
    )
    
  }) %...>% vNetOut()
  
})
  
  
#############################################
#
# DENDROGRAM ####
#
#############################################

## Output the Dendrogram #####
output$dendro <- renderVisNetwork({
  
  node = isolate({ input$dendro_selected }); visualizeQvalues = isolate({ input$visualizeQvalues });
  
  vNetOut_data() %...>% (function(vNetOut_data){
    
    if(visualizeQvalues){
      vNetOut = vNetOut_data %>% extract2("vNetOut_qvalues")
      shinyjs::show(id = "qvalues-legend")
    }else{
      vNetOut = vNetOut_data %>% extract2("vNetOut")
      shinyjs::hide(id = "qvalues-legend")
    }
    
    selected = ifelse(length(node) > 0 && nchar(node) > 0 && node %in% vNetOut$x$nodes$id, node, vNetOut$x$nodes$id[1])
    
    # Create the network
    vNetOut %>%
      visOptions(
        autoResize = T,
        height = "100%",
        nodesIdSelection = list(
          enabled = TRUE,
          values = vNetOut$x$nodes$id,
          main = "Node ID",
          selected = selected,
          style = 'width: 100px; height: 25px;'
        ),
        highlightNearest = list(
          enabled = TRUE,
          algorithm = "hierarchical",
          degree = 1E10
        )
      ) %>%
      visNodes(
        label = "Node ID",
        font = list(size = 50),
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
  
})
  
  
## Observe when a node is selected in dendrogram ####
dendro_dat <- reactive({

  node = input$dendro_selected;
  
  promise_all(K2summary=taxonomer_results(), profile_dat=profile_dat()) %...>%
    with({
      
      ##Shiny Packages####
      require(K2Taxonomer)
      require(Biobase)
      require(BiocGenerics)
      
      info <- K2info(K2summary)  # Profile information
      K2res <- K2results(K2summary) # Format K2 results
      K2dendrogram <- K2dendro(K2summary) # Create static dendrogram/heatmap
      labs <- get_leaves_attr(K2dendrogram, "label") ## Get sample order ####
      
      if(node %in% names(K2res)){
        
        # Get matrix and sort
        samp_stab <- as.matrix(K2res[[node]]$stability$samples)
        ord <- order(match(colnames(samp_stab), labs))
        samp_stab <- as.matrix(samp_stab)[ord, ord]
        
        # Get sample groups
        modList <- K2res[[node]]$obs
        
        # Create column annotation
        colrowAnnot <- data.frame(Group = c(rep("Group:1", length(modList[[1]])),
                                            rep("Group:2", length(modList[[2]]))),
                                  row.names = c(modList[[1]], modList[[2]])
        )[colnames(samp_stab), , drop = FALSE]
        
        # If no selections just color groups
        colSidePalette <- c("#000000", "#808080")
        names(colSidePalette) <- c("Group:1", "Group:2")
        
        # Get observations
        obs1 <- K2res[[node]]$obs[[1]]
        obs2 <- K2res[[node]]$obs[[2]]
        
        # Format Cluster information
        infoSub <- info[c(obs1, obs2), , drop = FALSE]
        infoSub$Group <- "1"
        infoSub$Group[rownames(infoSub) %in% obs2] <- "2"
        infoSub <- infoSub[ , c(ncol(infoSub), seq(ncol(infoSub) - 1)) ]
        
        # Getting bootstrap summary stat
        bootProb <- K2res[[node]]$bootP
        nodeStab <- K2res[[node]]$stability$node
        sampStab <- K2res[[node]]$stability$clusters
        outBoot <- paste0("Bootstrap Probability: <b>", bootProb, "</b><br>")
        outNode <- paste0("Node Stability: <b>", signif(nodeStab, 2), "</b><br>")
        outStab <- paste0("Cluster Stability: <br> &emsp; Group 1: <b>", signif(sampStab[1], 2), "</b> &emsp; Group 2: <b>",  signif(sampStab[2], 2))
        stat <- paste(outBoot, outNode, outStab)
        
        list(
          node=node,
          samp_stab=samp_stab,
          colrowAnnot=colrowAnnot,
          colSidePalette=colSidePalette,
          infoSub=infoSub,
          stat=stat
        )
        
      }else{
        
        return(NULL)
        
      }
      
    }) %...!% { return(NULL) }
  
}) %>% bindCache(input$portal_id, "-K2Taxonomer-nodeselected-", input$dendro_selected) %>%
  bindEvent(input$dendro_selected)


#############################################
#
# HEATMAP AND STABILITY TABlE #####
#
#############################################

## Update annotation variables ####
observeEvent({
  input$portal_id
}, {
  
  ## Set select input options for annotations bar
  taxonomer_results() %...>% (function(K2summary){

    ##Shiny Packages####
    require(K2Taxonomer)
    require(Biobase)
    require(BiocGenerics)

    # Parse results
    info <- K2info(K2summary)  # Profile information
    meta <- K2meta(K2summary) # Get meta data

    #Create variable options
    varOptions <- sort(colnames(info))
    names(varOptions) <- varOptions

    if(!is.null(meta$cohorts)) {
      varOptions <- varOptions[varOptions != meta$cohorts]
    } else {
      varOptions <- varOptions[varOptions != "sampleID"]
    }

    shinyjs::hide(id = "mVal")
    shinyjs::hide(id = "visualizeQvalues")
    
    session$sendCustomMessage("updateK2TaxonomerVarOptions", sort(as.character(varOptions)))

  }) %...!% { return(NULL) }

})


##Observe the selections of covariates#####
observeEvent({
  input$selCov
}, {

  selection <- input$selCov; selCov <- selCov();

  taxonomer_results() %...>% (function(K2summary){

    ##Shiny Packages####
    require(K2Taxonomer)
    require(Biobase)
    require(BiocGenerics)

    info <- K2info(K2summary)  # Profile information

    if(all(selection %in% colnames(info))){
      new_selection = unique(c(selCov, selection)) #If selections are made add colors
    }else if(any(selection %in% "RESET")){
      new_selection = NULL
      updateSelectizeInput(session, inputId = "selCov", selected="")
    }

    return(new_selection)

  }) %...>% selCov()

})


#############################################
#
# HEATMAP #####
#
#############################################

output$heatmapPlot <- renderPlotly({

  selCov <- selCov();

  dendro_dat() %...>% (function(dendro_dat){

    shinyjs::show(id = "selCov")

    samp_stab <- dendro_dat %>% extract2("samp_stab")
    colrowAnnot <- dendro_dat %>% extract2("colrowAnnot")
    colSidePalette <- dendro_dat %>% extract2("colSidePalette")

    if(length(selCov) > 0){

      info <- dendro_dat %>% extract2("infoSub")

      # Initialize
      colValues <- c()
      colSidePalette <- c()

      # Get info for these samples
      infoSub <- info[colnames(samp_stab), selCov]
      colrowAnnot <- cbind(infoSub, colrowAnnot)
      colnames(colrowAnnot) <- c(selCov, "Group")

      # SET VALUES AND COLOR PALLETES
      for (i in selCov) {
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

      # Create palette of all possible factors
      colSidePalette <- c(colSidePalette, "#000000", "#808080", rep("#D3D3D3", length(selCov)))
      names(colSidePalette) <- c(colValues, "Group:1", "Group:2", paste0(selCov, ":NA"))

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

    hm %>% ggplotly() %>% layout(hoverlabel=list(bgcolor="white"))

  }) %...!% {

    shinyjs::hide(id = "selCov")

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

    hm

  }

})


## Output stability statistics ####
output$stabStats <- renderDataTable({

  dendro_dat() %...>%
    extract2("stat") %...>%
    as.data.frame(stat=stat) %...!% { return(NULL) }

}, escape = FALSE, server = TRUE, colnames = NULL, rownames = FALSE, selection = "none",
  options = list(
    dom = "T",
    headerCallback =  JS( 'function(thead, data) { $(thead).find("th").eq(0).css("text-align", "left"); }')
  )
)


#############################################
#
# CLUSTER SECTION - GROUP MEMBER TAB #####
#
#############################################

## Get group member data ####
infoTabTable <- reactive({

  viewAll = input$viewAll;

  promise_all(dendro_dat=dendro_dat(), profile_dat=profile_dat()) %...>%
    with({

      infoSub <- dendro_dat %>% extract2("infoSub")

      if(viewAll){
        return(infoSub)
      }else{
        infoSub_reduce <- infoSub[, which(colnames(infoSub) %in% c("Group", colnames(profile_dat)))]
        return(infoSub_reduce)
      }

    }) %...!% { return(NULL) }

}) %>% bindEvent(input$viewAll, input$dendro_selected)


## Output group member information for each selected node ####
output$infoTab <- renderDataTable({

  infoTabTable() %...>%
    data.table.round() %...>%
    datatable(
      rownames = FALSE,
      extensions = 'Buttons',
      selection = "single",
      options = list(
        deferRender = FALSE,
        paging = TRUE,
        searching = TRUE,
        ordering = TRUE,
        pageLength = 20,
        scrollX = TRUE,
        scrollY = 400,
        scrollCollapse = TRUE,
        dom = 'T<"clear">Blfrtip',
        buttons=c('copy','csv','print')
      )
    )  %...!% {

      datatable(
        data.frame(Warning="\n No node selected. \n", stringsAsFactors = FALSE),
        rownames = FALSE,
        extensions = 'Buttons',
        selection = "none",
        options = list(
          dom = 'T',
          columnDefs = list(
            list(className = 'dt-center', targets = "_all")
          )
        )
      )

    }

})


#############################################
#
# CLUSTER SECTION - META-VARIABLE TAB #####
#
#############################################

## Generate table for meta-variable tests ####
output$metaVarTab <- renderDataTable({

  vNetOut_data() %...>% extract2("K2modTestFram") %...>%
    (function(K2modTestFram){

      shinyjs::show(id = "visualizeQvalues")

      K2modTestFram %>%
        data.table.round() %>%
        dplyr::arrange(Split, Node, Variable, `P Value`, `Q Value`) %>% 
        datatable(
          rownames = FALSE,
          escape = FALSE,
          extensions = 'Buttons',
          selection = "single",
          options = list(
            columnDefs = list(list(className='dt-center', targets = "_all")),
            deferRender = FALSE,
            paging = TRUE,
            searching = TRUE,
            ordering = TRUE,
            pageLength = 20,
            scrollX = TRUE,
            scrollY = 400,
            scrollCollapse = TRUE,
            dom = 'T<"clear">Blfrtip',
            buttons=c('copy','csv','print')
          )
        )

    }) %...!% {

      shinyjs::hide(id = "visualizeQvalues")

      # Set null data table
      K2modTestFram = data.frame(Warning="No meta-variable results.")

      K2modTestFram %>%
        datatable(
          colnames = NULL,
          rownames = FALSE,
          escape = FALSE,
          selection = "single",
          options = list(
            columnDefs = list(
              list(className = 'dt-center', targets = "_all")
            ),
            dom = 'T'
          )
        )

    }

})


#############################################
#
# DIFFERENTIAL ANALYSIS RESULTS #####
#
#############################################

# Get dgeTable with selected node
observeEvent({
  input$dendro_selected
  input$portal_id
}, {

  node = input$dendro_selected;

  ## Exact the dge table
  K2Taxonomer_Tables() %...>% (function(K2Taxonomer_Tables){

    dgeTable <- K2Taxonomer_Tables %>% extract2("dgeTable")

    list(
      gse_click = FALSE,
      plot_df = NULL,
      plot_geneSet = NULL,
      dge_dat = dgeTable,
      geneSelHE = NULL,
      nodeSelHE = node,
      groupSelHE = NULL,
      directionSelHE = NULL
    )

  }) %...>% dge_dat()

})


###############################################
#
# HYPERENRICHMENT TABLE #####
#
###############################################

# Get enrTable with selected node
observeEvent({
  input$portal_id
  input$dendro_selected
}, {

  node = input$dendro_selected;

  ## Exact the dge table
  K2Taxonomer_Tables() %...>% (function(K2Taxonomer_Tables){

    enrTable <- K2Taxonomer_Tables %>% extract2("enrTable")

    list(
      dge_click = FALSE,
      plot_df = NULL,
      plot_gene = NULL,
      gse_dat = enrTable,
      gsetSelDGE = NULL,
      nodeSelDGE = node,
      groupSelDGE = NULL,
      directionSelDGE = NULL
    )

  }) %...>% gse_dat()

})


## Render differential genes table ####
output$DGE <- DT::renderDataTable({

  req(dge_dat())

  # Get the node from enrichment table
  geneSelHE = dge_dat() %>% extract("geneSelHE") %>% unlist()
  nodeSelHE = dge_dat() %>% extract("nodeSelHE") %>% unlist()
  groupSelHE = dge_dat() %>% extract("groupSelHE") %>% unlist()
  directionSelHE = dge_dat() %>% extract("directionSelHE") %>% unlist()

  # Get differential gene expression results
  dgeTable = dge_dat() %>% extract2("dge_dat")

  # Get exact match for nodeID
  if (length(geneSelHE) > 0 && nchar(geneSelHE) > 0) geneSelHE <- paste0(paste0("^", geneSelHE, "$"), collapse = "|") else geneSelHE <- NULL
  if (length(nodeSelHE) > 0 && nchar(nodeSelHE) > 0) nodeSelHE <- paste0("^", nodeSelHE, "$") else nodeSelHE <- NULL
  if (length(groupSelHE) > 0 && nchar(groupSelHE) > 0) groupSelHE <- paste0("^", groupSelHE, "$") else groupSelHE <- NULL
  if (length(directionSelHE) > 0 && nchar(directionSelHE) > 0) directionSelHE <- paste0("^", directionSelHE, "$") else directionSelHE <- NULL

  dgeTable %>%
    dplyr::arrange(Node, Group, Direction, Gene) %>% 
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
          list(className = 'dt-center', targets = 1:(ncol(dgeTable)-1))
        ),
        search = list(regex = TRUE),
        searchCols = list(
          list(search = geneSelHE),
          list(search = nodeSelHE),
          list(search = groupSelHE),
          list(search = directionSelHE),
          rep(NULL, ncol(dgeTable)-4)
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
      )
    ) %>%
    formatStyle(c("Gene", "Direction", "Mean"), `border-right` = "solid 2px")

})


## Pop-up for help ####
observeEvent(input$geneHelp, {

  shinyBS::toggleModal(session, modalId = "geneHelpShow", toggle="open")

})


## Pop-up for CSV file ####
observeEvent(input$geneDL, {

  shinyBS::toggleModal(session, modalId="geneTabDL", toggle="open")

})


## Download CSV File ####
output$downloadGeneCSV <- downloadHandler(

  filename = function() {
    paste0(input$portal_id, "-gene-expression-k2results.csv")
  },

  content = function(file) {
    write.csv(dge_dat() %>% extract2("dge_dat"), file, row.names=F)
  }

)


## Get output when a cell value is clicked ####
observeEvent({
  input$DGE_cell_clicked
}, {

  # Make sure the cell is clicked on
  req(input$DGE_cell_clicked$value)

  # Get the cell clicked value
  clickedVal = input$DGE_cell_clicked$value;

  # Get differential gene expression results
  dgeTable = dge_dat() %>% extract2("dge_dat");

  # Get gene set enrichment results
  enrTable <- gse_dat() %>% extract2("gse_dat")

  # Get Value
  dgeVal <- gsub("<label for='|'>&#9992;</label>|'>&#128202;</label>|'>&#128202;&nbsp;&#9992;</label>", "", as.character(clickedVal))

  # Check that a link was clicked
  if (grepl("PlotRow|SendRow", dgeVal)) {

    rowNum = as.numeric(gsub("PlotRow|SendRow", "", dgeVal))

    taxonomer_results() %...>% (function(K2summary){

      ##Shiny Packages####
      require(K2Taxonomer)
      require(Biobase)
      require(BiocGenerics)

      # Get gene set enrichment and pathway
      gene2Pathway = K2gene2Pathway(K2summary)
      K2res = K2results(K2summary)
      eSet = K2eSet(K2summary)
      meta = K2meta(K2summary)

      GENERow <- dgeTable[rowNum, , drop = FALSE]

      # If plotting send the node to plot, otherwise global
      geneSelDGE <- GENERow[, "Gene"]
      nodeSelDGE <- GENERow[, "Node"]
      groupSelDGE <- GENERow[, "Group"]
      directionSelDGE <- GENERow[, "Direction"]

      if(geneSelDGE %in% names(gene2Pathway)) {
        gsetSelDGE <- gene2Pathway[[geneSelDGE]]
      } else {
        gsetSelDGE <- "NO GENE SETS FOUND."
      }

      obs1 = K2res[[nodeSelDGE]]$obs[[1]];
      obs2 = K2res[[nodeSelDGE]]$obs[[2]];
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

        # update the enrichment results
        list(
          dge_click = grepl("PlotRow", dgeVal),
          plot_df = df,
          plot_gene = geneSelDGE,
          gse_dat = enrTable,
          gsetSelDGE = gsetSelDGE,
          nodeSelDGE = nodeSelDGE,
          groupSelDGE = groupSelDGE,
          directionSelDGE = directionSelDGE
        )

      }

    }) %...>% gse_dat()

  }

})


#############################################
#
# GENE EXPRESSION PLOT #####
#
#############################################

output$genePlot <- renderPlotly({

  req(gse_dat())

  dge_click = gse_dat() %>% extract("dge_click") %>% unlist()

  if(dge_click){

    # Get the plot data
    df <- gse_dat() %>% extract2("plot_df")
    gene <- gse_dat() %>% extract("plot_gene") %>% unlist()

    shinyjs::show(id="Gene-legend")

    if(any(df$Group %in% "Vehicle")){ shinyjs::show(id="Gene-Vehicle") }else{ shinyjs::hide(id="Gene-Vehicle") }

    # Plot
    p <- ggplot(data = df, aes(x = Observation, y = Expression)) +
      geom_boxplot(aes(y = Expression2, fill = Group)) +
      geom_line(aes(group = Observation)) +
      geom_point(aes(colour = Group), size = 3) +
      geom_point(aes(y = Mean), shape = 3, size = 3) +
      facet_grid(~Group2, scales = "free_x") +
      scale_colour_manual(values = c("Group 1" = "#ff8c00", "Vehicle" = "#808080", "Group 2" = "#9932cc")) +
      scale_fill_manual(values = c("Group 1" = "#ff8c00", "Vehicle" = "#808080", "Group 2" = "#9932cc")) +
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

    shinyjs::hide(id="Gene-legend")

    text = paste("\n Select a gene above \n to show observation-level expression.")

    p <- ggplot() +
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

    p

  }

})


# Render hyperenrichment table#####
output$HE <- DT::renderDataTable({

  req(gse_dat())

  # Get a list of search hits from the node
  gsetSelDGE = gse_dat() %>% extract("gsetSelDGE") %>% unlist()
  nodeSelDGE = gse_dat() %>% extract("nodeSelDGE") %>% unlist()
  groupSelDGE = gse_dat() %>% extract("groupSelDGE") %>% unlist()
  directionSelDGE = gse_dat() %>% extract("directionSelDGE") %>% unlist()

  # Exact the enrichment table
  enrTable = gse_dat() %>% extract2("gse_dat") %>% mutate(`Gene Set` = gsub("_", " ", !!!syms("Gene Set"))) 

  # Rename columns
  colnames(enrTable) <- gsub("_", "<br>", colnames(enrTable))

  # Get exact match for nodeID
  if (length(gsetSelDGE) && nchar(gsetSelDGE) > 0) gsetSelDGE <- paste0("^", gsub("; ", "$|^", gsetSelDGE), "$") else gsetSelDGE <- NULL
  if (length(nodeSelDGE) && nchar(nodeSelDGE) > 0) nodeSelDGE <- paste0("^", nodeSelDGE, "$") else nodeSelDGE <- NULL
  if (length(groupSelDGE) && nchar(groupSelDGE) > 0) groupSelDGE <- paste0("^", groupSelDGE, "$") else groupSelDGE <- NULL
  if (length(directionSelDGE) && nchar(directionSelDGE) > 0) directionSelDGE <- paste0("^", directionSelDGE, "$") else directionSelDGE <- NULL

  # Create DT object
  enrTable %>%
    dplyr::arrange(Node, Group, Direction, `Gene Set`) %>% 
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
          list(className = 'dt-center', targets = 1:(ncol(enrTable)-1))
        ),
        search = list(regex = TRUE),
        searchCols = list(
          list(search = gsetSelDGE),
          list(search = nodeSelDGE),
          list(search = groupSelDGE),
          list(search = directionSelDGE),
          rep(NULL, ncol(enrTable)-4)
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
      )
    ) %>%
    formatStyle(c("Gene Set", "Direction", "N<br>Gene Set", "Diff<br>ssGSEA"), `border-right` = "solid 2px")

})


# Pop-up for help ####
observeEvent(input$hyperHelp, {

  shinyBS::toggleModal(session, modalId = "hyperHelpShow", toggle="open")

})


# Pop-up for download ####
observeEvent(input$hyperDL, {

  shinyBS::toggleModal(session, modalId = "hyperTabDL", toggle="open")

})


# Download CSV File ####
output$downloadHyperCSV <- downloadHandler(

  filename = function() {
    paste0(input$portal_id, "-enrichment-k2results.csv")
  },

  content = function(file) {
    write.csv(gse_dat() %>% extract2("gse_dat"), file, row.names=F)
  }
)


## Observe row selection ####
observeEvent({
  input$HE_cell_clicked
}, {

  # Make sure a cell is clicked on
  req(input$HE_cell_clicked$value)

  # Get the cell clicked value
  clickedVal = input$HE_cell_clicked$value;

  # Get differential gene expression results
  dgeTable <- dge_dat() %>% extract2("dge_dat")

  # Get gene set enrichment results
  enrTable <- gse_dat() %>% extract2("gse_dat")

  # Get Value
  hyperVal <- gsub("<label for='|'>&#9992;</label>|'>&#128202;</label>|'>&#128202;&nbsp;&#9992;</label>", "", as.character(clickedVal))

  # If PlotRow then set nodeSelHE
  if (grepl("PlotRow|SendRow", hyperVal)) {

    rowNum <- as.numeric(sub("PlotRow|SendRow", "", hyperVal))

    taxonomer_results() %...>% (function(K2summary){

      ##Shiny Packages####
      require(K2Taxonomer)
      require(Biobase)
      require(BiocGenerics)

      K2res = K2results(K2summary) # Format K2 results
      genesets = K2genesets(K2summary) # Get geneset lists
      eSet = K2gSet(K2summary);
      meta = K2meta(K2summary)

      # select information of the selected row
      HYPERRow <- enrTable[rowNum, , drop = FALSE]

      # get search parameters
      geneSet = HYPERRow[, "Gene Set"]
      geneSelHE = genesets[[geneSet]]
      nodeSelHE = HYPERRow[, "Node"]
      groupSelHE = HYPERRow[, "Group"]
      directionSelHE = HYPERRow[, "Direction"]

      # Plot Pathway
      obs1 = K2res[[nodeSelHE]]$obs[[1]];
      obs2 = K2res[[nodeSelHE]]$obs[[2]];
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

      }

      list(
        gse_click = grepl("PlotRow", hyperVal),
        plot_df = df,
        plot_geneSet = geneSet,
        dge_dat = dgeTable,
        geneSelHE = geneSelHE,
        nodeSelHE = nodeSelHE,
        groupSelHE = groupSelHE,
        directionSelHE = directionSelHE
      )

    }) %...>% dge_dat()

  }

})


#############################################
#
# SINGLE-SAMPLE ENRICHEMENT #####
#
#############################################

## Render pathwayPlot####
output$pathwayPlot <- renderPlotly({

  req(dge_dat())

  gse_click = dge_dat() %>% extract("gse_click") %>% unlist()

  if(gse_click){

    df <- dge_dat() %>% extract2("plot_df")
    geneSet <- dge_dat() %>% extract2("plot_geneSet")

    shinyjs::show(id="HE-legend")

    if(any(df$Group %in% "Vehicle")){ shinyjs::show(id="HE-Vehicle") }else{ shinyjs::hide(id="HE-Vehicle") }

    # Plot
    p <- ggplot(data = df, aes(x = Observation, y = Expression)) +
      geom_boxplot(aes(y = Expression2, fill = Group)) +
      geom_line(aes(group = Observation)) +
      geom_point(aes(colour = Group), size = 3) +
      geom_point(aes(y = Mean), shape = 3, size = 3) +
      facet_grid(~Group2, scales = "free_x") +
      scale_colour_manual(values = c("Group 1" = "#ff8c00", "Vehicle" = "#808080", "Group 2" = "#9932cc")) +
      scale_fill_manual(values = c("Group 1" = "#ff8c00", "Vehicle" = "#808080", "Group 2" = "#9932cc")) +
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

    shinyjs::hide(id="HE-legend")

    text <- paste("\n Select a pathway above \n to show observation-level enrichment. \n")

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

