
# Create reactive values ####
search_results <- reactiveVal(NULL)
dendro_dat <- reactiveVal(NULL)
dge_dat <- reactiveVal(NULL)
gse_dat <- reactiveVal(NULL)
selCov <- reactiveVal(NULL)
qvalues <- reactiveVal("No")
pathSel <- reactiveVal("A")

## Get annotation variables
observeEvent(input$main_page, {
  
  req(input$main_page %in% "K2 Taxanomer Results")
  
  ## Set select input options for annotations bar
  taxonomer_results() %...>% extract2("options") %...>% extract2("varOptions") %...>% {
    varOptions <- .
    updateSelectInput(session, inputId="selCov", choices=c("Add Annotation to Heatmap:" = "", "RESET" = "RESET", varOptions))
  }
  
  return(NULL)
  
}, ignoreInit=TRUE)

#############################################
#
# MEMBER SEARCH SECTION ####
#
#############################################

observeEvent(input$search_string, {

  mstring=input$mstring
  
  search_results(NULL)

  promise_all(taxonomer_results=taxonomer_results(), profile_dat=profile_dat()) %...>% with({
    
    infoMat <- taxonomer_results %>% extract2("infoMat") 
    chemicalMat <- infoMat[,which(colnames(infoMat) %in% colnames(profile_dat))]
    
    # Find matches in the terminal leafs
    infoInd <- unique(which(`dim<-`(grepl(mstring, chemicalMat, ignore.case = T), dim(chemicalMat)), arr.ind=TRUE)[,1])
    
    # Generate string to show
    showString <- sapply(infoInd, function(row) paste0(paste(paste0(colnames(chemicalMat), ": ", chemicalMat[row,]), collapse = ";\n"), "\n"))
    
    # Get number of matches
    nMatches <- paste0(length(infoInd), " matche(s)")
    valueString <- c("A", rownames(chemicalMat)[infoInd])
    names(valueString) <- c(nMatches, showString)
    valueString
    
  }) %...>% search_results()
  
  return(NULL)
    
}, ignoreInit=TRUE)


# Show matches for the search string#######
output$Match <- renderUI({

  req(search_results())

  mstring=isolate({ input$mstring })
  label=paste0("Results for <em style='color: red;'>", mstring, "</em>:")
  
  selectInput(inputId = "mVal", label = HTML(label), choices = search_results(), selectize = TRUE, width = "100%")

})

# Observe the selection of search string results#####
observeEvent(input$mVal, {
  
  mVal=input$mVal
  
  pathSel(NULL)
  
  taxonomer_results() %...>% extract2("vNetOut") %...>% {
    vNetOut <- .
    sel <- as.character(vNetOut$x$nodes$id[grep(paste0("^", mVal, "<br>|<br>", mVal, "<br>|", mVal, "$"), vNetOut$x$nodes$title)])
    
    if(length(sel) == 0) {
      "A"
    }else{
      sel
    }
  } %...>% pathSel()
  
  return(NULL)
  
}, ignoreInit=TRUE)


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

# Render Dendrogram #####
output$dendro <- renderVisNetwork({

  selected=pathSel(); qvalues=qvalues();
  
  taxonomer_results() %...>% {
    
    if(qvalues=="No"){
      vNetOut <- .[["vNetOut"]]
    }else{
      vNetOut <- .[["vNetOut_qvalues"]]
    }

    vNetOut %>%
      visOptions(
        autoResize = T,
        height = "100%",
        nodesIdSelection = list(
          enabled = TRUE,
          values = vNetOut$x$nodes$id,
          main = "Node ID",
          style = 'width: 100px; height: 25px;',
          selected = selected
        ),
        highlightNearest = list(
          enabled = TRUE,
          algorithm = "hierarchical",
          degree = 1E10
        )
      ) %>%
      visNodes(
        label = "Select a node:",
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

  }
})

#Observe node selected in dendrogram####
observeEvent(input$dendro_selected, {
  
  node = input$dendro_selected
  
  ## reset the dge click button
  dge_dat(
    list(
      dge_click="No"
    )
  )
  
  ## reset the gse click button
  gse_dat(
    list(
      gse_click="No"
    )
  )

  if(is.null(node) | node == ""){

    shinyjs::hide(id = "selCov")
    shinyjs::hide(id = "viewAll")
    dendro_dat(
      list(
        selection="No",
        node=NULL
      )
    )

  }else{

    pathSel(node)

    promise_all(taxonomer_results=taxonomer_results(), profile_dat=profile_dat()) %...>% with({
      
      info <- taxonomer_results %>% extract2("info")
      K2res <- taxonomer_results %>% extract2("K2res")
      labs <- taxonomer_results %>% extract2("options") %>% extract2("labs")

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
        
        # infoSub with with reduce column names
        infoSub_reduce <- infoSub[, which(colnames(infoSub) %in% c("Group", colnames(profile_dat)))]

        # Getting bootstrap summary stat
        bootProb <- K2res[[node]]$bootP
        nodeStab <- K2res[[node]]$stability$node
        sampStab <- K2res[[node]]$stability$clusters
        outBoot <- paste0("<br> Bootstrap Probability: <b>", bootProb, "</b>")
        outNode <- paste0("<br> Node Stability: <b>", signif(nodeStab, 2), "</b> <br>")
        outStab <- paste0("Cluster Stability: <br> &emsp; Group 1: <b>", signif(sampStab[1], 2), "</b> &emsp; Group 2: <b>",  signif(sampStab[2], 2))
        stat <- paste(outBoot, outNode, outStab)

        shinyjs::show(id = "selCov")
        shinyjs::show(id = "viewAll")
        
        list(
          selection="Yes",
          node=node,
          samp_stab=samp_stab,
          colrowAnnot=colrowAnnot,
          colSidePalette=colSidePalette,
          info=info,
          infoSub=infoSub,
          infoSub_reduce=infoSub_reduce,
          stat=stat
        )
        
      }else{
        
        shinyjs::hide(id = "selCov")
        shinyjs::hide(id = "viewAll")
        
        list(
          selection="No",
          node=NULL
        )
        
      }
    
    }) %...>% dendro_dat()
    
  }
  
  return(NULL)
  
}, ignoreInit = TRUE)

#############################################
#
# CLUSTER SECTION - STABILITY TAB #####
#
#############################################

##Observe the selections of covariates#####
observeEvent(input$selCov, {
  
  selection <- input$selCov; selCov <- selCov();
  
  taxonomer_results() %...>% extract2("info") %...>% {
    
    info <- .
    
    if(selection %in% colnames(info)){
      new_selection <- unique(c(selCov, selection)) #If selections are made add colors
    }else{
      new_selection <- NULL
    }
    
    return(new_selection)
    
  } %...>% selCov()
  
  return(NULL)
  
}, ignoreInit = TRUE)
    
##Getting the heatmap data####
hm_dat <- reactive({
  
  req(dendro_dat())
  
  selCov <- selCov(); selected=dendro_dat() %>% extract2("selection"); 
  
  if(selected %in% c("No")){   
    
    return(NULL)
    
  }else{
    
    samp_stab <- dendro_dat() %>% extract2("samp_stab")
    colrowAnnot <- dendro_dat() %>% extract2("colrowAnnot")
    colSidePalette <- dendro_dat() %>% extract2("colSidePalette")  
    
    if(!is.null(selCov)){
      
      info <- dendro_dat() %>% extract2("info")  
      
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
      
      # Create pallette of all possible factors
      colSidePalette <- c(colSidePalette, "#000000", "#808080", rep("#D3D3D3", length(selCov)))
      names(colSidePalette) <- c(colValues, "Group:1", "Group:2", paste0(selCov, ":NA"))
      
    }
    
    list(
      samp_stab=samp_stab,
      colrowAnnot=colrowAnnot,
      colSidePalette=colSidePalette
    )
    
  }

})

#############################################
#
# HEATMAP #####
#
#############################################
output$heatmapPlot <- renderPlotly({
  
  if(!is.null(hm_dat())){
    
    samp_stab <- hm_dat() %>% extract2("samp_stab")
    colrowAnnot <- hm_dat() %>% extract2("colrowAnnot")
    colSidePalette <- hm_dat() %>% extract2("colSidePalette")  
    
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
      showticklabels = FALSE,
      width = paste0(input$dimension[1]*0.5, "px")
    ) 
    
    # Remove legend
    hm$x$layout$showlegend <- FALSE
    
    # Change value to cosine similarity
    whHeatmap <- which(unlist(lapply(hm$x$data, function(x) x$type == "heatmap"))) # Get heatmap index
    hm$x$data[[whHeatmap]]$text <- sub("value:", "cos similarity:", hm$x$data[[whHeatmap]]$text)
    
  }else{
    
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
    
    hm <- hm %>% ggplotly(width = 0.5*as.numeric(input$dimension[1])) %>% layout(hoverlabel = list(bgcolor="white"))
    
  }
  
  return(hm)
  
})

#Render stability statistics####
output$stabStats <- renderUI({

  req(dendro_dat())

  dendro_dat() %>% extract2("stat") %>% HTML()

})

##Observe the selection of view
info_tab_dat <- reactive({
  
  req(dendro_dat())
  
  viewAll=input$viewAll; selected=dendro_dat() %>% extract2("selection");
  
  if(!selected %in% "No"){
    
    if(viewAll %in% FALSE){
      dendro_dat() %>% extract2("infoSub_reduce") %>% data.table.round()
    }else{
      dendro_dat() %>% extract2("infoSub") %>% data.table.round()
    }
    
  }else{
    
    return(NULL)
    
  }
  
})

#############################################
#
# CLUSTER SECTION - GROUP MEMBER TAB #####
#
#############################################

##Render table of information for each node####
output$infoTab <- renderDataTable({
  
  if(!is.null(info_tab_dat())){
    
    info_tab_dat() %>% 
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
      )
    
  }else{
    
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

# Generate table of meta-variable tests####
output$metaVarTab <- renderDataTable({

  taxonomer_results() %...>% extract2("K2modTestFram") %...>% {
    
    K2modTestFram <- .
  
    if(!is.null(K2modTestFram)){
      
      K2modTestFram %>% datatable(
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
      
    } else {
      
      # Set null data table
      K2modTestFramNULL <- data.frame("No meta-variable results.");
      colnames(K2modTestFramNULL) <- NULL
      
      K2modTestFramNULL %>% datatable(
        rownames = FALSE,
        escape = FALSE,
        extensions = 'Buttons',
        selection = "single",
        options = list(
          columnDefs = list(
            list(className = 'dt-center', targets = "_all")
          ),
          deferRender = FALSE,
          dom = 'T'
        )
      )
      
    }
  }
})


# Clicks to visualize dendrogram by Q-values####
observeEvent(input$visualizeQvalues, {

  qvalues("Yes")

}, ignoreInit = TRUE)

# Reset the dendrogram ####
observeEvent(input$resetQvalues,  {

  qvalues("No")

}, ignoreInit = TRUE)


#############################################
#
# DIFFERENTIAL ANALYSIS RESULTS #####
#
#############################################

# Render gene table results####
output$DGE <- DT::renderDataTable({
  
  req(dendro_dat())
  
  gse_click = gse_dat() %>% extract2("gse_click") %>% unlist()
  
  if(gse_click == "Yes"){
    
    # Get the node from enrichment table
    nodeID = gse_dat() %>% extract("nodeSelHE") %>% unlist()
    geneList = gse_dat() %>% extract("geneList")  %>% unlist()
    groupSelHE = gse_dat() %>% extract("groupSelHE")  %>% unlist()
    
  }else{ 
    
    nodeID = dendro_dat() %>% extract("node") %>% unlist() 
    geneList = NULL
    groupSelHE = NULL
    
  }

  # Exact the dge table
  taxonomer_results() %...>% extract2("dgeTable") %...>% {
    
    # Get the dge table
    DGETABLE <- .
    
    # Get exact match for nodeID
    if (!is.null(nodeID)) nodeID <- paste0("^", nodeID, "$") 
    if (!is.null(geneList)) geneList <- paste(paste0("^", geneList, "$"), collapse = "|")
    if (!is.null(groupSelHE)) groupSelHE <- paste0("^", groupSelHE, "$") 
    
    # Create data table object
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
          list(search = nodeID),
          list(search = groupSelHE),
          rep(NULL, ncol(DGETABLE)-3)
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
      )) %>%
      formatRound(c("Mean", "Diff"), digits = 2) %>%
      formatSignif(c("P Value", "FDR"), digits = 2) %>%
      formatStyle(c("Gene", "Direction", "Mean"), `border-right` = "solid 2px")
    
  }
})

# Functions for help and download the data ####
geneHelpShow <- function() {
  div(
    id = "geneHelp",
    modalDialog(
      "Use ^SEARCHTERM$ to filter for exact matches in columns",
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

## Pop-up for help ####
observeEvent(input$geneHelp, {
  
  showModal(geneHelpShow())
  
}, ignoreInit = TRUE)

# Download CSV File ####
output$downloadGeneCSV <- downloadHandler(
  
  filename = function() {
    paste(fname, "-generesults-", Sys.Date(), ".csv", sep="")
  },
  
  content = function(file) {
    taxonomer_results() %...>% extract2("dgeTable") %...>% {
      dgeTable <- .
      write_csv(dgeTable, file)
    }
  }
  
)

# Pop-up for CSV file ####
observeEvent(input$geneDL, {
  
  showModal(geneTabDL())
  
}, ignoreInit = TRUE)

# Get output when a cell value is clicked ####
observeEvent(input$DGE_cell_clicked, {
  
  req(input$DGE_cell_clicked$value)
  
  # Get Value
  dgeVal <- gsub("<label for='|'>&#9992;</label>|'>&#128202;</label>", "", as.character(input$DGE_cell_clicked$value))
  
  # Check that a link was clicked
  if (grepl("PlotRow|SendRow", dgeVal)) {
    
    rowNum <- as.numeric(sub("PlotRow|SendRow", "", dgeVal))
    
    taxonomer_results() %...>% {
      
      gene2Pathway = .[["gene2Pathway"]]
      K2res = .[["K2res"]];
      eSet = .[["eSet"]];
      meta = .[["meta"]]
      dgeTable = .[["dgeTable"]]
      
      GENERow <- dgeTable[rowNum, , drop = FALSE]
      
      # If plotting send the node to plot, otherwise global
      gene <- GENERow[, "Gene"]
      nodeSelDGE <- GENERow[, "Node"]
      groupDGE <- GENERow[, "Group"]
      
      if(gene %in% names(gene2Pathway)) {
        dgeHits <- gene2Pathway[[gene]]
      } else {
        dgeHits <- "NO GENE SETS FOUND."
      }
      
      obs1 = K2res[[nodeSelDGE]]$obs[[1]];
      obs2 = K2res[[nodeSelDGE]]$obs[[2]];
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
        dge_click="Yes",
        df=df,
        gene=gene,
        nodeSelDGE=nodeSelDGE,
        groupDGE=groupDGE,
        dgeHits=dgeHits
      )
      
    } %...>% dge_dat()
    
  }
}, ignoreInit = TRUE)

#############################################
#
# GENE EXPRESSION #####
#
#############################################

output$genePlot <- renderPlotly({
  
  req(dge_dat())
  
  dge_click = dge_dat() %>% extract2("dge_click") %>% unlist()
  
  if(dge_click == "Yes"){
    
    df <- dge_dat() %>% extract2("df")
    gene <- dge_dat() %>% extract2("gene")
    
    # Plot
    p <- ggplot(data = df, aes(x = Observation, y = Expression)) +
      geom_boxplot(aes(y = Expression2, fill = Group)) +
      geom_line(aes(group = Observation)) +
      geom_point(aes(colour = Group), size = 3) +
      geom_point(aes(y = Mean), shape = 3, size = 3) +
      facet_grid(~Group2, scales = "free_x") +
      scale_colour_manual(values = c("Group 1" = "darkorange", "Vehicle" = "grey", "Group 2" = "darkorchid1")) +
      scale_fill_manual(values = c("Group 1" = "darkorange", "Vehicle" = "grey", "Group 2" = "darkorchid1")) +
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
    
    p <- p %>% ggplotly(width = input$dimension[1]) %>% layout(hoverlabel = list(bgcolor="white")) 
    
  }
  
  return(p)
  
})

#############################################
#
# HYPERENRICHMENT TABLE #####
#
#############################################

# Render hyperenrichment table#####
output$HE <- DT::renderDataTable({

  req(dendro_dat())
  
  dge_click = dge_dat() %>% extract2("dge_click") %>% unlist()
  
  if(dge_click == "Yes"){
    
    # Get the node from enrichment table
    nodeID = dge_dat() %>% extract("nodeSelDGE") 
    
    # Get a list of group from the node
    groupID = dge_dat() %>% extract("groupDGE") %>% unlist()
    
    # Get a list of dge hits from the node
    dgeHits = dge_dat() %>% extract("dgeHits") %>% unlist()
    
  }else{
    
    # Get the node from enrichment table
    nodeID = dendro_dat() %>% extract2("node") %>% unlist()
    
    # Get a list of group from the node
    groupID = NULL
    
    # Get a list of dge hits from the node
    dgeHits = NULL
    
  } 

  # Exact the dge table
  taxonomer_results() %...>% extract2("enrTable") %...>% {
    
    # Get the gse table
    ENRTABLE <- .

    # Add line breaks
    ENRTABLE <- ENRTABLE %>% mutate(`Gene Set` = gsub("_", " ", !!!syms("Gene Set"))) 
    colnames(ENRTABLE) <- gsub("_", "<br>", colnames(ENRTABLE))
    
    # Get exact match for nodeID
    if (!is.null(nodeID)) nodeID <- paste0("^", nodeID, "$")
    if (!is.null(dgeHits)) dgeHits <- paste0("^", gsub("; ", "$|^", dgeHits), "$")
    if (!is.null(groupID)) groupID <- paste0("^", groupID, "$")
    
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
          list(visible = FALSE, targets=c(12, 13))
        ),
        search = list(regex = TRUE),
        searchCols = list(
          list(search = dgeHits),
          list(search = nodeID),
          list(search = groupID),
          rep(NULL, ncol(ENRTABLE)-3)
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
      )) %>%
      formatRound(c("Mean<br>ssGSEA", "Diff<br>ssGSEA"), digits = 2) %>%
      formatSignif(c("P Value<br>Hyper", "FDR<br>Hyper", "P Value<br>ssGSEA", "FDR<br>ssGSEA"), digits = 2)%>%
      formatStyle(c("Gene Set", "Direction", "N<br>Gene Set", "Diff<br>ssGSEA"), `border-right` = "solid 2px")
    
  }
})

# Functions for help
hyperHelpShow <- function() {
  div(
    id = "hyperHelp",
    modalDialog(
      "Use ^SEARCHTERM$ to filter for exact matches in columns",
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
    paste(fname, "-enrresults-", Sys.Date(), ".csv", sep="")
  },
  
  content = function(file) {
    
    taxonomer_results() %...>% extract2("enrTable") %...>% {
      enrTable <- .
      write_csv(enrTable, file)
    }
    
  }
)

# Pop-up for download ####
observeEvent(input$hyperDL, {
  
  showModal(hyperTabDL())
  
}, ignoreInit = TRUE)

# Observe row selection ####
observeEvent(input$HE_cell_clicked, {
  
  req(input$HE_cell_clicked$value)
  
  # Get Value
  hyperVal <- gsub("<label for='|'>&#9992;</label>|'>&#128202;</label>", "", as.character(input$HE_cell_clicked$value))
  
  # If PlotRow then set nodeSelHE
  if (grepl("PlotRow|SendRow", hyperVal)) {
    
    rowNum <- as.numeric(sub("PlotRow|SendRow", "", hyperVal))
    
    taxonomer_results() %...>% {
      
      genesets = .[["genesets"]]
      K2res = .[["K2res"]];
      eSet = .[["gSet"]];
      meta = .[["meta"]]
      enrTable = .[["enrTable"]]
      
      HYPERRow <- enrTable[rowNum, , drop = FALSE]
      
      geneSet=HYPERRow[, "Gene Set"]
      nodeSelHE=HYPERRow[, "Node"]
      groupSelHE=HYPERRow[, "Group"]
      dirSelHE=HYPERRow[, "Direction"]
      Hits=strsplit(HYPERRow[, "Hits"], ",")[[1]]
      geneList=genesets[[geneSet]]
      
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
        gse_click="Yes",
        df=df,
        geneSet=geneSet,
        nodeSelHE=nodeSelHE,
        groupSelHE=groupSelHE,
        dirSelHE=dirSelHE,
        Hits=Hits,
        geneList=geneList
      )
      
    } %...>% gse_dat()
    
  }
}, ignoreInit = TRUE)

#############################################
#
# SINGLE-SAMPLE ENRICHEMENT #####
#
#############################################

# Render pathwayPlot####
output$pathwayPlot <- renderPlotly({
  
  req(gse_dat())
  
  gse_click = gse_dat() %>% extract2("gse_click") %>% unlist()
  
  if(gse_click == "Yes"){
    
    df <- gse_dat() %>% extract2("df")
    geneSet <- gse_dat() %>% extract2("geneSet")
    
    # Plot
    p <- ggplot(data = df, aes(x = Observation, y = Expression)) +
      geom_boxplot(aes(y = Expression2, fill = Group)) +
      geom_line(aes(group = Observation)) +
      geom_point(aes(colour = Group), size = 3) +
      geom_point(aes(y = Mean), shape = 3, size = 3) +
      facet_grid(~Group2, scales = "free_x") +
      scale_colour_manual(values = c("Group 1" = "darkorange", "Vehicle" = "grey", "Group 2" = "darkorchid1")) +
      scale_fill_manual(values = c("Group 1" = "darkorange", "Vehicle" = "grey", "Group 2" = "darkorchid1")) +
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
    
    p <- p %>% ggplotly(width = input$dimension[1]) %>% layout(hoverlabel = list(bgcolor="white")) 
    
  }
  
  return(p)
  
})



