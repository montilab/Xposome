
##A reactive value to keep track if a gene set is selected####
marker_plot <- reactiveVal()

##Observe then marker gene set is selected#####
observeEvent(input$marker, {
  
  marker_plot(input$marker)
  
}, ignoreInit=TRUE)

##Observe then marker gene set is selected#####
observeEvent(input$marker_gene, {
  
  marker_plot(input$marker_gene)
  
}, ignoreInit=TRUE)

##Observe then marker TAS is selected#####
observeEvent(input$marker_tas, {
  
  marker_plot(input$marker_tas)
  
}, ignoreInit=TRUE)

##Observe then marker gene set is selected#####
observeEvent(input$marker_view, {
  
  marker_plot(input$marker_view)
  
}, ignoreInit=TRUE)

##Get a list of gene set#####
get_gs_eset <- function(gslist, gsname, gsmethod, annot_prof, match_id = "sig_id"){
  ind <- grep(paste0(".*", dsmap[[gsname]], ".*", gsmethod, ".*"), names(gslist))
  res <- gslist[[ind]]
  ind2 <- match(pData(res)[,match_id], annot_prof[, match_id])
  pData(res) <- annot_prof[ind2,]
  return(res)
}

##Output the gene set selection####
output$marker_gs_selection <- renderUI({
  
  req(input$marker_gsname, input$marker_gsmethod)
  
  selectInput(
    inputId = "marker_gs",
    label = "Select a gene set:",
    choices = sort(rownames(
      get_gs_eset(
        gslist = dat[["Gene Set Enrichment"]], 
        gsname = input$marker_gsname, 
        gsmethod = input$marker_gsmethod, 
        annot_prof = dat[["Profile Annotation"]],
        match_id = "sig_id")
    ))
  )
    
})

##Observe which marker gene set is selected#####
observeEvent(input$marker_gs, {

  marker_plot(input$marker_gs)
  
}, ignoreInit=TRUE)

##Observe which marker gene method is selected#####
observeEvent(input$marker_gsmethod, {
  
  marker_plot(input$marker_gsmethod)

}, ignoreInit=TRUE)

##Get a list of connectivity set#####
get_conn_eset <- function(
  connlist, 
  conn_name, 
  annot_prof,
  match_id = "sig_id"){
  
  ind <- which(names(connmap) %in% conn_name)
  res <- connlist[[ind]]
  ind2 <- match(pData(res)[, match_id], annot_prof[, match_id])
  pData(res) <- annot_prof[ind2,]
  
  return(res)
}

##Output the gene set selection####
output$marker_conn_selection <- renderUI({
  
  req(input$marker_conn_name)
  
  selectInput(
    inputId = "marker_conn",
    label = "Select a gene set:",
    choices = rownames(get_conn_eset(
      connlist = dat[["Connectivity"]], 
      conn_name = input$marker_conn_name, 
      annot_prof = dat[["Profile Annotation"]],
      match_id = "sig_id"
    ))
  )
  
})

##Observe the marker gene connectivity is selected#####
observeEvent(input$marker_conn, {
  
  marker_plot(input$marker_conn)
  
}, ignoreInit=TRUE)

##Get differential expression gene set#####
get_de_eset <- function(annot_prof, match_id = "sig_id", hm = FALSE){
  res <- dat[["Gene Expression"]]
  ind2 <- match(pData(res)[, match_id], annot_prof[, match_id])
  pData(res) <- annot_prof[ind2,]
  
  if(hm){
    inds <- fData(res)[, "Landmark Gene"] %in% "Yes"
    res <- res[inds,]
  }
  
  return(res)
}

##Function to create density and boxplot for marker explorer#####
get_de_by_gene_hist <- function(
  input, eset, annot_prof, 
  match_id = "sig_id", 
  col_id = NA, 
  col_colors = c("grey", "green", "orange"), 
  col_names = c("N/A", "-", "+"), 
  header = "mod Z-scores",
  tas = 0,
  plot = "Density"){
  
  pData(eset) <- annot_prof[match(colnames(eset), annot_prof[, match_id]),]
  eset <- eset[, eset$TAS >= tas]
  rowid <- which(rownames(eset) %in% input)[1]
  x <- as.numeric(exprs(eset)[rowid,])
  
  plot_wrapper <- function(df, plot = "Density", ...){
    
    if(plot %in% "Density") {
      
      res <- ggplot(data=df, aes_string(x = "x", fill = "cols")) + 
        geom_density(position = "identity", alpha = 0.5, ...)
      
    }else if(plot %in% "Boxplot"){
      
      res <- ggplot(data=df, aes_string(x = "cols", y= "x", fill = "cols")) + 
        geom_boxplot(
          position = "identity", 
          width = 0.2,
          alpha = 0.5, 
          outlier.fill = NULL,
          outlier.alpha = NULL
        )
      
    }
    
    return(res)
    
  }
  
  if(is.na(col_id)){
    
    p.title <- paste("Distribution of ", header, " across profiles for ", input, " (Overall)", sep = "")
    background <- as.numeric(exprs(eset))
    df <- rbind(data.frame(x = x, cols = "query"),
                data.frame(x = background, cols = "background"))
    df$cols <- factor(df$cols, levels = c("background", "query"))
    col_vec <- c("grey", "red")
    names(col_vec) <- c("background", "query")
    
    p <- plot_wrapper(df=df, plot=plot) +
      xlab(header) + 
      ylab("Count") + 
      scale_fill_manual(
        name = "Overall", 
        values = col_vec,
        breaks = names(col_vec),
        labels = names(col_vec)
      ) +
      ggtitle(p.title) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))
    
  }else{
    
    p.title <- paste("Distribution of ", header, " across profiles for ", input, " (by ", col_id, ")", sep = "")
    cols <- pData(eset)[, col_id]
    cols <- as.character(cols)
    cols_match <- col_colors
    names(cols_match) <- col_names
    df <- data.frame(x = x, cols= cols)
    df$cols <- factor(df$cols, levels = col_names)
    
    p <- plot_wrapper(df=df, plot=plot) +
      scale_fill_manual(
        name = col_id,
        values = cols_match, 
        breaks = names(cols_match), 
        labels = names(cols_match)) +
      xlab(header) + 
      ylab("Density") + 
      ggtitle(p.title) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))
    
  }
  
  return(p)
  
}

##Getting the table profile ranked by mod Z-scores for marker explorer####
get_de_by_gene_table <- function(
  input, eset, annot_prof, 
  match_id = "sig_id",  
  header = "mod Z-scores",
  tas = 0){
  
  pData(eset) <- annot_prof[match(colnames(eset), annot_prof[, match_id]),]
  eset <- eset[, eset$TAS >= tas]
  rowid <- which(rownames(eset) %in% input)[1]
  x <- as.numeric(exprs(eset)[rowid,])
  ord <- order(x, decreasing = TRUE)
  pdat <- pData(eset)
  
  df <- cbind(value = x, pdat)
  df <- df[ord,]
  
  colnames(df)[colnames(df) %in% "value"] <- header
  
  return(df)
  
}

#Output marker explorer plots#####
observeEvent(marker_plot(), {
  
  if(input$marker == "Genes"){
    
    if(is.null(input$marker_gene)) return(NULL) 

    es <- get_de_eset(annot_prof = dat[["Profile Annotation"]], match_id = "sig_id")
    markerid <- input$marker_gene
    header <- "mod Z-scores"
    
  }else if(input$marker == "Gene Sets"){   
    
    if(is.null(input$marker_gsname) | is.null(input$marker_gsmethod) | is.null(input$marker_gs)) return(NULL)
      
    es <- get_gs_eset(
      gslist = dat[["Gene Set Enrichment"]], 
      gsname = input$marker_gsname, 
      gsmethod = input$marker_gsmethod, 
      annot_prof = dat[["Profile Annotation"]],
      match_id = "sig_id"
    )
    
    markerid <- input$marker_gs
    header <- "Gene Set Scores"
    
  }else if(input$marker == "CMap Connectivity"){
    
    if(is.null(input$marker_conn_name) | is.null(input$marker_conn)) return(NULL)
    
    es <- get_conn_eset(
      connlist = dat[["Connectivity"]], 
      conn_name = input$marker_conn_name, 
      annot_prof = dat[["Profile Annotation"]],
      match_id = "sig_id"
    )
    
    markerid <- input$marker_conn
    header <- "Connectivity Score (Percentile)"
    
  }
  
  tas <- input$marker_tas
  plot <- input$marker_view
  es <- es[, pData(es)[, "TAS"] > tas]
    
  p1 <- get_de_by_gene_hist(
    input = markerid, 
    eset=es,
    annot_prof = dat[["Profile Annotation"]], 
    match_id = "sig_id",
    col_id = NA, 
    header = header,
    tas = tas,
    plot = plot
  )
  
  p2 <- get_de_by_gene_hist(
    input = markerid, 
    eset=es,
    annot_prof = dat[["Profile Annotation"]], 
    match_id = "sig_id",
    col_id = "Carcinogenicity", 
    col_colors = c("grey","green", "orange"), 
    col_names = c("N/A","-", "+"),
    header = header,
    tas = tas,
    plot = plot
  )
  
  p3 <- get_de_by_gene_hist(
    input = markerid, 
    eset=es,
    annot_prof = dat[["Profile Annotation"]], 
    match_id = "sig_id",
    col_id = "Genotoxicity", 
    col_colors = c("grey","pink", "purple"), 
    col_names = c("N/A", "-", "+"),
    header = header,
    tas = tas,
    plot = plot
  )
  
  ##Define the width and height of the plot
  if(plot %in% "Density"){
    w = 1000; h=300
  }else if(plot %in% "Boxplot"){
    w = 600; h=400
  }
  
  ##Output the first plot#####
  output$marker_plot_1 <- renderPlot({
    p1
  })
  
  #Output the second plots#####
  output$marker_plot_2 <- renderPlot({
    p2
  })
  
  #Output the third plots#####
  output$marker_plot_3 <- renderPlot({
    p3
  })
  
  ##Download pdf format#####
  output$marker_download_pdf <- downloadHandler(
    filename = paste0("Carcinogenome_download_", header, "_", markerid, ".pdf"),
    content = function(file){
      ggsave(
        file, device = "pdf", grid.arrange(p1, p2, p3),
        width = w/130, height = h/35, 
        dpi = 300
      )
    }
  )
  
  ##Download png format#####
  output$marker_download_png <- downloadHandler(
    filename = paste0("Carcinogenome_download_", header, "_", markerid, ".png"),
    content = function(file){
      ggsave(
        file, device = "png", grid.arrange(p1, p2, p3),
        width = w/130, height = h/35, 
        dpi = 300
      )
    }
  )
  
  ##Output the marker table header####
  output$marker_table_header <- renderUI({
    h3(paste0("Table of profiles ranked by ", header))
  })
  
  ##Output the marker table####
  output$marker_table <-  DT::renderDataTable({
  
    data.table.round(
      get_de_by_gene_table(
        input=markerid, 
        eset=es, 
        annot_prof = dat[["Profile Annotation"]], 
        match_id = "sig_id",  
        header = header,
        tas = tas
      )
    )
    
  }, escape = FALSE, extensions = 'Buttons', server = TRUE, colnames = c('Entry'=1), class = "display",
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
    buttons=c('copy','csv','print'))
  )
  
}, ignoreInit=TRUE)
  
  
  