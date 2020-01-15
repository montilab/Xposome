
##Get a list of gene set#####
get_gs_eset <- function(gslist, gsname, gsmethod, annot_prof, match_id = "sig_id"){
  ind <- grep(paste0(".*", dsmap[[gsname]], ".*", gsmethod, ".*"), names(gslist))
  res <- gslist[[ind]]
  ind2 <- match(pData(res)[,match_id], annot_prof[, match_id])
  pData(res) <- annot_prof[ind2,]
  return(res)
}

##Output the gene set selection####
observeEvent(c(input$marker_gsname, input$marker_gsmethod), {
  
  updateSelectInput(
    session,
    inputId = "marker_gs",
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
observeEvent(input$marker_conn_name, {
  
  updateSelectInput(
    session,
    inputId = "marker_conn",
    choices = rownames(get_conn_eset(
      connlist = dat[["Connectivity"]], 
      conn_name = input$marker_conn_name, 
      annot_prof = dat[["Profile Annotation"]],
      match_id = "sig_id"
    ))
  )
  
})

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
    
    p.title <- paste("Distribution of ", header, " across profiles\n for ", input, " (Overall)", sep = "")
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
    
    p.title <- paste("Distribution of ", header, " across profiles\n for ", input, " (by ", col_id, ")", sep = "")
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

##Create reactive marker data#####
marker_data <- reactiveVal(NULL); 
marker_id <- reactiveVal(NULL);
marker_header <- reactiveVal(NULL);
marker_key <- reactiveVal(NULL);

##Observe event when a button is clicked####
observeEvent(input$de_generate, {
  
  req(input$marker, input$marker_tas, input$marker_view, input$marker_gene)
  
  es <- get_de_eset(annot_prof = dat[["Profile Annotation"]], match_id = "sig_id")
  es <- es[, pData(es)[, "TAS"] > input$marker_tas]
  
  marker_data(es)
  marker_id(input$marker_gene)
  marker_header("mod Z-scores")
  marker_key(paste0(session$clientData$url_search, "_", input$marker, "_", input$marker_tas, "_", input$marker_view, "_", input$marker_gene))
  
})

##Observe event when a button is clicked####
observeEvent(input$es_generate, {
  
  req(input$marker, input$marker_tas, input$marker_view, input$marker_gsname, input$marker_gsmethod, input$marker_gs)
  
  es <- get_gs_eset(
    gslist = dat[["Gene Set Enrichment"]],
    gsname = input$marker_gsname,
    gsmethod = input$marker_gsmethod,
    annot_prof = dat[["Profile Annotation"]],
    match_id = "sig_id"
  )
  es <- es[, pData(es)[, "TAS"] > input$marker_tas]
  
  marker_data(es)
  marker_id(input$marker_gs)
  marker_header("Gene Set Scores")
  marker_key(paste0(session$clientData$url_search, "_", input$marker, "_", input$marker_tas, "_", input$marker_view, "_", input$marker_gsname, "_", input$marker_gsmethod, "_", input$marker_gs))
  
})

##Observe event when a button is clicked####
observeEvent(input$conn_generate, {
  
  req(input$marker, input$marker_tas, input$marker_view, input$marker_conn_name, input$marker_conn)
  
  es <- get_conn_eset(
    connlist = dat[["Connectivity"]],
    conn_name = input$marker_conn_name,
    annot_prof = dat[["Profile Annotation"]],
    match_id = "sig_id"
  )
  es <- es[, pData(es)[, "TAS"] > input$marker_tas]
  
  marker_data(es)
  marker_id(input$marker_conn)
  marker_header("Connectivity Score (Percentile)")
  marker_key(paste0(session$clientData$url_search, "_", input$marker, "_", input$marker_tas, "_", input$marker_view, "_", input$marker_conn_name, "_", input$marker_conn))
  
})

##Output the first plot#####
output$marker_plot_1 <- renderCachedPlot({
  
  req(marker_data(), marker_id(), marker_header(), marker_key(), input$marker_tas, input$marker_view)
  
  get_de_by_gene_hist(
    input = marker_id(),
    eset = marker_data(),
    annot_prof = dat[["Profile Annotation"]],
    match_id = "sig_id",
    col_id = NA,
    header = marker_header(),
    tas = input$marker_tas,
    plot = input$marker_view
  )
  
}, cacheKeyExpr = { list(marker_key()) })

##Output the first plot#####
output$marker_plot_2 <- renderCachedPlot({
  
  req(marker_data(), marker_id(), marker_header(), marker_key(), input$marker_tas, input$marker_view)
  
  get_de_by_gene_hist(
    input = marker_id(),
    eset = marker_data(),
    annot_prof = dat[["Profile Annotation"]],
    match_id = "sig_id",
    col_id = "Carcinogenicity",
    col_colors = c("grey","green", "orange"),
    col_names = c("N/A","-", "+"),
    header = marker_header(),
    tas = input$marker_tas,
    plot = input$marker_view
  )
  
}, cacheKeyExpr = { list(marker_key()) })

##Output the first plot#####
output$marker_plot_3 <- renderCachedPlot({
  
  req(marker_data(), marker_id(), marker_header(), marker_key(), input$marker_tas, input$marker_view)
  
  get_de_by_gene_hist(
    input = marker_id(),
    eset = marker_data(),
    annot_prof = dat[["Profile Annotation"]],
    match_id = "sig_id",
    col_id = "Genotoxicity",
    col_colors = c("grey","pink", "purple"),
    col_names = c("N/A", "-", "+"),
    header = marker_header(),
    tas = input$marker_tas,
    plot = input$marker_view
  )
  
}, cacheKeyExpr = { list(marker_key()) })

##Define the width and height of the plot
w <- reactiveVal(NULL); h <- reactiveVal(NULL);

observeEvent(input$marker_view, {
  
  req(input$marker_view)
  
  if(input$marker_view %in% "Density"){
    w(1000); h(300)
  }else if(input$marker_view %in% "Boxplot"){
    w(600); h(400)
  }
  
})

##Download pdf format#####
output$marker_download_pdf <- downloadHandler(
  
  filename = paste0("Carcinogenome_download_", marker_header(), "_", marker_id(), ".pdf"),
  
  content = function(file){
    
    p1 <- get_de_by_gene_hist(
      input = marker_id(),
      eset = marker_data(),
      annot_prof = dat[["Profile Annotation"]],
      match_id = "sig_id",
      col_id = NA,
      header = marker_header(),
      tas = input$marker_tas,
      plot = input$marker_view
    )
    
    p2 <- get_de_by_gene_hist(
      input = marker_id(),
      eset = marker_data(),
      annot_prof = dat[["Profile Annotation"]],
      match_id = "sig_id",
      col_id = "Carcinogenicity",
      col_colors = c("grey","green", "orange"),
      col_names = c("N/A","-", "+"),
      header = marker_header(),
      tas = input$marker_tas,
      plot = input$marker_view
    )
    
    p3 <- get_de_by_gene_hist(
      input = marker_id(),
      eset = marker_data(),
      annot_prof = dat[["Profile Annotation"]],
      match_id = "sig_id",
      col_id = "Genotoxicity",
      col_colors = c("grey","pink", "purple"),
      col_names = c("N/A", "-", "+"),
      header = marker_header(),
      tas = input$marker_tas,
      plot = input$marker_view
    )
    
    ggsave(
      file, device = "pdf", grid.arrange(p1, p2, p3),
      width = w()/130, height = h()/35,
      dpi = 300
    )
    
  }
)

##Download png format#####
output$marker_download_png <- downloadHandler(
  
  filename = paste0("Carcinogenome_download_", marker_header(), "_", marker_id(), ".png"),
  
  content = function(file){
    
    p1 <- get_de_by_gene_hist(
      input = marker_id(),
      eset = marker_data(),
      annot_prof = dat[["Profile Annotation"]],
      match_id = "sig_id",
      col_id = NA,
      header = marker_header(),
      tas = input$marker_tas,
      plot = input$marker_view
    )
    
    p2 <- get_de_by_gene_hist(
      input = marker_id(),
      eset = marker_data(),
      annot_prof = dat[["Profile Annotation"]],
      match_id = "sig_id",
      col_id = "Carcinogenicity",
      col_colors = c("grey","green", "orange"),
      col_names = c("N/A","-", "+"),
      header = marker_header(),
      tas = input$marker_tas,
      plot = input$marker_view
    )
    
    p3 <- get_de_by_gene_hist(
      input = marker_id(),
      eset = marker_data(),
      annot_prof = dat[["Profile Annotation"]],
      match_id = "sig_id",
      col_id = "Genotoxicity",
      col_colors = c("grey","pink", "purple"),
      col_names = c("N/A", "-", "+"),
      header = marker_header(),
      tas = input$marker_tas,
      plot = input$marker_view
    )
    
    ggsave(
      file, device = "png", grid.arrange(p1, p2, p3),
      width = w()/130, height = h()/35,
      dpi = 300
    )
    
  }
)


##Output the marker table header####
output$marker_table_header <- renderUI({
  
  req(marker_header())
  
  h3(paste0("Table of profiles ranked by ", marker_header()))
  
})

##Output the marker table####
output$marker_table <-  DT::renderDataTable({

  req(marker_data(), marker_id(), marker_header(), input$marker_tas)
  
  data.table.round(
    get_de_by_gene_table(
      input = marker_id(),
      eset = marker_data(),
      annot_prof = dat[["Profile Annotation"]],
      match_id = "sig_id",
      header = marker_header(),
      tas = input$marker_tas
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


