
##Function to get morpheus link for heatmap####
get_morpheus_link <- function(url = "toy.gct", domain = "https://carcinogenome.org/data", tas = 0){
  
  url <- sprintf('{
	"dataset" : "%s/%s",
	"columns" : [ { "field" : "id", "display" : ["color"] },
				  { "field" : "Chemical Name", "display" : ["color"] },
				  { "field" : "CAS", "display" : ["color"] },
				  { "field" : "dose (uM)", "display" : ["color"] },
				  { "field" : "Carcinogenicity", "display" : ["color"] },
				  { "field" : "Genotoxicity", "display" : ["color"] },
			    { "field" : "TAS", "display" : ["color"] } ],
	"rows" : [ { "field" : "genesets", "display" : ["text"] }],
	"columnFilter" : {
		"filters" : [{
		"field" : "TAS",
		"type" : "range",
		"min" : %f,
		"max" : 1.0
		}]
		}
	}', domain, url, tas)
  
  url <- URLencode(URL = url)
  url <- paste("https://software.broadinstitute.org/morpheus/?json=", url, sep = "")
  return(url)
}

##Function to get heatmap####
get_heatmap_gct <- function(ds, dsmap, method, domain){
  dsname <- dsmap[[ds]]
  url <- paste(dsname, "_", method, ".gct", sep = "")
}

##Show morpheus link###
show_morpheus <- reactiveVal(FALSE)

#Output the morpheus result link to heatmap####
output$morpheus_result_link <- renderUI({
  
  req(show_morpheus() %in% TRUE, input$marker_tas_hm, input$marker_gsname_hm, input$marker_gsmethod_hm)
  
  HTML(
    paste0(
      '<a target="_blank" href="',
      get_morpheus_link(
        url = get_heatmap_gct(
          ds = isolate({ input$marker_gsname_hm }), 
          dsmap = dsmap, 
          method = isolate({ input$marker_gsmethod_hm })
          ), 
        domain = domain, tas = isolate({ input$marker_tas_hm })
      ),
      '">', 'Open interactive heatmap in Morpheus', 
      '</a>'
    )
  )
  
})

#Function to covert color to hex color codes####
to.hex <- function(x){
  cols <- col2rgb(x)
  red <- cols[1]
  green <- cols[2]
  blue <- cols[3]
  return(rgb(red, green, blue, maxColorValue = 255))
}

#For testing purposes####
# source("ggheat.continuous.R")
# dat <- readRDS(paste0("data/HEPG2/data.RDS"))
# dat <- readRDS(paste0("data/ADIPO/data.RDS"))
# 
# annot_prof = dat[["Profile Annotation"]]
# match_id = "sig_id";
# hm = FALSE;
# n_genes = 500;
# 
# ##Get differential expression gene set####
# get_de_eset <- function(annot_prof, match_id = "sig_id", hm = FALSE, n_genes = NULL){
# 
#   res <- dat[["Gene Expression"]]
#   ind2 <- match(colnames(res), annot_prof[, match_id])
#   pData(res) <- annot_prof[ind2,]
# 
#   if(hm){
#     inds <- fData(res)[, "Landmark Gene"] %in% "Yes"
#     res <- res[inds,]
#   }
# 
#   if(!is.null(n_genes)){
#     mad_gene_list <- apply(exprs(res), 1, mad)
#     mad_gene_list <- sort(mad_gene_list, decreasing = TRUE)[1:n_genes]
#     ind3 <- which(rownames(res) %in% names(mad_gene_list))
#     res <- res[ind3, ]
#   }
# 
#   return(res)
# 
# }
# 
# annot_prof <- dat[["Profile Annotation"]] %>% mutate(PPARg_Mod=ifelse(is.na(PPARg_Mod), "Vehicle", PPARg_Mod))
# annot_prof$PPARg_Mod <- factor(annot_prof$PPARg_Mod, levels = c("Yes", "No", "Suspected", "Vehicle"), ordered=is.ordered(c("Yes", "No", "Suspected", "Vehicle")))
# 
# eset <- get_de_eset(annot_prof = annot_prof, match_id = "sig_id", hm = FALSE, n_genes = 500)
# tas = 0.2
# 
# eset <- get_de_eset(annot_prof = dat[["Profile Annotation"]], match_id = "sig_id", hm = TRUE)
# tas = 0.2

##Function to create heatmap for heatmap explorer#####
plot_heatmap_static <- function(eset, tas){
  
  col_legend <- list(
    Carcinogenicity = list(
      col_breaks = c("+", "-", "N/A"), 
      col_values = sapply(c("orange", "green", "grey"), to.hex),
      col_labels = c("+", "-", "N/A")
    ),
    Genotoxicity = list(
      col_breaks = c("+", "-", "N/A"), 
      col_values = sapply(c("purple", "pink", "grey"), to.hex),
      col_labels = c("+", "-", "N/A")
    )
  )
  
  hmcolors <- function(...) scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, ...)
  
  eset <- eset[, which(pData(eset)[, "TAS"] >= tas)]
  
  ind.remove <- apply(exprs(eset), 1, function(i){
    any(is.nan(i))
  })
  
  eset <- eset[!ind.remove, ]
  
  hc <- clust_eset(eset)
  
  ncols <- ncol(eset)
  nrows <- nrow(eset)
  
  h <- min(nrows*15+200, 3000)
  w <- max(min(ncols*15+50, 3000), 400)
  
  xsize <- 4
  if (ncols < 500) 
    xsize <- 5
  if (ncols < 100)
    xsize <- 6
  if(ncols < 10)
    xsize <- 10
  if(ncols > 1000)
    xsize <- 0 
  
  ysize <- 4
  if (nrows < 500) 
    ysize <- 5
  if (nrows < 100)
    ysize <- 6
  if(nrows < 10)
    ysize <- 10
  if(nrows > 1000)
    ysize <- 0 
  
  col_lab <- c("Carcinogenicity", "Genotoxicity")
  
  ph2 <- 3*length(col_lab)
  ph3 <- nrows+20
  ph1 <- 0.1*(ph2+ph3)
  
  p.heights <- c(ph1, ph2, ph3)
  
  #render plot#####
  p <- ggheat.continuous.single(
    eset = eset, 
    hc = hc$hc, 
    hr = hc$hr, 
    hmcolors = hmcolors,
    hmtitle = "",
    col_lab = col_lab, 
    col_legend = col_legend,
    ylabstr = "",
    fout = NA, 
    p.heights = p.heights,
    xsize = xsize,
    ysize = ysize, 
    ysizelab = 7
  )
  
  return(p)
  
}

##Function to create heatmap for heatmap explorer for adipogens#####
plot_heatmap_static_adipo <- function(eset, tas){
  
  col_legend <- list(
    PPARg_Mod = list(
      col_breaks = c("Yes", "No", "Suspected", "Vehicle"), 
      col_values = sapply(c("green", "orange", "purple", "grey"), to.hex),
      col_labels = c("Yes", "No", "Suspected", "Vehicle")
    )
  )
  
  hmcolors <- function(...) scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, ...)
  
  eset <- eset[, which(pData(eset)[, "TAS"] >= tas)]
  
  ind.remove <- apply(exprs(eset), 1, function(i){
    any(is.nan(i))
  })
  
  eset <- eset[!ind.remove, ]
  
  hc <- clust_eset(eset)
  
  ncols <- ncol(eset)
  nrows <- nrow(eset)
  
  h <- min(nrows*15+200, 3000)
  w <- max(min(ncols*15+50, 3000), 400)
  
  xsize <- 4
  if (ncols < 500) 
    xsize <- 5
  if (ncols < 100)
    xsize <- 6
  if(ncols < 10)
    xsize <- 10
  if(ncols > 1000)
    xsize <- 0 
  
  ysize <- 4
  if (nrows < 500) 
    ysize <- 5
  if (nrows < 100)
    ysize <- 6
  if(nrows < 10)
    ysize <- 10
  if(nrows > 1000)
    ysize <- 0 
  
  col_lab <- c("PPARg_Mod")
  
  ph2 <- 3*length(col_lab)
  ph3 <- nrows+20
  ph1 <- 0.1*(ph2+ph3)
  
  p.heights <- c(ph1, ph2, ph3)
  
  #render plot#####
  p <- ggheat.continuous.single(
    eset = eset, 
    hc = hc$hc, 
    hr = hc$hr, 
    hmcolors = hmcolors,
    hmtitle = "",
    col_lab = col_lab, 
    col_legend = col_legend,
    ylabstr = "",
    fout = NA, 
    p.heights = p.heights,
    xsize = xsize,
    ysize = ysize, 
    ysizelab = 7
  )
  
  return(p)
  
}

##Create reactive hm data#####
hm_data <- reactiveVal(NULL); 
hm_key <- reactiveVal(NULL);

##Observe event when a button is clicked####
observeEvent(input$hm_de_generate, {
  
  req(input$marker_hm, input$marker_tas_hm)
  
  if(session$clientData$url_search == "?ADIPO"){ 
    es <- get_de_eset(
      annot_prof = if(isolate({ session$clientData$url_search }) == "?ADIPO"){ dat[["Chemical Annotation"]] }else{ dat[["Profile Annotation"]] },
      match_id = ifelse(isolate({ session$clientData$url_search }) == "?ADIPO", "Chemical", "sig_id"),
      hm = FALSE, 
      n_genes = input$numberthreshold
    )
    hm_key(paste0(session$clientData$url_search, "_", input$marker_hm, "_", input$marker_tas_hm, "_", input$numberthreshold))
  }else{
    es <- get_de_eset(
      annot_prof = if(isolate({ session$clientData$url_search }) == "?ADIPO"){ dat[["Chemical Annotation"]] }else{ dat[["Profile Annotation"]] },
      match_id = ifelse(isolate({ session$clientData$url_search }) == "?ADIPO", "Chemical", "sig_id"),
      hm = TRUE
    )
    hm_key(paste0(session$clientData$url_search, "_", input$marker_hm, "_", input$marker_tas_hm))
  }
  
  hm_data(es)
  show_morpheus(FALSE) 
  
}, ignoreInit=TRUE)

##Observe event when a button is clicked####
observeEvent(input$hm_es_generate, {
  
  req(input$marker_hm, input$marker_tas_hm, input$marker_gsname_hm, input$marker_gsmethod_hm)
  
  es <- get_gs_eset(
    gslist = dat[["Gene Set Enrichment"]],
    gsname = input$marker_gsname_hm,
    gsmethod = input$marker_gsmethod_hm,
    annot_prof = if(isolate({ session$clientData$url_search }) == "?ADIPO"){ dat[["Chemical Annotation"]] }else{ dat[["Profile Annotation"]] },
    match_id = ifelse(isolate({ session$clientData$url_search }) == "?ADIPO", "Chemical", "sig_id")
  )

  hm_data(es)
  hm_key(paste0(session$clientData$url_search, "_", input$marker_hm, "_", input$marker_tas_hm, "_", input$marker_gsname_hm, "_", input$marker_gsmethod_hm))
  show_morpheus(TRUE) 
  
}, ignoreInit=TRUE)

##Observe event when a button is clicked####
observeEvent(input$hm_conn_generate, {
  
  req(input$marker_hm, input$marker_tas_hm, input$marker_conn_name_hm)
  
  es <- get_conn_eset(
    connlist = dat[["Connectivity"]],
    conn_name = input$marker_conn_name_hm,
    annot_prof = if(isolate({ session$clientData$url_search }) == "?ADIPO"){ dat[["Chemical Annotation"]] }else{ dat[["Profile Annotation"]] },
    match_id = ifelse(isolate({ session$clientData$url_search }) == "?ADIPO", "Chemical", "sig_id")
  )
  
  hm_data(es)
  hm_key(paste0(session$clientData$url_search, "_", input$marker_hm, "_", input$marker_tas_hm, "_", input$marker_conn_name_hm))
  show_morpheus(FALSE) 
  
}, ignoreInit=TRUE)


##Change width and height of plot when the data changed###
output$heatmap_holder <- renderUI({
  
  req(hm_data())
  
  nrows <- nrow(hm_data())
  ncols <- ncol(hm_data())
  
  w <- max(min(ncols*15+50, 3000), 400)
  h <- min(nrows*15+200, 3000)
  
  plotOutput(outputId = "hm_plot", width="100%", height=h) %>% withSpinner(type=4, color="#0dc5c1", proxy.height="400px")
  
})


##Output heatmap explorer plots#####
output$hm_plot <- renderCachedPlot({
  
  req(hm_data(), input$marker_tas_hm, hm_key())
  
  if(session$clientData$url_search != "?ADIPO"){ 
    p <- plot_heatmap_static(eset=hm_data(), tas=input$marker_tas_hm)
  }else{
    p <- plot_heatmap_static_adipo(eset=hm_data(), tas=input$marker_tas_hm)
  }
  
  do.call(grid.arrange, p)
  
}, cacheKeyExpr = { list(hm_key()) })

#Download pdf####
output$hm_download_pdf <- downloadHandler(
  
  filename = paste0("carcinogenome_download_heatmap.pdf"),
  
  content = function(file){
    
    if(session$clientData$url_search != "?ADIPO"){ 
      p <- plot_heatmap_static(eset=hm_data(), tas=input$marker_tas_hm)
    }else{
      p <- plot_heatmap_static_adipo(eset=hm_data(), tas=input$marker_tas_hm)
    }
    
    nrows <- nrow(hm_data())
    ncols <- ncol(hm_data())
    w <- max(min(ncols*15+50, 3000), 400)
    h <- min(nrows*15+200, 3000)
    
    ggsave(
      plot = do.call(grid.arrange, p),
      filename = file,
      device = "pdf",
      width = w/61, height = h/61,
      dpi = 300
    )
  }
  
)

#Download png####
output$hm_download_png <- downloadHandler(
  
  filename = paste0("carcinogenome_download_heatmap.png"),
  
  content = function(file){
    
    if(session$clientData$url_search != "?ADIPO"){ 
      p <- plot_heatmap_static(eset=hm_data(), tas=input$marker_tas_hm)
    }else{
      p <- plot_heatmap_static_adipo(eset=hm_data(), tas=input$marker_tas_hm)
    }
    
    nrows <- nrow(hm_data())
    ncols <- ncol(hm_data())
    w <- max(min(ncols*15+50, 3000), 400)
    h <- min(nrows*15+200, 3000)
    
    ggsave(
      plot = do.call(grid.arrange, p),
      filename = file,
      device = "png",
      width = w/61, height = h/61,
      dpi = 300
    )
  }
  
)


