
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

#Output the morpheus result link to heatmap####
output$morpheus_result_link <- renderUI({
  
  req(input$marker_gsname_hm, input$marker_gsmethod_hm, input$marker_tas_hm)
  
  HTML(
    paste0(
      '<a target="_blank" href="',
      get_morpheus_link(
        url = get_heatmap_gct(
          ds=input$marker_gsname_hm, 
          dsmap = dsmap, 
          method = input$marker_gsmethod_hm
        ), 
        domain = domain, tas = input$marker_tas_hm
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
  
  eset <- eset[, pData(eset)[, "TAS"] >= tas]
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

##A reactive value to keep track if a gene set is selected####
heatmap_plot <- reactiveVal()

##Observe then marker gene set is selected#####
observeEvent(input$marker_hm, {
  
  heatmap_plot(input$marker_hm)
  
}, ignoreInit=TRUE)

##Observe then marker TAS is selected#####
observeEvent(input$marker_tas_hm, {
  
  heatmap_plot(input$marker_tas_hm)
  
}, ignoreInit=TRUE)

##Observe which marker gene set is selected#####
observeEvent(input$marker_gsname_hm, {
  
  heatmap_plot(input$marker_gsname_hm)
  
}, ignoreInit=TRUE)

##Observe which marker gene method is selected#####
observeEvent(input$marker_gsmethod_hm, {
  
  heatmap_plot(input$marker_gsmethod_hm)
  
}, ignoreInit=TRUE)

##Observe the marker gene connectivity is selected#####
observeEvent(input$marker_conn_name_hm, {
  
  heatmap_plot(input$marker_conn_name_hm)
  
}, ignoreInit=TRUE)

##Output heatmap explorer plots#####
observeEvent(heatmap_plot(), {
  
  if(input$marker_hm == "Genes (Landmark)"){
    
    es <- get_de_eset(annot_prof = dat[["Profile Annotation"]], match_id = "sig_id", hm = TRUE)
    
  }else if(input$marker_hm == "Gene Sets"){
    
    if(is.null(input$marker_gsname_hm) | is.null(input$marker_gsmethod_hm)) return(NULL)
    
    es <- get_gs_eset(
      gslist = dat[["Gene Set Enrichment"]], 
      gsname = input$marker_gsname_hm, 
      gsmethod = input$marker_gsmethod_hm, 
      annot_prof = dat[["Profile Annotation"]],
      match_id = "sig_id"
    )
    
  }else if (input$marker_hm == "CMap Connectivity"){
    
    if(is.null(input$marker_conn_name_hm)) return(NULL)
    
    es <- get_conn_eset(
      connlist = dat[["Connectivity"]], 
      conn_name = input$marker_conn_name_hm, 
      annot_prof = dat[["Profile Annotation"]],
      match_id = "sig_id"
    )
    
  }
  
  tas <- input$marker_tas_hm
    
  ncols <- ncol(es)
  nrows <- nrow(es)
  
  h <- min(nrows*15+200, 3000)
  w <- max(min(ncols*15+50, 3000), 400)
  
  p <- plot_heatmap_static(eset=es, tas=tas)

  #Output the first plot#####
  output$hm_plot <- renderPlot({
    do.call(grid.arrange, p)
  })
  
  #Download pdf ####
  output$hm_download_pdf <- downloadHandler(
    filename = paste0("carcinogenome_download_heatmap.pdf"),
    content = function(file){ 			
      ggsave(
        plot = do.call(grid.arrange, p), 
        filename = file, 
        device = "pdf", 
        width = w/61, height = h/61, 
        dpi = 300
      )
    })
  
  #Download png####
  output$hm_download_png <- downloadHandler(
    filename = paste0("carcinogenome_download_heatmap.png"),
    content = function(file){ 			
      ggsave(
        plot = do.call(grid.arrange, p), 
        filename = file, 
        device = "png", 
        width = w/61, height = h/61, 
        dpi = 300
      )
    })

}, ignoreInit=TRUE)

