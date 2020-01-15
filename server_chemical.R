

#Get description for each chemical##### 
get_chemical_description <- function(input, annot_chem){
  buid <- get_BUID(input = input, tab = annot_chem)
  res <- annot_chem[annot_chem$BUID %in% buid, ]
  return(res[1,])
}

##Output the chemical table####
output$chemical_table <- DT::renderDataTable({
  
  req(input$chem)
  
  data.table.round(
    get_chemical_description(
      input = input$chem, 
      annot_chem = dat[["Chemical Annotation"]]
    )
  )
  
}, escape = FALSE, extensions = 'Buttons', server = TRUE, rownames=FALSE, 
options = list(
  deferRender = FALSE,
  scrollX = TRUE, 
  dom = 'T<"clear">Blfrtip'
))


##Summarize gene expression#### 
summarize_eset <- function(
  mat, 
  summarize.func = c("mean", "median", "max", "min", "Q1", "Q3"),
  do.scorecutoff = TRUE, scorecutoff = c(-0.6, 0.6), 
  do.nmarkers = TRUE, nmarkers = c(100, 100)){
  
  summarize.func <- match.arg(summarize.func)
  x <- apply(mat, 1, match.fun(summarize.func))
  x <- as.numeric(x)
  n <- length(x)
  
  if(do.nmarkers){
    
    ind0 <- sum(x > 0)
    n1 <- min(nmarkers[1], ind0)
    n2 <- min(nmarkers[2], n-ind0)
    ord <- order(x, decreasing = TRUE)
    n2ind <- n-n2+1
    
    if(n1 == 0 & n2 == 0) x.ind.nmarkers<- NULL
    else if(n2 == 0) x.ind.nmarkers <- ord[1:n1]
    else  x.ind.nmarkers <- c(ord[1:n1], ord[n2ind:n])
  } else
    x.ind.nmarkers<-1:n
  
  if(do.scorecutoff)
    #TODO: rank by score here too
    x.ind.scorecutoff <- which(x > scorecutoff[2] | x < scorecutoff[1])
  else
    x.ind.scorecutoff <- 1:n
  
  inds <- intersect(x.ind.nmarkers, x.ind.scorecutoff)
  inds <- inds[order(x[inds], decreasing = TRUE)]
  
  return(list(inds = inds, scores = x[inds]))
  
} 

##Link gene expression to genecards.org####
get_genecard_link <- function(genesymbol){
  sprintf('<a href="http://www.genecards.org/cgi-bin/carddisp.pl?gene=%s&keywords=%s" target="_blank" class="btn btn-primary">%s</a>', genesymbol, genesymbol, genesymbol)
}

##Get gene expression####
get_de <- function(
  input, tab, eset, annot_prof, 
  col_id, match_id = "sig_id", 
  header = "ModZScore", 
  landmark = FALSE, landmark_id = "Landmark Gene", landmark_positive = "Yes",
  do.nmarkers = TRUE, do.scorecutoff = TRUE, 
  scorecutoff = c(-2, 2), nmarkers = c(100, 100),
  summarize.func = c("mean", "median", "max", "min", "Q1", "Q3")){
  
  i <- get_BUID(input, tab)
  pData(eset) <- annot_prof[match(colnames(eset), annot_prof[, match_id]),]
  eset <- eset[, eset$BUID %in% i]
  
  if(landmark) { eset <- eset[fData(eset)[, landmark_id] %in% landmark_positive,] }
  
  mat <- exprs(eset)
  fdat <- fData(eset)
  pdat <- pData(eset)
  
  colnames(mat) <- paste(header, " ", pdat[, col_id], "uM", sep = "")
  res <- summarize_eset(mat=mat, summarize.func=summarize.func, do.scorecutoff=do.scorecutoff, scorecutoff=scorecutoff, do.nmarkers=do.nmarkers, nmarkers=nmarkers)
  
  res.ind <- res$inds
  res.scores <- res$scores
  direction <- sapply(res.scores, function(i){
    if(i > 0) return("Up") else return("Down")
  })
  
  tab <- cbind(fdat[res.ind,, drop = FALSE], Direction = direction, SummaryScore=res.scores, mat[res.ind,, drop = FALSE])
  colnames(tab)[colnames(tab) %in% "SummaryScore"] <- "Summary Score"
  
  #return hyperlink to genecard.org
  tab$"Gene Symbol" <- sapply(as.character(tab$"Gene Symbol"), get_genecard_link)
  
  return(tab)
  
}

##Output the gene expression table####
output$gene_expression_table <- DT::renderDataTable({
  
  req(input$chem, input$filterbyinput_de, input$filterbyinput_de, input$range_de, input$numberthresleft_de, input$numberthresright_de, input$summarizefunc_de)
  
  data.table.round(
    get_de(
      input = input$chem, 
      tab = dat[["Chemical Annotation"]][, c("Chemical Name", "BUID", "CAS")], 
      eset = dat[["Gene Expression"]], 
      annot_prof = dat[["Profile Annotation"]],
      col_id = ifelse(dat[["title"]]=="MCF10A Portal", "unique_ID_by_chem", "dose (uM)"),
      match_id = "sig_id",
      header = "ModZScore", 
      landmark = input$landmark_de,
      landmark_id = "Landmark Gene", 
      landmark_positive = "Yes",
      do.scorecutoff = "score" %in% input$filterbyinput_de, 
      do.nmarkers = "number" %in% input$filterbyinput_de, 
      scorecutoff = c(input$range_de[1], input$range_de[2]), 
      nmarkers = c(input$numberthresleft_de, input$numberthresright_de),
      summarize.func = input$summarizefunc_de
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
  

##Summarize gene set enrichment#####
summarize_gsproj <- function(
  eset, 
  annot_prof, 
  col_id, 
  match_id = "sig_id", 
  header = "GS Score",
  summarize.func = c("mean", "median", "max", "min", "Q1", "Q3")){
  
  head(pData(eset))
  summarize.func <- match.arg(summarize.func)
  res <- apply(exprs(eset), 1, match.fun(summarize.func))
  res <- as.numeric(res)
  
  mat <- exprs(eset)	
  colnames(mat) <- paste(header, " ", pData(eset)[,col_id], "uM",sep = "")
  
  res <- cbind(fData(eset), score = res, mat)
  res <- res[order(res$score, decreasing = TRUE),, drop = FALSE]
  colnames(res)[colnames(res) %in% "score"]<- "Summary Score"
  return(res)
}

##Get the gene set hyperlink####
get_geneset_link <- function(geneset){
  sprintf('<a href="http://software.broadinstitute.org/gsea/msigdb/cards/%s" target="_blank" class="btn btn-primary">%s</a>', geneset, geneset)
}

##Get gene set enrichment#####
get_gsproj <- function(
  input, gslist, tab, 
  gsname, gsmethod,
  annot_prof, 
  col_id, 
  match_id = "sig_id", 
  header = "GS Score",
  summarize.func = c("mean", "median", "max", "min", "Q1", "Q3")){
  
  i <- get_BUID(input, tab)
  ind <- grep(paste0(".*", dsmap[[gsname]], ".*", gsmethod, ".*"), names(gslist))
  res <- gslist[[ind]]
  
  ind2 <- match(pData(res)[,match_id], annot_prof[, match_id])
  pData(res) <- annot_prof[ind2,]
  
  res <- res[, res$BUID %in% i]
  
  res <- summarize_gsproj(eset = res, annot_prof=annot_prof, col_id=col_id, match_id=match_id, header=header, summarize.func=summarize.func)
  
  #return hyperlink to MSigDB genesets
  if(gsname %in% c("Hallmark", "C2")) res$genesets <- sapply(as.character(res$genesets), get_geneset_link)
  
  return(res)
}

#Output the gene set enrichment table####
output$gene_set_enrichment_table <- DT::renderDataTable({
  
  req(input$chem, input$gsname, input$gsmethod, input$summarize_gs)
  
  data.table.round(
    get_gsproj(
      input = input$chem, 
      gslist = dat[["Gene Set Enrichment"]], 
      tab = dat[["Chemical Annotation"]][, c("Chemical Name", "BUID", "CAS")], 
      gsname = input$gsname, 
      gsmethod = input$gsmethod,
      annot_prof = dat[["Profile Annotation"]], 
      col_id = ifelse(dat[["title"]]=="MCF10A Portal", "unique_ID_by_chem", "dose (uM)"), 
      match_id = "sig_id", 
      header = "GS Score",
      summarize.func = input$summarize_gs
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


##Get connectivity####
get_connectivity <- function(
  input, 
  tab, 
  annot_prof,
  connlist, 
  conn_name,
  col_id,
  match_id = "sig_id",
  header = "Connectivity Score",
  summarize.func = c("mean", "median", "max", "min", "Q1", "Q3")){
  
  i <- get_BUID(input, tab)
  res <- connlist[[conn_name]]
  ind2 <- match(pData(res)[, match_id], annot_prof[, match_id])
  pData(res) <- annot_prof[ind2,]
  res <- res[, res$BUID %in% i]
  res <- summarize_gsproj(eset = res, annot_prof=annot_prof, col_id=col_id, match_id=match_id, header=header, summarize.func=summarize.func)
  return(res)
  
}

#Output connectivity table####
output$connectivity_table <- DT::renderDataTable({
  
  req(input$chem, input$conn_name, input$summarizefunc_conn)
  
  data.table.round(
    get_connectivity(
      input = input$chem, 
      tab = dat[["Chemical Annotation"]][, c("Chemical Name", "BUID", "CAS")], 
      annot_prof = dat[["Profile Annotation"]],
      connlist = dat[["Connectivity"]], 
      conn_name = input$conn_name,
      col_id = ifelse(dat[["title"]]=="MCF10A Portal", "unique_ID_by_chem", "dose (uM)"),
      match_id = "sig_id",
      header = "Connectivity Score",
      summarize.func = input$summarizefunc_conn
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

###################################################
#
# GENE EXPRESSION
#
###################################################

#observe when restore button is clicked####
observeEvent(input$de_restore, {

  updateCheckboxInput(session, inputId = "landmark_de", value = defaults[["landmark_de"]])
  updateSelectInput(session, inputId = "summarizefunc_de", selected = defaults[["summarizefunc_de"]])
  updateCheckboxGroupInput(session, inputId = "filterbyinput_de", selected = defaults[["filterbyinput_de"]])
  updateSelectInput(session, inputId = "range_de", selected = defaults[["range_de"]])
  updateSliderInput(session, inputId = "numberthresleft_de", value = defaults[["numberthresleft_de"]])
  updateSliderInput(session, inputId = "numberthresright_de", value = defaults[["numberthresright_de"]])

}, ignoreInit=TRUE)

##Observe when hide button is clicked####
observeEvent(input$de_hide, {
  updateCollapse(session, "de_opt_panel", close = "de_options")
}, ignoreInit=TRUE)


##Observe when show button is clicked####
observeEvent(input$de_show, {
  updateCollapse(session, "de_opt_panel", open = "de_options")
}, ignoreInit=TRUE)


###################################################
#
# GENE SET ENRICHMENT
#
###################################################

#observe when restore button is clicked####
observeEvent(input$es_restore, {
  
  updateSelectInput(session, inputId = "gsname", selected = "Hallmark")
  updateSelectInput(session, inputId = "gsmethod", selected = "gsva")
  updateSelectInput(session, inputId = "summarize_gs", selected = "median")
  
}, ignoreInit=TRUE)

##Observe when hide button is clicked####
observeEvent(input$es_hide, {
  updateCollapse(session, "es_opt_panel", close = "es_options")
}, ignoreInit=TRUE)


##Observe when show button is clicked####
observeEvent(input$es_show, {
  updateCollapse(session, "es_opt_panel", open = "es_options")
}, ignoreInit=TRUE)


###################################################
#
# CONNECTIVITY
#
###################################################

#observe when restore button is clicked####
observeEvent(input$conn_restore, {
  
  updateSelectInput(session, inputId = "conn_name", selected = "pcl")
  updateSelectInput(session, inputId = "summarizefunc_conn", selected = "median")
  
}, ignoreInit=TRUE)

##Observe when hide button is clicked####
observeEvent(input$conn_hide, {
  updateCollapse(session, "conn_opt_panel", close = "conn_options")
}, ignoreInit=TRUE)


##Observe when show button is clicked####
observeEvent(input$conn_show, {
  updateCollapse(session, "conn_opt_panel", open = "conn_options")
}, ignoreInit=TRUE)

