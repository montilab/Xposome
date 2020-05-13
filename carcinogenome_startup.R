

# Defaults for gene expression subtab ####
defaults <- list(
  landmark_de = FALSE, 
  summarizefunc_de = "median", 
  filterbyinput_de = c("score", "number"),
  range_de = c(-2, 2), 
  numberthresleft_de = 10, 
  numberthresright_de = 10
)

# Create a list of gene enrichment methods ####
dsmap_method <- list("gsva"="gsva", "ssgsea"="ssgsea", "zscore"="zscore")

# The gsva helptext method####
helptext_method <- paste(
  "gsva, ssgea, zscore: from R Bioconductor package GSVA",
  sep = "<br>"
)

# Create method names for connectivity mapping #####
connmap <- list("Perturbagen Classes" = "pcl", "Perturbagens" = "pert")

##Link gene expression to genecards.org####
get_genecard_link <- function(genesymbol){
  sprintf('<a href="http://www.genecards.org/cgi-bin/carddisp.pl?gene=%s&keywords=%s" target="_blank" class="btn btn-primary">%s</a>', genesymbol, genesymbol, genesymbol)
}

##Get the gene set enrichment hyperlink####
get_geneset_link <- function(geneset){
  sprintf('<a href="http://software.broadinstitute.org/gsea/msigdb/cards/%s" target="_blank" class="btn btn-primary">%s</a>', geneset, geneset)
}

# Get the 25th percentile ####
Q1 <- function(x){ 
  quantile(x, 0.25, na.rm = T)
}

# Get the 75th percentile ####
Q3 <- function(x){
  quantile(x, 0.75, na.rm = T)
}

# Get the chemical name####
get_chem_description <- function(chemical_dat, chem, BUID=FALSE){
  
  pos <- lapply(chemical_dat[,c("Chemical_Name", "BUID", "CAS")], function(x){
    w <- grep(as.character(chem), x, fixed = TRUE)
    if(length(w) > 0){
      return(w)
    }
  }) %>% unlist()
  
  if(BUID){
    return(chemical_dat$BUID[pos])
  }else{
    return(pos)
  }
  
}

# Function to round the data table values ####
data.table.round <- function(dt, digits = 3){
  
  cols <- sapply(colnames(dt), function(i) is.numeric(dt[,i]))
  cols <- names(which(cols))
  
  for(i in cols)
    dt[,i] <- round(dt[,i], digits)
  
  dt <- data.table(dt)
  
  return(dt)
  
}

##Summarize gene expression####
summarize_eset <- function(
  mat,
  summarize.func = "mean",
  do.scorecutoff = TRUE, scorecutoff = c(-0.6, 0.6),
  do.nmarkers = TRUE, nmarkers = c(100, 100)){
  
  x <- apply(mat, 1, match.fun(summarize.func))
  x <- as.numeric(x)
  n <- length(x)
  
  if(do.nmarkers){
    
    ind0 <- sum(x > 0)
    n1 <- min(nmarkers[1], ind0)
    n2 <- min(nmarkers[2], n-ind0)
    ord <- order(x, decreasing = TRUE)
    n2ind <- n-n2+1
    
    if(n1 == 0 & n2 == 0) x.ind.nmarkers <- NULL
    else if(n2 == 0) x.ind.nmarkers <- ord[1:n1]
    else  x.ind.nmarkers <- c(ord[1:n1], ord[n2ind:n])
    
  } else { x.ind.nmarkers <- 1:n }
  
  if(do.scorecutoff){
    #TODO: rank by score here too
    x.ind.scorecutoff <- which(x > scorecutoff[2] | x < scorecutoff[1])
  }else {
    x.ind.scorecutoff <- 1:n
  }
  
  inds <- intersect(x.ind.nmarkers, x.ind.scorecutoff)
  inds <- inds[order(x[inds], decreasing = TRUE)]
  
  return(list(inds = inds, scores = x[inds]))
  
}

##Get gene expression####
get_de <- function(
  chem, annot_var,
  profile_dat, chemical_dat, expression_dat,
  header = "ModZScore",
  summarize.func = "mean",
  landmark = FALSE, 
  do.nmarkers = TRUE, nmarkers = c(100, 100),
  do.scorecutoff = TRUE, scorecutoff = c(-2, 2)){

  #getting chemical BUID####
  BUID <- get_chem_description(chemical_dat=chemical_dat, chem=chem, BUID=TRUE)

  #getting signature id ####
  profile_dat <- profile_dat[which(profile_dat[, annot_var] %in% colnames(exprs(expression_dat))),]
  sig <- unique(profile_dat[which(profile_dat$BUID %in% BUID), annot_var])
  exposure <- unique(profile_dat$unique_ID_by_chem[which(profile_dat$BUID %in% BUID)])
  
  #getting expression data####
  eset <- expression_dat[,as.character(sig)]
  
  #getting feature data####
  fdat <- fData(expression_dat)
  
  if(ncol(fdat) > 0){
    fdat <- fdat
  }else{
    fdat <- data.frame(Gene=rownames(eset))
  }
  
  #check it landmark is selected
  if(landmark) { 
    eset <- eset[which(fdat$Landmark_Gene %in% "Yes"),] 
  }

  #summarise the expression set
  mat <- exprs(eset)
  colnames(mat) <- paste0(header, " ", exposure)
  res <- summarize_eset(mat=exprs(eset), summarize.func=summarize.func, do.scorecutoff=do.scorecutoff, scorecutoff=scorecutoff, do.nmarkers=do.nmarkers, nmarkers=nmarkers)

  #get expression results
  res.ind <- res$inds; res.scores <- res$scores;
  
  #determine whether gene is up or down regulated
  direction <- sapply(res.scores, function(i){
    if(i > 0) return("Up") else return("Down")
  })

  #creata the summary table 
  if(ncol(mat) > 1){
    tab <- cbind(fdat[res.ind,, drop = FALSE], Direction = direction, SummaryScore=res.scores, mat[res.ind,, drop = FALSE])
    colnames(tab)[colnames(tab) %in% "SummaryScore"] <- "Summary Score"
  }else{
    tab <- cbind(fdat[res.ind,, drop = FALSE], Direction = direction, mat[res.ind,, drop = FALSE])
  }

  #return hyperlink from genecard.org
  tab$Gene <- sapply(as.character(tab$Gene), get_genecard_link)

  return(tab)

}

##Summarize gene set enrichment#####
get_gsenrichment <- function(
  chem, annot_var,
  profile_dat, chemical_dat, expression_dat,
  gsname = "Hallmark",
  header = "GS Score",
  summarize.func = "mean"){
  
  #getting chemical BUID###
  BUID <- get_chem_description(chemical_dat=chemical_dat, chem=chem, BUID=TRUE)
  
  #getting signature id ####
  profile_dat <- profile_dat[which(profile_dat[, annot_var] %in% colnames(exprs(expression_dat))),]
  sig <- unique(profile_dat[which(profile_dat$BUID %in% BUID), annot_var])
  exposure <- unique(profile_dat$unique_ID_by_chem[which(profile_dat$BUID %in% BUID)])
  
  #getting expression data####
  eset <- expression_dat[,as.character(sig)]

  #getting feature data####
  fdat <- fData(expression_dat)
  
  if(ncol(fdat) > 0){
    fdat <- fdat
  }else{
    fdat <- data.frame(Geneset=rownames(eset))
  }
  
  #getting summart table####
  mat <- exprs(eset)
  colnames(mat) <- paste0(header, " ", exposure)
  
  res <- apply(mat, 1, match.fun(summarize.func))
  res <- as.numeric(res)
  
  if(ncol(mat) > 1){
    res <- cbind(fdat, score = res, mat)
    res <- res[order(res$score, decreasing = TRUE),, drop = FALSE]
    colnames(res)[colnames(res) %in% "score"]<- "Summary Score"
  }else{
    res <- cbind(fdat, mat)
  }
  
  #return hyperlink to MSigDB genesets
  if(gsname %in% c("Hallmark", "C2")) res$Geneset <- sapply(as.character(res$Geneset), get_geneset_link)
  
  return(res)
  
}


##Summarize connectivity map#####
get_connectivity <- function(
  chem, annot_var,
  profile_dat, chemical_dat, expression_dat,
  header = "Connectivity Score",
  summarize.func = "mean"){
  
  #getting chemical BUID####
  BUID <- get_chem_description(chemical_dat=chemical_dat, chem=chem, BUID=TRUE)
  
  #getting signature id ####
  profile_dat <- profile_dat[which(profile_dat[, annot_var] %in% colnames(exprs(expression_dat))),]
  sig <- unique(profile_dat[which(profile_dat$BUID %in% BUID), annot_var])
  exposure <- unique(profile_dat$unique_ID_by_chem[which(profile_dat$BUID %in% BUID)])
  
  #getting expression data####
  eset <- expression_dat[,as.character(sig)]
  
  #getting feature data####
  fdat <- fData(expression_dat)
  
  if(ncol(fdat) > 0){
    fdat <- fdat
  }else{
    fdat <- data.frame(Connectivity_Id=rownames(eset))
  }
  
  #getting summart table####
  mat <- exprs(eset)
  colnames(mat) <- paste0(header, " ", exposure)
  
  res <- apply(mat, 1, match.fun(summarize.func))
  res <- as.numeric(res)
  
  if(ncol(mat) > 1){
    res <- cbind(fdat, score = res, mat)
    res <- res[order(res$score, decreasing = TRUE),, drop = FALSE]
    colnames(res)[colnames(res) %in% "score"]<- "Summary Score"
  }else{
    res <- cbind(fdat, mat)
  }

  return(res)
  
}

#plot wrapper###
plot_wrapper <- function(df, plot = "Density", ...){
  
  if(plot %in% "Density") {
    
    res <- ggplot(data=df, aes_string(x = "x", fill = "cols")) + 
      geom_density(position = "identity", alpha = 0.5)
    
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


##Function to create density and boxplot for marker explorer#####
get_de_by_gene_hist <- function(
  marker_id,
  expression_dat,
  profile_dat,
  annot_var = "Sig_Id",
  col_id = "Carcinogenicity",
  col_colors = c("grey", "green", "orange"), 
  col_names = c("N/A", "-", "+"), 
  header = "Mod Z-scores",
  marker_tas = 0,
  plot = "Density",
  replace_na = "N/A"){
  
  profile_dat <- profile_dat[which(profile_dat[, annot_var] %in% colnames(exprs(expression_dat))),]
  sig <- unique(profile_dat[which(profile_dat$TAS >= min(marker_tas) & profile_dat$TAS <= max(marker_tas)), annot_var])
  eset <- exprs(expression_dat)[,as.character(sig)]
  rowid <- which(rownames(eset) %in% marker_id)
  x <- as.numeric(eset[rowid,])
  
  if(is.na(col_id)){
    
    p.title <- paste("Distribution of ", header, " Across Profiles\n for ", marker_id, " (Overall)\n", sep = "")
    background <- as.numeric(eset)
    df <- rbind(data.frame(x = x, cols = "query"),
                data.frame(x = background, cols = "background"))
    df$cols <- factor(df$cols, levels = c("background", "query"), ordered=is.ordered(c("background", "query")))
    col_vec <- c("grey", "red")
    names(col_vec) <- c("background", "query")
    
    p <- plot_wrapper(df=df, plot=plot) +
      xlab(header) + 
      ylab("Density") + 
      scale_fill_manual(
        name = NULL, 
        values = col_vec,
        breaks = names(col_vec),
        labels = names(col_vec)
      ) +
      ggtitle(p.title) +
      theme_bw() +
      theme(
        plot.margin = margin(10, 0, 10, 0),
        plot.title = element_text(hjust = 0.5)
      )
    
  }else{
    
    p.title <- paste("Distribution of ", header, " across profiles\n for ", marker_id, " (by ", col_id, ")\n", sep = "")
    cols <- profile_dat[which(profile_dat[,annot_var] %in% as.character(sig)), col_id] 
    cols <- as.character(cols)
    cols_match <- col_colors
    names(cols_match) <- col_names
    df <- data.frame(x = x, cols= cols, stringsAsFactors=FALSE) %>% mutate(cols=ifelse(is.na(cols), replace_na, cols))
    df$cols <- factor(df$cols, levels = col_names, ordered=is.ordered(col_names))
    
    p <- plot_wrapper(df=df, plot=plot) +
      scale_fill_manual(
        name = NULL,
        values = cols_match, 
        breaks = names(cols_match), 
        labels = names(cols_match)
      ) +
      xlab(header) + 
      ylab("Density") + 
      ggtitle(p.title) +
      theme_bw() +
      theme(
        plot.margin = margin(10, 0, 10, 0),
        plot.title = element_text(hjust = 0.5)
      )
    
  }
  
  return(p)
  
}


##Get the table profile ranked by mod Z-scores for marker explorer####
get_de_by_gene_table <- function(
  marker_id,
  expression_dat,
  profile_dat,
  annot_var = "Sig_Id",
  header = "Mod Z-scores",
  marker_tas = 0){
  
  profile_dat <- profile_dat[which(profile_dat[, annot_var] %in% colnames(exprs(expression_dat))),]
  sig <- unique(profile_dat[which(profile_dat$TAS >= min(marker_tas) & profile_dat$TAS <= max(marker_tas)), annot_var])
  eset <- exprs(expression_dat)[,as.character(sig)]
  rowid <- which(rownames(eset) %in% marker_id)
  x <- as.numeric(eset[rowid,])

  pdat <- profile_dat[which(profile_dat[,annot_var] %in% as.character(sig)),]
  
  df <- cbind(value = x, pdat)
  df <- df[order(x, decreasing = TRUE),]
  
  colnames(df)[colnames(df) %in% "value"] <- header
  
  return(df)
  
}


