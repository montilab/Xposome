
# Defaults for gene expression subtab ####
de_defaults <<- list(
  landmark_de = FALSE, 
  summarizefunc_de = "median", 
  filterbyinput_de = c("score", "number"),
  range_de = c(-2, 2), 
  numberthresleft_de = 100, 
  numberthresright_de = 100
)

# Create a list of gene enrichment methods ####
gsva_method <<- list("gsva"="gsva")

# The gsva helptext method####
helptext_gsva_method <<- paste(
  "gsva from R Bioconductor package GSVA",
  sep = "<br>"
)

gs_default <<- data.frame(
  name = c("Hallmark", "C2", "NURSA"),
  geneset = c("gsscores_h.all.v7.0", "gsscores_c2.cp.reactome.v7.0", "gsscores_nursa_consensome_Cbyfdrvalue_0.01")
)

# Get gene link from genecards.org ####
get_genecard_link <<- function(genesymbol){
  sprintf('<a href="http://www.genecards.org/cgi-bin/carddisp.pl?gene=%s&keywords=%s" target="_blank" style="text-decoration:none;">%s</a>', genesymbol, genesymbol, genesymbol)
}

# Get gene set enrichment link ####
get_geneset_link <<- function(geneset){
  label <- gsub("_", " ", geneset)
  geneset <- gsub(" ", "_", geneset)
  sprintf('<a href="http://software.broadinstitute.org/gsea/msigdb/cards/%s" target="_blank" style="text-decoration:none;">%s</a>', geneset, label)
}

# Create connectivity classes #####
connmap <<- list("Perturbagen Classes" = "pcl", "Perturbagens" = "pert")

# Get the 25th percentile ####
Q1 <<- function(x){ 
  quantile(x, 0.25, na.rm = T)
}

# Get the 75th percentile ####
Q3 <<- function(x){
  quantile(x, 0.75, na.rm = T)
}

# RColorBrewer palette for plotting
col_palettes <<- c("Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd", "BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral", "Set1", "Set2", "Set3", "Paired", "Accent", "Pastel1", "Pastel2", "Dark2")

# Get the chemical name ####
get_chem_description <<- function(chemical_dat, chem, chemical_id=FALSE){
  
  pos <- lapply(c("Chemical_Id", "CAS"), function(x){
    w <- which(chemical_dat[,x][which(!chemical_dat[,x] %in% c(NA, ""))] %in% chem)
    if(length(w) > 0){
      return(w)
    }
  }) %>% unlist()
  
  if(length(pos) > 0){
    if(chemical_id){
      return(unique(chemical_dat$Chemical_Id[pos]))
    }else{
      return(chemical_dat[pos,])
    }
  }
  
}

# Round the data table values ####
data.table.round <<- function(dt, digits = 3){
  
  cols <- sapply(colnames(dt), function(i) is.numeric(dt[,i]))
  cols <- names(which(cols))
  
  for(i in cols)
    dt[,i] <- round(dt[,i], digits)
  
  dt <- data.table(dt)
  
  return(dt)
  
}

##Get gene expression####
get_de <<- function(
  chem, 
  profile_dat, chemical_dat, expression_dat,
  header = "ModZScore"
){
  
  #matching expression column names with profile annotation
  annot_var <- ifelse(all(colnames(expression_dat) %in% profile_dat$Sig_Id), "Sig_Id", "Chemical_Id")
  
  #getting chemical BUID####
  chemical_id <- get_chem_description(chemical_dat=chemical_dat, chem=chem, chemical_id=TRUE)

  #getting signature id ####
  profile_dat <- profile_dat[which(profile_dat[, annot_var] %in% colnames(expression_dat)),]
  exposure <- sort(unique(profile_dat$unique_ID_by_chem[which(profile_dat$Chemical_Id %in% chemical_id)]))

  #summarise the expression set
  eSet <- exprs(expression_dat); mat <- matrix(NA, nrow=nrow(eSet), ncol=length(exposure), byrow=T, dimnames=list(rownames(eSet), paste0(header, "<br> ", exposure)))
  
  for(i in seq_along(exposure)){
    #i=1;
    sig <- unique(profile_dat[which(profile_dat$unique_ID_by_chem %in% exposure[i] & profile_dat$Chemical_Id %in% chemical_id), annot_var])
    if(length(sig) > 1){
      mat[,i] <- rowMeans(eSet[,as.character(sig)], na.rm=T)
    }else{
      mat[,i] <- eSet[,as.character(sig)]
    }
  }
  
  data <- mat %>% as.data.frame() %>% rownames_to_column(var="Gene")
  
  return(data)

}


get_de_filter <<- function(
  de_dat, landmark_dat,
  summarize.func = "mean",
  landmark = TRUE, 
  do.nmarkers = TRUE, nmarkers = c(100, 100),
  do.scorecutoff = TRUE, scorecutoff = c(-2, 2)
){
  
  # create a matrix
  mat <- as.matrix(de_dat %>% column_to_rownames(var="Gene"))
  
  x <- apply(mat, 1, match.fun(summarize.func))
  x <- as.numeric(x)
  n <- length(x)

  if(do.nmarkers){

    ind_down <- which(x < 0)
    ind_up <- which(x > 0)

    ord_down <- data.frame(
      pos=ind_down,
      x=x[ind_down]
    ) %>% arrange(x)

    ord_up <- data.frame(
      pos=ind_up,
      x=x[ind_up]
    ) %>% arrange(desc(x))

    n1 = ifelse(as.numeric(nmarkers[1]) > nrow(ord_down), nrow(ord_down), as.numeric(nmarkers[1]));
    n2 = ifelse(as.numeric(nmarkers[2]) > nrow(ord_up), nrow(ord_up), as.numeric(nmarkers[2]));

    if(n1 == 0 & n2 == 0){
      x.ind.nmarkers <- 0
    }else if(n2 == 0 & n1 > 0){
      x.ind.nmarkers <- ord_down$pos[1:n1]
    }else if(n1 == 0 & n2 > 0){
      x.ind.nmarkers <- ord_up$pos[1:n2]
    }else if(n1 > 0 & n2 > 0){
      x.ind.nmarkers <- c(ord_down$pos[1:n1], ord_up$pos[1:n2])
    }

  }else {

    x.ind.nmarkers <- 1:n

  }

  if(do.scorecutoff){
    x.ind.scorecutoff <- c(which(x <= as.numeric(scorecutoff[1])), which(x >= as.numeric(scorecutoff[2])))
  }else {
    x.ind.scorecutoff <- 1:n
  }

  inds <- intersect(x.ind.nmarkers, x.ind.scorecutoff)

  #get expression results
  res.ind <- inds; res.scores <- x[inds]

  #determine whether gene is up or down regulated
  direction <- sapply(res.scores, function(i){
    if(i > 0) return("Up") else return("Down")
  })

  # Get the final filtered data
  fdat <- de_dat[res.ind,, drop = FALSE]
  
  #check it landmark is selected
  if(landmark) {
    fdat <- fdat %>% 
      mutate(Landmark_Gene=ifelse(Gene %in% landmark_dat$Gene, "Yes", "No")) %>% 
      select(Gene, Landmark_Gene, everything())
  }
  
  #creata the summary table
  table <- cbind(fdat, SummaryScore=res.scores, Direction = direction)
  table <- table[order(table$SummaryScore, decreasing = TRUE),, drop = FALSE]
  colnames(table)[which(colnames(table) %in% "SummaryScore")] <- "Summary Score"

  #return hyperlink from genecard.org
  table$Gene <- sapply(as.character(table$Gene), get_genecard_link)
  
  return(table)
  
}
  

##Summarize gene set enrichment#####
get_gsenrichment <<- function(
  chem,
  profile_dat, chemical_dat, expression_dat,
  header = "GS Score"
){
  
  #matching expression column names with profile annotation
  annot_var <- ifelse(all(colnames(expression_dat) %in% profile_dat$Sig_Id), "Sig_Id", "Chemical_Id")
  
  #getting chemical BUID####
  chemical_id <- get_chem_description(chemical_dat=chemical_dat, chem=chem, chemical_id=TRUE)

  #getting signature id ####
  profile_dat <- profile_dat[which(profile_dat[, annot_var] %in% colnames(expression_dat)),]
  exposure <- sort(unique(profile_dat$unique_ID_by_chem[which(profile_dat$Chemical_Id %in% chemical_id)]))

  #summarise the expression set
  eSet <- exprs(expression_dat); mat <- matrix(NA, nrow=nrow(eSet), ncol=length(exposure), byrow=T, dimnames=list(rownames(eSet), paste0(header, " ", exposure)))

  for(i in seq_along(exposure)){
    #i=1;
    sig <- unique(profile_dat[which(profile_dat$unique_ID_by_chem %in% exposure[i] & profile_dat$Chemical_Id %in% chemical_id), annot_var])
    if(length(sig) > 1){
      mat[,i] <- rowMeans(eSet[,as.character(sig)], na.rm=T)
    }else{
      mat[,i] <- eSet[,as.character(sig)]
    }
  }
  

  data <- mat %>% as.data.frame() %>% rownames_to_column(var="Geneset")
  
  return(data) 
  
}


get_gsenrichment_filter <<- function(
  gs_dat,
  gsname = "Hallmark", 
  summarize.func = "mean"
){
  
  #Get name of gs collection
  gsname <- gs_default$name[which(gs_default$geneset %in% gsname)]
  
  # create a matrix
  mat <- as.matrix(gs_dat %>% column_to_rownames(var="Geneset"))
  
  res <- apply(mat, 1, match.fun(summarize.func))
  res <- as.numeric(res)
  
  table <- cbind(gs_dat, SummaryScore = res)
  table <- table[order(table$SummaryScore, decreasing = TRUE),, drop = FALSE]
  colnames(table)[which(colnames(table) %in% "SummaryScore")] <- "Summary Score"

  # Get hyperlink for MSigDB genesets
  if(gsname %in% c("Hallmark", "C2")) table$Geneset <- sapply(as.character(table$Geneset), get_geneset_link)

  return(table)
  
}


##Summarize connectivity map#####
get_connectivity <<- function(
  chem, 
  profile_dat, chemical_dat, expression_dat,
  header = "Connectivity Score"
){
  
  #matching expression column names with profile annotation
  annot_var <- ifelse(all(colnames(expression_dat) %in% profile_dat$Sig_Id), "Sig_Id", "Chemical_Id")
  
  #getting chemical BUID####
  chemical_id <- get_chem_description(chemical_dat=chemical_dat, chem=chem, chemical_id=TRUE)
  
  #getting signature id ####
  profile_dat <- profile_dat[which(profile_dat[, annot_var] %in% colnames(expression_dat)),]
  exposure <- sort(unique(profile_dat$unique_ID_by_chem[which(profile_dat$Chemical_Id %in% chemical_id)]))
  
  #summarise the expression set
  eSet <- exprs(expression_dat); mat <- matrix(NA, nrow=nrow(eSet), ncol=length(exposure), byrow=T, dimnames=list(rownames(eSet), paste0(header, " ", exposure)))
  
  for(i in seq_along(exposure)){
    #i=1;
    sig <- unique(profile_dat[which(profile_dat$unique_ID_by_chem %in% exposure[i] & profile_dat$Chemical_Id %in% chemical_id), annot_var])
    
    if(length(sig) > 1){
      mat[,i] <- rowMeans(eSet[,as.character(sig)], na.rm=T)
    }else{
      mat[,i] <- eSet[,as.character(sig)]
    }
  }
  
  data <- mat %>% as.data.frame() %>% rownames_to_column(var="Connectivity_Id")
  
  return(data) 
  
}


get_connectivity_filter <<- function(
  conn_dat,
  summarize.func = "mean"
){
  
  # create a matrix
  mat <- as.matrix(conn_dat %>% column_to_rownames(var="Connectivity_Id"))
  
  res <- apply(mat, 1, match.fun(summarize.func))
  res <- as.numeric(res)
  
  table <- cbind(conn_dat, SummaryScore = res)
  table <- table[order(table$SummaryScore, decreasing = TRUE),, drop = FALSE]
  colnames(table)[which(colnames(table) %in% "SummaryScore")] <- "Summary Score"
  
  return(table)
  
}

## Create OVERALL plot wrapper for density or boxplot ####
all_wrapper_df <<- function(df, view, marker_id){
  
  df <- df %>% drop_na()
  
  if(view == "Boxplot"){
    
    plot_data <- df %>% 
      summarise_at(
        vars(x), 
        .funs = list(
          min = min,
          q1 = ~quantile(., probs = 0.25),
          median = median,
          q3 = ~quantile(., probs = 0.75),
          max = max
        )
      ) %>% 
      gather(key="stats", value="x") %>% 
      mutate(carcinogencity=marker_id)
    
  }else if(view == "Density"){
    
    plot_data <- density(df$x) %>%  
      extract(c("x", "y")) %>% 
      as.data.frame() %>% 
      mutate(carcinogencity=marker_id)
    
  }
  
  return(plot_data)
  
}

## Create EXPOSURE plot wrapper for density or boxplot ####
exposure_wrapper_df <<- function(df, view, exposure_variable){
  
  df <- df %>% drop_na()
  
  if(view == "Boxplot"){
    
    plot_data <- df %>% 
      group_by(exposure_value) %>% 
      summarise_at(
        vars(x), 
        .funs = list(
          min = min,
          q1 = ~quantile(., probs = 0.25),
          median = median,
          q3 = ~quantile(., probs = 0.75),
          max = max
        )
      ) %>% 
      gather(key="stats", value="x", -exposure_value) %>% 
      mutate(exposure_variable = paste("By", exposure_variable))
    
  }else if(view == "Density"){
    
    plot_data <- df %>% 
      group_by(exposure_value) %>% 
      group_split() %>%
      map_dfr(
        function(dat){
          dff <- dat %>% data.frame()
          
          if(nrow(dff) > 1){
            den <- density(dff$x) %>%
              extract(c("x", "y")) %>%
              as.data.frame() %>%
              mutate(
                exposure_variable = paste("By", exposure_variable),
                exposure_value=unique(dff$exposure_value)
              )
          }
        }
      )
    
  }
  
  return(plot_data)
  
}
  
## Function to create OVERALL density and boxplot for marker explorer #####
get_overall_marker_plot <<- function(
  data, marker_id, view, header
){
  
  ## select color for plotting
  cols_match <- c("grey", "red")
  names(cols_match) <- c("All", marker_id)
  title <- paste0("Distribution of ", header, " Across Profiles for ", marker_id, " (Overall)")
  
  if(view %in% "Density") {
    
    res <- ggplot(data=data, aes(x=x, y=y, fill=carcinogencity)) + 
      geom_area(position='identity', alpha=0.5) +
      geom_line(position='identity')
    
  }else if(view %in% "Boxplot"){
    
    res <- ggplot(data=data, aes(x=carcinogencity, y=x, fill=carcinogencity)) + 
      geom_boxplot(
        position = "identity", 
        width = 0.2,
        alpha = 0.5
      )
    
  }
  
  p <- res +
    scale_fill_manual(
      name = "Legend:",
      values = cols_match,
      breaks = names(cols_match),
      labels = names(cols_match)
    ) +
    xlab("") + 
    ylab("Density") + 
    ggtitle(title) +
    theme_bw() +
    theme(
      plot.margin = margin(0, 5, 0, 0),
      plot.title = element_text(hjust = 0.5, face="bold")
    ) +
    theme(
      legend.direction = "vertical", 
      legend.box = "vertical",
      legend.title = element_text(size=12, face="bold")
    )
  
  p
  
}


## Function to create EXPOSURE density and boxplot for marker explorer #####
get_exposure_marker_plot <<- function(
  df,
  marker_id,
  header = "Mod Z-scores",
  view = "Density"
){

  # Get the exposure variable and select color for plotting
  variable = unique(df$exposure_value) %>% sort(na.last=T)
  cols_match = brewer.pal(9, "Set1")[1:length(variable)]
  names(cols_match) = variable
  n = length(unique(df$exposure_variable))

  p.title <- paste0("Distribution of ", header, " across profiles for ", marker_id)

  if(view %in% "Density") {

    res <- ggplot(data=df, aes(x=x, y=y, fill = exposure_value)) +
      geom_area(position='identity', alpha=0.5) +
      geom_line(position='identity') +
      facet_wrap(~exposure_variable, nrow=n, scales='free') 

  }else if(view %in% "Boxplot"){

    res <- ggplot(data=df, aes(x = exposure_value, y = x, fill = exposure_value)) +
      geom_boxplot(
        position = "identity",
        width = 0.2,
        alpha = 0.5,
        outlier.fill = NULL,
        outlier.alpha = NULL
      ) +
      facet_wrap(~exposure_variable, nrow=n, scales='free')

  }

  p <- res +
    scale_fill_manual(
      name = "Legend:",
      values = cols_match,
      breaks = names(cols_match),
      labels = names(cols_match)
    ) +
    xlab("") +
    ylab("Density") +
    ggtitle(p.title) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5, face="bold", margin = margin(2, 0, 2, 0)),
      strip.text = element_text(size = 12, face="bold")
    ) +
    theme(panel.spacing = unit(1, "lines")) +
    theme(
      legend.direction = "vertical",
      legend.box = "vertical",
      legend.title = element_text(size=12, face="bold", margin = margin(2, 0, 2, 0))
    )

  p

}

## Get the table profiles ranked by mod Z-scores for marker explorer ####
get_marker_table <<- function(
  marker_id,
  expression_dat,
  profile_dat,
  header = "Mod Z-scores",
  tas = c(0, 0.2)){

  #matching expression column names with profile annotation
  annot_var <- ifelse(all(colnames(expression_dat) %in% profile_dat$Sig_Id), "Sig_Id", "Chemical_Id")

  profile_dat <- profile_dat %>%
    filter(as.numeric(TAS) >= as.numeric(tas[1]) & as.numeric(TAS) <= as.numeric(tas[2])) %>%
    mutate(Id=(!!!syms(annot_var)))

  rowid <- which(rownames(expression_dat) %in% marker_id)
  eset <- exprs(expression_dat)[rowid,]

  df <- profile_dat %>% mutate(Id=as.character(Id)) %>%
    left_join(data.frame(Id=as.character(names(eset)), value=as.numeric(eset))) %>%
    select(-Id) %>%
    arrange(desc(value)) %>%
    select(value, everything())

  colnames(df)[which(colnames(df) %in% "value")] <- header

  return(df)

}

