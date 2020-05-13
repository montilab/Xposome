
##CREATE JSON FILES#####
CreateJSON <- function(outputpath, portal, genesetname, ge, proann, nr, nc, rnames, cnames, marker){
  
  #Clean up na in expresssion set
  es_na <- c(which(is.na(ge)), which(is.nan(ge)))
  ge[es_na] <- "null";
  
  #Clean up na in character and numeric string
  character_cols <- sapply(colnames(proann), function(x){ !is.numeric(proann[,x]) })
  character_cols <- names(which(character_cols))
  numeric_cols <- sapply(colnames(proann), function(x){ is.numeric(proann[,x]) })
  numeric_cols <- names(which(numeric_cols))
  
  #Character string
  if(length(character_cols) > 0){
    for(c in character_cols){
      #c=3;
      dat <-  as.character(proann[,c])
      na <- which(is.na(dat))
      
      if(length(na) > 0){
        dat[na] <- "N/A"
      }
      
      l <- grep("\n", dat, fixed = TRUE, useBytes = TRUE)
      
      if(length(l) > 0){
        for(i in l){
          dat[i] <- gsub("\n", " ", dat[i], fixed=TRUE, useBytes = TRUE)
        }
      }
      proann[,c] <- dat
    }
  }
  
  #Round numeric to 3 digits 
  if(length(numeric_cols) > 0){
    for(l in numeric_cols){
      #l=2;
      dat <- round(proann[,l], 3)
      proann[,l] <- dat
    }
  }
  
  #Get which marker is selected;
  if(marker=="Gene Sets"){
    markername <- "genesets"
  }else if(marker=="Genes"){
    markername <- "genes"
  }else if(marker=="Landmark"){
    markername <- "landmark"
  }else if(marker=="Connectivity"){
    markername <- "connectivity_id"
  }
  
  #Output the json file
  sink(outputpath)
  cat(paste0('{', '\n'))
  cat('\t', paste0('"rows": ', nr, ',\n'))
  cat('\t', paste0('"columns": ', nc, ',\n'))
  cat('\t', paste0('"seriesArrays": [[', '\n'))
  series <- NULL;
  for(j in 1:nr){
    #j=1;
    series <- c(series, paste0('[', paste0(ge[j,], collapse = ', '), ']', collapse = ''))
  }
  cat(paste0('\t\t', series, collapse = ',\n'), '\n')
  cat('\t', "]],", "\n")
  cat('\t', paste0('"seriesDataTypes": ["Float32"],', '\n'))
  cat('\t', paste0('"seriesNames": ', '["', genesetname, '"],', '\n'))
  cat('\t', paste0('"rowMetadataModel": {', '\n'))
  cat('\t', '\t', paste0('"vectors": [', '\n'))
  cat('\t', '\t', '\t', paste0('{', '\n'))
  cat('\t', '\t', '\t', '\t', paste0('"name": "', markername, '",', "\n"))
  cat('\t', '\t', '\t', '\t', paste0('"array": [', paste0(paste0('"', rnames, '"'), collapse=", "), ']', '\n'))
  cat('\t', '\t', '\t', paste0('}', '\n'))
  cat('\t', '\t', paste0(']', '\n'))
  cat('\t', paste0('},', '\n'))
  cat('\t', paste0('"columnMetadataModel": {', '\n'))
  cat('\t', '\t', paste0('"vectors": [', '\n'))
  phenovar <- colnames(proann)
  columnseries <- NULL
  for(v in 1:length(phenovar)){
    #v=1;
    dat <- proann[,phenovar[v]]
    
    if(class(dat) != "numeric"){
      columnseries <- c(
        columnseries, 
        paste0(
          paste0('\t\t\t{', '\n'),
          paste0('\t\t\t\t"name": ', '"', phenovar[v], '"', ',', '\n'),
          paste0('\t\t\t\t"array": [', paste0(paste0('"', dat, '"'), collapse=", "), ']', '\n'),
          paste0('\t\t\t}'),
          collapse=""
        )
      )
    }else{
      
      na <- which(is.na(dat))
      
      if(length(na) > 0){
        dat[na] <- "null"
      }
      
      columnseries <- c(
        columnseries, 
        paste0(
          paste0('\t\t\t{', '\n'),
          paste0('\t\t\t\t"name": ', '"', phenovar[v], '"', ',', '\n'),
          paste0('\t\t\t\t"array": [', paste0(dat, collapse=", "), ']', '\n'),
          paste0('\t\t\t}'),
          collapse=""
        )
      )
    }
  }
  cat(paste0(columnseries, collapse = ",\n"), '\n')
  cat('\t', '\t', paste0(']', '\n'))
  cat('\t', paste0('}', '\n'))
  cat("}", sep="")
  sink()
  
}

##CREATE HTML FILES#####
CreateHTML <- function(outputpath, portal, genesetname, proann, marker, cluster=FALSE){
  
  #Create column annotation####
  phenovar <- colnames(proann);
  columnseries <- NULL;
  n <- ifelse(length(phenovar) >= 10, 10, length(phenovar));
  
  for(v in 1:n){
    #v=1;
    columnseries <- c(
      columnseries, 
      paste0(
        paste0('\t\t\t{', '\n'),
        paste0('\t\t\t\tfield: ', '"', phenovar[v], '"', ', display: ["color"]', '\n'),
        paste0('\t\t\t}'),
        collapse=""
      )
    )
  }
  
  #Determine which marker is selected
  if(marker=="Gene Sets"){
    markername <- "genesets"
  }else if(marker=="Genes"){
    markername <- "genes"
  }else if(marker=="Landmark"){
    markername <- "landmark"
  }else if(marker=="Connectivity"){
    markername <- "connectivity_id"
  }
  
  #Determine whether to include cluster
  if(cluster){
    clustermethod <- paste0(
      '\t\t\tname: "Hierarchical Clustering",', '\n',
      '\t\t\tparams: {cluster: "Rows and columns"}', '\n',
      collapse = ""
    )
  }else{
    clustermethod <- paste0(
      '\t\t\t', '\n', 
      '\t\t\t', '\n',
      collapse=""
    )
  }
  
  #Output the html file
  sink(outputpath)
  cat(
    '<!DOCTYPE html>', '\n',
    '<html>', '\n',
    '<head>', '\n',
    '\t', '<meta http-equiv="Content-Type" content="text/html;charset=utf-8">', '\n',
    '\t', '<meta http-equiv="X-UA-Compatible" content="IE=edge">', '\n',
    '\t', '<meta name="viewport" content="user-scalable=no, width=device-width, initial-scale=1, maximum-scale=1">', '\n',
    '\t', '<title>Morpheus</title>', '\n',
    '\t', '<link rel="stylesheet" href="https://software.broadinstitute.org/morpheus/css/morpheus-latest.min.css">', '\n',
    '\t', '<link rel="shortcut icon" href="https://software.broadinstitute.org/morpheus/favicon.ico" type="image/x-icon">', '\n',
    '\t', '<script type="text/javascript" src="https://software.broadinstitute.org/morpheus/js/morpheus-external-latest.min.js"></script>', '\n',
    '\t', '<script src="https://software.broadinstitute.org/morpheus/js/morpheus-latest.min.js"></script>', '\n',
    '</head>', '\n',
    '<body>', '\n',
    '<noscript>', '\n',
    '<p>Please enable JavaScript</p>', '\n',
    '</noscript>', '\n',
    '<div style="width: 100%; padding: 20px 20px 20px 20px;" id="vis"></div>', '\n',
    '<script type="text/javascript">', '\n',
    '\t', 'function xhrSuccess() {', '\n',
    '\t', '\t', 'this.callback.apply(this, this.arguments);', '\n',
    '\t', '}', '\n',
    '\t', 'function xhrError() {', '\n', 
    '\t', '\t', 'console(this.statusText);', '\n', 
    '\t', '}', '\n',
    '\t', 'function loadFile(url, callback) {', '\n', 
    '\t', '\t', 'var xhr = new XMLHttpRequest();', '\n', 
    '\t', '\t', 'xhr.callback = callback;', '\n', 
    '\t', '\t', 'xhr.arguments = Array.prototype.slice.call(arguments, 2);', '\n', 
    '\t', '\t', 'xhr.onload = xhrSuccess;', '\n', 
    '\t', '\t', 'xhr.onerror = xhrError;', '\n', 
    '\t', '\t', 'xhr.open("GET", url, true);', '\n', 
    '\t', '\t', 'xhr.send(null);', '\n', 
    '\t', '}', '\n', 
    '\t', 'function showHeatmap() {', '\n', 
    '\t', '\t', 'var json = JSON.parse(this.responseText);', '\n', 
    '\t', '\t', 'new morpheus.HeatMap({', '\n',
    '\t', '\t', '\t', 'el: $("#vis"),', '\n',
    '\t', '\t', '\t', 'portal: morpheus.portal.fromJSON(json),', '\n',
    '\t', '\t', '\t', 'columns: [', '\n',
    '\t', paste0(columnseries, collapse = ",\n"), '\n',
    '\t', '\t', '\t', '],', '\n',
    '\t', '\t', '\t', 'rows: [ ', '\n',
    '\t', '\t', '\t', '\t', '{', '\n',
    '\t', '\t', '\t', '\t', '\t', paste0('field: "', markername, '", display: ["text"]'), '\n',
    '\t', '\t', '\t', '\t', '}', '\n',
    '\t', '\t', '\t', '],', '\n',
    '\t', '\t', '\t', 'columnFilter: {', '\n',
    '\t', '\t', '\t', '\t', 'filters : [{', '\n',
    '\t', '\t', '\t', '\t', '\t', 'field : "TAS",', '\n',
    '\t', '\t', '\t', '\t', '\t', 'type : "range",', '\n',
    '\t', '\t', '\t', '\t', '\t', 'min : 0,', '\n',
    '\t', '\t', '\t', '\t', '\t', 'max : 1.0', '\n',
    '\t', '\t', '\t', '\t', '}]', '\n',
    '\t', '\t', '\t', '},', '\n',
    '\t', '\t', '\t', 'tools: [{', '\n',
    '\t', clustermethod,
    '\t', '\t', '\t', '}]', '\n',
    '\t', '\t', '});', '\n',
    '\t', "}", "\n",
    paste0('loadFile("', genesetname, '.json", showHeatmap);'), '\n',
    '</script>', '\n',
    '</body>', '\n',
    '</html>',
    sep=""
  )
  sink()
  
}

## Portal data#####
portal_data <- reactiveVal(NULL);

## Observe when add data is clicked ####
observeEvent(input$Add_Project_Add_Button, {
  
  Project=trimws(input$Add_Project_Name);
  Cell_Line=trimws(input$Add_Cell_Line_Name);
  Portal=trimws(input$Add_Portal_Name);
  Experimental_Design=trimws(input$Add_Experimental_Design);
  GS_Enrichment_Version=ifelse(input$add_cur_gs_enrichment_option=="Yes", trimws(input$Add_Enrichment_Version), trimws(input$Add_New_Enrichment_Version));
  Description=trimws(input$Add_Description);  
  
  proj_dat <- data.frame(projectdata())
  new_proj <- data.frame(
    Project=Project, 
    Cell_Line=Cell_Line, 
    Portal=Portal, 
    Experimental_Design=Experimental_Design,
    GS_Enrichment_Version=GS_Enrichment_Version, 
    Description=Description,
    stringsAsFactors = FALSE
  )
  
  if(Project=="" | Cell_Line=="" | Portal=="" | Experimental_Design=="" | is.na(GS_Enrichment_Version) | is.character(as.numeric(GS_Enrichment_Version)) | as.numeric(GS_Enrichment_Version) <= 0 | Description==""){
    addprojectwarningmsg("Please fill in the required (*) fields.")
    return(NULL)
  }
  
  if(input$add_conn_option %in% "Yes" & (is.null(conn_pcl_file()) | is.null(conn_pert_file()))){
    return(NULL)
  }
  
  if(is.null(intro_file()) | is.null(pro_file()) | is.null(chem_file()) | is.null(ge_file())){
    return(NULL)
  } 
  
  #Validate to see the project exists
  validate_proj <- proj_dat %>% 
    filter(
      Project %in% !!Project
    )
  
  if(nrow(validate_proj) > 0){
    
    addprojectwarningmsg("This project is already existed. Please enter another project name.")
    
  }else{
    
    addprojectwarningmsg("")
    
    ##########################################################################################
    #
    # CALCULATE MOD Z-SCORES AND REPLICATE CORRELATION (CC) 
    # 
    ##########################################################################################
    
    #Get the gene expression, profile and chemical annotation;
    gene_expression <- ge_file(); chem_ann <- chem_file(); pro_ann <- pro_file();
    
    #Whether to add TAS and Modzscores
    Add_TAS = TRUE #input$Add_TAs; 
    Add_Modzscores = FALSE #input$Add_Modzscores; 
    add_conn_option = "Yes" #input$add_conn_option;
    add_cur_gs_enrichment_option = "Yes" #input$add_cur_gs_enrichment_option;
    
    gene_expression <- readRDS(paste0("~/Documents/Internship Project/Exposome app/K2Example/TGGates_Example/Expression_Set.RDS"))
    chem_ann <- readRDS(paste0("~/Documents/Internship Project/Exposome app/K2Example/TGGates_Example/Chemical_Annotation.RDS"))
    pro_ann <- readRDS(paste0("~/Documents/Internship Project/Exposome app/K2Example/TGGates_Example/Profile_Annotation.RDS"))
    
    if(Add_TAS %in% TRUE | Add_Modzscores %in% TRUE){
      
      # Define a  data frame to store number of replicates and ModZscores
      chem_replicate <- data.frame(Chemical_Id = unique(chem_file$Chemical_Id), N_replicates = NA, CC = NA, SS=NA, TAS = NA, stringsAsFactors = FALSE)
      
      # Create the modzscore expression set
      ModZ_Expression_Set <- matrix(NA, nrow=nrow(gene_expression), ncol=nrow(chem_replicate), byrow=T, dimnames=list(rownames(gene_expression), chem_replicate$Chemical_Id))

      # Getting the number of replicates for each chemical
      for(i in 1:nrow(chem_replicate)){
        #i=1;
        chem_replicate$N_replicates[i] <- nrow(pro_ann %>% filter(Chemical_Id %in% chem_replicate$Chemical_Id[i]))
        
        rep <- rownames(pro_ann)[which(pro_ann$Chemical_Id %in% chem_replicate$Chemical_Id[i])]
        
        if(length(rep) == 1){
          
          chem_replicate$CC[i] <- 1
          
          w=1 #unweighted
          
          #Calculate the modzscore for the chemical
          modzscore <-  w*gene_expression[,rep]
          
          ModZ_Expression_Set[,chem_replicate$Chemical_Id[i]] <- modzscore
          
        }else {
          
          # Calculate the spearman correlation between the pairwise replicates 
          spearman_corr <- matrix(NA, nrow=length(rep), ncol=length(rep), byrow=T, dimnames=list(rep, rep))
          
          for(l in 1:nrow(spearman_corr)){
            for(k in 1:nrow(spearman_corr)){
              #l=1, k=1;
              spearman_corr[l,k] <- cor(gene_expression[,rep[l]], gene_expression[,rep[k]], method = c("spearman"))
            }
          }
          
          #CALCULATE THE MODZ-SCORES
          if(length(rep) == 2){
            
            w=0.5 #unweighted
            
            #Calculate the modzscore for the chemical
            modzscore <- 0
            
            for(m in 1:length(rep)){
              #m=2;
              modzscore  <- w*gene_expression[,rep[m]] + modzscore
            }
            
            ModZ_Expression_Set[,chem_replicate$Chemical_Id[i]] <- modzscore
            
          }else{
            
            rep_corr <- NULL;
            
            for(g in 1:length(rep)){
              #g=3;
              corr <- spearman_corr[rep[g],rep[which(!rep %in% rep[g])]]
              rep_corr <- c(rep_corr, abs(sum(corr, na.rm=T)));
            }
            
            w <- rep_corr/sum(rep_corr, na.rm=TRUE)
            
            if(1-sum(w, na.rm=T) > 0.001) {
              print(rep_corr)
              print(w)
              print('incorrect calculation of w!')
            }
            
            #Calculate the modzscore for the chemical
            modzscore <- 0
            
            for(m in 1:length(rep)){
              #m=2;
              modzscore  <- w[m]*gene_expression[,rep[m]] + modzscore
            }
            
            ModZ_Expression_Set[,chem_replicate$Chemical_Id[i]] <- modzscore
            
          }
          
          #CALCULATE THE CC
          corr <- spearman_corr[lower.tri(spearman_corr)]
          quan_75 <- quantile(corr, 0.75, na.rm = T)
          chem_replicate$CC[i] <- quan_75
          
        }

        chem_replicate$SS[i] = length(which(abs(ModZ_Expression_Set[,i]) >= 2))
        chem_replicate$TAS[i] = sqrt(chem_replicate$SS[i]*max(c(0,chem_replicate$CC[i]))/nrow(ModZ_Expression_Set)) 
      }
      
      TAS <- chem_replicate[, c("Chemical_Id", "TAS")]
      
      # chemical annotation information
      chem_ann <- chem_ann %>% left_join(TAS) 
      
      # profile annotation information
      pro_ann <- pro_ann %>% left_join(TAS)
      
      # profile annotation information
      gene_expression <- ModZ_Expression_Set
      
    }
    
    ##Create the portal list#####
    portalDat <- list();
    
    #create the profile annotation data####
    profile_info <- data.frame(pro_ann, stringsAsFactors = FALSE)
    rownames(profile_info) <- profile_info$Sig_Id
    portal_data[["Profile Annotation"]] <- profile_info
    
    #create the chemical annotation data####
    chemical_info <- data.frame(chem_ann, stringsAsFactors = FALSE)
    rownames(chemical_info) <- chemical_info$Chemical_Id
    portal_data[["Chemical Annotation"]] <- chemical_info
    
    #create phenotypic data for gene expression
    pData <- data.frame(Sig_Id=colnames(gene_expression), stringsAsFactors = FALSE)
    rownames(pData) <- pData$Sig_Id
    phenoData <- new("AnnotatedDataFrame", data=pData)
    
    #create feature data for gene expression
    fData <- data.frame(Gene = rownames(gene_expression), stringsAsFactors = FALSE)
    rownames(fData) <- fData$Gene
    featureData <- new("AnnotatedDataFrame", data=fData)
    
    #create gene expression set####
    expressionSet <- matrix(gene_expression, nrow=nrow(gene_expression), ncol=ncol(gene_expression), byrow=TRUE, dimnames=list(rownames(gene_expression), colnames(gene_expression)))
    eSet <- ExpressionSet(assayData=expressionSet, phenoData=phenoData, featureData=featureData)
    portalDat[["Gene Expression"]] <-  eSet
    
    # Create a null list for connectivity map####
    connectivity_map <- list();
    
    if(add_conn_option %in% "Yes"){
      
      conn_pcl <- conn_pcl_file(); conn_perl <- conn_pcl_file()
      
      # Create classes for CMap connectivity #####
      connmap <- c("pcl", "pert")
      
      for(p in 1:length(connmap)){
        #p=1;
        if(classes[c] %in% "pcl"){
          conn_ge <- matrix(conn_pcl, nrow=nrow(conn_pcl), ncol=ncol(conn_pcl), byrow=TRUE, dimnames=list(rownames(conn_pcl), colnames(conn_pcl)))
        }else{
          conn_ge <- matrix(conn_pert, nrow=nrow(conn_pert), ncol=ncol(conn_pert), byrow=TRUE, dimnames=list(rownames(conn_pert), colnames(conn_pert)))
        }
        
        #create phenotypic data
        pData <- data.frame(Sig_Id=colnames(conn_ge), stringsAsFactors = FALSE)
        rownames(pData) <- pData$Sig_Id
        phenoData <- new("AnnotatedDataFrame", data=pData)
        
        #create feature data
        fData <- data.frame(Gene=rownames(conn_ge), stringsAsFactors = FALSE)
        rownames(fData) <- fData$Gene
        featureData <- new("AnnotatedDataFrame", data=fData)
        
        #create expression set
        expressionSet <- conn_ge()
        eSet <- ExpressionSet(assayData=expressionSet, phenoData=phenoData, featureData=featureData)
        connectivity_map[[paste0(connmap[p])]] <- eSet

        #create morpheus heatmap for connectivity map
        genesetname <- paste0(connmap[p], "_connectivity")
        ge <- expressionSet
        
        nr <- nrow(ge); rnames <- rownames(ge);
        nc <- ncol(ge); cnames <- colnames(ge);
        portal=Portal; marker <- "Connectivity"; cluster <- FALSE;
        
        jsonoutputpath=dir.create(paste0("www/JSON/", Portal, "/", genesetname, ".json"), showWarnings = FALSE, recursive = TRUE);
        htmloutputpath=dir.create(paste0("www/JSON/", Portal, "/", genesetname, ".html"), showWarnings = FALSE, recursive = TRUE);
        
        if(all(rownames(proann) %in% cnames)){
          CreateJSON(outputpath=jsonoutputpath, portal=portal, genesetname=genesetname, ge=ge, proann=proann, nr=nr, nc=nc, rnames=rnames, cnames=cnames, marker=marker)
          CreateHTML(outputpath=htmloutputpath, portal=portal, genesetname=genesetname, proann=proann, marker=marker, cluster=cluster)
        }else{
          CreateJSON(outputpath=jsonoutputpath, portal=portal, genesetname=genesetname, ge=ge, proann=chemann, nr=nr, nc=nc, rnames=rnames, cnames=cnames, marker=marker)
          CreateHTML(outputpath=htmloutputpath, portal=portal, genesetname=genesetname, proann=chemann, marker=marker, cluster=cluster)
        }
        
      }
      
      closeAllConnections() 
      
    }
    
    portalDat[["Connectivity"]] <- connectivity_map
    
    #create gene set enrichment####
    gsscores <- list(); 
    
    # Read in the gene set collection
    if(add_cur_gs_enrichment_option=="Yes"){
      gsscores_hallmark <- getGmt(pase0("data/Enrichment Gene Set/v", GS_Enrichment_Version, "/h.all.v", GS_Enrichment_Version, ".0.gmt"))
      gsscores_c2_reactome <- getGmt(pase0("data/Enrichment Gene Set/v", GS_Enrichment_Version, "/c2.cp.reactome.v", GS_Enrichment_Version, ".0.gmt"))
      gsscores_nursa <- getGmt(pase0("data/Enrichment Gene Set/v", GS_Enrichment_Version, "/nursa_consensome_Cbyfdrvalue_0.01_v", GS_Enrichment_Version, ".0.gmt"))
    }else{
      gsscores_hallmark <- hall_mark_file()
      gsscores_c2_reactome <- c2_file()
      gsscores_nursa <- nursa_file()
    }  
    
    # Run gene set enrichment analysis for hallmark, C2, and NURSA
    genesetname=c(paste0("h.all.v", GS_Enrichment_Version, ".0"), paste0("c2.cp.reactome.v", GS_Enrichment_Version, ".0"), paste0("nursa_consensome_Cbyfdrvalue_0.01_v", GS_Enrichment_Version, ".0")); 
    geneset=c("gsscores_hallmark", "gsscores_c2_reactome", "gsscores_nursa"); 
    method=c("gsva", "ssgsea", "zscore");
    
    # Getting differential expression 
    gene_expression <- portalDat[["Gene Expression"]]@assayData[["exprs"]];
    
    for(u in 1:length(genesetname)){
      #u=1;
      for(m in 1:length(method)){
        #m=1;
        gsva_es <- gsva(expr=gene_expression, gset.idx.list=get(paste0(geneset[u])), method=method[m], mx.diff=TRUE)
        
        #create phenotypic data
        pData <- data.frame(Sig_Id = colnames(gsva_es), stringsAsFactors = FALSE)
        rownames(pData) <- pData$Sig_Id
        phenoData <- new("AnnotatedDataFrame", data=pData)
        
        #create feature data
        fData <- data.frame(Gene=rownames(gsva_es), stringsAsFactors = FALSE)
        rownames(fData) <- fData$Gene
        featureData <- new("AnnotatedDataFrame", data=fData)
        
        eset <- ExpressionSet(assayData=gsva_es, phenoData=phenoData, featureData=featureData)
        gsscores[[paste0("gsscores_", genesetname[u], "_", method[m])]] <- eset
        
        ##Create output data
        ge <- gsva_es;
        
        nr <- nrow(ge); rnames <- rownames(ge);
        nc <- ncol(ge); cnames <- colnames(ge);
        portal=Portal;
        marker <- "Gene Sets"; cluster <- FALSE;
        
        jsonoutputpath=paste0("www/JSON/", portal, "/", genesetname[u], "_", method[m], ".json")
        htmloutputpath=paste0("www/JSON/", portal, "/", genesetname[u], "_", method[m], ".html")
        
        if(all(rownames(proann) %in% cnames)){
          CreateJSON(outputpath=jsonoutputpath, dataset=portal, genesetname=paste0(genesetname[u], "_", method[m]), ge=ge, proann=proann, nr=nr, nc=nc, rnames=rnames, cnames=cnames, marker=marker)
          CreateHTML(outputpath=htmloutputpath, dataset=portal, genesetname=paste0(genesetname[u], "_", method[m]), proann=proann, marker=marker, cluster=cluster)
        }else{
          CreateJSON(outputpath=jsonoutputpath, dataset=portal, genesetname=paste0(genesetname[u], "_", method[m]), ge=ge, proann=chemann, nr=nr, nc=nc, rnames=rnames, cnames=cnames, marker=marker)
          CreateHTML(outputpath=htmloutputpath, dataset=portal, genesetname=paste0(genesetname[u], "_", method[m]), proann=chemann, marker=marker, cluster=cluster)
        }
        
      }
    }
    
    portalDat[["Gene Set Enrichment"]] <- gsscores
    
    ##create portal title and introduction page
    portalDat[["title"]] <- paste0(Portal, " Portal")
    portalDat[["about page"]] <- paste0("introduction_", Portal, ".Rmd")
    
    newproject <- proj_dat %>% rbind(new_proj)
    projectdata(newproject)
    project_table_message(paste0(Project, ' portal has been added.'))
    removeModal()  
    
  }
  
})



