
## Observe when add data is clicked ####
observeEvent({
  input$Add_Project_Name
  input$Add_Cell_Line_Name
  input$Add_Portal_Name
  input$Add_Description
}, {
  
  ##obtain the input values
  Project=trimws(input$Add_Project_Name);
  Cell_Line=trimws(input$Add_Cell_Line_Name);
  Portal=trimws(input$Add_Portal_Name);
  Description=trimws(input$Add_Description); 
  
  if(Project=="" | Cell_Line=="" | Portal=="" | Description==""){
    addprojectwarningmsg("Please fill in the required (*) fields.")
  }else{
    addprojectwarningmsg("")
  }
  
}, ignoreInit=TRUE)

## Observe when add data is clicked ####
observeEvent(input$Add_Project_Add_Button, {
  
  ##obtain the current project list
  proj_dat <- data.frame(projectdata())
  
  ##obtain the input values
  Project=trimws(input$Add_Project_Name);
  Cell_Line=trimws(input$Add_Cell_Line_Name);
  Portal=trimws(input$Add_Portal_Name);
  Description=trimws(input$Add_Description);  
  Landmark_Gene=trimws(input$Add_Landmark);  
  Enrichment_Version=7;
  
  ## check all the input variables###
  check <- c();
  
  if(Project=="" | Cell_Line=="" | Portal=="" | Description==""){
    addprojectwarningmsg("Please fill in the required (*) fields.")
    check <- c(check, "No")
  }else{
    addprojectwarningmsg("")
  }
  
  GS_Collection=ifelse(input$add_cur_enrichment_option=="Yes", "Default", trimws(input$Add_New_Enrichment_GS));

  if(GS_Collection==""){
    addenrichmentgswarningmsg("Please enter a valid name.")
    check <- c(check, "No")
  }else{
    addenrichmentgswarningmsg("")
  }
  
  GS_Collection_Link=ifelse(input$add_cur_enrichment_option=="Yes", "https://www.gsea-msigdb.org/gsea/msigdb; https://signalingpathways.org/", trimws(input$Add_New_Enrichment_Link));
  
  if(GS_Collection_Link==""){
    addenrichmentlinkwarningmsg("Please enter a valid link.")
    check <- c(check, "No")
  }else{
    addenrichmentlinkwarningmsg("")
  }
  
  if(!is.null(pro_file())){
    
    Compound=input$add_variable_compound; 

    if(Compound==""){
      addcompoundvarwarningmsg("Please pick a selection.")
      check <- c(check, "No")
    }else{
      addcompoundvarwarningmsg("")
    }
    
    Exposure=input$add_variable_exposure;
  
    if(length(Exposure)==0){
      addexposurevarwarningmsg("Please pick a selection.")
      check <- c(check, "No")
    }else{
      addexposurevarwarningmsg("")
    }
    
    Exposure_Phenotype=input$add_variable_exposure_phenotype;
    
    if(length(Exposure_Phenotype)==0){
      addexposurephenotypevarwarningmsg("Please pick a selection.")
      check <- c(check, "No")
    }else{
      for(v in seq_along(Exposure_Phenotype)){ 
        #v=1;
        if(is.null(input[[paste0("metavar_", Exposure_Phenotype[v])]])){
          check <- c(check, "No")
        }
      }
      addexposurephenotypevarwarningmsg("")
    }
  }
  
  if(is.null(intro_file()) | is.null(pro_file()) | is.null(chem_file()) | is.null(ge_file())){
    check <- c(check, "No")
  } 
  
  if(input$add_conn_option %in% "Yes" & (is.null(conn_pcl_file()) | is.null(conn_pert_file()))){
    check <- c(check, "No")
  }

  if(input$add_cur_enrichment_option %in% "No" & is.null(gs_collection_file())){
    check <- c(check, "No")
  }
  
  if(any(check %in% "No")) { addinputwarningmsg("Please fill in the required (*) fields."); return(NULL) }
  
  #Validate to see the project exists
  validate_proj <- proj_dat %>% 
    filter(
      Project %in% !!Project
    )
  
  #Validate to see the project exists
  validate_portal <- proj_dat %>% 
    filter(
      Portal %in% !!Portal
    )
  
  if(nrow(validate_proj) > 0 | nrow(validate_portal) > 0){
    
    if(nrow(validate_proj) > 0){
      addinputwarningmsg("This project is already existed. Please enter another project name.")
    }
    
    if(nrow(validate_portal) > 0){
      addinputwarningmsg("This portal name is already existed. Please enter another portal name.")
    }
    
    if(nrow(validate_proj) > 0 & nrow(validate_portal) > 0){
      addinputwarningmsg("Both project and portal name are already existed. Please enter another name for project and portal.")
    }
    
  }else{
    
    shinyjs::disable(id="Add_Project_Add_Button")
    shinyjs::disable(id="Add_Project_Cancel_Button")
    
    addprojectwarningmsg(""); addinputwarningmsg(""); #remove all warning messages
    
    ##Getting introduction data####
    intro_file <- intro_file();
    
    ##Getting chemical and profile annotation####
    chem_ann <- chem_file(); pro_ann <- pro_file();
    
    ##Getting gene expression####
    add_ge_file_type <- input$add_ge_file_type; Add_Landmark <- input$Add_Landmark;
    
    if(add_ge_file_type %in% ".csv"){
      gene_expression <- ge_file()
    }else{
      gene_expression <- exprs(ge_file())
    }
    
    gene_expression <- as.matrix(gene_expression, nrow=nrow(gene_expression), ncol=ncol(gene_expression), byrow=TRUE, dimnames=list(rownames(gene_expression), colnames(gene_expression)))
    var <- ifelse(all(colnames(gene_expression) %in% pro_ann$Sig_Id), "Sig_Id", "Chemical_Id")
    
    #create phenotypic data
    pData <- data.frame(pro_ann[match(colnames(gene_expression), pro_ann[,var]),], stringsAsFactors=TRUE)
    pData <- pData[!duplicated(pData[,var]),]
    rownames(pData) <- pData[,var]
    phenoData <- new("AnnotatedDataFrame", data=pData)
    
    #create feature data
    if(add_ge_file_type == ".RDS"){
      
      fData <- fData(ge_file())
      
      if(nrow(fData)==0){
        fData <- data.frame(Gene=rownames(gene_expression), stringsAsFactors = TRUE)
        rownames(fData) <- fData$Gene
      }else{
        if(!"Gene" %in% colnames(fData)){
          fData <- data.frame(Gene=rownames(gene_expression), fData, stringsAsFactors = TRUE)
          rownames(fData) <- fData$Gene
        }
      }
      
    }else{
      
      fData <- data.frame(Gene=rownames(gene_expression), stringsAsFactors = TRUE)
      rownames(fData) <- fData$Gene
      
    }
    
    #create feature data
    featureData <- new("AnnotatedDataFrame", data=fData)
    
    #create expression set
    orig_expressionSet <- ExpressionSet(assayData=gene_expression, phenoData=phenoData, featureData=featureData)
    
    ##Getting the variables annotation
    if(is.null(input$Add_TAS)){ Add_TAS <- FALSE }else{ Add_TAS <- input$Add_TAS }
    if(is.null(input$Add_Modzscores)){ Add_Modzscores <- FALSE }else{ Add_Modzscores <- input$Add_Modzscores}
    
    ##Getting connectivity map####
    add_conn_option <- input$add_conn_option;
    add_conn_pcl_file_type <- input$add_conn_pcl_file_type;
    add_conn_pert_file_type <- input$add_conn_pert_file_type;
    
    if(add_conn_option %in% "Yes"){
      if(add_conn_pcl_file_type %in% ".csv"){
        conn_pcl <- conn_pcl_file()
      }else{
        conn_pcl_dat <- conn_pcl_file()
        conn_pcl <- exprs(conn_pcl_file())       
      }
      
      if(add_conn_pert_file_type %in% ".csv"){
        conn_pert <- conn_pert_file()
      }else{
        conn_pert_dat <- conn_pert_file()
        conn_pert <- exprs(conn_pert_file())       
      }
    }
    
    ##Gene set enrichment####
    add_cur_enrichment_option <- input$add_cur_enrichment_option;
    
    # Read in the gene set collection for gene set enrichment analysis###
    if(add_cur_enrichment_option=="Yes"){
      gsscores_hallmark <- getGmt(paste0("data/Enrichment Gene Set/h.all.v", Enrichment_Version, ".0.gmt"))
      gsscores_c2_reactome <- getGmt(paste0("data/Enrichment Gene Set/c2.cp.reactome.v", Enrichment_Version, ".0.gmt"))
      gsscores_nursa <- getGmt(paste0("data/Enrichment Gene Set/nursa_consensome_Cbyfdrvalue_0.01.gmt"))
    }else{
      gs_collection_path <- gs_collection_file() %>% extract2("path")
      gs_collection <- gs_collection_file() %>% extract2("data")
      ##Save the new gene set file####
      file.copy(from=gs_collection_path, to=file.path("data/Enrichment Gene Set", paste0(GS_Collection, ".gmt")), overwrite=TRUE)
    }
    
    ##Taxonomer parameters####
    cohorts <- cohorts(); featMetric <- input$add_feature_metric; ssGSEAalg <- input$add_ssGSEA_method; 
    connectivity_var <- input$add_connectivity_var; connectivity_test <- "";

    if(!is.null(connectivity_var)){ 
      if(connectivity_var){
        connectivity_var <- TRUE
        connectivity_test <- input$add_connectivity_test 
      }else{
        connectivity_var <- FALSE 
      }
    }else{
      connectivity_var <- FALSE 
    }
    
    ##Get the meta-variable test####
    methods <- unlist(lapply(seq_along(Exposure_Phenotype), function(v){ input[[paste0("metavar_", Exposure_Phenotype[v])]] }))
    
    ##Create new progress bar
    progress <- AsyncProgress$new(message = "Creating new portal:", value=0)
    
    fut <- future({
      
      ##Shiny Packages####
      require(K2Taxonomer)
      require(visNetwork) #
      require(Biobase) #
      require(BiocGenerics)
      
      ##Create new portal directory to store data and results####
      dir.create(paste0("data/", Portal), showWarnings=FALSE, recursive=TRUE)
      dir.create(paste0("www/JSON/", Portal), showWarnings=FALSE, recursive=TRUE)
      dir.create(paste0("www/RMD"), showWarnings=FALSE, recursive=TRUE)
      
      ##########################################################################################
      #
      # INTRODUCTION PAGE####
      #
      ##########################################################################################
      
      progress$inc(1/10, detail = "Saving introduction page")
      
      ##create introduction page
      file.copy(from=intro_file, to=paste0("www/RMD/introduction_", Portal, ".Rmd"), overwrite=TRUE)
      
      print("Saving introduction page")
      
      ##########################################################################################
      #
      # CHEMICAL AND PROFILE ANNOTATION####
      #
      ##########################################################################################
      
      progress$inc(2/10, detail = "Saving chemical and profile annotation")
      
      #Get the gene expression, profile and chemical annotation;
      # gene_expression <- read.csv(paste0("~/Documents/Internship Project/Exposome app/K2Example/TGGates_Example/Expression_Set.csv"), header=TRUE, row.names=1, check.names=FALSE, stringsAsFactors = TRUE)
      # chem_ann <- read.csv(paste0("~/Documents/Internship Project/Exposome app/K2Example/TGGates_Example/Chemical_Annotation.csv"), header=TRUE, row.names=1, check.names=FALSE, stringsAsFactors = TRUE)
      # pro_ann <- read.csv(paste0("~/Documents/Internship Project/Exposome app/K2Example/TGGates_Example/Profile_Annotation.csv"), header=TRUE, row.names=1, check.names=FALSE, stringsAsFactors = TRUE)

      ##########################################################################################
      #
      # GET THE UNIQUE ID BY CHEM FOR PRO ANN####
      #
      ##########################################################################################
      pro_ann$unique_ID_by_chem <- lapply(1:nrow(pro_ann), function(r){ paste0(unlist(pro_ann[r,Exposure]), collapse="_") }) %>% unlist()
      
      ##########################################################################################
      #
      # CALCULATE MOD Z-SCORES AND REPLICATE CORRELATION (CC) FOR TAS####
      #
      ##########################################################################################
 
      if(Add_TAS==TRUE | Add_Modzscores==TRUE){
        
        #print(head(pro_ann)); print(dim(pro_ann)); print(head(chem_ann)); print(dim(chem_ann));
        calc_results <- TAS_Modzscores_Calculation(pro_ann=pro_ann, chem_ann=chem_ann, gene_expression=gene_expression)
        
        if(Add_TAS){
          # get TAS
          TAS <- calc_results[["TAS"]]
          
          # chemical annotation information
          chem_ann <- chem_ann %>% left_join(TAS) %>% select(-Number_of_Replicates) 
          
          # profile annotation information
          pro_ann <- pro_ann %>% left_join(TAS)
        }
        
        # get modzscores
        if(Add_Modzscores){
          Modzscores <- calc_results[["Modzscores"]]
          gene_expression <- Modzscores
        }
        
      }
      
      ##save data to portal folder####
      saveRDS(pro_ann, paste0("data/", Portal, "/Profile_Annotation.RDS"))
      saveRDS(chem_ann, paste0("data/", Portal, "/Chemical_Annotation.RDS"))
      
      print("Saving chemical and profile annotation")
      
      ##########################################################################################
      #
      # GENE EXPESSION ####
      #
      ##########################################################################################
      
      progress$inc(3/10, detail = "Saving expression set")
      
      #Create a expression set####
      gene_expression <- as.matrix(gene_expression, nrow=nrow(gene_expression), ncol=ncol(gene_expression), byrow=TRUE, dimnames=list(rownames(gene_expression), colnames(gene_expression)))
      var <- ifelse(all(colnames(gene_expression) %in% pro_ann$Sig_Id), "Sig_Id", "Chemical_Id")
      
      #create phenotypic data
      pData <- data.frame(pro_ann[match(colnames(gene_expression), pro_ann[,var]),], stringsAsFactors=TRUE)
      pData <- pData[!duplicated(pData[,var]),]
      rownames(pData) <- pData[,var]
      phenoData <- new("AnnotatedDataFrame", data=pData)
      
      #create feature data
      fData <- fData(orig_expressionSet)
      
      if(Add_Landmark){
        landmark <- readRDS("data/Landmark/landmark_gene.RDS")
        
        if("Landmark_Gene" %in% colnames(fData)){
          fData <- fData %>% select(-Landmark_Gene)
        }
        
        fData <- (fData %>% left_join(landmark, by="Gene")) %>% replace_na(list(Landmark_Gene="Unknown"))
        rownames(fData) <- fData$Gene
      }
      
      featureData <- new("AnnotatedDataFrame", data=fData)
      
      #create expression set
      expressionSet <- ExpressionSet(assayData=gene_expression, phenoData=phenoData, featureData=featureData)
      
      ##Add data to portal object####
      saveRDS(expressionSet, paste0("data/", Portal, "/Expression_Set.RDS"))
      print("Saving gene expression file")
      
      ##Create morpheus heatmap####
      eset <- expressionSet
      landmark <- colnames(fData(eset)) %in% "Landmark_Gene"
      
      if(any(landmark %in% TRUE)){
        genesetname <- paste0("landmark_gene")
        inds <- fData(eset)[,"Landmark_Gene"] %in% "Yes"
        eset <- eset[inds,]
        marker <- "Landmark"
        cluster <- FALSE
      }else{
        genesetname <- paste0("gene_expression")
        eset <- eset
        marker <- "Genes"
        cluster <- FALSE
      }
      
      ge <- exprs(eset)
      feature <- fData(eset)
      cnames <- colnames(ge)
      nr <- nrow(ge); nc <- ncol(ge);
      
      jsonoutputpath=paste0("www/JSON/", Portal, "/", genesetname, ".json")
      htmloutputpath=paste0("www/JSON/", Portal, "/", genesetname, ".html")
      
      if(var %in% "Sig_Id"){
        CreateJSON(outputpath=jsonoutputpath, dataset=Portal, genesetname=genesetname, ge=ge, proann=pro_ann, feature=feature, nr=nr, nc=nc, marker=marker)
        CreateHTML(outputpath=htmloutputpath, dataset=Portal, genesetname=genesetname, proann=pro_ann, feature=feature, marker=marker, cluster=cluster)
      }else{
        CreateJSON(outputpath=jsonoutputpath, dataset=Portal, genesetname=genesetname, ge=ge, proann=chem_ann, feature=feature, nr=nr, nc=nc, marker=marker)
        CreateHTML(outputpath=htmloutputpath, dataset=Portal, genesetname=genesetname, proann=chem_ann, feature=feature, marker=marker, cluster=cluster)
      }
      
      print("Saving gene expression Morpheous heatmap")
      
      ##########################################################################################
      #
      # CONNECTIVITY MAP#####
      #
      ##########################################################################################
      
      progress$inc(4/10, detail = "Saving connectivity map")
      
      if(add_conn_option %in% "Yes"){
        
        # Create a null list for connectivity map####
        connectivity_map <- list(pcl=NULL, pert=NULL);
        
        # Create classes for CMap connectivity #####
        connmap <- c("pcl", "pert")
        
        for(p in 1:length(connmap)){
          #p=1;
          add_conn_file_type <- get(paste0("add_conn_", connmap[p], "_file_type"))
          conn_dat <- get(paste0("conn_", connmap[p], "_dat"))
          
          if(connmap[p] %in% "pcl"){
            conn_ge <- as.matrix(conn_pcl, nrow=nrow(conn_pcl), ncol=ncol(conn_pcl), byrow=TRUE, dimnames=list(rownames(conn_pcl), colnames(conn_pcl)))
          }else{
            conn_ge <- as.matrix(conn_pert, nrow=nrow(conn_pert), ncol=ncol(conn_pert), byrow=TRUE, dimnames=list(rownames(conn_pert), colnames(conn_pert)))
          }
          
          var <- ifelse(all(colnames(conn_ge) %in% pro_ann$Sig_Id), "Sig_Id", "Chemical_Id")
          
          #create phenotypic data
          pData <- data.frame(pro_ann[match(colnames(conn_ge), pro_ann[,var]),], stringsAsFactors = TRUE)
          pData <- pData[!duplicated(pData[,var]),]
          rownames(pData) <- pData[,var]
          phenoData <- new("AnnotatedDataFrame", data=pData)
          
          #create feature data
          if(add_conn_file_type == ".RDS"){
            
            fData <- fData(conn_dat)
            
            if(nrow(fData)==0){
              fData <- data.frame(Connectivity_Id=rownames(conn_ge), stringsAsFactors = TRUE)
              rownames(fData) <- fData$Connectivity_Id
            }else{
              if(!"Gene" %in% colnames(fData)){
                fData <- data.frame(Gene=rownames(conn_ge), fData, stringsAsFactors = TRUE)
                rownames(fData) <- fData$Gene
              }
            }
            
          }else{
            
            fData <- data.frame(Connectivity_Id=rownames(conn_ge), stringsAsFactors = TRUE)
            rownames(fData) <- fData$Connectivity_Id
            
          }
          
          #create feature data
          featureData <- new("AnnotatedDataFrame", data=fData)
          
          #create expression set
          conn_expressionSet <- conn_ge
          eSet <- ExpressionSet(assayData=conn_expressionSet, phenoData=phenoData, featureData=featureData)
          connectivity_map[[paste0(connmap[p])]] <- eSet
          
          #create morpheus heatmap for connectivity map
          genesetname <- paste0(connmap[p], "_connectivity")
          eset <- eSet
          ge <- exprs(eset)
          feature <- fData(eset)
          cnames <- colnames(ge)
          
          nr <- nrow(ge); nc <- ncol(ge);
          marker <- "Connectivity"; cluster <- FALSE;
          
          jsonoutputpath=paste0("www/JSON/", Portal, "/", genesetname, ".json")
          htmloutputpath=paste0("www/JSON/", Portal, "/", genesetname, ".html")
          
          if(var %in% "Sig_Id"){
            CreateJSON(outputpath=jsonoutputpath, dataset=Portal, genesetname=genesetname, ge=ge, proann=pro_ann, feature=feature, nr=nr, nc=nc, marker=marker)
            CreateHTML(outputpath=htmloutputpath, dataset=Portal, genesetname=genesetname, proann=pro_ann, feature=feature, marker=marker, cluster=cluster)
          }else{
            CreateJSON(outputpath=jsonoutputpath, dataset=Portal, genesetname=genesetname, ge=ge, proann=chem_ann, feature=feature, nr=nr, nc=nc, marker=marker)
            CreateHTML(outputpath=htmloutputpath, dataset=Portal, genesetname=genesetname, proann=chem_ann, feature=feature, marker=marker, cluster=cluster)
          }
        }
        
        ##save data to portal folder####
        saveRDS(connectivity_map, paste0("data/", Portal, "/Connectivity.RDS"))
        
      }
      
      print("Saving connectivity map file")
      print("Saving connectivity Morpheous heatmap")
      
      ##########################################################################################
      #
      # GENE SET ENRICHMENT ANALYSIS####
      #
      ##########################################################################################
      
      progress$inc(5/10, detail = "Saving gene set enrichment results")
      
      #create gene set enrichment####
      gsscores <- list(); gsvamethod=ssGSEAalg;
      
      if(add_cur_enrichment_option == "No"){
        genesetcollection=GS_Collection;
        geneset="gs_collection";
      }else{
        # Run gene set enrichment analysis for hallmark, C2, and NURSA
        genesetcollection=c(paste0("h.all.v", Enrichment_Version, ".0"), paste0("c2.cp.reactome.v", Enrichment_Version, ".0"), paste0("nursa_consensome_Cbyfdrvalue_0.01"));
        geneset=c("gsscores_hallmark", "gsscores_c2_reactome", "gsscores_nursa");
      }

      # Getting differential expression
      for(u in 1:length(genesetcollection)){
        #u=1;
        for(m in 1:length(gsvamethod)){
          #m=1;
          #run the analysis
          gsva_es <- gsva(expr=gene_expression, gset.idx.list=get(paste0(geneset[u])), method=gsvamethod[m], mx.diff=TRUE)
          
          #get the phenotypic variable
          var <- ifelse(all(colnames(gsva_es) %in% pro_ann$Sig_Id), "Sig_Id", "Chemical_Id")
          
          #create phenotypic data
          pData <- data.frame(pro_ann[match(colnames(gsva_es), pro_ann[,var]),], stringsAsFactors = TRUE)
          pData <- pData[!duplicated(pData[,var]),]
          rownames(pData) <- pData[,var]
          phenoData <- new("AnnotatedDataFrame", data=pData)
          
          #create feature data
          fData <- data.frame(Geneset=rownames(gsva_es), stringsAsFactors = TRUE)
          rownames(fData) <- fData$Geneset
          featureData <- new("AnnotatedDataFrame", data=fData)
          
          #Create expression set
          eSet <- ExpressionSet(assayData=gsva_es, phenoData=phenoData, featureData=featureData)
          
          #store the expression in a list
          genesetname <- paste0("gsscores_", genesetcollection[u], "_", gsvamethod[m])
          gsscores[[genesetname]] <- eSet
          
          ##Create morpheus heatmap
          eset <- eSet
          ge <- exprs(eset)
          feature <- fData(eset)
          cnames <- colnames(ge)
          
          nr <- nrow(ge); nc <- ncol(ge);
          marker <- "Gene Sets"; cluster <- FALSE;
          
          jsonoutputpath=paste0("www/JSON/", Portal, "/", genesetname, ".json")
          htmloutputpath=paste0("www/JSON/", Portal, "/", genesetname, ".html")
          
          if(var %in% "Sig_Id"){
            CreateJSON(outputpath=jsonoutputpath, dataset=Portal, genesetname=genesetname, ge=ge, proann=pro_ann, feature=feature, nr=nr, nc=nc, marker=marker)
            CreateHTML(outputpath=htmloutputpath, dataset=Portal, genesetname=genesetname, proann=pro_ann, feature=feature, marker=marker, cluster=cluster)
          }else{
            CreateJSON(outputpath=jsonoutputpath, dataset=Portal, genesetname=genesetname, ge=ge, proann=chem_ann, feature=feature, nr=nr, nc=nc, marker=marker)
            CreateHTML(outputpath=htmloutputpath, dataset=Portal, genesetname=genesetname, proann=chem_ann, feature=feature, marker=marker, cluster=cluster)
          }
          
        }
      }
      
      ##Add data to portal object####
      saveRDS(gsscores, paste0("data/", Portal, "/GS_Enrichment.RDS"))
      print("Saving gene set enrichment file")
      print("Saving gene set enrichment Morpheous heatmap")
      
      ##########################################################################################
      #
      # TAXONOMER ANALYSIS####
      #
      ##########################################################################################
      
      progress$inc(6/10, detail = "Saving taxonomer results")
      
      print(cohorts)
      
      if(!is.null(cohorts)){
        
        #Create the info class vector to run the metavariable test
        infoClassVector <- NULL;
        
        #Create a expression set####
        eSet <- orig_expressionSet
        #print(eSet)
        
        for(v in seq_along(Exposure_Phenotype)){
          #v=1;
          #print(input[[paste0("metavar_", Exposure_Phenotype[v])]])
          test <- meta_variable_test$method[which(meta_variable_test$statistical_test %in% methods[v])]
          
          if(test %in% "factor1" & all(toupper(c("No", "Yes")) %in% toupper(pData(eSet)[,Exposure_Phenotype[v]]))){
            pData(eSet)[,Exposure_Phenotype[v]] <- factor(pData(eSet)[,Exposure_Phenotype[v]], levels = c("No", "Yes"))
          }
          
          infoClassVector <- c(infoClassVector, test)
        }
        
        names(infoClassVector) <- Exposure_Phenotype; #print(infoClassVector);
        
        # fname <- "MCF10A"; connectivity_test <- TRUE; infoClassVector <- NULL; Exposure_Phenotype <- NULL;
        # eset <- readRDS(paste0("data/", fname, "/Expression_Set.RDS"))
        # conn_pcl <- exprs(readRDS(paste0("data/", fname, "/Connectivity.RDS"))[["pcl"]])
        # conn_pert <- exprs(readRDS(paste0("data/", fname, "/Connectivity.RDS"))[["pert"]])
        
        ##Add connectivity test##### 
        if(connectivity_var){
          pData <- pData(eset)
          conn_pcl_transpose <- t(conn_pcl) %>% as.data.frame()
          conn_pert_transpose <- t(conn_pert) %>% as.data.frame() 
          pData <- (pData %>% cbind(conn_pcl_transpose[match(rownames(pData), rownames(conn_pcl_transpose)),])) %>% cbind(conn_pert_transpose[match(rownames(pData), rownames(conn_pert_transpose)),])
          infoClassVector <- c(infoClassVector, rep(connectivity_test, ncol(conn_pcl_transpose)), rep(connectivity_test, ncol(conn_pert_transpose)))
          names(infoClassVector) <- c(Exposure_Phenotype, colnames(conn_pcl_transpose), colnames(conn_pert_transpose))
        }
        
        ##Hyperenrichment analysis####
        if(add_cur_enrichment_option == "Yes"){
          
          hallmark_genelist <- lapply(seq_along(gsscores_hallmark), function(g){ gsscores_hallmark[[g]]@geneIds })
          names(hallmark_genelist) <- names(gsscores_hallmark)
          
          c2_reactome_genelist <- lapply(seq_along(gsscores_c2_reactome), function(g){ gsscores_c2_reactome[[g]]@geneIds })
          names(c2_reactome_genelist) <- names(gsscores_c2_reactome)
          
          nursa_genelist <- lapply(seq_along(gsscores_nursa), function(g){ gsscores_nursa[[g]]@geneIds })
          names(nursa_genelist) <- names(gsscores_nursa)
          
          # Combine gene set enrichment for hallmark, C2, and NURSA
          geneSetList <- do.call(c, list(hallmark_genelist, c2_reactome_genelist, nursa_genelist))
          
        }else{
          
          collection_genelist <- lapply(seq_along(gs_collection), function(g){ gs_collection[[g]]@geneIds })
          names(collection_genelist) <- names(gs_collection)
          
          geneSetList <- collection_genelist
          
        }  
        
        # Run K2Taxonomer
        
        # Set seed
        RNGkind("L'Ecuyer-CMRG")
        set.seed(12345678)
        
        K2res <- runK2Taxonomer(
          eSet = eSet,
          cohorts = cohorts,
          featMetric = featMetric,
          infoClass = infoClassVector,
          genesets = geneSetList,
          ssGSEAalg = ssGSEAalg,
          ssGSEAcores = 1,
          stabThresh = 0.67
        )
        
        #print(head(K2res))
        
        ##save K2Taxonomer results
        saveRDS(K2res, paste0("data/", Portal, "/K2results.RDS"))
      }
      
      print("Saving K2Taxonomer results")
      
    })
    
    ## Show notification on error or user interrupt
    fut <- catch(
      fut,
      function(e){
        print(e$message)
        #showNotification(e$message)
      })
    
    ## When done with analysis, remove progress bar
    fut <- finally(fut, function(){
      
      progress$close()
      
      ##########################################################################################
      #
      # ADD CHEMICAL INTERACTIONS AND EXPOSURES TO PROJECT LIST####
      # 
      ##########################################################################################
      new_proj <- data.frame(
        Project=Project,
        Cell_Line=Cell_Line,
        Portal=Portal,
        GS_Collection=GS_Collection,
        GS_Collection_Link=GS_Collection_Link,
        Landmark_Gene=Landmark_Gene,
        Description=Description,
        stringsAsFactors = TRUE
      )
      
      exposure_categorical <- unlist(lapply(seq_along(Exposure), function(v){
        return(Exposure[v])
      }))
      
      exposure_phenotype_categorical <- unlist(lapply(seq_along(Exposure_Phenotype), function(v){
        return(Exposure_Phenotype[v])
      }))
      
      new_proj <- cbind(
        new_proj, 
        TAS_Modzscores=paste0(c(Add_TAS, Add_Modzscores), collapse=", "), 
        Exposure_Levels=paste0(exposure_categorical, collapse=", "), 
        Exposure_Phenotype=paste0(exposure_phenotype_categorical, collapse=", "), 
        Exposure_Phenotype_Test=paste0(methods, collapse=", "), 
        Connectivity_Test=paste0(c(connectivity_var, connectivity_test), collapse=", "),
        Feature_Filtering=featMetric
      )
      
      new_proj <- new_proj[,colnames(proj_dat)]
      
      if(all(is.na(proj_dat))){
        newproject <- new_proj
      }else{
        newproject <- proj_dat %>% rbind(new_proj)
      }
      
      projectdata(newproject)
      project_table_message(paste0(Portal, ' portal has been added. Click "Save" to keep this results.'))
      removeModal()
      
      shinyjs::enable(id="Add_Project_Add_Button")
      shinyjs::enable(id="Add_Project_Cancel_Button")
      
    })
    
    print("Generating new portal....")
    
  }
  
})


## Observe when cancel button is clicked
observeEvent(input$Add_Project_Cancel_Button, {
  
  #Reset file input####
  session$sendCustomMessage("ResetFileInput", "add_intro_file")
  session$sendCustomMessage("ResetFileInput", "add_pro_file")
  session$sendCustomMessage("ResetFileInput", "add_ge_file")
  session$sendCustomMessage("ResetFileInput", "add_conn_pcl_file")
  session$sendCustomMessage("ResetFileInput", "add_conn_pert_file")
  session$sendCustomMessage("ResetFileInput", "add_hallmark_file")
  session$sendCustomMessage("ResetFileInput", "add_c2_file")
  session$sendCustomMessage("ResetFileInput", "add_nursa_file")
  
  ##Remove error messages####
  addprojectwarningmsg(NULL)
  addinputwarningmsg(NULL)
  addcompoundvarwarningmsg(NULL)
  addexposurevarwarningmsg(NULL)
  addexposurephenotypevarwarningmsg(NULL)
  addenrichmentgswarningmsg(NULL)
  addenrichmentlinkwarningmsg(NULL)
  
  ##Remove data messages####
  portal(NULL)
  cohorts(NULL)
  intro_file_msg(NULL)
  pro_file_msg(NULL)
  ge_file_msg(NULL)
  conn_pcl_file_msg(NULL)
  conn_pert_file_msg(NULL)
  gs_collection_file_msg(NULL)
  project_table_message(NULL)
  
  ##Remove data####
  intro_file(NULL)
  pro_file(NULL)
  chem_file(NULL)
  ge_file(NULL)
  conn_pcl_file(NULL)
  conn_pert_file(NULL)
  gs_collection_file(NULL)

  ##Close modal log###
  removeModal()
  
})

    
    
    
