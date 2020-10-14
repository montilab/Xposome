
## Observe when add data is clicked ####
observeEvent({
  input$Edit_Project_Name
  input$Edit_Cell_Line_Name
  input$Edit_Portal_Name
  input$Edit_Description
}, {
  
  ##obtain the input values
  Project=trimws(input$Edit_Project_Name);
  Cell_Line=trimws(input$Edit_Cell_Line_Name);
  Portal=trimws(input$Edit_Portal_Name);
  Description=trimws(input$Edit_Description); 
  
  if(Project=="" | Cell_Line=="" | Portal=="" | Description==""){
    editprojectwarningmsg("Please fill in the required (*) fields.")
  }else{
    editprojectwarningmsg("")
  }
  
}, ignoreInit=TRUE)

## Observe when edit data is clicked ####
observeEvent(input$Edit_Project_Add_Button, {
  
  ##obtain the project list
  proj_dat <- read_csv(paste0("data/Project_List.csv")) 
  
  ##obtain the current project list
  portal <- portal()

  ##get the row of selected project###
  row <- which(proj_dat$Portal %in% portal$Portal)

  ##obtain the input values
  Project=trimws(input$Edit_Project_Name);
  Cell_Line=trimws(input$Edit_Cell_Line_Name);
  Portal=trimws(input$Edit_Portal_Name);
  Description=trimws(input$Edit_Description);  
  Landmark_Gene=trimws(input$Edit_Landmark);  
  Enrichment_Version=7;
  
  ## check all the input variables###
  check <- c();
  
  if(Project=="" | Cell_Line=="" | Portal=="" | Description==""){
    editprojectwarningmsg("Please fill in the required (*) fields.")
    check <- c(check, "No")
  }else{
    editprojectwarningmsg("")
  }
  
  ##Check Introduction Page####
  if(input$edit_files %in% c("All", "Introduction Page")){
    if(is.null(intro_file())){
      check <- c(check, "No")
    }  
  }
  
  ##Check Profile, Chemical and Gene Expression Files####
  if(input$edit_files %in% c("All", "Profile Annotation", "Gene Expression", "K2Taxonomer") | (input$edit_files %in% c("Connectivity Map") & input$edit_conn_pro_option %in% 'Yes')){
    
    if(is.null(pro_file()) | is.null(chem_file()) | is.null(ge_file())){
      check <- c(check, "No")
    } 
    
    if(!is.null(pro_file())){
      
      Compound=input$edit_variable_compound; 
      
      if(Compound==""){
        editcompoundvarwarningmsg("Please pick a selection.")
        check <- c(check, "No")
      }else{
        editcompoundvarwarningmsg("")
      }
      
      Exposure=input$edit_variable_exposure;
      
      if(length(Exposure)==0){
        editexposurevarwarningmsg("Please pick a selection.")
        check <- c(check, "No")
      }else{
        editexposurevarwarningmsg("")
      }
      
      Exposure_Phenotype=input$edit_variable_exposure_phenotype; methods=NULL;
      
      if(length(Exposure_Phenotype)==0){
        editexposurephenotypevarwarningmsg("Please pick a selection.")
        check <- c(check, "No")
      }else{
        for(v in seq_along(Exposure_Phenotype)){ 
          #v=1;
          if(is.null(input[[paste0("metavar_", Exposure_Phenotype[v])]])){
            check <- c(check, "No")
            methods <- c(methods, NULL)
          }else{
            methods <- c(methods, input[[paste0("metavar_", Exposure_Phenotype[v])]])
          }
        }
        editexposurephenotypevarwarningmsg("")
      }
    }
    
    featMetric <- input$edit_feature_metric; 
    
  }else{
    
    Exposure=unlist(strsplit(as.character(portal$Exposure_Levels), ",", fixed=TRUE)) %>% trimws()
    Exposure_Phenotype=unlist(strsplit(as.character(portal$Exposure_Phenotype), ",", fixed=TRUE)) %>% trimws()
    methods=unlist(strsplit(as.character(portal$Exposure_Phenotype_Test), ",", fixed=TRUE)) %>% trimws()
    featMetric=portal$Feature_Filtering
    
  }
  
  ##Check GS Enrichment####
  if(input$edit_files %in% c("All", "Profile Annotation", "Gene Expression", "GS Enrichment", "K2Taxonomer") | (input$edit_files %in% c("Connectivity Map") & input$edit_conn_pro_option %in% 'Yes')){
    
    GS_Collection=ifelse(input$edit_cur_enrichment_option=="Yes", "Default", trimws(input$Edit_New_Enrichment_GS));
    
    if(GS_Collection==""){
      editenrichmentgswarningmsg("Please enter a valid name.")
      check <- c(check, "No")
    }else{
      editenrichmentgswarningmsg("")
    }
    
    GS_Collection_Link=ifelse(input$edit_cur_enrichment_option=="Yes", "https://www.gsea-msigdb.org/gsea/msigdb; https://signalingpathways.org/", trimws(input$Edit_New_Enrichment_Link));
    
    if(GS_Collection_Link==""){
      editenrichmentlinkwarningmsg("Please enter a valid link.")
      check <- c(check, "No")
    }else{
      editenrichmentlinkwarningmsg("")
    }
    
    if(input$edit_cur_enrichment_option %in% "No" & input$edit_gs_collection_file_option %in% "Yes" & is.null(gs_collection_file())){
      check <- c(check, "No")
    }
    
  }else{
    
    GS_Collection=portal$GS_Collection
    GS_Collection_Link=portal$GS_Collection_Link
    
  }
  
  ##Check Connectivity Map####
  if(input$edit_files %in% c("Connectivity Map") | (input$edit_files %in% c("All", "Profile Annotation", "Gene Expression") & input$edit_conn_option %in% "Yes")){
    
    if(is.null(conn_pcl_file()) | is.null(conn_pert_file())){
      check <- c(check, "No")
    }
    
    connectivity_var <- input$edit_connectivity_var; connectivity_test <- "";
    
    if(!is.null(connectivity_var)){ 
      if(connectivity_var){
        connectivity_var <- TRUE
        connectivity_test <- input$edit_connectivity_test 
      }else{
        connectivity_var <- FALSE 
      }
    }else{
      connectivity_var <- FALSE 
    }
    
  }else{
    
    connectivity_var=unlist(strsplit(as.character(portal$Connectivity_Test), ",", fixed=TRUE))[1] %>% trimws() 
    connectivity_test=unlist(strsplit(as.character(portal$Connectivity_Test), ",", fixed=TRUE))[2] %>% trimws() 
  
  }
  
  if(any(check %in% "No")) { editinputwarningmsg("Please fill in the required (*) fields."); return(NULL) }
  
  #Validate to see the project exists
  validate_proj <- which(Project %in% proj_dat$Project[which(!proj_dat$Project %in% portal$Project)])
  
  #Validate to see the project exists
  validate_portal <- which(Portal %in% proj_dat$Portal[which(!proj_dat$Portal %in% portal$Portal)])
  
  #print(validate_portal); print(validate_portal);
  
  if(length(validate_proj) > 0 | length(validate_portal) > 0){
    
    if(length(validate_proj) > 0){
      editinputwarningmsg("This project is already existed. Please enter another project name.")
    }
    
    if(length(validate_portal) > 0){
      editinputwarningmsg("This portal name is already existed. Please enter another portal name.")
    }
    
    if(length(validate_proj) > 0 & length(validate_portal) > 0){
      editinputwarningmsg("Both project and portal name are already existed. Please enter another project and portal name.")
    }
    
  }else{
    
    shinyjs::disable(id="Edit_Project_Add_Button")
    shinyjs::disable(id="Edit_Project_Cancel_Button")
    
    ##remove all warning messages
    editprojectwarningmsg(""); editinputwarningmsg(""); 
    
    ##notify user when the analysis is completed
    project_table_message(paste0(Portal, ' portal has been edited. Click "Save" to keep this changes.'))
    
    ##Getting the file option####
    edit_files <- input$edit_files; 
    edit_conn_option <- input$edit_conn_option;
    edit_pro_ge_option <- input$edit_pro_ge_option; 
    edit_conn_pro_option <- input$edit_conn_pro_option;
    edit_cur_enrichment_option <- input$edit_cur_enrichment_option;
    edit_gs_collection_file_option <- input$edit_gs_collection_file_option;

    ##Getting the import file option####
    edit_ge_file_type <- input$edit_ge_file_type;         
    edit_conn_pcl_file_type <- input$edit_conn_pcl_file_type;
    edit_conn_pert_file_type <- input$edit_conn_pert_file_type;   
    
    ##Getting the TAS and Modzscore####
    Edit_TAS <- input$Edit_TAS; Edit_Modzscores <- input$Edit_Modzscores; 
    
    ##K2Taxonomer parameters####
    cohorts <- cohorts(); edit_ssGSEA_method <- input$edit_ssGSEA_method; edit_feature_metric <- input$edit_feature_metric;
    
    ##Getting chemical and profile annotation and gene expression####
    intro_file <- intro_file(); pro_file <- pro_file(); chem_file <- chem_file(); ge_file <- ge_file();
    #print(head(pro_file)); print(dim(pro_file)); print(head(chem_file)); print(dim(chem_file));
    
    ##Getting connectivity map and the gs enrichment file####
    conn_pcl_file <- conn_pcl_file(); conn_pert_file <- conn_pert_file(); gs_collection_file <- gs_collection_file();

    ##Create new progress bar
    progress <- AsyncProgress$new(message = "Creating new portal:", value=0)
    
    fut <- future({
      
      ##Shiny Packages####
      require(K2Taxonomer)
      require(visNetwork) 
      require(Biobase) 
      require(BiocGenerics)
      
      ##Rename portal directory if portal name changed####
      if(Portal != portal$Portal){
        ##Create directory to store data
        data_path <- paste0('data/', portal$Portal); 
        data_files <- list.files(data_path)
        new_data_path <- paste0('data/', Portal); 
        dir.create(new_data_path, showWarnings=FALSE)
        walk(seq_along(data_files), function(f){ file.copy(from=file.path(data_path, data_files[f]), to=new_data_path, overwrite=TRUE) })        
        
        ##Create directory to store json files
        json_path <- paste0('www/JSON/', portal$Portal);
        json_files <- list.files(json_path)
        new_json_path <- paste0('www/JSON/', Portal);
        dir.create(new_json_path, showWarnings=FALSE)
        walk(seq_along(json_files), function(f){ file.copy(from=file.path(json_path, json_files[f]), to=new_json_path, overwrite=TRUE) })    
        
        ##Create directory to store rmd files
        rmd_path <- paste0('www/RMD');
        file.copy(from=paste0(rmd_path, "/introduction_", portal$Portal, ".Rmd"), to=paste0(rmd_path, "/introduction_", Portal, ".Rmd")) 
      }else{
        ##Create portal directory to store data and results####
        dir.create(paste0("data/", Portal), showWarnings=FALSE, recursive=TRUE)
        dir.create(paste0("www/JSON/", Portal), showWarnings=FALSE, recursive=TRUE)
        dir.create(paste0("www/RMD"), showWarnings=FALSE, recursive=TRUE)
      }
      
      ##########################################################################################
      #
      # INTRODUCTION PAGE####
      #
      ##########################################################################################
      progress$inc(1/10, detail = "Saving introduction page")
      
      if(edit_files %in% c('Introduction Page', 'All')){
        file.copy(from=intro_file, to=paste0("www/RMD/introduction_", Portal, ".Rmd"), overwrite=TRUE)
      }
      
      print("Saving introduction page")
      
      ##########################################################################################
      #
      # PROFILE AND CHEMICAL ANNOTATION #####
      #
      ##########################################################################################
      progress$inc(2/10, detail = "Saving chemical and profile annotation")
      
      ##########################################################################################
      #
      # GET THE UNIQUE ID BY CHEM FOR PRO ANN####
      #
      ##########################################################################################
      pro_file$unique_ID_by_chem <- lapply(1:nrow(pro_file), function(r){ paste0(unlist(pro_file[r,Exposure]), collapse="_") }) %>% unlist()
      
      ##Getting gene expression####
      if((edit_files == 'Profile Annotation' & edit_pro_ge_option %in% "Yes") | edit_files == 'All' | edit_files == 'Gene Expression' | (edit_files %in% "Connectivity Map" & edit_conn_pro_option %in% 'Yes')){
        
        if(edit_ge_file_type %in% ".csv"){
          gene_expression <- ge_file
        }else {
          gene_expression <- exprs(ge_file)
        }
        
        if(edit_ge_file_type == ".RDS"){
          
          fData <- fData(ge_file)
          
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
        
      }else{
        
        gene_expression <- exprs(ge_file)
        
        fData <- fData(ge_file)
        
        if(nrow(fData)==0){
          fData <- data.frame(Gene=rownames(gene_expression), stringsAsFactors = TRUE)
          rownames(fData) <- fData$Gene
        }else{
          if(!"Gene" %in% colnames(fData)){
            fData <- data.frame(Gene=rownames(gene_expression), fData, stringsAsFactors = TRUE)
            rownames(fData) <- fData$Gene
          }
        }
        
      }
      
      gene_expression <- as.matrix(gene_expression, nrow=nrow(gene_expression), ncol=ncol(gene_expression), byrow=TRUE, dimnames=list(rownames(gene_expression), colnames(gene_expression)))
      var <- ifelse(all(colnames(gene_expression) %in% pro_file$Sig_Id), "Sig_Id", "Chemical_Id")
      
      #create phenotypic data
      pData <- data.frame(pro_file[match(colnames(gene_expression), pro_file[,var]),], stringsAsFactors=TRUE)
      pData <- pData[!duplicated(pData[,var]),]
      rownames(pData) <- pData[,var]
      phenoData <- new("AnnotatedDataFrame", data=pData)
      
      #create feature data
      featureData <- new("AnnotatedDataFrame", data=fData)
      
      #create expression set
      orig_expressionSet <- ExpressionSet(assayData=gene_expression, phenoData=phenoData, featureData=featureData)
      
      ##########################################################################################
      #
      # CALCULATE MOD Z-SCORES AND REPLICATE CORRELATION (CC) FOR TAS####
      #
      ##########################################################################################
      ##Getting the variables annotation
      if(is.null(Edit_TAS)){ Edit_TAS <- FALSE }else{ Edit_TAS <- Edit_TAS }
      if(is.null(Edit_Modzscores)){ Edit_Modzscores <- FALSE }else{ Edit_Modzscores <- Edit_Modzscores }
      
      #print(Edit_TAS); print(Edit_Modzscores);
      
      if((edit_files == 'Profile Annotation' & edit_pro_ge_option=='Yes') | edit_files == 'All' | edit_files == 'Gene Expression' | (edit_files == "Connectivity Map" & edit_conn_pro_option %in% 'Yes')){
        
        if(Edit_TAS==TRUE | Edit_Modzscores==TRUE){
          
          calc_results <- TAS_Modzscores_Calculation(pro_file, chem_file, gene_expression)
          
          if(Edit_TAS){
            # get TAS
            TAS <- calc_results[["TAS"]]
            
            # chemical annotation information
            chem_file <- chem_file %>% left_join(TAS) %>% select(-Number_of_Replicates)
            
            # profile annotation information
            pro_file <- pro_file %>% left_join(TAS)
          }
          
          # get modzscores
          if(Edit_Modzscores){
            Modzscores <- calc_results[["Modzscores"]]
            gene_expression <- Modzscores
          }
          
        }
        
      }
      
      ##save data to portal folder####
      saveRDS(pro_file, paste0("data/", Portal, "/Profile_Annotation.RDS"))
      saveRDS(chem_file, paste0("data/", Portal, "/Chemical_Annotation.RDS"))
      
      print("Saving chemical and profile annotation")
      
      ##########################################################################################
      #
      # GENE EXPESSION ####
      #
      ##########################################################################################
      progress$inc(3/10, detail = "Saving expression set")
      
      #Create a expression set####
      gene_expression <- as.matrix(gene_expression, nrow=nrow(gene_expression), ncol=ncol(gene_expression), byrow=TRUE, dimnames=list(rownames(gene_expression), colnames(gene_expression)))
      var <- ifelse(all(colnames(gene_expression) %in% pro_file$Sig_Id), "Sig_Id", "Chemical_Id")
      
      #create phenotypic data
      pData <- data.frame(pro_file[match(colnames(gene_expression), pro_file[,var]),], stringsAsFactors=TRUE)
      pData <- pData[!duplicated(pData[,var]),]
      rownames(pData) <- pData[,var]
      phenoData <- new("AnnotatedDataFrame", data=pData)
      
      ##Check if the project include landmark gene#####
      if(!Landmark_Gene %in% portal$Landmark_Gene){
        if(Landmark_Gene){
          if(!'Landmark_Gene' %in% colnames(fData)){
            landmark <- readRDS("data/Landmark/landmark_gene.RDS")
            fData <- (fData %>% left_join(landmark, by="Gene")) %>% replace_na(list(Landmark_Gene="Unknown"))
            rownames(fData) <- fData$Gene
          }
        }else{
          if('Landmark_Gene' %in% colnames(fData)){
            fData <- fData %>% select(-Landmark_Gene)
          }
        }
      }
      
      ##Create new feature data with landmark genes
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
        CreateJSON(outputpath=jsonoutputpath, dataset=Portal, genesetname=genesetname, ge=ge, proann=pro_file, feature=feature, nr=nr, nc=nc, marker=marker)
        CreateHTML(outputpath=htmloutputpath, dataset=Portal, genesetname=genesetname, proann=pro_file, feature=feature, marker=marker, cluster=cluster)
      }else{
        CreateJSON(outputpath=jsonoutputpath, dataset=Portal, genesetname=genesetname, ge=ge, proann=chem_file, feature=feature, nr=nr, nc=nc, marker=marker)
        CreateHTML(outputpath=htmloutputpath, dataset=Portal, genesetname=genesetname, proann=chem_file, feature=feature, marker=marker, cluster=cluster)
      }
      
      print("Saving gene expression Morpheous heatmap")
      
      ##########################################################################################
      #
      # CONNECTIVITY MAP#####
      #
      ##########################################################################################
      progress$inc(4/10, detail = "Saving connectivity map")
      
      if(edit_files %in% "Connectivity Map" | (edit_files %in% c("All", "Profile Annotation", "Gene Expression") & edit_conn_option=="Yes")){
        
        if(edit_conn_pcl_file_type %in% ".csv"){
          conn_pcl <- conn_pcl_file
        }else{
          conn_pcl_dat <- conn_pcl_file
          conn_pcl <- exprs(conn_pcl_file)       
        }
        
        if(edit_conn_pert_file_type %in% ".csv"){
          conn_pert <- conn_pert_file
        }else{
          conn_pert_dat <- conn_pert_file
          conn_pert <- exprs(conn_pert_file)       
        }
        
        # Create a null list for connectivity map####
        connectivity_map <- list(pcl=NULL, pert=NULL);
        
        # Create classes for CMap connectivity #####
        connmap <- c("pcl", "pert")
        
        for(p in 1:length(connmap)){
          #p=1;
          edit_conn_file_type <- get(paste0("edit_conn_", connmap[p], "_file_type"))
          conn_dat <- get(paste0("conn_", connmap[p], "_dat"))
          
          if(connmap[p] %in% "pcl"){
            conn_ge <- as.matrix(conn_pcl, nrow=nrow(conn_pcl), ncol=ncol(conn_pcl), byrow=TRUE, dimnames=list(rownames(conn_pcl), colnames(conn_pcl)))
          }else{
            conn_ge <- as.matrix(conn_pert, nrow=nrow(conn_pert), ncol=ncol(conn_pert), byrow=TRUE, dimnames=list(rownames(conn_pert), colnames(conn_pert)))
          }
          
          var <- ifelse(all(colnames(conn_ge) %in% pro_file$Sig_Id), "Sig_Id", "Chemical_Id")
          
          #create phenotypic data
          pData <- data.frame(pro_file[match(colnames(conn_ge), pro_file[,var]),], stringsAsFactors = TRUE)
          pData <- pData[!duplicated(pData[,var]),]
          rownames(pData) <- pData[,var]
          phenoData <- new("AnnotatedDataFrame", data=pData)
          
          #create feature data
          if(edit_conn_file_type == ".RDS"){
            
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
          expressionSet <- conn_ge
          eSet <- ExpressionSet(assayData=expressionSet, phenoData=phenoData, featureData=featureData)
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
            CreateJSON(outputpath=jsonoutputpath, dataset=Portal, genesetname=genesetname, ge=ge, proann=pro_file, feature=feature, nr=nr, nc=nc, marker=marker)
            CreateHTML(outputpath=htmloutputpath, dataset=Portal, genesetname=genesetname, proann=pro_file, feature=feature, marker=marker, cluster=cluster)
          }else{
            CreateJSON(outputpath=jsonoutputpath, dataset=Portal, genesetname=genesetname, ge=ge, proann=chem_file, feature=feature, nr=nr, nc=nc, marker=marker)
            CreateHTML(outputpath=htmloutputpath, dataset=Portal, genesetname=genesetname, proann=chem_file, feature=feature, marker=marker, cluster=cluster)
          }
        }
        
        ##save data to portal folder####
        saveRDS(connectivity_map, paste0("data/", Portal, "/Connectivity.RDS"))
        
        print("Saving connectivity map file")
        print("Saving connectivity Morpheous heatmap")
        
      }
      
      ##########################################################################################
      #
      # GENE SET ENRICHMENT & K2TAXONOMER ANALYSIS#####
      #
      ##########################################################################################
      
      if(edit_files %in% c("All", "Profile Annotation", "Gene Expression", "GS Enrichment", "K2Taxonomer") | (edit_files %in% c("Connectivity Map") & edit_conn_pro_option %in% 'Yes')){
        
        ##########################################################################################
        #
        # GENE SET ENRICHMENT ANALYSIS####
        #
        ##########################################################################################
        progress$inc(5/10, detail = "Saving gene set enrichment results")
        
        # Read in the gene set collection for gene set enrichment analysis###
        if(edit_cur_enrichment_option == "Yes"){
          gsscores_hallmark <- getGmt(paste0("data/Enrichment Gene Set/h.all.v", Enrichment_Version, ".0.gmt"))
          gsscores_c2_reactome <- getGmt(paste0("data/Enrichment Gene Set/c2.cp.reactome.v", Enrichment_Version, ".0.gmt"))
          gsscores_nursa <- getGmt(paste0("data/Enrichment Gene Set/nursa_consensome_Cbyfdrvalue_0.01.gmt"))
        }else{
          if(edit_gs_collection_file_option %in% "Yes"){
            gs_collection_path <- gs_collection_file %>% extract2("path")
            gs_collection <- gs_collection_file %>% extract2("data")
            file.copy(from=gs_collection_path, to=file.path("data/Enrichment Gene Set", paste0(GS_Collection, ".gmt")), overwrite=TRUE)
          }else{
            gs_collection <- getGmt(paste0("data/Enrichment Gene Set/", portal$GS_Collection, ".gmt"))
          }
        }
        
        #create gene set enrichment####
        gsscores <- list(); 
        
        if(edit_cur_enrichment_option == "No"){
          # Run gene set enrichment analysis for new gs
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
          for(m in 1:length(edit_ssGSEA_method)){
            #m=1;
            #run the analysis
            gsva_es <- gsva(expr=gene_expression, gset.idx.list=get(paste0(geneset[u])), method=edit_ssGSEA_method[m], mx.diff=TRUE)
            
            #get the phenotypic variable
            var <- ifelse(all(colnames(gsva_es) %in% pro_file$Sig_Id), "Sig_Id", "Chemical_Id")
            
            #create phenotypic data
            pData <- data.frame(pro_file[match(colnames(gsva_es), pro_file[,var]),], stringsAsFactors = TRUE)
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
            genesetname <- paste0("gsscores_", genesetcollection[u], "_", edit_ssGSEA_method[m])
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
              CreateJSON(outputpath=jsonoutputpath, dataset=Portal, genesetname=genesetname, ge=ge, proann=pro_file, feature=feature, nr=nr, nc=nc, marker=marker)
              CreateHTML(outputpath=htmloutputpath, dataset=Portal, genesetname=genesetname, proann=pro_file, feature=feature, marker=marker, cluster=cluster)
            }else{
              CreateJSON(outputpath=jsonoutputpath, dataset=Portal, genesetname=genesetname, ge=ge, proann=chem_file, feature=feature, nr=nr, nc=nc, marker=marker)
              CreateHTML(outputpath=htmloutputpath, dataset=Portal, genesetname=genesetname, proann=chem_file, feature=feature, marker=marker, cluster=cluster)
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
        
        if(!is.null(cohorts)){
          
          ##Create the info class vector to run the metavariable test
          infoClassVector <- NULL;
          
          #Get a expression set####
          eSet <- orig_expressionSet
          
          for(v in seq_along(Exposure_Phenotype)){
            #v=1;
            #print(input[[paste0("metavar_", Exposure_Phenotype[v])]])
            test <- meta_variable_test$method[which(meta_variable_test$statistical_test %in% methods[v])]
            
            if(test %in% "factor1" & all(toupper(c("Yes", "No")) %in% toupper(pData(eSet)[,Exposure_Phenotype[v]]))){
              pData(eSet)[,Exposure_Phenotype[v]] <- factor(pData(eSet)[,Exposure_Phenotype[v]], levels = c("No", "Yes"))
            }
            
            infoClassVector <- c(infoClassVector, test)
          }
          
          names(infoClassVector) <- Exposure_Phenotype; 
          
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
          if(edit_cur_enrichment_option == "Yes"){
            
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
          
          # Run K2Taxonomer!
          # Always important to set a seed
          set.seed(12345678)
          
          K2res <- runK2Taxonomer(
            eSet = eSet,
            cohorts = cohorts,
            featMetric = edit_feature_metric,
            infoClass = infoClassVector,
            genesets = geneSetList,
            ssGSEAalg = edit_ssGSEA_method,
            ssGSEAcores = 1,
            stabThresh = 0.67
          )
          
          ##save K2Taxonomer results
          saveRDS(K2res, paste0("data/", Portal, "/K2results.RDS"))
          
        }
        
        print("Saving K2Taxonomer results")
        
      }
      
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
      
      #close progress bar
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
      
      print(cohorts())
      
      if(edit_files %in% c("None", "Introduction Page") | (edit_files %in% "Connectivity Map" & edit_conn_pro_option %in% "No")){
        TAS_Modzscores=portal$TAS_Modzscores 
      }else{
        if(is.null(cohorts())){
          TAS_Modzscores=portal$TAS_Modzscores 
        }else{
          TAS_Modzscores=paste0(c(Edit_TAS, Edit_Modzscores), collapse=", ")
        }
      }
      
      new_proj <- cbind(
        new_proj, 
        TAS_Modzscores=TAS_Modzscores,
        Exposure_Levels=paste0(exposure_categorical, collapse=", "), 
        Exposure_Phenotype=paste0(exposure_phenotype_categorical, collapse=", "), 
        Exposure_Phenotype_Test=paste0(methods, collapse=", "), 
        Connectivity_Test=paste0(c(connectivity_var, connectivity_test), collapse=", "),
        Feature_Filtering=featMetric
      )
      
      new_proj <- new_proj[,colnames(proj_dat)]
      proj_dat <- proj_dat[-row,]
      newproject <- proj_dat %>% rbind(new_proj)
      projectdata(newproject)
      removeModal()
      
      shinyjs::enable(id="Edit_Project_Add_Button")
      shinyjs::enable(id="Edit_Project_Cancel_Button")
      
    })
      
  }
})

## Observe when cancel button is clicked
observeEvent(input$Edit_Project_Cancel_Button, {
  
  #Reset file input####
  session$sendCustomMessage("ResetFileInput", "edit_intro_file")
  session$sendCustomMessage("ResetFileInput", "edit_pro_file")
  session$sendCustomMessage("ResetFileInput", "edit_ge_file")
  session$sendCustomMessage("ResetFileInput", "edit_conn_pcl_file")
  session$sendCustomMessage("ResetFileInput", "edit_conn_pert_file")
  session$sendCustomMessage("ResetFileInput", "edit_gs_collection_file")

  ##Remove error messages####
  editprojectwarningmsg(NULL)
  editinputwarningmsg(NULL)
  editcompoundvarwarningmsg(NULL)
  editexposurevarwarningmsg(NULL)
  editexposurephenotypevarwarningmsg(NULL)
  editenrichmentgswarningmsg(NULL)
  editenrichmentlinkwarningmsg(NULL)
  
  ##Remove data messages###
  cohorts(NULL)
  project_table_message(NULL)
  intro_file_msg(NULL)
  pro_file_msg(NULL)
  ge_file_msg(NULL)
  conn_pcl_file_msg(NULL)
  conn_pert_file_msg(NULL)
  gs_collection_file_msg(NULL)
  
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

