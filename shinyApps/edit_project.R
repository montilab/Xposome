
## Observe when edit data is clicked ####
observeEvent(input$Edit_Project_Add_Button, {
  
  ##obtain the project list
  proj_dat <- read_csv(paste0("data/Project_List.csv")); 
  
  ##obtain the current project list
  portal <- portal();

  ##get the row of selected project###
  row <- which(proj_dat$Portal %in% portal$Portal)

  ##obtain the input values
  Project=trimws(input$Edit_Project_Name);
  Cell_Line=trimws(input$Edit_Cell_Line_Name);
  Portal=trimws(input$Edit_Portal_Name) %>% gsub(" ", "_", .);
  Description=trimws(input$Edit_Description);  
  
  ## check all the input variables###
  check <- c();
  
  ##Check Portal name, make sure no white space and starting number###
  if(Portal != ""){
    
    checkspace <- length(grep(" ", Portal, fixed=TRUE)) > 0 
    checknumber <- length(grep("[0-9]", substr(Portal,1,1), fixed=TRUE)) > 0
    
    if(checkspace){
      editprojectwarningmsg("Portal name cannot contain spaces.")
      check <- c(check, "No")
    }
    
    if(checknumber){
      editprojectwarningmsg("Portal name cannot start with a number.")
      check <- c(check, "No")
    }
    
    ##if portal name is met, no error message
    if(checkspace==FALSE & checknumber==FALSE){
      editprojectwarningmsg("")
    }
    
  }
  
  ##Check project, cell lines, and description
  if(Project=="" | Cell_Line=="" | Portal=="" | Description==""){
    check <- c(check, "No")
  }
  
  ##Check Introduction Page####
  if(input$edit_files %in% c("All", "Introduction Page")){
    if(is.null(intro_file())){
      check <- c(check, "No")
    }  
  }
  
  ##Check profile, chemical, gene expression, connectivity map, gs enrichment and k2taxonomer####
  if(input$edit_files %in% c("All", "Profile Annotation", "Gene Expression", "Connectivity Map", "GS Enrichment", "K2Taxonomer")){
    
    ##Check profile, chemical, gene expression files####
    if(is.null(pro_file()) | is.null(chem_file()) | is.null(ge_file())){
      check <- c(check, "No")
    } 
    
    ##If redo analysis, check the compound variables##
    if((input$edit_files %in% "All" & input$edit_Analysis == "Yes") | (input$edit_files %in% "Profile Annotation" & input$edit_Analysis == "Yes") | (input$edit_files %in% "Gene Expression" & input$edit_Analysis == "Yes") | (input$edit_files %in% "Connectivity Map" & input$edit_Analysis == "Yes") | input$edit_files == "K2Taxonomer"){    
      
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
    
    ##Check Connectivity Map####
    if(input$edit_files %in%  c("All", "Profile Annotation", "Gene Expression", "Connectivity Map", "K2Taxonomer")){
      
      if(input$edit_files %in% "Connectivity Map" | (input$edit_files %in% c("All", "Profile Annotation", "Gene Expression") & input$edit_conn_option %in% "Yes")){
        if(is.null(conn_pcl_file()) | is.null(conn_pert_file())){
          check <- c(check, "No")
        }
      }
      
      if((input$edit_files %in% "All" & input$edit_Analysis == "Yes") | (input$edit_files %in% "Profile Annotation" & input$edit_Analysis == "Yes") | (input$edit_files %in% "Gene Expression" & input$edit_Analysis == "Yes") | (input$edit_files %in% "Connectivity Map" & input$edit_Analysis == "Yes") | input$edit_files == "K2Taxonomer"){    
        
        connectivity_var <- input$edit_connectivity_var;
        
        if(connectivity_var){
          connectivity_var <- TRUE
          connectivity_test <- input$edit_connectivity_test 
        }else{
          connectivity_var <- FALSE 
          connectivity_test <- ""
        }
      }
      
    }else{
      
      connectivity_var=as.character(portal$Connectivity_Variable)
      connectivity_test=as.character(portal$Connectivity_Test)
      
    }
    
    ##Check GS Enrichment####
    if((input$edit_files %in% "All" & input$edit_Analysis == "Yes") | (input$edit_files %in% "Profile Annotation" & input$edit_Analysis == "Yes") | (input$edit_files %in% "Gene Expression" & input$edit_Analysis == "Yes") | (input$edit_files %in% "Connectivity Map" & input$edit_Analysis == "Yes") | input$edit_files == "GS Enrichment"){    
      
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
      
      if(input$edit_cur_enrichment_option %in% "No" & input$edit_gs_collection_file_option %in% "Yes"){
        if(is.null(gs_collection_file())){
          check <- c(check, "No")
        }
      }
      
    }else{
      
      GS_Collection=portal$GS_Collection
      GS_Collection_Link=portal$GS_Collection_Link
      
    }

  }
  
  if(any(check %in% "No")) { editinputwarningmsg("Please fill in the required (*) fields."); return(NULL) }
  
  #Validate to see if the project name exists
  validate_proj <- which(Project %in% proj_dat$Project[which(!proj_dat$Project %in% portal$Project)])
  
  #Validate to see if the portal name exists
  validate_portal <- which(Portal %in% proj_dat$Portal[which(!proj_dat$Portal %in% portal$Portal)])
  
  #check portal and project names
  if(length(validate_proj) > 0 | length(validate_portal) > 0){
    
    if(length(validate_proj) > 0){
      editinputwarningmsg("This project is already existed. Please enter another project name.")
    }
    
    if(length(validate_portal) > 0){
      editinputwarningmsg("This portal name is already existed. Please enter another portal name.")
    }
    
    if(length(validate_proj) > 0 & length(validate_portal) > 0){
      editinputwarningmsg("Both project and portal name are already existed. Please enter another project name and portal name.")
    }
    
  }else{
    
    ##Disable the add project and cancel button
    shinyjs::disable(id="Edit_Project_Add_Button")
    shinyjs::disable(id="Edit_Project_Cancel_Button")
    
    ##remove all warning messages
    editprojectwarningmsg(""); editinputwarningmsg(""); 
    
    ##notify user when the analysis is completed
    project_table_message(paste0(Portal, ' portal has been edited. Click "Save" to keep this changes.'))
    
    ##Getting the file option####
    edit_files <- input$edit_files; edit_Analysis <- input$edit_Analysis;
    
    if(edit_files %in% c("None", "Introduction Page")){
      
      ##Rename portal directory if portal name changed####
      if(Portal != portal$Portal){
        
        ##Create directory to store json files
        json_path <- paste0('www/JSON/', portal$Portal);
        json_files <- list.files(json_path)
        new_json_path <- paste0('www/JSON/', Portal);
        dir.create(new_json_path, showWarnings=FALSE)
        walk(seq_along(json_files), function(f){ file.copy(from=file.path(json_path, json_files[f]), to=new_json_path, overwrite=TRUE) })    
        
        ##Create directory to store rmd files
        rmd_path <- paste0('www/RMD');
        file.copy(from=paste0(rmd_path, "/introduction_", portal$Portal, ".Rmd"), to=paste0(rmd_path, "/introduction_", Portal, ".Rmd")) 
        
        # Initialize list to hold values for PortalDataset entity
        portalDataset <- list(
          portal = Portal,
          timestamp = format(Sys.Date(), "%a %b %d %X %Z %Y")
        )
        
        # Retrieve list of all PortalDataset entities in hive
        datasets <- listEntities("PortalDataset", portal=portal$Portal)
        
        # Sort by timestamp and extract most recent dataset to convenience object
        datasets <- datasets[order(sapply(datasets, slot, "timestamp"))]
        dataset <- datasets[[length(datasets)]]
        
        # Read in the profile data ####
        pro_file <- getWorkFileAsObject(
          hiveWorkFileID(dataset@ProfileAnnotationRDS)
        )
        
        # Upload profile annotation to WorkFiles,
        # and add WorkFile IDs to list of values for portal dataset list
        portalDataset$ProfileAnnotationRDS <- GeneHive::objectId(
          GeneHive::storeObjectAsWorkFile(pro_file)
        )
        
        print("Saving profile annotation")
        
        # Read in the chemical data ####
        chem_file <- getWorkFileAsObject(
          hiveWorkFileID(dataset@ChemicalAnnotationRDS)
        )
        
        # Upload chemical annotation to WorkFiles,
        # and add WorkFile IDs to list of values for portal dataset list
        portalDataset$ChemicalAnnotationRDS <- GeneHive::objectId(
          GeneHive::storeObjectAsWorkFile(chem_file)
        )
        
        print("Saving chemical annotation")
        
        # Read in the expression data ####
        expressionSet <- getWorkFileAsObject(
          hiveWorkFileID(dataset@GeneExpressionRDS)
        )
        
        # Upload ExpressionSet to WorkFile,
        # and add WorkFile ID to list of values for portal dataset list
        portalDataset$GeneExpressionRDS <- GeneHive::objectId(
          GeneHive::storeObjectAsWorkFile(expressionSet)
        )
        
        print("Saving gene expression file")
        
        # Read in the gs enrichment data ####
        gsscores <- getWorkFileAsObject(
          hiveWorkFileID(dataset@GeneSetEnrichmentRDS)
        )
        
        # Upload GSEA data to WorkFile,
        # and add WorkFile ID to list of values for portal dataset list
        portalDataset$GeneSetEnrichmentRDS <- GeneHive::objectId(
          GeneHive::storeObjectAsWorkFile(gsscores)
        )
        
        print("Saving gene set enrichment file")
        
        # Read in the connectivity data ####
        connectivity_map <- getWorkFileAsObject(
          hiveWorkFileID(dataset@ConnectivityRDS)
        )
        
        # Upload CMap data to WorkFile,
        # and add WorkFile ID to list of values for portal dataset list
        portalDataset$ConnectivityRDS <- GeneHive::objectId(
          GeneHive::storeObjectAsWorkFile(connectivity_map)
        )
        
        print("Saving connectivity map file")
        
        K2res <- getWorkFileAsObject(
          hiveWorkFileID(dataset@K2TaxonomerResultsRDS)
        )
        
        # Upload K2Taxonomer results to WorkFile,
        # and add WorkFile ID to list of values for portal dataset list
        portalDataset$K2TaxonomerResultsRDS <- GeneHive::objectId(
          GeneHive::storeObjectAsWorkFile(K2res)
        )
        
        print("Saving K2Taxonomer results")
        
        # Add PortalDataset entity to GeneHive
        do.call(
          GeneHive::addEntity, args=c(.class="PortalDataset", portalDataset)
        )
        
      }
      
      if(edit_files == 'Introduction Page'){
        intro_file <- intro_file(); 
        file.copy(from=intro_file, to=paste0("www/RMD/introduction_", Portal, ".Rmd"), overwrite=TRUE)
      }
      
      portal$Project <- Project;
      portal$Cell_Line <- Cell_Line;
      portal$Portal <- Portal;
      portal$Description <- Description; 
      
      proj_dat <- proj_dat[-row,]
      newproject <- proj_dat %>% rbind(portal)
      projectdata(newproject)  
      
      removeModal()
      
      shinyjs::enable(id="Edit_Project_Add_Button")
      shinyjs::enable(id="Edit_Project_Cancel_Button")
      
    }else{
      
      ##Getting input option####
      edit_pro_option <- input$edit_pro_option; 
      edit_ge_option <- input$edit_ge_option; 
      edit_conn_option <- input$edit_conn_option;
      edit_cur_enrichment_option <- input$edit_cur_enrichment_option;
      edit_gs_collection_file_option <- input$edit_gs_collection_file_option;
      
      ##Getting the import file option####
      edit_ge_file_type <- input$edit_ge_file_type;         
      edit_conn_pcl_file_type <- input$edit_conn_pcl_file_type;
      edit_conn_pert_file_type <- input$edit_conn_pert_file_type;   
      
      ##Getting landmark genes and enrichment version####
      Landmark_Gene <- trimws(input$Edit_Landmark); Enrichment_Version <- 7;
      
      ##Getting the TAS and Modzscore####
      Edit_TAS <- input$Edit_TAS; Edit_Modzscores <- input$Edit_Modzscores; 
      
      ##K2Taxonomer parameters####
      cohorts <- cohorts(); edit_ssGSEA_method <- input$edit_ssGSEA_method; edit_feature_metric <- input$edit_feature_metric;
      
      ##Getting chemical and profile annotation and gene expression####
      intro_file <- intro_file(); pro_file <- pro_file(); chem_file <- chem_file(); ge_file <- ge_file();
      
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
          
          ##rename directory to store json files
          json_path <- paste0('www/JSON/', portal$Portal);
          json_files <- list.files(json_path)
          new_json_path <- paste0('www/JSON/', Portal);
          dir.create(new_json_path, showWarnings=FALSE)
          walk(seq_along(json_files), function(f){ file.copy(from=file.path(json_path, json_files[f]), to=new_json_path, overwrite=TRUE) })    
          
          ##rename directory to store rmd files
          rmd_path <- paste0('www/RMD');
          file.copy(from=paste0(rmd_path, "/introduction_", portal$Portal, ".Rmd"), to=paste0(rmd_path, "/introduction_", Portal, ".Rmd")) 
          
        }else{
          
          ##Create new portal directories to store Morpheus heatmaps and .Rmd file
          dir.create(paste0("www/JSON/", Portal), showWarnings=FALSE, recursive=TRUE)
          dir.create(paste0("www/RMD"), showWarnings=FALSE, recursive=TRUE)
          
        }
        
        # Initialize list to hold values for PortalDataset entity
        portalDataset <- list(
          portal = Portal,
          timestamp = format(Sys.Date(), "%a %b %d %X %Z %Y")
        )
        
        ##########################################################################################
        #
        # INTRODUCTION PAGE####
        #
        ##########################################################################################
        
        progress$inc(1/10, detail = "Saving introduction page")
        
        if(edit_files == 'All'){
          file.copy(from=intro_file, to=paste0("www/RMD/introduction_", Portal, ".Rmd"), overwrite=TRUE)
        }
        
        print("Saving introduction page")
        
        ##########################################################################################
        #
        # GENE EXPESSION ####
        #
        ##########################################################################################
        
        # Increment the progress bar
        progress$inc(3/10, detail = "Saving expression set")
        
        if(edit_files %in% c("All", "Gene Expression") | (edit_files %in% c("Profile Annotation", "Connectivity Map") & edit_ge_option %in% 'Yes')){
          
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
              }
              rownames(fData) <- fData$Gene
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
            }
            rownames(fData) <- fData$Gene
          }
          
        }
        
        if(edit_Analysis == 'Yes'){
          
          ##Check if the project include landmark gene#####
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
        
        if(edit_files %in% c("All", "Gene Expression") | (edit_files %in% c("Profile Annotation", "Connectivity Map") & edit_ge_option %in% 'Yes') | (edit_files %in% c("Profile Annotation", "Connectivity Map") & edit_ge_option %in% 'No' & edit_Analysis == 'Yes' & Landmark_Gene %in% portal$Landmark_Gene)){
          
          # create an expression matrix
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
          expressionSet <- ExpressionSet(assayData=gene_expression, phenoData=phenoData, featureData=featureData)
          
          # Upload ExpressionSet to WorkFile,
          # and add WorkFile ID to list of values for portal dataset list
          portalDataset$GeneExpressionRDS <- GeneHive::objectId(
            GeneHive::storeObjectAsWorkFile(expressionSet)
          )
          
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
          
        }
          
        ##########################################################################################
        #
        # CHEMICAL AND PROFILE ANNOTATION####
        #
        ##########################################################################################
        
        # Increment the progress bar
        progress$inc(2/10, detail = "Saving chemical and profile annotation")
        
        if(edit_files %in% c('All', 'Profile Annotation') | (edit_file %in% c("Gene Expression", "Connectivity Map") & edit_pro_option %in% 'Yes')){
          
          if(edit_Analysis == 'Yes'){
            
            # GET THE UNIQUE ID BY CHEM FOR PROFILE ANNOTATION
            pro_file$unique_ID_by_chem <- lapply(1:nrow(pro_file), function(r){ paste0(unlist(pro_file[r,Exposure]), collapse="_") }) %>% unlist()
            
            ##########################################################################################
            #
            # CALCULATE MOD Z-SCORES AND REPLICATE CORRELATION (CC) FOR TAS####
            #
            ##########################################################################################
            
            if(Edit_TAS==TRUE){
              
              calc_results <- TAS_Modzscores_Calculation(pro_file, chem_file, gene_expression)
              
              # get TAS
              TAS <- calc_results[["TAS"]]
              
              # chemical annotation information
              chem_file <- chem_file %>% left_join(TAS) %>% select(-Number_of_Replicates)
              
              # profile annotation information
              pro_file <- pro_file %>% left_join(TAS)
              
            }
            
          }
          
          # Upload profile annotation to WorkFiles,
          # and add WorkFile IDs to list of values for portal dataset list
          portalDataset$ProfileAnnotationRDS <- GeneHive::objectId(
            GeneHive::storeObjectAsWorkFile(pro_file)
          )
          
          # Upload chemical annotation to WorkFiles,
          # and add WorkFile IDs to list of values for portal dataset list
          portalDataset$ChemicalAnnotationRDS <- GeneHive::objectId(
            GeneHive::storeObjectAsWorkFile(chem_file)
          )
          
          print("Saving chemical and profile annotation")
          
        }
        
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
                }
                rownames(fData) <- fData$Gene
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
          
          # Upload CMap data to WorkFile,
          # and add WorkFile ID to list of values for portal dataset list
          portalDataset$ConnectivityRDS <- GeneHive::objectId(
            GeneHive::storeObjectAsWorkFile(connectivity_map)
          )
          
          print("Saving connectivity map file")
          print("Saving connectivity Morpheous heatmap")
          
        }
        
        ##########################################################################################
        #
        # GENE SET ENRICHMENT ANALYSIS####
        #
        ##########################################################################################
        
        progress$inc(5/10, detail = "Saving gene set enrichment results")
        
        if((edit_files %in% c("All", "Profile Annotation", "Gene Expression", "Connectivity Map") & edit_Analysis == "Yes") | edit_files %in% "GS Enrichment"){
          
          # create an expression matrix
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
          expressionSet <- ExpressionSet(assayData=gene_expression, phenoData=phenoData, featureData=featureData)
          
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
              gs_collection <- getGmt(paste0("data/Enrichment Gene Set/", GS_Collection, ".gmt"))
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
          
          # Upload GSEA data to WorkFile,
          # and add WorkFile ID to list of values for portal dataset list
          portalDataset$GeneSetEnrichmentRDS <- GeneHive::objectId(
            GeneHive::storeObjectAsWorkFile(gsscores)
          )
          
          print("Saving gene set enrichment file")
          print("Saving gene set enrichment Morpheous heatmap")
          
        }
        
        
        ##########################################################################################
        #
        # TAXONOMER ANALYSIS####
        #
        ##########################################################################################
        
        progress$inc(6/10, detail = "Saving taxonomer results")
        
        if((edit_files %in% c("All", "Profile Annotation", "Gene Expression", "Connectivity Map") & edit_Analysis == "Yes") | edit_files == "K2Taxonomer"){
          
          # create an expression matrix
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
          expressionSet <- ExpressionSet(assayData=gene_expression, phenoData=phenoData, featureData=featureData)
          
          #Get a expression set####
          eSet <- expressionSet
          
          ##Create the info class vector to run the metavariable test
          infoClassVector <- NULL;
          
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
          if(GS_Collection == "Default"){
            
            # Read in the gene set collection for gene set enrichment analysis###
            gsscores_hallmark <- getGmt(paste0("data/Enrichment Gene Set/h.all.v", Enrichment_Version, ".0.gmt"))
            gsscores_c2_reactome <- getGmt(paste0("data/Enrichment Gene Set/c2.cp.reactome.v", Enrichment_Version, ".0.gmt"))
            gsscores_nursa <- getGmt(paste0("data/Enrichment Gene Set/nursa_consensome_Cbyfdrvalue_0.01.gmt"))
            
            hallmark_genelist <- lapply(seq_along(gsscores_hallmark), function(g){ gsscores_hallmark[[g]]@geneIds })
            names(hallmark_genelist) <- names(gsscores_hallmark)
            
            c2_reactome_genelist <- lapply(seq_along(gsscores_c2_reactome), function(g){ gsscores_c2_reactome[[g]]@geneIds })
            names(c2_reactome_genelist) <- names(gsscores_c2_reactome)
            
            nursa_genelist <- lapply(seq_along(gsscores_nursa), function(g){ gsscores_nursa[[g]]@geneIds })
            names(nursa_genelist) <- names(gsscores_nursa)
            
            # Combine gene set enrichment for hallmark, C2, and NURSA
            geneSetList <- do.call(c, list(hallmark_genelist, c2_reactome_genelist, nursa_genelist))
            
          }else{
            
            gs_collection <- getGmt(paste0("data/Enrichment Gene Set/", GS_Collection, ".gmt"))
            
            collection_genelist <- lapply(seq_along(gs_collection), function(g){ gs_collection[[g]]@geneIds })
            names(collection_genelist) <- names(gs_collection)
            
            geneSetList <- collection_genelist
            
          }
          
          # Run K2Taxonomer!
          # Always important to set a seed
          RNGkind("L'Ecuyer-CMRG")
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
          
          # Upload K2Taxonomer results to WorkFile,
          # and add WorkFile ID to list of values for portal dataset list
          portalDataset$K2TaxonomerResultsRDS <- GeneHive::objectId(
            GeneHive::storeObjectAsWorkFile(K2res)
          )
          
          print("Saving K2Taxonomer results")
          
        }
        
      })
      
      ## Show notification on error or user interrupt
      fut <- catch(
        fut,
        function(e){
          print(e$message)
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
          Enrichment_Version = Enrichment_Version,
          Landmark_Gene=Landmark_Gene,
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
          TAS=Edit_Modzscores, 
          Modzscores=Edit_Modzscores,
          Exposure_Levels=paste0(exposure_categorical, collapse=", "), 
          Exposure_Phenotype=paste0(exposure_phenotype_categorical, collapse=", "), 
          Exposure_Phenotype_Test=paste0(methods, collapse=", "), 
          Connectivity_Variable=connectivity_var,
          Connectivity_Test=connectivity_test,
          Feature_Filtering=featMetric,
          Description=Description
        )
        
        new_proj <- new_proj[,colnames(proj_dat)]
        proj_dat <- proj_dat[-row,]
        newproject <- proj_dat %>% rbind(new_proj)
        projectdata(newproject)
        
        # Add PortalDataset entity to GeneHive
        do.call(
          GeneHive::addEntity, args=c(.class="PortalDataset", portalDataset)
        )
        
        removeModal()
        
        shinyjs::enable(id="Edit_Project_Add_Button")
        shinyjs::enable(id="Edit_Project_Cancel_Button")
        
      })
    }
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

