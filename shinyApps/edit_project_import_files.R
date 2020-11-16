

#import introduction file#####
observeEvent(input$Edit_Project_Add_Button, {
  
  req(input$edit_files == 'Introduction Page' | input$edit_files == 'All')
  
  inputfile <- input$edit_intro_file;
  
  if(is.null(inputfile)){
    intro_file_msg("Please choose a file to import.")
    intro_file(NULL)
    return(NULL)
  }
  
})

observeEvent({
  input$EditProject
  input$edit_files
  input$edit_intro_file
}, {
  
  req(input$edit_files == 'Introduction Page' | input$edit_files == 'All')
  
  inputfile <- input$edit_intro_file
  
  if(is.null(inputfile)){
    intro_file_msg("")
    intro_file(NULL)
    return(NULL)
  }
  
  tryCatch({
    
    extension <- grep(toupper(".Rmd"), toupper(substr(inputfile$datapath, nchar(inputfile$datapath)-4, nchar(inputfile$datapath))), fixed = TRUE)
    
    if(length(extension) == 0){
      intro_file_msg("Incorrect file format. Please check your file again.")
      intro_file(NULL)
      return(NULL)
    }else{
      intro_file_msg("")
      intro_file(inputfile$datapath)
    }
    
  }, error=function(err){
    intro_file_msg("Import failed. Please check your file again.")
    intro_file(NULL)
    return(NULL)
  }, warning=function(war){
    intro_file_msg("Import failed. Please check your file again.")
    intro_file(NULL)
    return(NULL)
  })
  
})

##Output introduction warning message###
output$edit_intro_file_msg <- renderUI({
  
  req(intro_file_msg())
  
  p(class="fileInputMsg",  HTML(intro_file_msg()))
  
})

#import profile annotation file#####
observeEvent({
  input$EditProject
  input$edit_files
  input$edit_ge_pro_option
  input$edit_conn_pro_option
}, {
  
  req((input$edit_files == 'Gene Expression' & input$edit_ge_pro_option == 'No') | input$edit_files %in% c("None", "Introduction Page", "GS Enrichment", "K2Taxonomer") | (input$edit_files == "Connectivity Map" & input$edit_conn_pro_option %in% 'No'))
  
  pro_file_msg("")
  portal <- portal()
  exposure <- unlist(strsplit(as.character(portal$Exposure_Levels), ",", fixed=TRUE)) %>% trimws()
  exposure_phenotype <- unlist(strsplit(as.character(portal$Exposure_Phenotype), ",", fixed=TRUE)) %>% trimws()
  
  # Retrieve list of all PortalDataset entities in hive matching portal name
  datasets <- listEntities("PortalDataset", portal=portal$Portal)
  # Sort by timestamp and extract most recent dataset to convenience object
  datasets <- datasets[order(sapply(datasets, slot, "timestamp"))]
  dataset <- datasets[[length(datasets)]]

  tryCatch({ 
    # Read the profile annotation object from the hive
    pro_dat <- GeneHive::getWorkFileAsObject(
      hiveWorkFileID(dataset@ProfileAnnotationRDS)
    )
    exposure_variable <- unique(colnames(pro_dat)[which(!colnames(pro_dat) %in% c("Sig_Id", "Chemical_Id", "Chemical_Name", "BUID", "CAS", "TAS"))])
    if((input$edit_files == 'Gene Expression' & input$edit_ge_pro_option == 'No') | input$edit_files %in% "K2Taxonomer" | input$edit_files %in% "GS Enrichment"){
      updateSelectInput(session, inputId="edit_variable_compound", choices="Chemical_Id")
      updateSelectInput(session, inputId="edit_variable_exposure", choices=exposure_variable, selected=exposure)
      updateSelectInput(session, inputId="edit_variable_exposure_phenotype", choices=exposure_variable, selected=exposure_phenotype)
      updateSelectInput(session, inputId="edit_feature_metric", selected=unlist(strsplit(as.character(portal$Feature_Filtering), ",", fixed=TRUE))[1] %>% trimws())
    }
    pro_file(pro_dat)
  }, error=function(err){
    print(err)
  }, warning=function(war){
    print(war)
  })
  
  tryCatch({ 
    # Read the chemical annotation object from the hive
    chem_dat <- GeneHive::getWorkFileAsObject(
      hiveWorkFileID(dataset@ChemicalAnnotationRDS)
    )
    chem_file(chem_dat)
  }, error=function(err){
    print(err)
  }, warning=function(war){
    print(war)
  })
  
})

#import profile annotation file#####
observeEvent(input$Edit_Project_Add_Button, {
  
  req(input$edit_files == 'Profile Annotation' | input$edit_files == 'All' | (input$edit_files == 'Gene Expression' & input$edit_ge_pro_option=='Yes') | (input$edit_files == "Connectivity Map" & input$edit_conn_pro_option %in% 'Yes'))
  
  inputfile <- input$edit_pro_file;
  inputtype <- input$edit_pro_file_type; 
  
  if(is.null(inputfile)){
    pro_file_msg("Please choose a file to import.")
    chem_file(NULL)
    pro_file(NULL)
    return(NULL)
  }

})

#import profile annotation file#####
observeEvent({
  input$EditProject
  input$edit_files
  input$edit_pro_file
  input$edit_pro_file_type
  input$edit_ge_pro_option
  input$edit_conn_pro_option
}, {
  
  req(input$edit_files == 'Profile Annotation' | input$edit_files == 'All' | (input$edit_files == 'Gene Expression' & input$edit_ge_pro_option=='Yes') | (input$edit_files == "Connectivity Map" & input$edit_conn_pro_option %in% 'Yes'))
  
  inputfile <- input$edit_pro_file;
  inputtype <- input$edit_pro_file_type; 
  
  if(is.null(inputfile)){
    updateSelectInput(session, inputId="edit_variable_compound", choices=c("Import a profile annotation" = ""))
    updateSelectInput(session, inputId="edit_variable_exposure", choices=c("Import a profile annotation" = ""))
    updateSelectInput(session, inputId="edit_variable_exposure_phenotype", choices=c("Import a profile annotation" = ""))
    updateSelectInput(session, inputId="edit_feature_metric", selected="sd")
    pro_file_msg("")
    chem_file(NULL)
    pro_file(NULL)
    return(NULL)
  }
  
  tryCatch({
    
    csv_ext <-  grep(toupper(".csv"), toupper(substr(inputfile$datapath, nchar(inputfile$datapath)-4, nchar(inputfile$datapath))), fixed = TRUE)
    rds_ext <-  grep(toupper(".rds"), toupper(substr(inputfile$datapath, nchar(inputfile$datapath)-4, nchar(inputfile$datapath))), fixed = TRUE)
    
    if(inputtype %in% ".csv" & length(csv_ext) > 0){
      dat <- read.csv(inputfile$datapath, header = TRUE, row.names = 1, check.names = FALSE, stringsAsFactors = FALSE)
    }else if(inputtype %in% ".RDS" & length(rds_ext) > 0){
      dat <- readRDS(inputfile$datapath)
    }else{
      updateSelectInput(session, inputId="edit_variable_compound", choices=c("Import a profile annotation" = ""))
      updateSelectInput(session, inputId="edit_variable_exposure", choices=c("Import a profile annotation" = ""))
      updateSelectInput(session, inputId="edit_variable_exposure_phenotype", choices=c("Import a profile annotation" = ""))
      updateSelectInput(session, inputId="edit_feature_metric", selected="sd")
      pro_file_msg("Incorrect file format. Please check your file again.")
      chem_file(NULL)
      pro_file(NULL)
      return(NULL)
    }
    
    variables <- c("Sig_Id", "Chemical_Id", "Chemical_Name", "BUID", "CAS")
    
    if(all(variables %in% colnames(dat))){
      exposure_variable <- unique(colnames(dat)[which(!colnames(dat) %in% c("Sig_Id", "Chemical_Id", "Chemical_Name", "BUID", "CAS", "TAS"))])
      updateSelectInput(session, inputId="edit_variable_compound", choices=c("Please select an option below" = "", "Chemical_Id"))
      updateSelectInput(session, inputId="edit_variable_exposure", choices=c("Please select an option below" = "", exposure_variable))
      updateSelectInput(session, inputId="edit_variable_exposure_phenotype", choices = c("Please select an option below" = "", exposure_variable))
      updateSelectInput(session, inputId="edit_feature_metric", selected="sd")
      chem_dat <- distinct(dat, Chemical_Id, Chemical_Name, BUID, CAS, .keep_all=TRUE) %>% select(-Sig_Id)
      rownames(chem_dat) <- chem_dat$Chemical_Id
      chem_file(chem_dat)
      pro_file_msg("")
      pro_file(dat)      
    }else{
      updateSelectInput(session, inputId="edit_variable_compound", choices=c("Import a profile annotation" = ""))
      updateSelectInput(session, inputId="edit_variable_exposure", choices=c("Import a profile annotation" = ""))
      updateSelectInput(session, inputId="edit_variable_exposure_phenotype", choices=c("Import a profile annotation" = ""))
      updateSelectInput(session, inputId="edit_feature_metric", selected="sd")
      errorMsg <- paste0("One or more of the required variables: <em>", paste0(variables, collapse = ", "), "</em> are missing from the dataset.")
      pro_file_msg(errorMsg)
      chem_file(NULL)
      pro_file(NULL)
      return(NULL)
    }
    
  }, error=function(err){
    print(err)
  }, warning=function(war){
    print(war)
  })

})

##Output profile warning message###
output$edit_pro_file_msg <- renderUI({
  
  req(pro_file_msg())
  
  p(class="fileInputMsg", HTML(pro_file_msg()))
  
})

##Getting the cohort option####
observeEvent({
  input$edit_variable_exposure
  input$edit_pro_file
  input$edit_pro_file_type
  input$edit_ge_file
  input$edit_ge_file_type
  input$edit_pro_ge_option
}, {
  
  req(pro_file(), chem_file(), ge_file(), input$edit_variable_exposure)
  
  pro_ann <- pro_file(); chem_ann <- chem_file(); gene_expression <- ge_file(); 
  var <- ifelse(all(colnames(gene_expression) %in% pro_ann$Sig_Id), "Sig_Id", "Chemical_Id");
  exposure <- input$add_variable_exposure;
  
  # Getting the number of replicates for each chemical
  if(var=="Sig_Id"){
    pro_ann$unique_ID_by_chem <- lapply(1:nrow(pro_ann), function(r){ paste0(unlist(pro_ann[r,exposure]), collapse="_") }) %>% unlist()
    chem_replicate <- pro_ann %>% group_by(Chemical_Id, unique_ID_by_chem) %>% summarise(Frequency=n())
  }else{
    chem_ann$unique_ID_by_chem <- lapply(1:nrow(chem_ann), function(r){ paste0(unlist(chem_ann[r,exposure]), collapse="_") }) %>% unlist()
    chem_replicate <- chem_ann %>% group_by(Chemical_Id, unique_ID_by_chem) %>% summarise(Frequency=n())
  }  
  
  if(any(chem_replicate$Frequency > 1)){
    
    cohorts("Chemical_Id")
    
  }else{
    
    cohorts(NULL)
    
  }
  
})

##Create Add TAS and ModZ option####
output$Edit_Tas_Modz <- renderUI({
  
  req(cohorts(), portal(), input$edit_files, input$edit_pro_ge_option)

  if((input$edit_files == 'Profile Annotation' & input$edit_pro_ge_option == "No") | input$edit_files %in% "GS Enrichment" | input$edit_files %in% "K2Taxonomer"){
    TAS=unlist(strsplit(as.character(portal()$TAS_Modzscores), ",", fixed=TRUE))[1] %>% trimws()
    Modzscores=unlist(strsplit(as.character(portal()$TAS_Modzscores), ",", fixed=TRUE))[2] %>% trimws()
  }else{
    TAS=FALSE; Modzscores=FALSE; TAS_disabled=FALSE; Modzscores_disabled=FALSE;
  }
  
  div(
    h4("Calculations:", style="padding-bottom: 10px;"),
    CheckBoxInputFunction(inputId="Edit_TAS", label="TAS", value=TAS, disabled=FALSE),
    CheckBoxInputFunction(inputId="Edit_Modzscores", label="Mod-Zscores", value=Modzscores, disabled=FALSE)
  )

})

##List of exposure phenotype statistical tests####
output$edit_metavar_variable_test <- DT::renderDataTable({
  
  req(pro_file(), input$edit_variable_exposure_phenotype)
  
  pro_ann=pro_file(); varlist=input$edit_variable_exposure_phenotype;
  
  test <- suppressWarnings({
    phenotype_test(pro_ann=pro_ann, varlist=varlist)
  })
  
  table = data.frame(Variable=varlist, test=test)
  colnames(table) <- c("Exposure Phenotype", "Statistical Test")
  return(table)
  
}, rownames=FALSE, server=FALSE, escape=FALSE, selection="none", 
options=list(
  dom="T", 
  columnDefs = list(list(className = 'dt-center', targets = "_all")),
  drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); }')
))

#import ge expression file#####
observeEvent({
  input$EditProject
  input$edit_files
  input$edit_pro_ge_option
  input$edit_conn_pro_option
  input$edit_pro_file
  input$edit_pro_file_type
}, {
  
  req((input$edit_files == 'Profile Annotation' & input$edit_pro_ge_option %in% "No") | input$edit_files %in% c("None", "Introduction Page", "GS Enrichment", "Taxonomer") | (input$edit_files == "Connectivity Map" & input$edit_conn_pro_option %in% 'No'))
  
  pro_ann <- pro_file(); portal <- portal();
  
  tryCatch({
    
    # Retrieve list of all PortalDataset entities in hive matching portal name
    datasets <- listEntities("PortalDataset", portal=portal$Portal)
    # Sort by timestamp and extract most recent dataset to convenience object
    datasets <- datasets[order(sapply(datasets, slot, "timestamp"))]
    dataset <- datasets[[length(datasets)]]
    # Read the ExpressionSet object from the hive
    dat <- GeneHive::getWorkFileAsObject(
      hiveWorkFileID(dataset@GeneExpressionRDS)
    )

    if(!is.null(pro_ann)){
      if(all(colnames(dat) %in% pro_ann$Sig_Id) | all(colnames(dat) %in% pro_ann$Chemical_Id)){
        match_colnames <- TRUE
      }else{
        match_colnames <- FALSE
      }
      
      if(!match_colnames){
        errorMsg <- paste0("The expression set and profile annotation do not match. Please check your files again.")
        ge_file_msg(errorMsg)
      }else{
        ge_file_msg("")
      }
    }else{
      ge_file_msg("")
    }
    
    ge_file(dat)
      
  }, error=function(err){
    print(err)
  }, warning=function(war){
    print(war)
  })

})

#import ge expression file#####
observeEvent(input$Edit_Project_Add_Button, {
  
  req((input$edit_files == 'Profile Annotation' & input$edit_pro_ge_option=='Yes') | input$edit_files == 'All' | input$edit_files == 'Gene Expression' |  (input$edit_files == "Connectivity Map" & input$edit_conn_pro_option %in% 'Yes'))
  
  inputfile <- input$edit_ge_file;
  inputtype <- input$edit_ge_file_type; 
  
  if(is.null(inputfile)){
    ge_file_msg("Please choose a file to import.")
    ge_file(NULL)
    return(NULL)
  }

})

#import ge expression file#####
observeEvent({
  input$EditProject
  input$edit_files
  input$edit_ge_file
  input$edit_ge_file_type
  input$edit_pro_ge_option
  input$edit_conn_pro_option
}, {
  
  req((input$edit_files == 'Profile Annotation' & input$edit_pro_ge_option=='Yes') | input$edit_files == 'All' | input$edit_files == 'Gene Expression' | (input$edit_files == "Connectivity Map" & input$edit_conn_pro_option %in% 'Yes'))
  
  inputfile <- input$edit_ge_file;
  inputtype <- input$edit_ge_file_type; 
  pro_ann <- pro_file();
  
  if(is.null(inputfile)){
    ge_file_msg("")
    ge_file(NULL)
    return(NULL)
  }
  
  tryCatch({
    
    csv_ext <-  grep(toupper(".csv"), toupper(substr(inputfile$datapath, nchar(inputfile$datapath)-4, nchar(inputfile$datapath))), fixed = TRUE)
    rds_ext <-  grep(toupper(".rds"), toupper(substr(inputfile$datapath, nchar(inputfile$datapath)-4, nchar(inputfile$datapath))), fixed = TRUE)
    
    if(inputtype %in% ".csv" & length(csv_ext) > 0){
      dat <- read.csv(inputfile$datapath, header = TRUE, row.names = 1, check.names = FALSE, stringsAsFactors = FALSE)
    }else if(inputtype %in% ".RDS" & length(rds_ext) > 0){
      dat <- readRDS(inputfile$datapath)
    }else{
      ge_file_msg("Incorrect file format. Please check your file again.")
      ge_file(NULL)
      return(NULL)
    }
    
    if(is.null(pro_ann)){
      ge_file_msg("Need a profile annotation file to match.")
      ge_file(NULL)
      return(NULL)
    }

    if(all(colnames(dat) %in% pro_ann$Sig_Id) | all(colnames(dat) %in% pro_ann$Chemical_Id)){
      match_colnames <- TRUE
    }else{
      match_colnames <- FALSE
    }

    if(match_colnames){
      ge_file_msg("")
      ge_file(dat)
    }else{
      errorMsg <- paste0("The expression set and profile annotation do not match. Please check your files again.")
      ge_file_msg(errorMsg)
      ge_file(NULL)
      return(NULL)
    }

  }, error=function(err){
    ge_file_msg("Import failed. Please check your file again.")
    ge_file(NULL)
    return(NULL)
  }, warning=function(war){
    ge_file_msg("Import failed. Please check your file again.")
    ge_file(NULL)
    return(NULL)
  })
  
})


##Output ge warning message####
output$edit_ge_file_msg <- renderUI({
  
  req(ge_file_msg())
  
  p(class="fileInputMsg",  HTML(ge_file_msg()))
  
})


#import connectivity map - perturbagens and perturbagensclass file#####
observeEvent({
  input$EditProject
  input$edit_files
  input$edit_conn_option
  input$edit_pro_file
  input$edit_pro_file_type
}, {
  
  req((input$edit_files %in% c("All", "Profile Annotation", "Gene Expression") & input$edit_conn_option %in% "No") | input$edit_files %in% "GS Enrichment" | input$edit_files %in% "K2Taxonomer")
  
  pro_ann <- pro_file(); portal <- portal();     
  connectivity_var <- unlist(strsplit(as.character(portal$Connectivity_Test), ",", fixed=TRUE))[1] %>% trimws() 
  connectivity_test=unlist(strsplit(as.character(portal$Connectivity_Test), ",", fixed=TRUE))[2] %>% trimws() 
  
  tryCatch({
    
    # Retrieve list of all PortalDataset entities in hive matching portal name
    datasets <- listEntities("PortalDataset", portal=portal$Portal)
    # Sort by timestamp and extract most recent dataset to convenience object
    datasets <- datasets[order(sapply(datasets, slot, "timestamp"))]
    dataset <- datasets[[length(datasets)]]
    # Read the list of ExpressionSets from the hive
    CMap <- GeneHive::getWorkFileAsObject(
      hiveWorkFileID(dataset@ConnectivityRDS)
    )
    # Copy the ExpressionSets into convenience variables
    pcl_dat <- CMap$pcl
    pert_dat <- CMap$pert
    
    if(!is.null(pro_ann)){
      if(all(colnames(pcl_dat) %in% pro_ann$Sig_Id) | all(colnames(pcl_dat) %in% pro_ann$Chemical_Id)){
        match_colnames <- TRUE
      }else{
        match_colnames <- FALSE
      }
      
      if(!match_colnames){
        errorMsg <- paste0("The connectivity map and profile annotation do not match. Please check your files again.")
        conn_pcl_file_msg(errorMsg)
      }
    }else{
      conn_pcl_file_msg("")
      conn_pert_file_msg("")
    }
    
    conn_pcl_file(pcl_dat)
    conn_pert_file(pert_dat)

    shinyjs::show(id="edit_connectivity_var"); shinyjs::show(id="edit_connectivity_test")
    
    if(input$edit_files %in% "K2Taxonomer"){
      updateCheckboxInput(session, inputId="edit_connectivity_var", value=connectivity_var)
      updateSelectInput(session, inputId="edit_connectivity_test", selected=connectivity_test)
    }else{
      updateCheckboxInput(session, inputId="edit_connectivity_var", value=FALSE)
      updateSelectInput(session, inputId="edit_connectivity_test", selected="1-sided Wilcox RS test")
    }
    
  }, error=function(err){
    shinyjs::hide(id="edit_connectivity_var")
    shinyjs::hide(id="edit_connectivity_test")
    print(err)
  }, warning=function(war){
    shinyjs::hide(id="edit_connectivity_var")
    shinyjs::hide(id="edit_connectivity_test")
    print(war)
  })
  
})

#import connectivity map - perturbagens class file#####
observeEvent(input$Edit_Project_Add_Button, {
  
  req(input$edit_files %in% "Connectivity Map" | (input$edit_files %in% c("All", "Profile Annotation", "Gene Expression") & input$edit_conn_option %in% "Yes"))
  
  inputfile <- input$edit_conn_pcl_file;
  inputtype <- input$edit_conn_pcl_file_type;
  
  if(is.null(inputfile)){
    conn_pcl_file_msg("Please choose a file to import.")
    conn_pcl_file(NULL)
    return(NULL)
  }
  
})

#import connectivity map - perturbagens class file#####
observeEvent({
  input$EditProject
  input$edit_files
  input$edit_conn_pcl_file
  input$edit_conn_pcl_file_type
  input$edit_conn_option
}, {
  
  req(input$edit_files %in% "Connectivity Map" | (input$edit_files %in% c("All", "Profile Annotation", "Gene Expression") & input$edit_conn_option %in% "Yes"))
  
  shinyjs::show(id="edit_connectivity_var"); shinyjs::show(id="edit_connectivity_test")
  updateCheckboxInput(session, inputId="edit_connectivity_var", value=FALSE)
  updateSelectInput(session, inputId="edit_connectivity_test", selected="1-sided Wilcox RS test")
  inputfile <- input$edit_conn_pcl_file;
  inputtype <- input$edit_conn_pcl_file_type;
  pro_ann <- pro_file();
  
  if(is.null(inputfile)){
    conn_pcl_file(NULL)
    return(NULL)
  }
  
  tryCatch({
    
    csv_ext <-  grep(toupper(".csv"), toupper(substr(inputfile$datapath, nchar(inputfile$datapath)-4, nchar(inputfile$datapath))), fixed = TRUE)
    rds_ext <-  grep(toupper(".rds"), toupper(substr(inputfile$datapath, nchar(inputfile$datapath)-4, nchar(inputfile$datapath))), fixed = TRUE)
    
    if(inputtype %in% ".csv" & length(csv_ext) > 0){
      dat <- read.csv(inputfile$datapath, header = TRUE, row.names = 1, check.names = FALSE, stringsAsFactors = FALSE)
    }else if(inputtype %in% ".RDS" & length(rds_ext) > 0){
      dat <- readRDS(inputfile$datapath)
    }else{
      conn_pcl_file_msg("Incorrect file format. Please check your file again.")
      conn_pcl_file(NULL)
      return(NULL)
    }
    
    if(is.null(pro_ann)){
      conn_pcl_file_msg("Need a profile annotation file to match.")
      conn_pcl_file(NULL)
      return(NULL)
    }
    
    match_colnames <- all(pro_ann$Sig_Id %in% colnames(dat)) | all(pro_ann$Chemical_Id %in% colnames(dat))
    
    if(match_colnames){
      conn_pcl_file_msg("")
      conn_pcl_file(dat)
    }else{
      errorMsg <- paste0("The connectivity map (perturbagens class) and profile annotation do not match. Please check your files again.")
      conn_pcl_file_msg(errorMsg)
      conn_pcl_file(NULL)
      return(NULL)
    }
    
  }, error=function(err){
    conn_pcl_file_msg("Import failed. Please check your file again.")
    conn_pcl_file(NULL)
    return(NULL)
  }, warning=function(war){
    conn_pcl_file_msg("Import failed. Please check your file again.")
    conn_pcl_file(NULL)
    return(NULL)
  })
  
})


##Output pcl warning message###
output$edit_conn_pcl_file_msg <- renderUI({
  
  req(conn_pcl_file_msg())
  
  p(class="fileInputMsg",  HTML(conn_pcl_file_msg()))
  
})

#import connectivity map - perturbagens file#####
observeEvent(input$Edit_Project_Add_Button, {
  
  req(input$edit_files %in% "Connectivity Map" | (input$edit_files %in% c("All", "Profile Annotation", "Gene Expression") & input$edit_conn_option %in% "Yes"))
  
  inputfile <- input$edit_conn_pert_file;
  inputtype <- input$edit_conn_pert_file_type;
  
  if(is.null(inputfile)){
    conn_pert_file_msg("Please choose a file to import.")
    conn_pert_file(NULL)
    return(NULL)
  }

})

#import connectivity map - perturbagens file#####
observeEvent({
  input$EditProject
  input$edit_files
  input$edit_conn_pert_file
  input$edit_conn_pert_file_type
  input$edit_conn_option
}, {
  
  req(input$edit_files %in% "Connectivity Map" | (input$edit_files %in% c("All", "Profile Annotation", "Gene Expression") & input$edit_conn_option %in% "Yes"))
  
  inputfile <- input$edit_conn_pert_file;
  inputtype <- input$edit_conn_pert_file_type;
  pro_ann <- pro_file();
  
  if(is.null(inputfile)){
    conn_pert_file(NULL)
    return(NULL)
  }
  
  tryCatch({
    
    csv_ext <-  grep(toupper(".csv"), toupper(substr(inputfile$datapath, nchar(inputfile$datapath)-4, nchar(inputfile$datapath))), fixed = TRUE)
    rds_ext <-  grep(toupper(".rds"), toupper(substr(inputfile$datapath, nchar(inputfile$datapath)-4, nchar(inputfile$datapath))), fixed = TRUE)
    
    if(inputtype %in% ".csv" & length(csv_ext) > 0){
      dat <- read.csv(inputfile$datapath, header = TRUE, row.names = 1, check.names = FALSE, stringsAsFactors = FALSE)
    }else if(inputtype %in% ".RDS" & length(rds_ext) > 0){
      dat <- readRDS(inputfile$datapath)
    }else{
      conn_pert_file_msg("Incorrect file format. Please check your file again.")
      conn_pert_file(NULL)
      return(NULL)
    }
    
    if(is.null(pro_ann)){
      conn_pert_file_msg("Need a profile annotation file to match.")
      conn_pert_file(NULL)
      return(NULL)
    }
    
    match_colnames <- (all(colnames(dat) %in% pro_ann$Sig_Id) | all(colnames(dat)%in% pro_ann$Chemical_Id))
    
    if(match_colnames){
      conn_pert_file_msg("")
      conn_pert_file(dat)
    }else{
      errorMsg <- paste0("The connectivity map (perturbagens) and profile annotation do not match. Please check your files again.")
      conn_pert_file_msg(errorMsg)
      conn_pert_file(NULL)
      return(NULL)
    }
    
  }, error=function(err){
    conn_pert_file_msg("Import failed. Please check your file again.")
    conn_pert_file(NULL)
    return(NULL)
  }, warning=function(war){
    conn_pert_file_msg("Import failed. Please check your file again.")
    conn_pert_file(NULL)
    return(NULL)
  })
  
})

##Output pert warning message###
output$edit_conn_pert_file_msg <- renderUI({
  
  req(conn_pert_file_msg())
  
  p(class="fileInputMsg",  HTML(conn_pert_file_msg()))
  
})

#import gs_collection file#####
observeEvent(input$Edit_Project_Add_Button, {
  
  req((input$edit_files %in% c("All", "Profile Annotation", "Gene Expression", "GS Enrichment", "K2Taxonomer") | (input$edit_files == "Connectivity Map" & input$edit_conn_pro_option %in% 'Yes')) & input$edit_cur_enrichment_option %in% "No" & input$edit_gs_collection_file_option %in% "Yes")
  
  inputfile <- input$edit_gs_collection_file;
  
  if(is.null(inputfile)){
    gs_collection_file_msg("Please choose a file to import.")
    gs_collection_file(NULL)
    return(NULL)
  }
  
})

#import gs_collection file#####
observeEvent({
  input$EditProject
  input$edit_files
  input$edit_gs_collection_file
  input$edit_conn_pro_option
  input$edit_cur_enrichment_option
  input$edit_gs_collection_file_option
}, {
  
  req((input$edit_files %in% c("All", "Profile Annotation", "Gene Expression", "GS Enrichment", "K2Taxonomer") | (input$edit_files == "Connectivity Map" & input$edit_conn_pro_option %in% 'Yes')) & input$edit_cur_enrichment_option %in% "No" & input$edit_gs_collection_file_option %in% "Yes")
  
  inputfile <- input$edit_gs_collection_file;
  
  if(is.null(inputfile)){
    gs_collection_file(NULL)
    return(NULL)
  }
  
  tryCatch({
    
    extension <- grep(toupper(".gmt"), toupper(substr(inputfile$datapath, nchar(inputfile$datapath)-4, nchar(inputfile$datapath))), fixed = TRUE)
    
    if(length(extension) == 0){
      gs_collection_file_msg("Incorrect file format. Please check your file again.")
      gs_collection_file(NULL)
      return(NULL)
    }else{
      gs_collection_file_msg("")
      data <- getGmt(inputfile$datapath)
      gs_collection_file(
        list(
          path=inputfile$datapath,
          data=data
        )
      )
    }
    
  }, error=function(err){
    gs_collection_file_msg("Import failed. Please check your file again.")
    gs_collection_file(NULL)
    return(NULL)
  }, warning=function(war){
    gs_collection_file_msg("Import failed. Please check your file again.")
    gs_collection_file(NULL)
    return(NULL)
  })
  
})


##Output gs_collection warning message###
output$edit_gs_collection_file_msg <- renderUI({
  
  req(gs_collection_file_msg())
  
  p(class="fileInputMsg",  HTML(gs_collection_file_msg()))
  
})

