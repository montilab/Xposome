

#import introduction file#####
intro_file <- reactiveVal(NULL)

observeEvent(input$Add_Project_Add_Button, {
  
  inputfile <- input$add_intro_file;

  if(is.null(inputfile)){
    updateTextInput(session, inputId="add_intro_file_msg", label=NULL, value="Please choose a file to import.")
    intro_file(NULL)
    return(NULL)
  }
  
  tryCatch({
    
    extension <- grep(toupper(".rmd"), toupper(inputfile$datapath), fixed = TRUE)
    
    if(length(extension) == 0){
      updateTextInput(session, inputId="add_intro_file_msg", label=NULL, value="Wrong file extension.")
      intro_file(NULL)
      return(NULL)
    }else{
      intro_file(inputfile$datapath)
    }

  }, error=function(err){
    updateTextInput(session, inputId="add_intro_file_msg", label=NULL, value="Import failed. Please check your file again.")
    intro_file(NULL)
    return(NULL)
  }, warning=function(war){
    updateTextInput(session, inputId="add_intro_file_msg", label=NULL, value="Import failed. Please check your file again.")
    intro_file(NULL)
    return(NULL)
  })
  
})

#import profile annotation file#####
pro_file <- reactiveVal(NULL) 
  
observeEvent(input$Add_Project_Add_Button, {
  
  inputfile <- input$add_pro_file;
  inputtype <- input$add_pro_file_type; 
  
  if(is.null(inputfile)){
    updateTextInput(session, inputId="add_pro_file_msg", label=NULL, value="Please choose a file to import.")
    pro_file(NULL)
    return(NULL)
  }
  
  tryCatch({
    
    csv_ext <- grep(toupper(".csv"), toupper(inputfile$datapath), fixed = TRUE)
    rds_ext <- grep(toupper(".rds"), toupper(inputfile$datapath), fixed = TRUE)
    
    if(inputtype %in% ".csv" & length(csv_ext) > 0){
      dat <- read.csv(inputfile$datapath, header = TRUE)
    }else if(inputtype %in% ".RDS" & length(rds_ext) > 0){
      dat <- readRDS(inputfile$datapath)
    }else{
      updateTextInput(session, inputId="add_pro_file_msg", label=NULL, value="Wrong file extension.")
      pro_file(NULL)
    }
    
    if(nrow(dat) == 0) { pro_file(NULL); return(NULL) }
    
    if(input$Add_Experimental_Design == "Multiple doses"){
      
      variables <- c("Sig_Id", "Chemical_Name", "Dose", "Carcinogenicity", "Genotoxicity", "BUID", "CAS")
      
      if(input$Add_TAS %in% FALSE){
        variables <- c(variables, "TAS")
      }
      
      if(all(colnames(dat) %in% variables)){
        updateTextInput(session, inputId="add_pro_file_msg", label=NULL, value="")
        rownames(dat) <- dat$Sig_Id
        pro_file(dat)      
      }else{
        errorMsg <- paste0("One or more of the following variables: ", paste0(variables, collapse = ", "), " are missing.")
        updateTextInput(session, inputId="add_pro_file_msg", label=NULL, value=errorMsg)
        pro_file(NULL)
        return(NULL)
      }
      
    }else if(input$Add_Experimental_Design == "Multiple replicates"){
      
      variables <- c("Sig_Id", "Chemical_Id", "Chemical_Name", "Dose", "PPARg_Mod", "BUID", "CAS")
      
      if(input$Add_TAS %in% FALSE){
        variables <- c(variables, "TAS")
      }
      
      if(all(colnames(dat) %in% variables)){
        updateTextInput(session, inputId="add_pro_file_msg", label=NULL, value="")
        rownames(dat) <- dat$Sig_Id
        pro_file(dat)      
      }else{
        errorMsg <- paste0("One or more of the following variables: ", paste0(variables, collapse = ", "), " are missing.")
        updateTextInput(session, inputId="add_pro_file_msg", label=NULL, value=errorMsg)
        pro_file(NULL)
        return(NULL)
      }
      
    }

  }, error=function(err){
    updateTextInput(session, inputId="add_pro_file_msg", label=NULL, value="Import failed. Please check your file again.")
    pro_file(NULL)
    return(NULL)
  }, warning=function(war){
    updateTextInput(session, inputId="add_pro_file_msg", label=NULL, value="Import failed. Please check your file again.")
    pro_file(NULL)
    return(NULL)
  })
  
})

#import chemical annotation file#####
chem_file <- reactiveVal(NULL)

observeEvent(input$Add_Project_Add_Button, {
  
  inputfile <- input$add_chem_file;
  inputtype <- input$add_chem_file_type; 
  
  if(is.null(inputfile)){
    updateTextInput(session, inputId="add_chem_file_msg", label=NULL, value="Please choose a file to import.")
    chem_file(NULL)
    return(NULL)
  }
  
  tryCatch({
    
    csv_ext <- grep(toupper(".csv"), toupper(inputfile$datapath), fixed = TRUE)
    rds_ext <- grep(toupper(".rds"), toupper(inputfile$datapath), fixed = TRUE)
    
    if(inputtype %in% ".csv" & length(csv_ext) > 0){
      dat <- read.csv(inputfile$datapath, header = TRUE)
    }else if(inputtype %in% ".RDS" & length(rds_ext) > 0){
      dat <- readRDS(inputfile$datapath)
    }else{
      updateTextInput(session, inputId="add_chem_file_msg", label=NULL, value="Wrong file extension.")
      chem_file(NULL)
      return(NULL)
    }
    
    if(nrow(dat) == 0) { chem_file(NULL); return(NULL) }
    
    if(input$Add_Experimental_Design == "Multiple doses"){
      
      variables <- c("Chemical_Name", "Dose", "Carcinogenicity", "Genotoxicity", "BUID", "CAS")
      
      if(input$Add_TAS %in% FALSE){
        variables <- c(variables, "TAS")
      }
      
      if(all(colnames(dat) %in% variables)){
        updateTextInput(session, inputId="add_chem_file_msg", label=NULL, value="")
        rownames(dat) <- dat$Chemical_Name
        chem_file(dat)      
      }else{
        errorMsg <- paste0("One or more of the following variables: ", paste0(variables, collapse = ", "), " are missing.")
        updateTextInput(session, inputId="add_chem_file_msg", label=NULL, value=errorMsg)
        chem_file(NULL)
        return(NULL)
      }
      
    }else if(input$Add_Experimental_Design == "Multiple replicates"){
      
      variables <- c("Chemical_Id", "Chemical_Name", "Dose", "PPARg_Mod", "BUID", "CAS")
      
      if(input$Add_TAS %in% FALSE){
        variables <- c(variables, "TAS")
      }
      
      if(all(colnames(dat) %in% variables)){
        updateTextInput(session, inputId="add_chem_file_msg", label=NULL, value="")
        rownames(dat) <- dat$Chemical_Id
        chem_file(dat)      
      }else{
        errorMsg <- paste0("One or more of the following variables: ", paste0(variables, collapse = ", "), " are missing.")
        updateTextInput(session, inputId="add_chem_file_msg", label=NULL, value=errorMsg)
        chem_file(NULL)
        return(NULL)
      }
      
    }
    
  }, error=function(err){
    updateTextInput(session, inputId="add_chem_file_msg", label=NULL, value="Import failed. Please check your file again.")
    chem_file(NULL)
    return(NULL)
  }, warning=function(war){
    updateTextInput(session, inputId="add_chem_file_msg", label=NULL, value="Import failed. Please check your file again.")
    chem_file(NULL)
    return(NULL)
  })
  
})

#import ge expression file#####
ge_file <- reactiveVal(NULL)

observeEvent(input$Add_Project_Add_Button, {
  
  inputfile <- input$add_ge_file;
  inputtype <- input$add_ge_file_type; 
  pro_ann <- pro_file();  chem_ann <- chem_file();
  
  if(is.null(inputfile)){
    updateTextInput(session, inputId="add_ge_file_msg", label=NULL, value="Please choose a file to import.")
    ge_file(NULL)
    return(NULL)
  }
  
  tryCatch({
    
    csv_ext <- grep(toupper(".csv"), toupper(inputfile$datapath), fixed = TRUE)
    rds_ext <- grep(toupper(".rds"), toupper(inputfile$datapath), fixed = TRUE)
    
    if(inputtype %in% ".csv" & length(csv_ext) > 0){
      dat <- read.csv(inputfile$datapath, header = TRUE)
    }else if(inputtype %in% ".RDS" & length(rds_ext) > 0){
      dat <- readRDS(inputfile$datapath)
    }else{
      updateTextInput(session, inputId="add_ge_file_msg", label=NULL, value="Wrong file extension.")
      ge_file(NULL)
      return(NULL)
    }
    
    if(nrow(dat) == 0) { ge_file(NULL); return(NULL) }
    
    if(input$Add_Experimental_Design == "Multiple doses"){
      
      match_colnames <- all(colnames(pro_ann) %in% colnames(dat))
      
      if(match_colnames){
        updateTextInput(session, inputId="add_ge_file_msg", label=NULL, value="")
      }else{
        errorMsg <- paste0("One or more of the variables in the expression set do not match the profile annotation.")
        updateTextInput(session, inputId="add_ge_file_msg", label=NULL, value=errorMsg)
        ge_file(NULL)
        return(NULL)
      }
      
    }else if(input$Add_Experimental_Design == "Multiple replicates"){
      
      match_colnames <- all(colnames(chem_ann) %in% colnames(dat))
      
      if(match_colnames){
        updateTextInput(session, inputId="add_ge_file_msg", label=NULL, value="")
        ge_file(dat)      
      }else{
        errorMsg <- paste0("One or more of the variables in the expression set do not match the chemical annotation.")
        updateTextInput(session, inputId="add_ge_file_msg", label=NULL, value=errorMsg)
        ge_file(NULL)
        return(NULL)
      }
      
    }
    
  }, error=function(err){
    updateTextInput(session, inputId="add_ge_file_msg", label=NULL, value="Import failed. Please check your file again.")
    ge_file(NULL)
    return(NULL)
  }, warning=function(war){
    updateTextInput(session, inputId="add_ge_file_msg", label=NULL, value="Import failed. Please check your file again.")
    ge_file(NULL)
    return(NULL)
  })
  
})

#import connectivity map - perturbagens class file#####
conn_pcl_file <- reactiveVal(NULL)

observeEvent(input$Add_Project_Add_Button, {
  
  req(input$add_conn_option %in% "Yes")
  
  inputfile <- input$add_conn_pcl_file;
  inputtype <- input$add_conn_pcl_file_type;

  if(is.null(inputfile)){
    updateTextInput(session, inputId="add_conn_pcl_file_msg", label=NULL, value="Please choose a file to import.")
    conn_pcl_file(NULL)
    return(NULL)
  }

  tryCatch({

    csv_ext <- grep(toupper(".csv"), toupper(inputfile$datapath), fixed = TRUE)
    rds_ext <- grep(toupper(".rds"), toupper(inputfile$datapath), fixed = TRUE)

    if(inputtype %in% ".csv" & length(csv_ext) > 0){
      dat <- read.csv(inputfile$datapath, header = TRUE)
    }else if(inputtype %in% ".RDS" & length(rds_ext) > 0){
      dat <- readRDS(inputfile$datapath)
    }else{
      updateTextInput(session, inputId="add_conn_pcl_file_msg", label=NULL, value="Wrong file extension.")
      conn_pcl_file(NULL)
      return(NULL)
    }

    if(nrow(dat) == 0) { conn_pcl_file(NULL); return(NULL) }

  }, error=function(err){
    updateTextInput(session, inputId="add_conn_pcl_file_msg", label=NULL, value="Import failed. Please check your file again.")
    conn_pcl_file(NULL)
    return(NULL)
  }, warning=function(war){
    updateTextInput(session, inputId="add_conn_pcl_file_msg", label=NULL, value="Import failed. Please check your file again.")
    conn_pcl_file(NULL)
    return(NULL)
  })

})

#import connectivity map - perturbagens class file#####
conn_pert_file <- reactiveVal(NULL)
  
observeEvent(input$Add_Project_Add_Button, {
  
  req(input$add_conn_option %in% "Yes")
  
  inputfile <- input$add_conn_pert_file;
  inputtype <- input$add_conn_pert_file_type;
  
  if(is.null(inputfile)){
    updateTextInput(session, inputId="add_conn_pert_file_msg", label=NULL, value="Please choose a file to import.")
    conn_pert_file(NULL)
    return(NULL)
  }
  
  tryCatch({
    
    csv_ext <- grep(toupper(".csv"), toupper(inputfile$datapath), fixed = TRUE)
    rds_ext <- grep(toupper(".rds"), toupper(inputfile$datapath), fixed = TRUE)
    
    if(inputtype %in% ".csv" & length(csv_ext) > 0){
      dat <- read.csv(inputfile$datapath, header = TRUE)
    }else if(inputtype %in% ".RDS" & length(rds_ext) > 0){
      dat <- readRDS(inputfile$datapath)
    }else{
      updateTextInput(session, inputId="add_conn_pert_file_msg", label=NULL, value="Wrong file extension.")
      conn_pert_file(NULL)
      return(NULL)
    }
    
    if(nrow(dat) == 0) { conn_pert_file(NULL); return(NULL) }
    conn_pert_file(dat)
    
  }, error=function(err){
    updateTextInput(session, inputId="add_conn_pert_file_msg", label=NULL, value="Import failed. Please check your file again.")
    conn_pert_file(NULL)
    return(NULL)
  }, warning=function(war){
    updateTextInput(session, inputId="add_conn_pert_file_msg", label=NULL, value="Import failed. Please check your file again.")
    conn_pert_file(NULL)
    return(NULL)
  })
  
})

