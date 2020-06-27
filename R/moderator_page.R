

#import introduction file#####
intro_file <- reactiveVal(NULL); intro_file_msg <- reactiveVal(NULL)

observeEvent(input$Add_Project_Add_Button, {
  
  inputfile <- input$add_intro_file;
  
  if(is.null(inputfile)){
    intro_file_msg("Please choose a file to import.")
    intro_file(NULL)
    return(NULL)
  }
  
})

observeEvent(input$add_intro_file, {
  
  inputfile <- input$add_intro_file;
  
  if(is.null(inputfile)){
    intro_file(NULL)
    return(NULL)
  }
  
  tryCatch({
    
    extension <- grep(toupper(".rmd"), toupper(substr(inputfile$datapath, nchar(inputfile$datapath)-4, nchar(inputfile$datapath))), fixed = TRUE)
    
    if(length(extension) == 0){
      intro_file_msg("Incorrect file format.")
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
output$add_intro_file_msg <- renderUI({
  
  req(intro_file_msg())
  
  p(class="fileInputMsg",  HTML(intro_file_msg()))
  
})

#import profile annotation file#####
pro_file <- reactiveVal(NULL); pro_file_msg <- reactiveVal(NULL);

observeEvent(input$Add_Project_Add_Button, {
  
  inputfile <- input$add_pro_file;
  inputtype <- input$add_pro_file_type; 
  
  if(is.null(inputfile)){
    pro_file_msg("Please choose a file to import.")
    pro_file(NULL)
    return(NULL)
  }
  
})

observeEvent(input$add_pro_file, {
  
  inputfile <- input$add_pro_file;
  inputtype <- input$add_pro_file_type; 
  
  if(is.null(inputfile)){
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
      pro_file_msg("Incorrect file format.")
      pro_file(NULL)
      return(NULL)
    }
    
    variables <- c("Sig_Id", "Chemical_Id", "Chemical_Name")
    
    if(all(variables %in% colnames(dat))){
      exposure_variable <- unique(colnames(dat)[which(!colnames(dat) %in% c("Sig_Id", "Chemical_Id", "Chemical_Name", "BUID", "CAS", "TAS"))])
      updateSelectInput(session, inputId = "add_variable_compound", choices=c("Please select an option below" = "", "Chemical_Id"))
      updateSelectInput(session, inputId = "add_variable_exposure", choices=c("Please select an option below" = "", exposure_variable))
      pro_file_msg("")
      pro_file(dat)      
    }else{
      errorMsg <- paste0("One or more of the required variables: <em>", paste0(variables, collapse = ", "), "</em> are missing from the dataset.")
      pro_file_msg(errorMsg)
      pro_file(NULL)
      return(NULL)
    }
    
  }, error=function(err){
    pro_file_msg("Import failed. Please check your file again.")
    pro_file(NULL)
    print(err)
    return(NULL)
  }, warning=function(war){
    pro_file_msg("Import failed. Please check your file again.")
    pro_file(NULL)
    return(NULL)
  })
  
})


##Output profile warning message###
output$add_pro_file_msg <- renderUI({
  
  req(pro_file_msg())
  
  p(class="fileInputMsg", HTML(pro_file_msg()))
  
})

##Create reactive value for cohort####
cohorts <- reactiveVal(NULL)

##Create Add TAS and ModZ option####
observeEvent(input$add_variable_exposure, {
  
  req(pro_file(), input$add_variable_exposure)
  
  pro_ann <- pro_file(); exposure <- input$add_variable_exposure;
  
  # Getting the number of replicates for each chemical
  pro_ann$unique_ID_by_chem <- lapply(1:nrow(pro_ann), function(r){ paste0(unlist(pro_ann[r,exposure]), collapse="_") }) %>% unlist()
  
  chem_replicate <- pro_ann %>% group_by(Chemical_Id, unique_ID_by_chem) %>% summarise(Frequency=n())
  
  if(any(chem_replicate$Frequency > 1)){
    
    cohorts("Chemical_Id")
    
  }else{
    
    cohorts(NULL)
    
  }
  
})

##Create Add TAS and ModZ option####
output$Add_Tas_Modz <- renderUI({
  
  req(cohorts())
  
  div(
    checkboxInput(inputId = "Add_TAS", label = "TAS", value=TRUE), 
    checkboxInput(inputId = "Add_Modzscores", label = "Mod-Zscores", value=TRUE), 
  )
  
})

#import chemical annotation file#####
chem_file <- reactiveVal(NULL); chem_file_msg <- reactiveVal(NULL);

observeEvent(input$Add_Project_Add_Button, {
  
  inputfile <- input$add_chem_file;
  inputtype <- input$add_chem_file_type; 
  
  if(is.null(inputfile)){
    chem_file_msg("Please choose a file to import.")
    chem_file(NULL)
    return(NULL)
  }
  
})

observeEvent( input$add_chem_file, {
  
  inputfile <- input$add_chem_file;
  inputtype <- input$add_chem_file_type; 
  
  if(is.null(inputfile)){
    chem_file(NULL)
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
      chem_file_msg("Incorrect file format.")
      chem_file(NULL)
      return(NULL)
    }
    
    variables <- c("Chemical_Id", "Chemical_Name", "BUID", "CAS")
    
    if(all(variables %in% colnames(dat))){
      exposure_phenotype_variables <- unique(colnames(dat)[which(!colnames(dat) %in% c("Chemical_Id", "Chemical_Name", "BUID", "CAS", "TAS"))])
      updateSelectInput(session, inputId = "add_variable_exposure_phenotype", choices = c("Please select an option below" = "", exposure_phenotype_variables))
      chem_file_msg("")
      chem_file(dat)      
    }else{
      errorMsg <- paste0("One or more of the required variables: <em>", paste0(variables, collapse = ", "), "</em> are missing from the dataset.")
      chem_file_msg(errorMsg)
      chem_file(NULL)
      return(NULL)
    }
    
  }, error=function(err){
    chem_file_msg("Import failed. Please check your file again.")
    chem_file(NULL)
    return(NULL)
  }, warning=function(war){
    chem_file_msg("Import failed. Please check your file again.")
    chem_file(NULL)
    return(NULL)
  })
  
})

##list of exposure phenotype statistical tests####
output$add_metavar_variable_test <- DT::renderDataTable({
  
  req(input$add_variable_exposure_phenotype, chem_file())
  
  shinyjs::show(id="add_connectivity_var")
  
  varlist = input$add_variable_exposure_phenotype
  
  test = unlist(lapply(seq_along(varlist), function(v){ 
    
    checkvar <- chem_file()[, varlist[v]]
    
    if(all(is.na(as.numeric(checkvar)))){
      if(all(toupper(c("Yes", "No")) %in% toupper(checkvar))){
        SelectInputFunction(id=varlist[v], label=NULL, choices=c("1-sided Fisher test"), selected="1-sided Fisher test") 
      }else{
        SelectInputFunction(id=varlist[v], label=NULL, choices=c("2-sided Fisher test"), selected="2-sided Fisher test") 
      }
    }else{
      SelectInputFunction(id=varlist[v], label=NULL, choices=c("1-sided Wilcox RS test", "2-sided Wilcox RS test", "1-sided t test", "2-sided t test"), selected=NULL) 
    }
    
  }))
  
  table = data.frame(Variable=varlist, test=test)
  colnames(table) <- c("Exposure Phenotype", "Statistical Test")
  
  return(table)
  
}, rownames=FALSE, server=FALSE, escape=FALSE, selection="none", 
options=list(
  dom="T", 
  columnDefs = list(list(className = 'dt-center', targets = "_all")),
  drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); }')
))

##Output chemical warning message###
output$add_chem_file_msg <- renderUI({
  
  req(chem_file_msg())
  
  p(class="fileInputMsg",  HTML(chem_file_msg()))
  
})

#import ge expression file#####
ge_file <- reactiveVal(NULL); ge_file_msg <- reactiveVal(NULL);

observeEvent(input$Add_Project_Add_Button, {
  
  inputfile <- input$add_ge_file;
  inputtype <- input$add_ge_file_type; 
  pro_ann <- pro_file();
  
  if(is.null(inputfile)){
    ge_file_msg("Please choose a file to import.")
    ge_file(NULL)
    return(NULL)
  }

})

observeEvent(input$add_ge_file, {
  
  inputfile <- input$add_ge_file;
  inputtype <- input$add_ge_file_type; 
  pro_ann <- pro_file();
  
  if(is.null(inputfile)){
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
      ge_file_msg("Incorrect file format.")
      ge_file(NULL)
      return(NULL)
    }

    if(is.null(pro_ann)){
      ge_file_msg("Need a profile annotation file to match.")
      ge_file(NULL)
      return(NULL)
    }
    
    if(all(colnames(dat) %in% pro_ann$Sig_Id) | all(colnames(dat)%in% pro_ann$Chemical_Id)){
      match_colnames <- TRUE
    }else{
      match_colnames <- FALSE
    }
    
    if(match_colnames){
      ge_file_msg("")
      ge_file(dat)
    }else{
      errorMsg <- paste0("The expression set and profile annotation do not match.")
      ge_file_msg(errorMsg)
      ge_file(NULL)
      return(NULL)
    }
    
  }, error=function(err){
    ge_file_msg("Import failed. Please check your file again.")
    ge_file(NULL)
    return(NULL)
  }, warning=function(war){
    ge_file_msg("")
    ge_file(dat)
    return(NULL)
  })
  
})

##Output ge warning message####
output$add_ge_file_msg <- renderUI({
  
  req(ge_file_msg())
  
  p(class="fileInputMsg",  HTML(ge_file_msg()))
  
})

#import connectivity map - perturbagens class file#####
conn_pcl_file <- reactiveVal(NULL); conn_pcl_file_msg <- reactiveVal(NULL);

observeEvent(input$Add_Project_Add_Button, {
  
  req(input$add_conn_option %in% "Yes")
  
  inputfile <- input$add_conn_pcl_file;
  inputtype <- input$add_conn_pcl_file_type;
  pro_ann <- pro_file();
  
  if(is.null(inputfile)){
    conn_pcl_file_msg("Please choose a file to import.")
    conn_pcl_file(NULL)
    return(NULL)
  }
  
})

observeEvent(input$add_conn_pcl_file, {
  
  req(input$add_conn_option %in% "Yes")
  
  inputfile <- input$add_conn_pcl_file;
  inputtype <- input$add_conn_pcl_file_type;
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
      conn_pcl_file_msg("Incorrect file format.")
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
      errorMsg <- paste0("The connectivity map (perturbagens class) and profile annotation do not match.")
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
output$add_conn_pcl_file_msg <- renderUI({
  
  req(conn_pcl_file_msg())
  
  p(class="fileInputMsg",  HTML(conn_pcl_file_msg()))
  
})

#import connectivity map - perturbagens class file#####
conn_pert_file <- reactiveVal(NULL); conn_pert_file_msg <- reactiveVal(NULL);

observeEvent(input$Add_Project_Add_Button, {
  
  req(input$add_conn_option %in% "Yes")
  
  inputfile <- input$add_conn_pert_file;
  inputtype <- input$add_conn_pert_file_type;
  pro_ann <- pro_file();
  
  if(is.null(inputfile)){
    conn_pert_file_msg("Please choose a file to import.")
    conn_pert_file(NULL)
    return(NULL)
  }
  
})

observeEvent(input$add_conn_pert_file, {
  
  req(input$add_conn_option %in% "Yes")
  
  inputfile <- input$add_conn_pert_file;
  inputtype <- input$add_conn_pert_file_type;
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
      conn_pert_file_msg("Incorrect file format.")
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
      errorMsg <- paste0("The connectivity map (perturbagens) and profile annotation do not match.")
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
output$add_conn_pert_file_msg <- renderUI({
  
  req(conn_pert_file_msg())
  
  p(class="fileInputMsg",  HTML(conn_pert_file_msg()))
  
})

#import hallmark file#####
hallmark_file <- reactiveVal(NULL); hallmark_file_msg <- reactiveVal(NULL);

observeEvent(input$Add_Project_Add_Button, {
  
  req(input$add_cur_enrichment_option %in% "No")
  
  inputfile <- input$add_hallmark_file;

  if(is.null(inputfile)){
    hallmark_file_msg("Please choose a file to import.")
    hallmark_file(NULL)
    return(NULL)
  }
  
})

observeEvent(input$add_hallmark_file, {
  
  req(input$add_cur_enrichment_option %in% "No")
  
  inputfile <- input$add_hallmark_file;

  if(is.null(inputfile)){
    hallmark_file(NULL)
    return(NULL)
  }
  
  tryCatch({
  
    extension <- grep(toupper(".gmt"), toupper(substr(inputfile$datapath, nchar(inputfile$datapath)-4, nchar(inputfile$datapath))), fixed = TRUE)
    
    if(length(extension) == 0){
      hallmark_file_msg("Incorrect file format.")
      hallmark_file(NULL)
      return(NULL)
    }else{
      hallmark_file_msg("")
      data <- getGmt(inputfile$datapath)
      hallmark_file(
        list(
          path=inputfile$datapath,
          data=data
        )
      )
    }
    
  }, error=function(err){
    hallmark_file_msg("Import failed. Please check your file again.")
    hallmark_file(NULL)
    return(NULL)
  }, warning=function(war){
    hallmark_file_msg("Import failed. Please check your file again.")
    hallmark_file(NULL)
    return(NULL)
  })
  
})

##Output hallmark warning message###
output$add_hallmark_file_msg <- renderUI({
  
  req(hallmark_file_msg())
  
  p(class="fileInputMsg",  HTML(hallmark_file_msg()))
  
})

#import c2 file#####
c2_file <- reactiveVal(NULL); c2_file_msg <- reactiveVal(NULL);

observeEvent(input$Add_Project_Add_Button, {
  
  req(input$add_cur_enrichment_option %in% "No")
  
  inputfile <- input$add_c2_file;

  if(is.null(inputfile)){
    c2_file_msg("Please choose a file to import.")
    c2_file(NULL)
    return(NULL)
  }
  
})

observeEvent(input$add_c2_file, {
  
  req(input$add_cur_enrichment_option %in% "No")
  
  inputfile <- input$add_c2_file;

  if(is.null(inputfile)){
    c2_file(NULL)
    return(NULL)
  }
  
  tryCatch({
    
    extension <- grep(toupper(".gmt"), toupper(substr(inputfile$datapath, nchar(inputfile$datapath)-4, nchar(inputfile$datapath))), fixed = TRUE)
    
    if(length(extension) == 0){
      c2_file_msg("Incorrect file format.")
      c2_file(NULL)
      return(NULL)
    }else{
      c2_file_msg("")
      data <- getGmt(inputfile$datapath)
      c2_file(
        list(
          path=inputfile$datapath,
          data=data
        )
      )
    }
    
  }, error=function(err){
    c2_file_msg("Import failed. Please check your file again.")
    c2_file(NULL)
    return(NULL)
  }, warning=function(war){
    c2_file_msg("Import failed. Please check your file again.")
    c2_file(NULL)
    return(NULL)
  })
  
})

##Output c2 warning message###
output$add_c2_file_msg <- renderUI({
  
  req(c2_file_msg())
  
  p(class="fileInputMsg",  HTML(c2_file_msg()))
  
})

#import nursa file#####
nursa_file <- reactiveVal(NULL); nursa_file_msg <- reactiveVal(NULL);

observeEvent(input$Add_Project_Add_Button, {
  
  req(input$add_cur_enrichment_option %in% "No")
  
  inputfile <- input$add_nursa_file;
  
  if(is.null(inputfile)){
    nursa_file_msg("Please choose a file to import.")
    nursa_file(NULL)
    return(NULL)
  }
  
})

observeEvent(input$add_nursa_file, {
  
  req(input$add_cur_enrichment_option %in% "No")
  
  inputfile <- input$add_nursa_file;

  if(is.null(inputfile)){
    nursa_file(NULL)
    return(NULL)
  }
  
  tryCatch({
    
    extension <- grep(toupper(".gmt"), toupper(substr(inputfile$datapath, nchar(inputfile$datapath)-4, nchar(inputfile$datapath))), fixed = TRUE)
    
    if(length(extension) == 0){
      nursa_file_msg("Incorrect file format.")
      nursa_file(NULL)
      return(NULL)
    }else{
      nursa_file_msg("")
      data <- getGmt(inputfile$datapath)
      nursa_file(
        list(
          path=inputfile$datapath,
          data=data
        )
      )
    }
    
  }, error=function(err){
    nursa_file_msg("Import failed. Please check your file again.")
    nursa_file(NULL)
    return(NULL)
  }, warning=function(war){
    nursa_file_msg("Import failed. Please check your file again.")
    nursa_file(NULL)
    return(NULL)
  })
  
})

##Output nursa warning message###
output$add_nursa_file_msg <- renderUI({
  
  req(nursa_file_msg())
  
  p(class="fileInputMsg", HTML(nursa_file_msg()))
  
})
