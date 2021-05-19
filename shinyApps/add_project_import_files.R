
#import introduction file
observeEvent(input$Add_Project_Add_Button, {
  
  inputfile <- input$add_intro_file;
  
  if(is.null(inputfile)){
    intro_file_msg("Please choose a file to import.")
    intro_file(NULL)
    return(NULL)
  }
  
})

#import introduction file
observeEvent(input$add_intro_file, {
  
  inputfile <- input$add_intro_file;
  
  if(is.null(inputfile)){
    intro_file(NULL)
    return(NULL)
  }
  
  tryCatch({
    
    extension <- grep(toupper(".rmd"), toupper(substr(inputfile$datapath, nchar(inputfile$datapath)-4, nchar(inputfile$datapath))), fixed = TRUE)
    
    if(length(extension) == 0){
      intro_file_msg("Incorrect file format. Please check your file again.")
      intro_file(NULL)
      return(NULL)
    }else{
      intro_file_msg("")
      intro_file(inputfile$datapath)
    }
    
  }, error=function(err){
    intro_file_msg("Import failed. Please check out our example template.")
    intro_file(NULL)
    print(err)
  }, warning=function(war){
    print(war)
  })
  
})

##Output introduction warning message
output$add_intro_file_msg <- renderUI({
  
  req(intro_file_msg())
  
  p(class="fileInputMsg",  HTML(intro_file_msg()))
  
})

#import profile annotation file#####
observeEvent(input$Add_Project_Add_Button, {
  
  inputfile <- input$add_pro_file;
  inputtype <- input$add_pro_file_type; 
  
  if(is.null(inputfile)){
    pro_file_msg("Please choose a file to import.")
    chem_file(NULL)
    pro_file(NULL)
    return(NULL)
  }
  
})

#import profile annotation file#####
observeEvent({
  input$add_pro_file
  input$add_pro_file_type
}, {
  
  inputfile <- input$add_pro_file;
  inputtype <- input$add_pro_file_type; 
  
  if(is.null(inputfile)){
    updateSelectInput(session, inputId="add_variable_compound", choices=c("Import a profile annotation" = ""))
    updateSelectInput(session, inputId="add_variable_exposure", choices=c("Import a profile annotation" = ""))
    updateSelectInput(session, inputId="add_variable_exposure_phenotype", choices=c("Import a profile annotation" = ""))
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
      updateSelectInput(session, inputId="add_variable_compound", choices=c("Import a profile annotation" = ""))
      updateSelectInput(session, inputId="add_variable_exposure", choices=c("Import a profile annotation" = ""))
      updateSelectInput(session, inputId="add_variable_exposure_phenotype", choices=c("Import a profile annotation" = ""))
      pro_file_msg("Incorrect file format. Please check your file again.")
      chem_file(NULL)
      pro_file(NULL)
      return(NULL)
    }
    
    variables <- c("Sig_Id", "Chemical_Id", "Chemical_Name", "BUID", "CAS")
    
    if(all(variables %in% colnames(dat))){
      exposure_variable <- unique(colnames(dat)[which(!colnames(dat) %in% c("Sig_Id", "Chemical_Id", "Chemical_Name", "BUID", "CAS", "TAS"))])
      updateSelectInput(session, inputId="add_variable_compound", choices=c("Please select an option below" = "", "Chemical_Id"))
      updateSelectInput(session, inputId="add_variable_exposure", choices=c("Please select an option below" = "", exposure_variable))
      updateSelectInput(session, inputId="add_variable_exposure_phenotype", choices = c("Please select an option below" = "", exposure_variable))
      pro_file_msg("")
      chem_file(distinct(dat, Chemical_Id, Chemical_Name, BUID, CAS, .keep_all=TRUE) %>% select(-Sig_Id))
      pro_file(dat)      
    }else{
      errorMsg <- paste0("One or more of the required variables: <em>", paste0(variables, collapse = ", "), "</em> are missing from the dataset.")
      updateSelectInput(session, inputId="add_variable_compound", choices=c("Import a profile annotation" = ""))
      updateSelectInput(session, inputId="add_variable_exposure", choices=c("Import a profile annotation" = ""))
      updateSelectInput(session, inputId="add_variable_exposure_phenotype", choices=c("Import a profile annotation" = ""))
      pro_file_msg(errorMsg)
      chem_file(NULL)
      pro_file(NULL)
      return(NULL)
    }
    
  }, error=function(err){
    pro_file_msg("Import failed. Please check out our example template.")
    chem_file(NULL)
    pro_file(NULL)
    print(err)
  }, warning=function(war){
    print(war)
  })
  
})

##Output profile warning message###
output$add_pro_file_msg <- renderUI({
  
  req(pro_file_msg())
  
  p(class="fileInputMsg", HTML(pro_file_msg()))
  
})

##Create Add TAS and ModZ option####
output$Add_Tas_Modz <- renderUI({
  
  req(cohorts())
  
  div(
    h4("Calculations:", style="padding-bottom: 10px;"),
    checkboxInput(inputId = "Add_TAS", label = "TAS", value=TRUE), 
    checkboxInput(inputId = "Add_Modzscores", label = "Mod-Zscores", value=TRUE), 
  )
  
})

#import ge expression file#####
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

observeEvent({
  input$add_ge_file
  input$add_ge_file_type
  input$add_pro_file
  input$add_pro_file_type
}, {
  
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
      ge_file_msg("Incorrect file format. Please check your file again.")
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

    if(match_colnames == FALSE){
      errorMsg <- paste0("The expression set and profile annotation do not match. Please check your files again.")
      ge_file_msg(errorMsg)
      ge_file(NULL)
      return(NULL)
    }

    if(inputtype %in% ".csv"){
      check_numeric <- all(TRUE %in% sapply(dat, is.numeric))
    }else{
      check_numeric <- all(TRUE %in% sapply(dat %>% exprs(), is.numeric))
    }
    
    if(check_numeric == FALSE){
      errorMsg <- paste0("The expression set MUST all be numeric. Please check your files again.")
      ge_file_msg(errorMsg)
      ge_file(NULL)
      return(NULL)
    }else{
      ge_file_msg("")
      ge_file(dat)
    }
    
  }, error=function(err){
    ge_file_msg("Import failed. Please check out our example template.")
    ge_file(NULL)
    print(err)
  }, warning=function(war){
    print(war)
  })
  
})

##Output ge warning message####
output$add_ge_file_msg <- renderUI({
  
  req(ge_file_msg())
  
  p(class="fileInputMsg",  HTML(ge_file_msg()))
  
})

##Create cohorts option####
observeEvent(input$add_variable_exposure, {
  
  req(pro_file(), chem_file(), ge_file(), input$add_variable_exposure)
  
  pro_ann <- pro_file(); chem_ann <- chem_file(); gene_expression <- ge_file(); 
  var <- ifelse(all(colnames(gene_expression) %in% pro_ann$Sig_Id), "Sig_Id", "Chemical_Id");
  exposure <- input$add_variable_exposure;
  
  # Getting the number of replicates for each chemical
  if(var=="Sig_Id"){
    pro_ann$unique_ID_by_chem <- lapply(1:nrow(pro_ann), function(r){ paste0(unlist(pro_ann[r,exposure]), collapse="_") }) %>% unlist()
    chem_replicate <- pro_ann %>% group_by(Chemical_Id, unique_ID_by_chem) %>% summarise(Frequency=n()) %>% ungroup()
  }else{
    chem_ann$unique_ID_by_chem <- lapply(1:nrow(chem_ann), function(r){ paste0(unlist(chem_ann[r,exposure]), collapse="_") }) %>% unlist()
    chem_replicate <- chem_ann %>% group_by(Chemical_Id, unique_ID_by_chem) %>% summarise(Frequency=n()) %>% ungroup()
  }  
  
  if(any(chem_replicate$Frequency > 1)){
    cohorts("Chemical_Id")
  }else{
    cohorts(NULL)
  }
  
})

## List of exposure phenotype statistical tests####
output$add_metavar_variable_test <- DT::renderDataTable({
  
  req(pro_file(), input$add_variable_exposure_phenotype)
  
  pro_ann=pro_file(); varlist=input$add_variable_exposure_phenotype;
  
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

#import connectivity map - perturbagens class file#####
observeEvent(input$Add_Project_Add_Button, {
  
  req(input$add_conn_option %in% "Yes")
  
  inputfile <- input$add_conn_pcl_file;
  inputtype <- input$add_conn_pcl_file_type;

  if(is.null(inputfile)){
    conn_pcl_file_msg("Please choose a file to import.")
    conn_pcl_file(NULL)
    return(NULL)
  }
  
})

observeEvent({
  input$add_conn_pcl_file
  input$add_conn_pcl_file_type
  input$add_pro_file
  input$add_pro_file_type
}, {
  
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
      conn_pcl_file_msg("Incorrect file format. Please check your file again.")
      conn_pcl_file(NULL)
      return(NULL)
    }
    
    if(is.null(pro_ann)){
      conn_pcl_file_msg("Need a profile annotation file to match.")
      conn_pcl_file(NULL)
      return(NULL)
    }
    
    if(all(colnames(dat) %in% pro_ann$Sig_Id) | all(colnames(dat)%in% pro_ann$Chemical_Id)){
      match_colnames <- TRUE
    }else{
      match_colnames <- FALSE
    }
    
    if(match_colnames == FALSE){
      errorMsg <- paste0("The connectivity map (perturbagens class) and profile annotation do not match. Please check your files again.")
      conn_pcl_file_msg(errorMsg)
      conn_pcl_file(NULL)
      return(NULL)
    }
    
    if(inputtype %in% ".csv"){
      check_numeric <- all(TRUE %in% sapply(dat, is.numeric))
    }else{
      check_numeric <- all(TRUE %in% sapply(dat %>% exprs(), is.numeric))
    }
    
    if(check_numeric == FALSE){
      errorMsg <- paste0("The expression set MUST all be numeric. Please check your files again.")
      conn_pcl_file_msg(errorMsg)
      conn_pcl_file(NULL)
      return(NULL)
    }else{
      conn_pcl_file_msg("")
      conn_pcl_file(dat)
    }
    
  }, error=function(err){
    conn_pcl_file_msg("Import failed. Please check out our example template.")
    conn_pcl_file(NULL)
    print(err)
  }, warning=function(war){
    print(war)
  })
  
}, ignoreNULL = FALSE)

##Output pcl warning message###
output$add_conn_pcl_file_msg <- renderUI({
  
  req(conn_pcl_file_msg())
  
  p(class="fileInputMsg",  HTML(conn_pcl_file_msg()))
  
})

#import connectivity map - perturbagens class file#####
observeEvent(input$Add_Project_Add_Button, {
  
  req(input$add_conn_option %in% "Yes")
  
  inputfile <- input$add_conn_pert_file;
  inputtype <- input$add_conn_pert_file_type;

  if(is.null(inputfile)){
    conn_pert_file_msg("Please choose a file to import.")
    conn_pert_file(NULL)
    return(NULL)
  }
  
})

observeEvent({
  input$add_conn_pert_file
  input$add_conn_pert_file_type
  input$add_pro_file
  input$add_pro_file_type
}, {
  
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
      conn_pert_file_msg("Incorrect file format. Please check your file again.")
      conn_pert_file(NULL)
      return(NULL)
    }
    
    if(is.null(pro_ann)){
      conn_pert_file_msg("Need a profile annotation file to match.")
      conn_pert_file(NULL)
      return(NULL)
    }
    
    if(all(colnames(dat) %in% pro_ann$Sig_Id) | all(colnames(dat)%in% pro_ann$Chemical_Id)){
      match_colnames <- TRUE
    }else{
      match_colnames <- FALSE
    }
    
    if(match_colnames == FALSE){
      errorMsg <- paste0("The connectivity map (perturbagens) and profile annotation do not match. Please check your files again.")
      conn_pert_file_msg(errorMsg)
      conn_pert_file(NULL)
      return(NULL)
    }
    
    if(inputtype %in% ".csv"){
      check_numeric <- all(TRUE %in% sapply(dat, is.numeric))
    }else{
      check_numeric <- all(TRUE %in% sapply(dat %>% exprs(), is.numeric))
    }
    
    if(check_numeric == FALSE){
      errorMsg <- paste0("The expression set MUST all be numeric. Please check your files again.")
      conn_pert_file_msg(errorMsg)
      conn_pert_file(NULL)
      return(NULL)
    }else{
      conn_pert_file_msg("")
      conn_pert_file(dat)
    }
    
  }, error=function(err){
    conn_pert_file_msg("Import failed. Please check out our example template.")
    conn_pert_file(NULL)
    print(err)
  }, warning=function(war){
    print(war)
  })
  
}, ignoreNULL = FALSE)

##Output pert warning message###
output$add_conn_pert_file_msg <- renderUI({
  
  req(conn_pert_file_msg())
  
  p(class="fileInputMsg",  HTML(conn_pert_file_msg()))
  
})

## enrichment messages
gs_name_message1 <- reactiveVal("")
gs_name_message2 <- reactiveVal("")
gs_name_message3 <- reactiveVal("")

gs_link_message1 <- reactiveVal("")
gs_link_message2 <- reactiveVal("")
gs_link_message3 <- reactiveVal("")

gs_file_message1 <- reactiveVal("")
gs_file_message2 <- reactiveVal("")
gs_file_message3 <- reactiveVal("")

gs_name_1 <- reactiveVal(NULL)
gs_name_2 <- reactiveVal(NULL)
gs_name_3 <- reactiveVal(NULL)

gs_link_1 <- reactiveVal(NULL)
gs_link_2 <- reactiveVal(NULL)
gs_link_3 <- reactiveVal(NULL)

gs_collection_file_1 <- reactiveVal(NULL)
gs_collection_file_2 <- reactiveVal(NULL)
gs_collection_file_3 <- reactiveVal(NULL)

## Observe the gs collection 1####
observeEvent(input$Add_Project_Add_Button, {
  
  req(input$add_cur_enrichment_option %in% "No", input$Add_Num_New_Enrichment_GS >= 1)
  
  ##Check the collection names
  gs_collection <- input$Add_New_Enrichment_GS_1; gs_message <- c(); 
  
  if(is.null(gs_collection)){
    
    gs_message <- c(gs_message, "Please enter a name for the gs collection")
    gs_name_1(NULL)
    
  }else{
    
    if(gs_collection == ""){
      
      gs_message <- c(gs_message, "Please enter a name for the gs collection")
      gs_name_1(NULL)
      
    }else{
      
      ##Check to make sure no white space and starting number###
      checkspace <- length(grep(" ", gs_collection, fixed=TRUE)) > 0
      checknumber <- length(grep("[0-9]", substr(gs_collection,1,1), perl=TRUE)) > 0
      
      if(checkspace){
        gs_message <- c(gs_message, "***Name of collection cannot contain spaces.")
        gs_name_1(NULL)
      }
      
      if(checknumber){
        gs_message <- c(gs_message, "***Name of collection cannot start with a number.")
        gs_name_1(NULL)
      }
      
      if(checkspace==FALSE & checknumber==FALSE){
        gs_message <- c(gs_message, "")
      }
      
      gs_name_1(gs_collection)
      
    }
  }
  
  gs_name_message1(paste0(gs_message, collapse="<br>"))
  
  ##Check the collection links 
  gs_collection_link <- input$Add_New_Enrichment_Link_1
  
  if(is.null(gs_collection_link)){
    gs_link_message1("Please enter a link to where the gs collection was obtained")
    gs_link_1(NULL)
  }else{
    if(gs_collection_link == ""){
      gs_link_message1("Please enter a link to where the gs collection was obtained")
      gs_link_1(NULL)
    }else{
      gs_link_message1("");
      gs_link_1(gs_collection_link)
    }
  }
  
  ##Check the import files 
  inputfile <- input$add_gs_collection_file_1
  
  if(is.null(inputfile)){
    
    gs_file_message1("Please choose a file to import.")
    gs_collection_file_1(NULL)
    
  }else{
    
    tryCatch({
      
      extension <- grep(toupper(".gmt"), toupper(substr(inputfile$datapath, nchar(inputfile$datapath)-4, nchar(inputfile$datapath))), fixed = TRUE)
      
      if(length(extension) == 0){
        gs_collection_file_1(NULL)
        gs_file_message1("Incorrect file format. Please check your file again.")
      }else{
        data <- getGmt(inputfile$datapath)
        gs_collection_file_1(
          list(
            path=inputfile$datapath,
            data=data
          )
        )
        gs_file_message1("")
      }
      
    }, error=function(err){
      gs_collection_file_1(NULL)
      gs_file_message1("Import failed. Please check out our example template.")
      print(err)
    }, warning=function(war){
      print(war)
    })
    
  }
  
})

##Output warning message for collection 1###
output$add_enrichment_gs_msg_1 <- renderUI({
  
  req(gs_name_message1())
  
  p(class="fileInputMsg",  HTML(gs_name_message1()))
  
})

output$add_enrichment_link_msg_1 <- renderUI({
  
  req(gs_link_message1())
  
  p(class="fileInputMsg",  HTML(gs_link_message1()))
  
})

output$add_gs_collection_file_msg_1 <- renderUI({
  
  req(gs_file_message1())
  
  p(class="fileInputMsg",  HTML(gs_file_message1()))
  
})

## Observe the gs collection 2####
observeEvent(input$Add_Project_Add_Button, {
  
  req(input$add_cur_enrichment_option %in% "No", input$Add_Num_New_Enrichment_GS >= 2)
  
  ##Check the collection names
  gs_collection <- input$Add_New_Enrichment_GS_2; gs_message <- c(); 
  
  if(is.null(gs_collection)){
    
    gs_message <- c(gs_message, "Please enter a name for the gs collection")
    gs_name_2(NULL)
    
  }else{
    
    if(gs_collection == ""){
      
      gs_message <- c(gs_message, "Please enter a name for the gs collection")
      gs_name_2(NULL)
      
    }else{
      
      ##Check to make sure no white space and starting number###
      checkspace <- length(grep(" ", gs_collection, fixed=TRUE)) > 0
      checknumber <- length(grep("[0-9]", substr(gs_collection,1,1), perl=TRUE)) > 0
      
      if(checkspace){
        gs_message <- c(gs_message, "***Name of collection cannot contain spaces.")
        gs_name_2(NULL)
      }
      
      if(checknumber){
        gs_message <- c(gs_message, "***Name of collection cannot start with a number.")
        gs_name_2(NULL)
      }
      
      if(checkspace==FALSE & checknumber==FALSE){
        gs_message <- c(gs_message, "")
        gs_name_2(gs_collection)
      }
    }
  }
  
  gs_name_message2(paste0(gs_message, collapse="<br>"))
  
  ##Check the collection links 
  gs_collection_link <- input$Add_New_Enrichment_Link_2
  
  if(is.null(gs_collection_link)){
    gs_link_message2("Please enter a link to where the gs collection was obtained")
    gs_link_2(NULL)
  }else{
    if(gs_collection_link == ""){
      gs_link_message2("Please enter a link to where the gs collection was obtained")
      gs_link_2(NULL)
    }else{
      gs_link_message2("");
      gs_link_2(gs_collection_link)
    }
  }
  
  ##Check the import files 
  inputfile <- input$add_gs_collection_file_2
  
  if(is.null(inputfile)){
    
    gs_file_message2("Please choose a file to import.")
    gs_collection_file_2(NULL)
    
  }else{
    
    tryCatch({
      
      extension <- grep(toupper(".gmt"), toupper(substr(inputfile$datapath, nchar(inputfile$datapath)-4, nchar(inputfile$datapath))), fixed = TRUE)
      
      if(length(extension) == 0){
        gs_collection_file_2(NULL)
        gs_file_message2("Incorrect file format. Please check your file again.")
      }else{
        data <- getGmt(inputfile$datapath)
        gs_collection_file_2(
          list(
            path=inputfile$datapath,
            data=data
          )
        )
        gs_file_message2("")
      }
      
    }, error=function(err){
      gs_collection_file_2(NULL)
      gs_file_message2("Import failed. Please check out our example template.")
      print(err)
    }, warning=function(war){
      print(war)
    })
    
  }
  
})

##Output warning message for collection 2###
output$add_enrichment_gs_msg_2 <- renderUI({
  
  req(gs_name_message2())
  
  p(class="fileInputMsg",  HTML(gs_name_message2()))
  
})

output$add_enrichment_link_msg_2 <- renderUI({
  
  req(gs_link_message2())
  
  p(class="fileInputMsg",  HTML(gs_link_message2()))
  
})

output$add_gs_collection_file_msg_2 <- renderUI({
  
  req(gs_file_message2())
  
  p(class="fileInputMsg",  HTML(gs_file_message2()))
  
})

## Observe the gs collection 3####
observeEvent(input$Add_Project_Add_Button, {
  
  req(input$add_cur_enrichment_option %in% "No", input$Add_Num_New_Enrichment_GS >= 3)
  
  ##Check the collection names
  gs_collection <- input$Add_New_Enrichment_GS_3; gs_message <- c(); 
  
  if(is.null(gs_collection)){
    
    gs_message <- c(gs_message, "Please enter a name for the gs collection")
    gs_name_3(NULL)
    
  }else{
    
    if(gs_collection == ""){
      
      gs_message <- c(gs_message, "Please enter a name for the gs collection")
      gs_name_3(NULL)
      
    }else{
      
      ##Check to make sure no white space and starting number###
      checkspace <- length(grep(" ", gs_collection, fixed=TRUE)) > 0
      checknumber <- length(grep("[0-9]", substr(gs_collection,1,1), perl=TRUE)) > 0
      
      if(checkspace){
        gs_message <- c(gs_message, "***Name of collection cannot contain spaces.")
        gs_name_3(NULL)
      }
      
      if(checknumber){
        gs_message <- c(gs_message, "***Name of collection cannot start with a number.")
        gs_name_3(NULL)
      }
      
      if(checkspace==FALSE & checknumber==FALSE){
        gs_message <- c(gs_message, "")
        gs_name_3(gs_collection)
      }
      
    }
  }
  
  gs_name_message3(paste0(gs_message, collapse="<br>"))
  
  ##Check the collection links 
  gs_collection_link <- input$Add_New_Enrichment_Link_3
  
  if(is.null(gs_collection_link)){
    gs_link_message3("Please enter a link to where the gs collection was obtained")
    gs_link_3(NULL)
  }else{
    if(gs_collection_link == ""){
      gs_link_message3("Please enter a link to where the gs collection was obtained")
      gs_link_3(NULL)
    }else{
      gs_link_message3("");
      gs_link_3(gs_collection_link)
    }
  }
  
  ##Check the import files 
  inputfile <- input$add_gs_collection_file_3
  
  if(is.null(inputfile)){
    
    gs_file_message3("Please choose a file to import.")
    gs_collection_file_3(NULL)
    
  }else{
    
    tryCatch({
      
      extension <- grep(toupper(".gmt"), toupper(substr(inputfile$datapath, nchar(inputfile$datapath)-4, nchar(inputfile$datapath))), fixed = TRUE)
      
      if(length(extension) == 0){
        gs_collection_file_3(NULL)
        gs_file_message3("Incorrect file format. Please check your file again.")
      }else{
        data <- getGmt(inputfile$datapath)
        gs_collection_file_3(
          list(
            path=inputfile$datapath,
            data=data
          )
        )
        gs_file_message3("")
      }
      
    }, error=function(err){
      gs_collection_file_3(NULL)
      gs_file_message3("Import failed. Please check out our example template.")
      print(err)
    }, warning=function(war){
      print(war)
    })
    
  }
  
})

##Output warning message for collection 3###
output$add_enrichment_gs_msg_3 <- renderUI({
  
  req(gs_name_message3())
  
  p(class="fileInputMsg",  HTML(gs_name_message3()))
  
})

output$add_enrichment_link_msg_3 <- renderUI({
  
  req(gs_link_message3())
  
  p(class="fileInputMsg",  HTML(gs_link_message3()))
  
})

output$add_gs_collection_file_msg_3 <- renderUI({
  
  req(gs_file_message3())
  
  p(class="fileInputMsg",  HTML(gs_file_message3()))
  
})
