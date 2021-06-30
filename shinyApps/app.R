##Xposome app with GeneHive###
print("Xposome app with GeneHive")

##Shiny Packages####
library(BiocManager)
library(data.table) #
library(ggdendro) #
library(jsonlite)
library(httr)
library(password) #generate random password
library(sodium) #password encryption
library(digest) #password encryption
library(sendmailR) #send email message
library(tidyverse)
library(magrittr)
library(ipc)

##Shiny Packages####
library(GeneHive)
library(K2Taxonomer)
library(visNetwork) #
library(Biobase) #
library(GSVA) #
library(GSEABase) #read in gmt files
library(limma) #
library(dendextend) #

##Shiny Packages####
library(shiny) #
library(shinyjs)
library(shinyBS) #
library(shinycssloaders) #
library(DT) #
library(ggplot2)
library(plotly) #
library(heatmaply)
library(RColorBrewer) #
library(promises)
library(future)
plan(multiprocess)

##Shiny options####
options(shiny.maxRequestSize=1000*1024^2)

##Define ui logic####
ui <- bootstrapPage(
  
  title=paste0("The Xposome Project"),
  
  tagList(
    tags$head(
      ##<!-- favicon -->####
      tags$link(href="IMAGES/github_logo.png", rel="shortcut icon"),

      ##<!-- overall style -->####
      tags$link(href="CSS/style.css", rel="stylesheet"),
      tags$link(href="CSS/main_style.css", rel="stylesheet"),
      tags$link(href="CSS/LogInStyle.css", rel="stylesheet"),
      tags$link(href="CSS/ModeratorStyle.css", rel="stylesheet"),
      
      ##<!-- load all styles for font-awesome icons -->####
      tags$link(href="CSS/all.css", rel="stylesheet"),
        
      ##<!-- javascript for bootstrap tooltip -->####
      tags$script(src="JS/popper.min.js", type="text/javascript"),
      
      ##<!-- javascript for xposome application -->####
      tags$script(src="JS/Javascript.js", type="text/javascript"),
      
      ##<!-- javascript for shinyjs package -->####
      shinyjs::useShinyjs()
    )
  ),
  
  ## Create an active clock  to prevent app from greying out ####
  div(style = "position: absolute; top: -100px;",
      textOutput("clock")
  ),
  
  ###the header#####
  fluidRow(
    column(
      width=12, class="header-container", id="header-ui",
      
      HTML(
      '
      <nav class="navbar navbar-default navbar-fixed-top">
        <div class="container-fluid">
          <div class="navbar-header">
            <span class="navbar-brand">
              <button class="logo-link" onclick="curlinkFun(\'home\')">
                <span>
                  <img src="IMAGES/github_logo.png" width="40" height="30"/>
                  <span>Xposome</span>
                </span>
              </button>
            </span>
          </div>
          
          <div class="navbar-link">
            <ul class="main_navbar">
              <li onclick="curlinkFun(\'home\')" id="home">Home</li>
              <li onclick="curlinkFun(\'overview\')" id="overview">Overview</li>
              <li onclick="curlinkFun(\'portal\')" id="portal">Portal</li>
              <a href="mailto:montilab@bu.edu" id="contact"><i class="far fa-envelope"></i>Contact</a>
              <button onclick="curlinkFun(\'sign_in\')" id="sign_in" class="btn btn-warning">Sign In</button>
            </ul>
          </div>
          
          <p style="font-weight: 500; width: 100%; color:red; text-align: center;">You are temporarily viewing the local version of the Xposome Portal as it is currently offline and under maintenance.</p>

        </div>
      </nav>
      '
      )
    ),
    
    ###The main body#####
    column(
      width=12, class="body-container", 
      div(
        id="error-ui",
        uiOutput("page_404") %>% withSpinner(id="loading-page", type=4, color="#0dc5c1")
      ),
      div(
        id="portal-ui",
        br(),
        uiOutput("home_page") %>% withSpinner(id="loading-page", type=4, color="#0dc5c1"),
        uiOutput("portal_page") %>% withSpinner(id="loading-page", type=4, color="#0dc5c1"),
        uiOutput("about_page") %>% withSpinner(id="loading-page", type=4, color="#0dc5c1"),
        uiOutput("sign_in_page") %>% withSpinner(id="loading-page", type=4, color="#0dc5c1")
      )
    ),
    
    column(
      width=12, class="footer-container", id="footer-ui",
      includeHTML("footer.html")
    )
  )
)

##Define Server Login####
server <- function(input, output, session) {
  
  cat("Session started.\n")

  ## Print this when a session starts
  onSessionEnded(function() { cat("Session ended.\n\n"); })  # this prints when a session ends

  ## Create a queue object----
  queue <- shinyQueue();

  ## Update the clock every 5s to prevent app from being inactive and grey out####
  output$clock <- renderText({
    invalidateLater(5000)
    Sys.time()
  })
  
  ## Add reactive timer to prevent app from being inactive and grey out####
  autoInvalidate <- reactiveTimer(10000)
  
  observe({
    autoInvalidate()
    cat(".")
  })

  ## Read in the project list ####
  projectlist <- tryCatch({

    read_csv(paste0("data/Project_List.csv")) %>% dplyr::arrange(Portal)

  }, error=function(err){

    data <- data.frame(
      Project=NA,
      Cell_Line=NA,
      Portal=NA,
      Enrichment_Version=NA,
      Landmark_Gene=NA,
      TAS_Modzscores=NA,
      Exposure_Levels=NA,
      Exposure_Phenotype=NA,
      Exposure_Phenotype_Test=NA,
      Connectivity_Test=NA,
      Feature_Filtering=NA,
      Description=NA,
      stringsAsFactors=TRUE
    )

    write.csv(data, "data/Project_List.csv", row.names=FALSE, na = "")

    return(data)

  })

  ## Read in the project list ####
  loginlist <- tryCatch({

    read_csv(paste0("data/User_Login_List.csv"))

  }, error=function(err){

    data <- data.frame(
      Firstname="Xposome",
      Lastname="Xposome",
      Username="Xposome",
      Password=sodium::password_store(as.character("Xposome")),
      Status="Moderator",
      Email="montilab@bu.edu",
      stringsAsFactors=TRUE
    )

    write.csv(data, "data/User_Login_List.csv", row.names=FALSE)

    return(data)

  })

  ##Create a list of wanted folders and files####
  wanted_files <- c(projectlist$Portal, "Connectivity Map", "Enrichment Gene Set", "Landmark", "Project_List.csv", "Template", "User_Login_List.csv", "Zebra Fish") 
  
  ##Get all files in the data folder###
  data_files <- list.files("data/")

  ##Remove unwanted folders/files####
  if(any(!data_files %in% wanted_files)){
    for(f in seq_along(data_files)){
      #f=1
      if(!data_files[f] %in% wanted_files){
        unlink(paste0("data/", data_files[f]), recursive=TRUE, force=TRUE)
      }
    }
  }

  ##Get all files in the json folder###
  json_files <- list.files("www/JSON")

  ##Remove unwanted folders/files####
  if(any(!json_files%in% projectlist$Portal)){
    for(f in seq_along(json_files)){
      #f=1
      if(!json_files[f] %in% projectlist$Portal){
        unlink(paste0("www/JSON/", json_files[f]), recursive=TRUE, force=TRUE)
      }
    }
  }

  ##Create a list of wanted folders and files####
  wanted_rmds <- c(paste0("introduction_", projectlist$Portal, ".Rmd"), "about_page.Rmd", "contact_page.Rmd")

  ##Get all files in the rmd folder###
  rmd_files <- list.files("www/RMD")

  ##Remove unwanted folders/files####
  if(any(!rmd_files %in% wanted_rmds)){
    for(f in seq_along(rmd_files)){
      #f=1
      if(!rmd_files[f] %in% wanted_rmds){
        unlink(paste0("www/RMD/", rmd_files[f]), recursive=TRUE, force=TRUE)
      }
    }
  }
  
  # UI object files ####
  source("ui_input.R", local=TRUE)
  source("login.R", local=TRUE)
  
  # Preproccessing data functions #####
  source("carcinogenome_startup.R", local=TRUE)
  source("taxonomer_startup.R", local=TRUE)
  
  # Morpheus heatmap ####
  source("morpheus_heatmap.R", local=TRUE)
  
  # TAS, Modzscores calculation ####
  source("tas_modzscores_calculation.R", local=TRUE)
  
  ## create reactive values for portal options ####
  dsmap <- reactiveVal(); 
  helptext_geneset <- reactiveVal(); 
  landmark <- reactiveVal();
  gs_collection <- reactiveVal();
  gs_collection_link <- reactiveVal();
  exposure_phenotype_test <- reactiveVal();
  exposure_phenotype <- reactiveVal();
  exposure_phenotype <- reactiveVal();
  
  ###load server code for page specified by the URL link
  url_search = isolate({ session$clientData$url_search })
  #print(url_search)
  
  if(nchar(url_search)==0 | length(url_search)==0) {
    
    tab_name <- "home"; fname <- "ADIPO"; subtab <- "annotation"; chemical_id <- "none"; chemical_tab <- "gene_expression";
    
  }else{
    
    portal_tab_name = strsplit(as.character(url_search), "&", fixed=T) %>% lapply(., function(x){ gsub("\\?page=", "", grep("\\?page=", x, value=T)) }) %>% lapply(., function(x){ x[!x==""] }) %>% unlist() %>% gsub("[[:space:]]", "_", .)
    portal_subtab = strsplit(as.character(url_search), "&", fixed=T) %>% lapply(., function(x){ gsub("tab=", "", grep("tab=", x, value=T)) }) %>% lapply(., function(x){ x[!x==""] }) %>% unlist() %>% gsub("[[:space:]]", "_", .)
    portal_chemical_id = strsplit(as.character(url_search), "&", fixed=T) %>% lapply(., function(x){ gsub("chemical_id=", "", grep("chemical_id=", x, value=T)) }) %>% lapply(., function(x){ x[!x==""] }) %>% unlist()
    portal_chemical_tab = strsplit(as.character(url_search), "&", fixed=T) %>% lapply(., function(x){ gsub("stat=", "", grep("stat=", x, value=T)) }) %>% lapply(., function(x){ x[!x==""] }) %>% unlist() %>% gsub("[[:space:]]", "_", .)
    
    if(length(portal_tab_name)==0){ 
      
      tab_name <- "home"; fname <- "ADIP0" ; 
      
    }else{
      
      if(toupper(portal_tab_name) %in% toupper(c("home", "overview", "portal", "sign_in"))){
        tab_name <- portal_tab_name; fname <- "ADIPO";
      }else if(toupper(portal_tab_name) %in% toupper(projectlist$Portal)){
        tab_name <- "portal"; fname <- portal_tab_name;
      }else{
        tab_name <- "404"; fname <- "ADIPO";
      }
      
    }
    
    if(length(portal_subtab)==0){ subtab <- "annotation" }else{ subtab <- portal_subtab }
    if(length(portal_chemical_id)==0){ chemical_id <- "none" }else{ chemical_id <- portal_chemical_id }
    if(length(portal_chemical_tab)==0){ chemical_tab <- "gene_expression" }else{ chemical_tab <- portal_chemical_tab }
    
  }
  
  ## Get the R files for the selected page ####
  print(tab_name); print(fname); print(subtab); print(chemical_id); print(chemical_tab);
  
  if(tab_name == "404"){
    
    shinyjs::hide(id="header-ui")
    shinyjs::hide(id="portal-ui")
    shinyjs::hide(id="footer-ui")
    
    output$page_404 <- renderUI({
      includeHTML("404.html")
    })
    
  }else{
    
    shinyjs::hide(id="error-ui")
    
    fname = projectlist$Portal[which(toupper(projectlist$Portal) %in% toupper(fname))]
    
    # Make sure the subtab is named correctly
    if(!subtab %in% c("annotation", "chemical_explorer", "marker_explorer", "heatmap_explorer", "k2_taxonomer_results", "compare_multiple")){
      subtab = "annotation"
    }
    
    # Make sure the statistical tab is named correctly
    if(!chemical_tab %in% c("gene_expression", "gene_set_enrichment", "connectivity")){
      chemical_tab = "gene_expression"
    }
    
    ## load and run server code for this page ####
    session$sendCustomMessage("SelectPortalTab", tab_name)
    session$sendCustomMessage("SelectedPortal", fname)
    session$sendCustomMessage("SelectedSubtab", subtab)
    session$sendCustomMessage("SelectedChemicalTab", chemical_tab)
    session$sendCustomMessage("SelectedChemicalId", chemical_id)
    
    ## load and run server code for this page ####
    source("home.R", local=TRUE)
    source("overview.R", local=TRUE)
    source("sign_in.R", local=TRUE)
    source("add_project_import_files.R", local=TRUE)
    source("add_project.R", local=TRUE)
    source("edit_project_import_files.R", local=TRUE)
    source("edit_project.R", local=TRUE)
    
    ## Read in data from GeneHive
    source("reactive_data.R", local=TRUE)
    
    # Run the main app###
    source("portal.R", local=TRUE)
    source("portal_main_page.R", local=TRUE)
    
    # Server logic ####
    source("server_annotation.R", local=TRUE)
    source("server_chemical.R", local=TRUE)
    source("server_marker.R", local=TRUE)
    source("server_heatmap.R", local=TRUE)
    source("server_taxonomic_clustering.R", local=TRUE)
    source("server_compare_multiple.R", local=TRUE)
    
  }
  
  ## Reconnecting to new sessions after grey out
  session$allowReconnect("force")
  
}

shinyApp(ui=ui, server=server)


