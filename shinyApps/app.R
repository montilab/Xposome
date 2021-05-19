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
plan(multisession)

##Shiny options####
options(repos = BiocManager::repositories())
options(shiny.maxRequestSize=1000*1024^2)

##Define ui logic####
ui <- bootstrapPage(
  
  title=paste0("The Xposome Project"),
  
  tagList(
    tags$head(
      ###<!-- favicon -->####
      tags$link(href="IMAGES/github_logo.png", rel="shortcut icon"),
      
      # ####<!-- overall style -->####
      tags$link(href="CSS/style.css", rel="stylesheet"),
      tags$link(href="CSS/main_style.css", rel="stylesheet"),
      tags$link(href="CSS/LogInStyle.css", rel="stylesheet"),
      tags$link(href="CSS/ModeratorStyle.css", rel="stylesheet"),

      ##<!-- javascript -->####
      tags$script(src="JS/Javascript.js", type="text/javascript"),
      shinyjs::useShinyjs()
      
      # ###<!-- jitter javascript -->####
      # tags$script(HTML(
      #   "((window.gitter = {}).chat = {}).options = {
      #         room: 'xposome/webapp'
      #    };"
      # )),
      # tags$script(type="text/javascript", src="https://sidecar.gitter.im/dist/sidecar.v1.js")
    )
  ),
  
  ## Create an active clock  to prevent app from greying out ####
  div(style = "position: absolute; top: -100px;",
      textOutput("clock")
  ),
  
  ###the header#####
  fluidRow(
    column(
      width=12,
      
      HTML(
      '
      <nav class="navbar navbar-default navbar-static-top" role="navigation">
        <div class="container-fluid">
          <div class="navbar-header">
            <button type="button" class="navbar-toggle collapsed" data-toggle="collapse" data-target="#navbar-collapse-3838">
              <span class="sr-only">Toggle navigation</span>
              <span class="icon-bar"></span>
              <span class="icon-bar"></span>
              <span class="icon-bar"></span>
            </button>
            <span class="navbar-brand">
              <a href="?page=home" class="logo-link" onclick="curlinkFun(&#39;home&#39;)">
                <span>
                  <img src="IMAGES/github_logo.png" width="50" height="40"/>
                  <span>Xposome</span>
                </span>
              </a>
            </span>
          </div>
          <div class="navbar-collapse collapse" id="navbar-collapse-3838">
            <div class="nav navbar-nav shiny-tab-input" id="main_navbar">
              <a onclick="curlinkFun(\'home\')" href="?page=home" id="home">Home</a>
              <a onclick="curlinkFun(\'portal\')" href="?page=portal" id="portal">Portal</a>
              <a onclick="curlinkFun(\'about\')" href="?page=about" id="about">About</a>
              <a onclick="curlinkFun(\'contact\')" href="?page=contact" id="contact">Contact</a>
              <a onclick="curlinkFun(\'sign_in\')" href="?page=sign_in" id="sign_in">Sign In</a>
            </div>
          </div>
        </div>
      </nav>
      '
      )
    )
  ),

  ###The main body#####
  fluidRow(
    class="body-container", style="position: relative; z-index: 0; overflow: scroll; height: 900px; padding: 0 0 0 0; margin: 0 0 0 0;",
    
    column(
      width=12, 
      uiOutput("pageStub") %>% withSpinner(id="loading-page", type=4, color="#0dc5c1")
    )
  ),
  
  ###the footer/copyright#####
  fluidRow(
    style="position: relative;", 
    
    column(
      width=12, 
      includeHTML("footer.html")
    )
  )
)

##Define Server Login####
server <- function(input, output, session) {
  
  cat("Session started.\n")

  # this prints when a session starts
  onSessionEnded(function() { cat("Session ended.\n\n"); })  # this prints when a session ends

  ##Create a queue object----
  queue <- shinyQueue();

  ##Execute signals every 100 milliseconds----
  queue$consumer$start(100);

  ##To signal STOP to the future-----
  interruptor <- AsyncInterruptor$new();

  # Update the clock every 5s to prevent app from being inactive and grey out####
  output$clock <- renderText({
    invalidateLater(5000)
    Sys.time()
  })

  ## Read in the project list ####
  projectlist <- tryCatch({

    read_csv(paste0("data/Project_List.csv"))

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
  wanted_files <- c("Connectivity Map", "Enrichment Gene Set", "Landmark", "Project_List.csv", "Template", "User_Login_List.csv")

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
  
  # Preproccessing data #####
  source("carcinogenome_startup.R", local=TRUE)
  source("taxonomer_startup.R", local=TRUE)
  
  # Morpheus heatmap ####
  source("morpheus_heatmap.R", local=TRUE)
  
  # TAS, Modzscores calculation ####
  source("tas_modzscores_calculation.R", local=TRUE)
  
  ## keep track of selected portal####
  selected_portal <- reactiveVal("ADIPO");

  ###load server code for page specified by the URL link
  url_search = isolate({ session$clientData$url_search })
  print(url_search)
  
  if(nchar(url_search)==0 | length(url_search)==0) {
    
    tab_name <- "home"; fname <- "ADIPO"; subtab <- "annotation"; chemical_id <- ""; chemical_tab <- "gene_expression";
    
  }else{
    
    portal_tab_name = strsplit(as.character(url_search), "&", fixed=T) %>% lapply(., function(x){ gsub("\\?page=", "", grep("\\?page=", x, value=T)) }) %>% lapply(., function(x){ x[!x==""] }) %>% unlist() %>% gsub("[[:space:]]", "_", .)
    portal_subtab = strsplit(as.character(url_search), "&", fixed=T) %>% lapply(., function(x){ gsub("tab=", "", grep("tab=", x, value=T)) }) %>% lapply(., function(x){ x[!x==""] }) %>% unlist() %>% gsub("[[:space:]]", "_", .)
    portal_chemical_id = strsplit(as.character(url_search), "&", fixed=T) %>% lapply(., function(x){ gsub("chemical_id=", "", grep("chemical_id=", x, value=T)) }) %>% lapply(., function(x){ x[!x==""] }) %>% unlist()
    portal_chemical_tab = strsplit(as.character(url_search), "&", fixed=T) %>% lapply(., function(x){ gsub("stat=", "", grep("stat=", x, value=T)) }) %>% lapply(., function(x){ x[!x==""] }) %>% unlist() %>% gsub("[[:space:]]", "_", .)
    
    if(length(portal_tab_name)==0){ 
      
      tab_name <- "home"; fname <- "ADIP0" ;
      
    }else{
      
      if(toupper(portal_tab_name) %in% toupper(c("home", "about", "contact", "sign_in", "portal"))){
        tab_name <- portal_tab_name; fname <- "ADIPO";
      }else if(toupper(portal_tab_name) %in% toupper(projectlist$Portal)){
        tab_name <- "portal"; fname <- portal_tab_name;
      }else{
        tab_name <- "home"; fname <- "ADIPO";
      }
      
    }
    
    if(length(portal_subtab)==0){ subtab <- "annotation" }else{ subtab <- portal_subtab }
    if(length(portal_chemical_id)==0){ chemical_id <- "" }else{ chemical_id <- portal_chemical_id }
    if(length(portal_chemical_tab)==0){ chemical_tab <- "gene_expression" }else{ chemical_tab <- portal_chemical_tab }
    
  }
  
  ## Get the R files for the selected page ####
  print(tab_name); print(fname); print(subtab); print(chemical_id); print(chemical_tab);
  
  if(toupper(tab_name) %in% toupper(c("home", "about", "contact"))){
    
    ## load and run server code for this page ####
    updateQueryString(paste0("?page=", tolower(tab_name)), mode="push")
    source(paste0("", tolower(tab_name), ".R"), local=TRUE)
    
  }else if(toupper(tab_name) %in% toupper("sign_in")){
    
    ## load and run server code for this page ####
    updateQueryString(paste0("?page=", tolower(tab_name)), mode="push")
    source(paste0("", tolower(tab_name), ".R"), local=TRUE)
    source("add_project_import_files.R", local=TRUE)
    source("add_project.R", local=TRUE)
    source("edit_project_import_files.R", local=TRUE)
    source("edit_project.R", local=TRUE)
    
  }else if(toupper(tab_name) %in% toupper("portal")){
    
    ## load and run server code for this page ####
    updateQueryString(paste0("?page=", fname), mode="push")
    
    # Make sure the subtab is named correctly
    if(!subtab %in% c("annotation", "chemical_explorer", "marker_explorer", "heatmap_explorer", "k2_taxanomer_results", "compare_multiple")){
      subtab = "annotation"
    }
    
    # Make sure the statistical tab is named correctly
    if(!chemical_tab %in% c("gene_expression", "gene_set_enrichment", "connectivity")){
      chemical_tab = "gene_expression"
    }
    
    # Get the correct file name###
    fname = projectlist$Portal[which(toupper(projectlist$Portal) %in% toupper(fname))]
    
    # Highlight the portal link
    session$sendCustomMessage("HighLightPortalTab", "portal")
    
    # Select the search portal name####
    selected_portal(fname)
    
    ## Read in data from GeneHive
    source("reactive_data_api.R", local=TRUE)
    
    #get input options####
    gs_collection <- projectlist$GS_Collection[which(projectlist$Portal == fname)]
    gs_collection_link <- projectlist$GS_Collection_Link[which(projectlist$Portal == fname)]
    landmark <- projectlist$Landmark_Gene[which(projectlist$Portal == fname)]
    exposure_phenotype_test <- unlist(strsplit(as.character(projectlist$Exposure_Phenotype_Test[which(projectlist$Portal == fname)]), ",", fixed=TRUE)) %>% trimws()
    exposure_phenotype <- unlist(strsplit(as.character(projectlist$Exposure_Phenotype[which(projectlist$Portal == fname)]), ",", fixed=TRUE)) %>% trimws()
    exposure_phenotype <- exposure_phenotype[which(exposure_phenotype_test %in% c("1-sided Fisher test", "2-sided Fisher test"))]
    
    ##Getting the helpext for different gene set enrichment
    if(gs_collection %in% "Default"){
      
      ##the default gs enrichment version####
      gs_enrichment_version <- 7
      
      ##Getting the gene set scores for diffrent gsva methods
      dsmap <- list(
        Hallmark=paste0("gsscores_h.all.v", gs_enrichment_version, ".0"),
        C2=paste0("gsscores_c2.cp.reactome.v", gs_enrichment_version, ".0"),
        NURSA=paste0("gsscores_nursa_consensome_Cbyfdrvalue_0.01")
      )
      
      ##Getting the helptext####
      helptext_geneset <- paste0(
        "<a href=\"https://www.gsea-msigdb.org/gsea/msigdb\">MSigDB Hallmark Pathways (v", gs_enrichment_version, ")</a><br>",
        "<a href=\"https://www.gsea-msigdb.org/gsea/msigdb\">MSigDB C2 reactome Pathways (v", gs_enrichment_version, ")</a><br>",
        "<a href=\"https://signalingpathways.org\">NURSA: Nuclear Receptor Signaling Atlas, consensome data for human</a><br>"
      )
      
    }else{
      
      ##Getting the gene set scores for diffrent gsva methods
      dsmap <- paste0("gsscores_", gs_collection)
      names(dsmap) <- paste0("gsscores_", gs_collection)
      
      ##Getting the helptext####
      helptext_geneset <- paste0("<a href=\"", gs_collection_link, "\">", gs_collection, "</a><br>")
      
    }
    
    # Run the main app###
    source("portal.R", local=TRUE)
    
    # Server logic ####
    source("server_annotation.R", local=TRUE)
    source("server_chemical.R", local=TRUE)
    source("server_marker.R", local=TRUE)
    source("server_heatmap.R", local=TRUE)
    source("server_taxonomic_clustering.R", local=TRUE)
    source("server_compare_multiple.R", local=TRUE)
     
  }else{
    
    ## load and run server code for this page ####
    output$pageStub <- renderUI({
      shinyjs::hide(id="header-container")
      shinyjs::hide(id="copyright-container")
      includeHTML("404.html")
    })
    
  }
}

shinyApp(ui=ui, server=server)

