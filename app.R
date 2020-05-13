

##Shiny Packages####
library(BiocManager)
options(repos = BiocManager::repositories())
library(data.table) #
library(ggdendro) #
library(jsonlite)

##Shiny Packages####
library(K2Taxonomer)
library(visNetwork) #
library(Biobase) #
library(GSVA) #
library(limma) #
library(dendextend) #

##Shiny Packages####
library(shiny) #
library(shinyjs)
library(shinyBS) #
library(shinycssloaders) #
library(shinythemes) #
library(DT) #
library(ggplot2)
library(plotly) #
library(heatmaply)
library(RColorBrewer) #
library(promises)
library(future)
plan(multiprocess)

##Create the storage memory to storage cache objects####
shinyOptions(cache = diskCache("./myapp-cache"))

##Define ui logic####
ui <- bootstrapPage(
  
  title=paste0("The Xposome Project"),
  
  tagList(
    tags$head(
      ###<!-- meta -->####   
      tags$meta(charset="UTF-8"),
      tags$meta(name="viewport", content="width=device-width, initial-scale=1.0"),
      
      ###<!-- favicon -->####   
      tags$link(href="IMAGES/bu_logo.png", rel="shortcut icon"),
      
      ####<!-- css style -->####
      tags$link(type="text/css", rel="stylesheet", href="CSS/LogInStyle.css"),
      tags$link(type="text/css", rel="stylesheet", href="CSS/MainStyle.css"),
      
      ####<!-- overall style -->####
      tags$link(type="text/css", rel="stylesheet", href="CSS/style.css"),
    
      ####<!-- javascript -->####
      shinyjs::useShinyjs(),
      tags$script(type="text/javascript", src="JS/google-analytics.js"),
      tags$script(type="text/javascript", src="JS/Javascript.js")
    )
  ),
  
  ## Create an active clock  to prevent app from greying out ####
  div(style = "position: absolute; top: -100px;",
      textOutput("clock")
  ),
  
  ####<!-- start project page -->####
  div(
    class="project-page",
    
    ###the header#####
    fluidRow(
      class="header-container",
      
      column(
        width=8, 
        
        div(class="text-md-left",
            a(href="?home", h2("The Xposome Project")),    
            h4(style="font-weight: 200;", "Chemical Screening using high-throughput transcriptomics assays")
        )
      ),
      
      column(
        width=4,
        
        div(class="text-md-right",
            a(onclick="curlinkFun('home')", href="?home", id="home", class="site-link", "Home"),
            a(onclick="curlinkFun('about')", href="?about", id="about", class="site-link", "About"),
            a(onclick="curlinkFun('contact')", href="?contact", id="contact", class="site-link", "Contact"),
            a(onclick="curlinkFun('sign_in')", href="?sign_in", id="sign_in", class="site-link", "Sign In")
        )
      )
    ),
    
    ###The main body#####
    fluidRow(
      class="body-container", style="padding: 0 0 0 0; margin: 0 0 0 0;",
      
      column(
        width=12, style="padding: 0 0 0 0; margin: 0 0 0 0;",

        shinyjs::hidden(
          div(
            id = "loading-content",
            div(class="loader"),
            h4("Loading...", id="loading_text")
          )
        )
      ),
      
      column(style="padding: 0 0 0 0; margin: 0 0 0 0;",
        width=12,
        uiOutput("pageStub")
      )
    ),
    
    ###the footer/copyright#####
    fluidRow(
      class="copyright-container",
      
      column(
        width=12,
        
        div(class="text-md-center", 
            p("The Xposome Project: Developed by Monti Lab at Boston University"),
            HTML(paste0("&copy; Monti Lab &diams; ", as.numeric(format(Sys.Date(), "%Y")), " &diams; All Rights Reserved."))
        )
      )
    )
  )
)

##Define Server Login####
server <- function(input, output, session) {
  
  # Update the clock every 5s to prevent app from being inactive and grey out####
  output$clock <- renderText({
    invalidateLater(5000)
    Sys.time()
  })
  
  # UI object files ####
  source("ui_input.R")

  ## Read in the project list ####
  projectlist <- reactiveVal({
    read.csv(paste0("data/Project_List.csv"), header = TRUE, as.is = TRUE)
  })
  
  ## Read in the login list ####
  loginlist <- reactiveVal({
      read.csv(paste0("data/User_Login_List.csv"), header = TRUE, as.is = TRUE)
  })
  
  ## Create reactive values ####
  profile_dat <- reactiveVal(NULL);
  chemical_dat <- reactiveVal(NULL);
  expression_dat <- reactiveVal(NULL);
  gs_enrichment_dat <- reactiveVal(NULL);
  connectivity_dat <- reactiveVal(NULL);
  #K2summary <- reactiveVal(NULL);
  
  # Observe when url changes ####
  observeEvent(session$clientData$url_search, {
    
    if(session$clientData$url_search==""){ 
      fname = "home"
    }else{ 
      fname = gsub("\\?", "", isolate({ session$clientData$url_search }))
    }
    
   ## Get the R files for the selected page ####
   if(fname %in% c("home", "about", "contact", "sign_in")){
     
     ## load and run server code for this page ####
     source("moderator_page.R", local=TRUE)
     source("add_project.R", local=TRUE)
     source(paste0(fname, ".R"), local=TRUE) 
     
   }else{
     
     ## Hide loader and show content ####
     shinyjs::show(id = "loading-content", anim = TRUE, animType = "fade")
     
     # Read in the profile data ####
     future({
       df <- readRDS(paste0("data/", fname, "/Profile_Annotation.RDS"))
       df
     }) %...>% profile_dat()
     
     # Read in the chemical data ####
     future({
       df <- readRDS(paste0("data/", fname, "/Chemical_Annotation.RDS"))
       df
     }) %...>% chemical_dat()
     
     # Read in the expression data ####
     future({
       df <- readRDS(paste0("data/", fname, "/Expression_Set.RDS"))
       df
     }) %...>% expression_dat()
     
     # Read in the connectivity data ####
     future({
       df <- readRDS(paste0("data/", fname, "/Connectivity.RDS"))
       df
     }) %...>% connectivity_dat()
     
     # Read in the gs enrichment data ####
     future({
       df <- readRDS(paste0("data/", fname, "/GS_Enrichment.RDS"))
       df
     }) %...>% gs_enrichment_dat()
     
     # # Read in K2 Taxonomver data ####
     # future({
     #   df <- readRDS(paste0("data/", fname, "/K2results.RDS"))
     #   df
     # }) %...>% K2summary()
     
     # Read in K2 Taxonomver data ####
     K2summary <- readRDS(paste0("data/", fname, "/K2results.rds"))
     
     # Preproccessing data #####
     source("carcinogenome_startup.R", local=TRUE)
     source("taxonomer_startup.R", local=TRUE)     
     
     # # Run the main app###
     source("main_app.R", local=TRUE)  
     
     # Server logic ####
     source("server_annotation.R", local=TRUE)
     source("server_chemical.R", local=TRUE)
     source("server_marker.R", local=TRUE)
     source("server_heatmap.R", local=TRUE)
     source("server_taxonomic_clustering.R", local=TRUE)
     source("server_compare_multiple.R", local=TRUE)
     
     ## Hide loader and show content ####
     shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade", time=1)

   }
   
  }, ignoreNULL = FALSE)
  
  ## Create reactive values####
  gs_enrichment_version <- reactiveVal(NULL);
  experimental_design <- reactiveVal(NULL);
  helptext_geneset <- reactiveVal(NULL);
  dsmap <- reactiveVal(NULL);
  
  observeEvent(projectlist(), {
    
    req(session$clientData$url_search)
    
    fname = gsub("\\?", "", isolate({ session$clientData$url_search }))
    version <- projectlist()$GS_Enrichment_Version[which(projectlist()$Portal == fname)]
    design <- projectlist()$Experimental_Design[which(projectlist()$Portal == fname)]

    ##Getting the genes set enrichment version 
    gs_enrichment_version(version)
    
    ##Getting the experimental design
    experimental_design(design)
    
    ##Getting the helpext for different gene set enrichment
    helptext_geneset(
      paste(
        "Hallmark: MSigDB Hallmark Pathways (v", version, ")", "<br>",
        "C2: MSigDB C2 reactome Pathways (v",  version, ")", "<br>",
        "NURSA: Nuclear Receptor Signaling Atlas, consensome data for human",
        sep = ""
      )
    )
    
    ##Getting the gene set scores for diffrent gsva methods
    dsmap(
      list(
        Hallmark=paste0("gsscores_h.all.v", version, ".0"),
        C2=paste0("gsscores_c2.cp.reactome.v", version, ".0"),
        NURSA=paste0("gsscores_nursa_consensome_Cbyfdrvalue_0.01")
      )
    )
    
  }, ignoreNULL = TRUE)

  ## Create reactive values####
  annot_var <- reactiveVal(NULL);
  gene_set <- reactiveVal(NULL);
  
  observeEvent(expression_dat(), {
    
    req(expression_dat())
    
    eset <- expression_dat()
    
    if(colnames(pData(eset)) %in% "Sig_Id"){
      annot_var("Sig_Id")
    }else if(colnames(pData(eset)) %in% "Chemical_Id"){
      annot_var("Chemical_Id")
    }
    
  }, ignoreInit = TRUE)
  
  ## Create reactive values####
  maxTAS <- reactiveVal(NULL);
  
  observeEvent(profile_dat(), {
    
    req(profile_dat())
    
    minProf<- 3
    max <- profile_dat()$TAS[order(profile_dat()$TAS, decreasing = TRUE)][minProf]
    max <- floor(max/0.1)*0.1    
    maxTAS(max)
    
  }, ignoreInit = TRUE)
  
}

shinyApp(ui=ui, server=server)


