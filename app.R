

##Shiny Packages####
library(BiocManager)
options(repos = BiocManager::repositories())
library(K2Taxonomer)
library(visNetwork)
library(GSVA)
library(limma)
library(dendextend)
library(Biobase)
library(rjson)

##Shiny Packages####
library(shiny)
library(shinyjs)
library(shinyBS)
library(shinycssloaders)
library(shinythemes)
library(DT)
library(tictoc)
library(tidyverse)
library(reshape2)
library(promises)
library(future)
plan(multiprocess)
library(ggplot2)
library(plotly)
library(heatmaply)
library(RColorBrewer)

##Create the storage memory to storage cache objects####
shinyOptions(cache = diskCache("./myapp-cache"))

##Define Server Login####
ui <- bootstrapPage(
  
  title=paste0("The Xposome Project"),
  
  tagList(
    tags$head(
      ###<!-- meta -->####   
      tags$meta(charset="UTF-8"),
      tags$meta(name="viewport", content="width=device-width, initial-scale=1.0"),
      
      ###<!-- favicon -->####   
      tags$link(href="IMAGES/bu_logo.png", rel="shortcut icon"),
      
      ####<!-- home style -->####
      tags$link(type="text/css", rel="stylesheet", href="CSS/style.css"),
      
      ####<!-- sign-in style -->####
      tags$link(type="text/css", rel="stylesheet", href="CSS/Util.css"),
      tags$link(type="text/css", rel="stylesheet", href="CSS/LogInStyle.css"),
      tags$link(type="text/css", rel="stylesheet", href="CSS/MainStyle.css"),
      
      ####<!-- javascript -->####
      tags$script(type="text/javascript", src="JS/google-analytics.js"),
      tags$script(type="text/javascript", src="JS/Javascript.js")
      
    )
  ),
  
  shinyjs::useShinyjs(),
  
  ####<!-- start project page -->####
  div(
    id="project-page",
    
    ###the header#####
    fluidRow(
      class="header-section",
      
      column(
        width=8,
        div(class="text-md-left",
            a(href="?home", h2("The Xposome Project")),    
            h4(style="font-weight: 200;", "Chemical Carcinogenicity Screening using high-throughput transcriptomics assays")
        )
      ),
      
      column(
        width=4,
        div(class="text-md-right",
            a(href="?home", class="site-link", "Home"),
            a(href="?about", class="site-link", "About"),
            a(href="?contact", class="site-link", "Contact"),
            a(href="?sign_in", class="site-link", "Sign In")
        )
      )
    ),
    
    ###The main body#####
    fluidRow(
      class="main-page", style="background: white;",
      
      div(
        id = "loading-content",
        div(class="loader", "Loading..."),
        h4("Loading...", style="color: white; opacity: 0.9;")
      ),
      
      hidden(
        column(
          width=12,
          id = "app-content",
          uiOutput("pageStub")
        )
      )
    ),
    
    ###the footer/copyright#####
    fluidRow(
      class="copyright-section",
      
      column(
        width=12,
        div(class="text-md-center", 
          div(class="copyright",
            p("The Xposome Project: Developed by Monti Lab at Boston University"),
            HTML(paste0("&copy; Monti Lab &diams; ", as.numeric(format(Sys.Date(), "%Y")), " &diams; All Rights Reserved"))
          )
        )
      )
    )
  )
)

##Define Server Login####
server <- function(input, output, session) {
  
  cat("Session started.\n")                                 # this prints when a session starts
  onSessionEnded(function() { cat("Session ended.\n\n") })  # this prints when a session ends
  
  #load server code for page specified in URL
  fname = isolate(session$clientData$url_search)

  if(nchar(fname)==0) { fname = "?home" } # blank means home page
  fname = paste0(substr(fname, 2, nchar(fname))) # remove leading "?", add ".R"

  #If the files are home, about, and contract, stay in the current page, else go to main_app
  source("ggheat.continuous.R")
  
  #Read in the data###
  if(fname %in% c("home", "about", "contact", "sign_in")){
    
     # Hide loader####
    shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade")    
    source(paste0(fname, ".R"), local=TRUE) # load and run server code for this page
    shinyjs::show(id = "app-content", anim = TRUE, animType = "fade")    
    
  }else{
    
    # Read in the data ####
    dat <- readRDS(paste0("data/", fname, "/data.RDS"))
    K2summary <- readRDS(paste0("data/", fname, "/K2results.rds"))
    
    # Preproccessing data #####
    source("startup_1.R", local=TRUE)
    source("startup_2.R", local=TRUE)
  
    # Run the app###
    source("main_app.R", local=TRUE)

    # Server logic ####
    source("server_annotation.R", local=TRUE)
    source("server_chemical.R", local=TRUE)
    source("server_marker.R", local=TRUE)
    source("server_heatmap.R", local=TRUE)
    source("server_taxonomic_clustering.R", local=TRUE)
    source("server_compare_multiple.R", local=TRUE)
    
    # Hide loader####
    shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade")  
    
    # Show content####
    shinyjs::show(id = "app-content", anim = TRUE, animType = "fade")  
    
  }  
}

shinyApp(ui=ui, server=server)





