

##Shiny Packages####
library(shiny)
library(shinycssloaders)
library(shinythemes)
library(tidyverse)
library(reshape2)
library(promises)
library(future)
plan(multiprocess)
library(ggplot2)
library(plotly)
library(ipc)
library(DT)
library(Biobase)
library(rjson)
library(shinyBS)

##Define Server Login####
ui <- bootstrapPage(
  
  title=paste0("The Exposome Project"),
    
  tagList(
    tags$head(
      ###<!-- meta -->####   
      tags$meta(charset="UTF-8"),
      tags$meta(name="viewport", content="width=device-width, initial-scale=1.0"),
      
      ###<!-- favicon -->####   
      tags$link(href="IMAGES/bu_logo.png", rel="shortcut icon"),
      
      ####<!-- home style -->####
      tags$link(type="text/css", rel="stylesheet", href="CSS/style.css"),

      ####<!-- javascript -->####
      tags$script(type="text/javascript", src="JS/google-analytics.js")
    )
  ),
  
  ####<!-- start project page -->####
  div(
    id="project-page",
    
    ###the header#####
    fluidRow(
      class="header-section",
      
      column(
        width=8,
        div(class="text-md-left",
            a(href="?home", h2("The Exposome Project")),    
            h4(style="font-weight: 200;", "Chemical Carcinogenicity Screening using high-throughput transcriptomics assays")
        )
      ),
      
      column(
        width=4,
        div(class="text-md-right",
            a(href="?home", class="site-link", title="Home", "Home"),
            a(href="?about", class="site-link", title="About", "About"),
            a(href="?contact", class="site-link", title="Contact", "Contact")
        )
      )
    ),
    
    ###The main body#####
    uiOutput("pageStub") %>% withSpinner(color="#0dc5c1", proxy.height="200px"),
    
    ###the footer#####
    fluidRow(
      class="copyright-section",
      
      column(
        width=12,
        div(class="text-md-center", 
          div(class="copyright",
            p("The Carcinogenome Project: Developed by Monti Lab at Boston University"),
            HTML("&copy; Monti Lab &diams; 2017 &diams; All rights reserved")
          )
        )
      )
    )
  )
  
)

##Define Server Login####
server <- function(input, output, session) {
  
  cat("Session started.\n")                               # this prints when a session starts
  onSessionEnded(function() { cat("Session ended.\n\n") })  # this prints when a session ends
  
  #load server code for page specified in URL
  fname = isolate(session$clientData$url_search)
  
  if(nchar(fname)==0) { fname = "?home" }              # blank means home page
  fname = paste0(substr(fname, 2, nchar(fname)), ".R") # remove leading "?", add ".R"

  #If the files are home, about, and contract, stay in the current page, else go to main_app
  source("ggheat.continuous.R")
  source(fname, local=TRUE) # load and run server code for this page
  source("server_annotation.R", local=TRUE)
  source("server_chemical.R", local=TRUE)
  source("server_marker.R", local=TRUE)
  source("server_heatmap.R", local=TRUE)
  
}

shinyApp(ui=ui, server=server)





