
##Xposome app without GeneHive###
print("Xposome app without GeneHive")

##Bioconductor packages####
library(BiocManager)
library(Biobase) #
library(GSVA) #
library(GSEABase) #read in gmt files

##R Packages for api####
library(httr)
library(jsonlite)

##Packages for generating random pwd and sending emails####
library(password) #generate random password
library(sodium) #password encryption
library(digest) #password encryption
library(sendmailR) #send email message

#Packages for pulling and summarizing data###
library(tidyverse)
library(magrittr)

##Shiny Packages####
library(shiny)
library(shinyBS)
library(shinyjs)
library(shinycssloaders) #
library(DT) #

##Packages for plotting####
library(ggplot2)
library(plotly) #
library(heatmaply)
library(RColorBrewer) #

##Parallel packages####
library(promises)
library(future)
plan(multisession)

##Packages for caching####
library(cachem)

##Genehive packages####
library(uuidtools)
library(GeneHive)

##K2Taxonomer packages####
library(data.table) #
library(ggdendro) #
library(limma) #
library(dendextend) #
library(visNetwork) #
library(K2Taxonomer)

##Options to specify the maximum web request size, which serves as a size limit for file uploads####
options(shiny.maxRequestSize=9e9, future.rng.onMisuse="ignore")

##Create a folder store cache data####
if(!dir.exists("./myapp-cache")){
  dir.create("./myapp-cache", showWarnings=F, recursive=t)
}

##Shiny options to store cache data####
shinyOptions(cache = cachem::cache_disk("./myapp-cache"))

## Read in the project list ####
projectlist <<- tryCatch({
  
  read.csv(paste0("www/data/Project_List.csv"), header=T, check.names=F, stringsAsFactors=F, allowEscapes=T) %>% 
    dplyr::arrange(Project, Portal)
  
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
  
  write.csv(data, "www/data/Project_List.csv", row.names=FALSE, na="")
  
  return(data)
  
})

## Read in the user login list ####
loginlist <<- tryCatch({
  
  read.csv(paste0("www/data/User_Login_List.csv"), header=T, check.names=F, stringsAsFactors=F, allowEscapes=T) %>% 
    dplyr::arrange(Firstname, Lastname)
  
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
  
  write.csv(data, "www/data/User_Login_List.csv", row.names=F, na="")
  
  return(data)
  
})

## Read in the landmark files ####
landmark_dat <<- readRDS(paste0("www/data/Landmark/landmark_gene.RDS"))

## Preproccessing for carcinogenome functions #####
source("carcinogenome_startup.R", local=TRUE)

## Preproccessing for taxonomer functions #####
source("taxonomer_startup.R", local=TRUE)

## Morpheus heatmap ####
source("morpheus_heatmap.R", local=TRUE)

## TAS and Modzscores calculation ####
source("tas_modzscores_calculation.R", local=TRUE)

## sign in functions ####
source("sign_in_startup.R", local=TRUE)

## Define ui logic ####
ui <- htmlTemplate("xposome.html")

## Define server logic ####
server <- function(input, output, session) {
  
  ## Print this when a session starts ####
  cat("Session started.\n")
  
  ## Start up the app ####
  session$sendCustomMessage("startUpApp", "Starts the application")

  ## Read in data for portal page ####
  source("reactive_data_local.R", local=TRUE)
  
  ## Read in source files for portal page ####
  source("server_portal.R", local=TRUE)
  source("server_annotation.R", local=TRUE)
  source("server_chemical.R", local=TRUE)
  source("server_marker.R", local=TRUE)
  source("server_taxonomic_clustering.R", local=TRUE)
  source("server_compare_multiple.R", local=TRUE)
  source("server_sign_in.R", local=TRUE)
  
  ## Print this when a session ends ####
  onSessionEnded(function() { cat("Session ended.\n\n"); }) 
  
}

## Start the app ####
shinyApp(ui=ui, server=server)


