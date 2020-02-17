

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
library(tidyverse)
library(reshape2)
library(promises)
library(future)
plan(multiprocess)
library(ggplot2)
library(plotly)
library(heatmaply)
library(RColorBrewer)

# Loading data files ####
HEPG2 <- readRDS(paste0("data/HEPG2/data.RDS"))
MCF10A <- readRDS(paste0("data/MCF10A/data.RDS"))
ADIPO <- readRDS(paste0("data/ADIPO/data.RDS"))

# Read in analysis results
K2summary <- readRDS("data/ADIPO/K2results.rds")

# Parse results
info <- K2info(K2summary); infoMat <- as.matrix(info) # Format information
K2res <- K2results(K2summary) # Format K2 results
dataMatrix <- K2data(K2summary) # Format dataMatrix
genesets <- K2genesets(K2summary) # Get geneset lists
gene2Pathway <- K2gene2Pathway(K2summary) # Get gene2pathway matching
eSet <- K2eSet(K2summary) # Get expression set
gSet <- K2gSet(K2summary) # Get gene set projection expression set
meta <- K2meta(K2summary) # Get meta data
K2dendrogram <- K2dendro(K2summary) # Create static dendrogram

# Create interactive dendrogram ####
vNetOut <- K2visNetwork(K2summary)

# Remove K2summary to save space
rm(K2summary)

# If too many observations in terminal labels, unlabel them
if (max(
  lengths(regmatches(vNetOut$x$nodes$label, gregexpr("\n", vNetOut$x$nodes$label)))
) > 20 ) {
  
  # Fix font size
  vNetOut$x$nodes$font.size <- 25
  vNetOut$x$nodes$font.size[vNetOut$x$nodes$shape == "box"] <- 0
  
  # Change shape
  vNetOut$x$nodes$shape[vNetOut$x$nodes$shape == "box"] <- "square"
}

# Format enrichment table
enrTable <- getEnrichmentTable(K2res)

# Remove unnecessary columns
enrTable <- enrTable[, !colnames(enrTable) %in% c("B", "ntot", "t")]

# Remove gse from K2res
K2res <- lapply(K2res, function(x) { x$gse <- NULL; return(x) })

# Add aliases for plotting and looking up
enrTable$Plot <- paste0("<label for='PlotRow",
                        seq(nrow(enrTable)),
                        "'>&#128202;</label>")
enrTable$Send <- paste0("<label for='SendRow",
                        seq(nrow(enrTable)),
                        "'>&#9992;</label>")

# Add links to gene sets
get_enrTablelink <- function(geneset){
  sprintf('<a href="http://software.broadinstitute.org/gsea/msigdb/cards/%s" style="text-decoration:none" target="_blank">&#128269;</a>', geneset, geneset)
}

enrTable$Link <- sapply(as.character(enrTable$category), get_enrTablelink)

# Format numbers to fit in table
for (i in c("pval_hyper", "fdr_hyper", "pval_limma", "fdr_limma")) {
  enrTable[,i] <- signif(enrTable[,i], digits = 2)
}

# Format numbers to fit in table
for (i in c("coef", "mean")) {
  enrTable[,i] <- round(enrTable[,i], digits = 2)
}

colnames(enrTable) <- c("Gene Set", "Node", "Group", "Direction", "P Value_Hyper", "FDR_Hyper", "N_Overlap", "N_Sig. Genes", "N_Gene Set", "P Value_ssGSEA", "FDR_ssGSEA", "Diff_ssGSEA", "Mean_ssGSEA", "Hits", "Plot", "Send", "Link")

# Get differential gene expression results
dgeTable <- getDGETable(K2res)

# Remove gse from K2res
K2res <- lapply(K2res, function(x) {x$dge <- NULL; return(x)} )

# Add aliases for plotting and looking up
dgeTable$Plot <- paste0("<label for='PlotRow",
                        seq(nrow(dgeTable)),
                        "'>&#128202;</label>")
dgeTable$Send <- paste0("<label for='SendRow",
                        seq(nrow(dgeTable)),
                        "'>&#9992;</label>")

## Add links to genes
get_dgeTable_link <- function(genesymbol){
  sprintf('<a href="http://www.genecards.org/cgi-bin/carddisp.pl?gene=%s&keywords=%s" style="text-decoration:none" target="_blank">&#128269;</a>', genesymbol, genesymbol)
}

dgeTable$Link <- sapply(as.character(dgeTable$gene), get_dgeTable_link)

# Reorder columns
dgeTable <- dgeTable[,c("gene", "split", "mod", "direction", "pval", "fdr", "coef", "mean", "Plot", "Send", "Link")]

# Format numbers to fit in table
for (i in c("pval", "fdr")) {
  dgeTable[,i] <- signif(dgeTable[,i], digits = 2)
}

# Format numbers to fit in table
for (i in c("coef", "mean")) {
  dgeTable[,i] <- round(dgeTable[,i], digits = 2)
}

# Rename columns
colnames(dgeTable) <- c("Gene", "Node", "Group", "Direction", "P Value", "FDR", "Diff", "Mean", "Plot", "Send", "Link")

## Set select input options for annotations bar
varOptions <- sort(colnames(info))
names(varOptions) <- varOptions

if(!is.null(meta$cohorts)) {
  varOptions <- varOptions[varOptions != "meta$cohorts"]
} else {
  varOptions <- varOptions[varOptions != "sampleID"]
}

varOptions <- c("Add Annotation to Heatmap:" = "", "RESET" = "RESET", varOptions)

## Get sample order ####
labs <- get_leaves_attr(K2dendrogram, "label")

# Get IDs of each group ####
obsMap <- unlist(lapply(K2res, function(x) x$obs), recursive = F)

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

      ####<!-- javascript -->####
      tags$script(type="text/javascript", src="JS/google-analytics.js")
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
            a(href="?contact", class="site-link", "Contact")
        )
      )
    ),
    
    ###The main body#####
    uiOutput("pageStub") %>% withSpinner(color="#0dc5c1", type=4, proxy.height="200px"),
    
    ###the footer/copyright#####
    fluidRow(
      class="copyright-section",
      
      column(
        width=12,
        div(class="text-md-center", 
          div(class="copyright",
            p("The Xposome Project: Developed by Monti Lab at Boston University"),
            HTML("&copy; Monti Lab &diams; 2017 &diams; All rights reserved")
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
  fname = paste0(substr(fname, 2, nchar(fname)), ".R") # remove leading "?", add ".R"

  #If the files are home, about, and contract, stay in the current page, else go to main_app
  source("ggheat.continuous.R")
  source(fname, local=TRUE) # load and run server code for this page
  source("server_annotation.R", local=TRUE)
  source("server_chemical.R", local=TRUE)
  source("server_marker.R", local=TRUE)
  source("server_heatmap.R", local=TRUE)
  source("server_taxonomic_clustering.R", local=TRUE)
  source("server_compare_multiple.R", local=TRUE)
  
}

shinyApp(ui=ui, server=server)





