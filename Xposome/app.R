

##Shiny Packages####
library(BiocManager)
library(data.table) #
library(ggdendro) #
library(jsonlite)
library(password) #generate random password
library(sodium) #password encryption
library(digest) #password encryption
library(sendmailR) #send email message
library(tidyverse)
library(magrittr)
library(ipc)

##Shiny Packages####
library(uuidtools)
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
      ###<!-- meta -->####   
      tags$meta(charset="UTF-8"),
      tags$meta(name="viewport", content="width=device-width, initial-scale=1.0"),
      
      ###<!-- favicon -->####   
      tags$link(href="IMAGES/bu_logo.png", rel="shortcut icon"),
      
      ####<!-- css style -->####
      tags$link(type="text/css", rel="stylesheet", href="CSS/LogInStyle.css"),
      tags$link(type="text/css", rel="stylesheet", href="CSS/ModeratorStyle.css"),
      
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
            a(onclick="curlinkFun('about')", href="?about", id="about", class="site-link",  "About"),
            a(onclick="curlinkFun('contact')", href="?contact", id="contact", class="site-link", "Contact"),
            a(onclick="curlinkFun('sign_in')", href="?sign_in", id="sign_in", class="site-link", "Sign In")
        )
      )
    ),
    
    ###The main body#####
    fluidRow(
      class="body-container", style="padding: 0 0 0 0; margin: 0 0 0 0;",
      
      column(style="padding: 0 0 0 0; margin: 0 0 0 0;",
        width=12,
        uiOutput("pageStub") %>% withSpinner(type=4, color="#0dc5c1")
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
  
  cat("Session started.\n")                                 # this prints when a session starts
  onSessionEnded(function() { cat("Session ended.\n\n")  })  # this prints when a session ends
  
  ##Create a queue object---- 
  queue <- shinyQueue();
  
  ## keep track of who log in and output error message####
  UserLog <- reactiveValues(Logged=FALSE);
  
  ##Execute signals every 100 milliseconds----
  queue$consumer$start(100); 
  
  ##To signal STOP to the future-----
  interruptor <- AsyncInterruptor$new();
  
  # Update the clock every 5s to prevent app from being inactive and grey out####
  output$clock <- renderText({
    invalidateLater(5000)
    Sys.time()
  })

  # UI object files ####
  source("ui_input.R", local=TRUE)
  source("login.R", local=TRUE)
  source("logout.R", local=TRUE)
  
  # Preproccessing data #####
  source("carcinogenome_startup.R", local=TRUE)
  source("taxonomer_startup.R", local=TRUE)  
  
  # Morpheus heatmap ####
  source("morpheus_heatmap.R", local=TRUE)
  
  # TAS, Modzscores calculation ####
  source("tas_modzscores_calculation.R", local=TRUE)
  
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
      Password=digest("Xposome"),
      Status="Moderator",
      stringsAsFactors=TRUE
    )
    
    write.csv(data, "data/User_Login_List.csv", row.names=FALSE)
    
    return(data)
    
  }) 

  ##Get all files in the data folder###
  data_files <- list.files("data/")

  ##Create a list of wanted folders and files####
  wanted_files <- c(projectlist$Portal, "Connectivity Map", "Enrichment Gene Set", "Landmark", "Project_List.csv", "Template", "User_Login_List.csv", "Zebra Fish") 
  
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
  
  if(any(!json_files%in% projectlist$Portal)){
    for(f in seq_along(json_files)){ 
      #f=1
      if(!json_files[f] %in% projectlist$Portal){
        unlink(paste0("www/JSON/", json_files[f]), recursive=TRUE, force=TRUE)
      }
    }
  }
  
  ##Get all files in the rmd folder###
  rmd_files <- list.files("www/RMD")

  ##Create a list of wanted folders and files####
  wanted_rmds <- c(paste0("introduction_", projectlist$Portal, ".Rmd"), "about_page.Rmd", "contact_page.Rmd") 
  
  if(any(!rmd_files %in% wanted_rmds)){
    for(f in seq_along(rmd_files)){ 
      #f=1
      if(!rmd_files[f] %in% wanted_rmds){
        unlink(paste0("www/RMD/", rmd_files[f]), recursive=TRUE, force=TRUE)
      }
    }
  }
  
  #load server code for page specified in URL
  fname = isolate({ session$clientData$url_search })

  if(nchar(fname)==0 | length(fname)==0) { fname = "?home" } # blank means home page
  fname = paste0(substr(fname, 2, nchar(fname))) # remove leading "?"

  if(fname %in% c("home", "about", "contact", "sign_in")){
    fname=fname
  }else{
    if(dir.exists(paste0("data/", fname))){
      fname=fname
    }else{
      fname="empty"
    }
  }
  
  ## Get the R files for the selected page ####
  if(fname %in% c("home", "about", "contact", "empty")){
    
    ## load and run server code for this page ####
    source(paste0("", fname, ".R"), local=TRUE) 

  }else if(fname %in% "sign_in"){
    
    ## load and run server code for this page ####
    source(paste0("", fname, ".R"), local=TRUE) 
    source("add_project_import_files.R", local=TRUE)
    source("add_project.R", local=TRUE)
    source("edit_project_import_files.R", local=TRUE)
    source("edit_project.R", local=TRUE)
    
  }else{
    # Temporary hard-coded list of WorkFile IDs
    WorkFileIDs <- list(
      ADIPO=c(
        "Chemical_Annotation.RDS"=171, "Connectivity.RDS"=154,
        "Expression_Set.RDS"=155, "GS_Enrichment.RDS"=156, "K2results.rds"=172,
        "Profile_Annotation.RDS"=173
      ),
      HEPG2=c(
        "Chemical_Annotation.RDS"=174, "Connectivity.RDS"=160,
        "Expression_Set.RDS"=161, "GS_Enrichment.RDS"=162, "K2results.rds"=175,
        "Profile_Annotation.RDS"=176
      ),
      MCF10A=c(
        "Chemical_Annotation.RDS"=177, "Connectivity.RDS"=166,
        "Expression_Set.RDS"=167, "GS_Enrichment.RDS"=168, "K2results.rds"=178,
        "Profile_Annotation.RDS"=179
      )
    )
    # Read in the profile data ####
    profile_dat <- reactive({
      
      future({
#        readRDS(paste0("data/", fname, "/Profile_Annotation.RDS"))
        getWorkFileAsObject(
          hiveWorkFileID(WorkFileIDs[[fname]]["Profile_Annotation.RDS"])
        )
      }) %...!% { return(NULL) }
      
    })
    
    # Read in the chemical data ####
    chemical_dat <- reactive({
      
      future({
#        readRDS(paste0("data/", fname, "/Chemical_Annotation.RDS"))
        getWorkFileAsObject(
          hiveWorkFileID(WorkFileIDs[[fname]]["Chemical_Annotation.RDS"])
        )
      }) %...!% { return(NULL) }
      
    })
    
    # Read in the expression data ####
    expression_dat <- reactive({
      
      future({
#        readRDS(paste0("data/", fname, "/Expression_Set.RDS"))
        getWorkFileAsObject(
          hiveWorkFileID(WorkFileIDs[[fname]]["Expression_Set.RDS"])
        )
      }) %...!% { return(NULL) }
      
    })
    
    # Read in the connectivity data ####
    connectivity_dat <- reactive({
      
      future({
#        readRDS(paste0("data/", fname, "/Connectivity.RDS"))
        getWorkFileAsObject(
          hiveWorkFileID(WorkFileIDs[[fname]]["Connectivity.RDS"])
        )
      }) %...!% { return(NULL) }
      
    })
    
    # Read in the gs enrichment data ####
    gs_enrichment_dat <- reactive({
      
      future({
#        readRDS(paste0("data/", fname, "/GS_Enrichment.RDS"))
        getWorkFileAsObject(
          hiveWorkFileID(WorkFileIDs[[fname]]["GS_Enrichment.RDS"])
        )
      }) %...!% { return(NULL) }
      
    })
    
    # Read in K2 Taxonomer data ####
    taxonomer_results <- reactive({
      
      future({
        
        ##Shiny Packages####
        require(K2Taxonomer)
        require(visNetwork) #
        require(Biobase) #
        require(BiocGenerics)
        
        # Read in K2 Taxonomer data ####
#        K2summary <- readRDS(paste0("data/", fname, "/K2results.RDS"))
        K2summary <- getWorkFileAsObject(
          hiveWorkFileID(WorkFileIDs[[fname]]["K2results.RDS"])
        )

        #print("hello1")
        
        # Parse results
        info <- K2info(K2summary)  # Profile information
        infoMat <- as.matrix(info) # Format information
        meta <- K2meta(K2summary) # Get meta data
        #print("hello2")
        
        #Create variable options
        varOptions <- sort(colnames(info))
        names(varOptions) <- varOptions
        
        if(!is.null(meta$cohorts)) {
          varOptions <- varOptions[varOptions != meta$cohorts]
        } else {
          varOptions <- varOptions[varOptions != "sampleID"]
        }
        #print("hello3")
        
        K2res <- K2results(K2summary) # Format K2 results
        
        # Get IDs of each group ####
        obsMap <- unlist(lapply(K2res, function(x) x$obs), recursive = F)
        
        dataMatrix <- K2data(K2summary) # Format dataMatrix
        genesets <- K2genesets(K2summary) # Get geneset lists
        gene2Pathway <- K2gene2Pathway(K2summary) # Get gene2pathway matching
        eSet <- K2eSet(K2summary) # Get expression set
        gSet <- K2gSet(K2summary) # Get gene set projection expression set
        #print("hello4")
        
        # Create static dendrogram
        K2dendrogram <- K2dendro(K2summary)
        
        ## Get sample order ####
        labs <- get_leaves_attr(K2dendrogram, "label")
        
        # Create interactive dendrogram ####
        vNetOut <- K2visNetwork(K2summary)
        #print("hello5")
        
        # If too many observations in terminal labels, unlabel them
        if (max(lengths(regmatches(vNetOut$x$nodes$label, gregexpr("\n", vNetOut$x$nodes$label)))) > 20 ) {
          
          # Fix font size
          vNetOut$x$nodes$font.size <- 25
          vNetOut$x$nodes$font.size[vNetOut$x$nodes$shape == "box"] <- 0
          
          # Change shape
          vNetOut$x$nodes$shape[vNetOut$x$nodes$shape == "box"] <- "square"
        }
        
        # Get the mod test table
        if(!is.null(K2res[[1]]$modTests)){
          
          # Format table
          K2modTestList <- lapply(K2res, function(x) {
            modTests <- x$modTests
            names(modTests) <- c("1", "2")
            do.call(rbind, modTests)
          })
          
          names(K2modTestList) <- names(K2res)
          K2modTestFram <- do.call(rbind, K2modTestList)[,c("value", "pval", "fdr")]
          
          # Get parent node
          K2modTestFram$Parent <- regmatches(rownames(K2modTestFram), regexpr("^[^.]+", rownames(K2modTestFram)))
          
          # Get direction to chile
          K2modTestFram$Direction <- as.character(gsub("[[:alpha:]]+[.]|[.][[:digit:]]+$", "", rownames(K2modTestFram)))
          
          # Get child
          K2modTestFram$Child <- apply(K2modTestFram[, c("Parent", "Direction")], 1, function(x){
            vSub <- vNetOut$x$edges[vNetOut$x$edges$from == x[1],]
            vSub$to[as.numeric(x[2])]
          })
          
          # Get split
          K2modTestFram$Split <- paste0(K2modTestFram$Parent, K2modTestFram$Direction)
          
          # Format p-values
          K2modTestFram <- K2modTestFram[!is.na(K2modTestFram$pval),]
          K2modTestFram <- K2modTestFram[order(K2modTestFram$pval),]
          
          # Get unrenamed variables of mod test for qvalues formatting
          mvTabSub <- K2modTestFram
          
          K2modTestFram <- K2modTestFram[, c("Split", "Child", "value", "pval", "fdr")]
          colnames(K2modTestFram) <- c("Split", "Node", "Variable", "P Value", "Q Value")
          
          K2modTestFram$`P Value` <- signif(K2modTestFram$`P Value`, 2)
          K2modTestFram$`Q Value` <- signif(K2modTestFram$`Q Value`, 2)
          
          # Color breaks for q values
          breaks <- c(1, 0.25, 0.1, 0.05, 0.01, 0.001, 0)
          breakColors <- brewer.pal(7, "Greens")
          mvTabSub$color <- sapply(mvTabSub$pval, function(pval) {
            breakColors[which.min(breaks >= pval)]
          })
          
          # Size breaks
          breaks <- c(1, 0.1, 0.05, 0.01, 0.001, 0.0001, 0)
          breakSize <- seq(length(breaks)) * 7
          mvTabSub$width <- sapply(mvTabSub$pval, function(pval) {
            breakSize[which.min(breaks >= pval)]
          })
          
          # Add 2 values
          vNetOut_qvalues <- vNetOut
          #print("hello6")
          
          # Change width of edges
          mEdge <- mvTabSub[, c("Parent", "Child", "width")][!duplicated(mvTabSub[, c("Parent", "Child")]),]
          colnames(mEdge) <- c("from", "to", "width")
          edgeFram <- merge(vNetOut_qvalues$x$edges, mEdge, all.x = TRUE, sort = FALSE)
          edgeFram$width[is.na(edgeFram$width)] <- 1
          edgeFram$color.inherit <- 'to'
          vNetOut_qvalues$x$edges <- edgeFram
          
          # Change color of edges
          mNode <- mvTabSub[, c("Child", "color")][!duplicated(mvTabSub[, c("Child")]),]
          colnames(mNode) <- c("id", "color.border")
          nodeFram <- left_join(vNetOut_qvalues$x$nodes, mNode)
          nodeFram$color.border[is.na(nodeFram$color.border)] <- brewer.pal(6, "Greens")[1]
          nodeFram$color.background <- nodeFram$color.border
          nodeFram$color.highlight <- 'red'
          vNetOut_qvalues$x$nodes <- nodeFram
          
        }else{
          
          K2modTestFram <- NULL
          
        }
        
        # Get differential gene expression results
        dgeTable <- getDGETable(K2summary)
        #print("hello7")
        
        ## Get the gene link
        dgeTable$Plot <- paste0("<label for='PlotRow", seq(nrow(dgeTable)), "'>&#128202;</label>")
        dgeTable$Link <- sapply(as.character(dgeTable$gene), get_dgeTable_link)
        
        # Reorder columns
        dgeTable <- dgeTable[,c("gene", "split", "mod", "direction", "pval", "fdr", "coef", "mean", "Plot", "Link")]
        
        # Format numbers to fit in table
        for (i in c("pval", "fdr")) {
          if(i %in% colnames(dgeTable)){
            dgeTable[,i] <- signif(dgeTable[,i], digits = 2)
          }
        }
        
        # Format numbers to fit in table
        for (i in c("coef", "mean")) {
          if(i %in% colnames(dgeTable)){
            dgeTable[,i] <- round(dgeTable[,i], digits = 2)
          }
        }
        
        # Rename columns
        colnames(dgeTable) <- c("Gene", "Node", "Group", "Direction", "P Value", "FDR", "Diff", "Mean", "Plot", "Link")
        
        # Get gene set enrichment results
        enrTable <- getEnrichmentTable(K2summary)
        
        # Remove unnecessary columns
        enrTable <- enrTable[, !colnames(enrTable) %in% c("B", "ntot", "t")]
        
        # Add links to gene sets
        enrTable$Plot <- paste0("<label for='PlotRow", seq(nrow(enrTable)), "'>&#128202;</label>")
        enrTable$Link <- sapply(as.character(enrTable$category), get_enrTablelink)
        
        # Format numbers to fit in table
        for (i in c("pval_hyper", "fdr_hyper", "pval_limma", "fdr_limma")) {
          if(i %in% colnames(enrTable)){
            enrTable[,i] <- signif(enrTable[,i], digits = 2)
          }
        }
        
        # Format numbers to fit in table
        for (i in c("coef", "mean")) {
          if(i %in% colnames(enrTable)){
            enrTable[,i] <- round(enrTable[,i], digits = 2)
          }
        }
        
        colnames(enrTable) <- c("Gene Set", "Node", "Group", "Direction", "P Value_Hyper", "FDR_Hyper", "N_Overlap", "N_Sig. Genes", "N_Gene Set", "P Value_ssGSEA", "FDR_ssGSEA", "Diff_ssGSEA", "Mean_ssGSEA", "Hits", "Plot", "Link")
        #print("hello8")
        
        # return a list of objects
        results <- list(
          info=info,
          infoMat=infoMat,
          meta=meta,
          K2res=K2res,
          dataMatrix=dataMatrix,
          genesets=genesets,
          gene2Pathway=gene2Pathway,
          eSet=eSet,
          gSet=gSet,
          dgeTable=dgeTable,
          enrTable=enrTable,
          K2dendrogram=K2dendrogram,
          vNetOut=vNetOut,
          vNetOut_qvalues=vNetOut_qvalues,
          K2modTestFram=K2modTestFram,
          options=list(
            varOptions=varOptions,
            labs=labs,
            obsMap=obsMap
          )
        )
        
        return(results)
        
      }) %...!% { return(NULL) }
      
    })
    
    ## Create reactive values####
    annot_var <- reactive({
      promise_all(pro_ann=profile_dat(), eset=expression_dat()) %...>% with({
        if(all(colnames(eset) %in% pro_ann$Sig_Id)){
          return("Sig_Id")
        }else{
          return("Chemical_Id")
        }
      })
    })
    
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
    
    # # Run the main app###
    source("main_app.R", local=TRUE)  
    
    # Server logic ####
    source("server_annotation.R", local=TRUE)
    source("server_chemical.R", local=TRUE)
    source("server_marker.R", local=TRUE)
    source("server_heatmap.R", local=TRUE)
    source("server_taxonomic_clustering.R", local=TRUE)
    source("server_compare_multiple.R", local=TRUE)
    
  }
}

shinyApp(ui=ui, server=server)

