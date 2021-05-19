
## portal page ####
output$pageStub <- renderUI({
  
  req(selected_portal())
  
  div(
    class="portal-page",
    
    h4(class="title-1", paste0("Project - ", projectlist$Project[which(projectlist$Portal == selected_portal())])),
    br(),
    selectInput(inputId="portal_id", label=NULL, choices=projectlist$Portal, selected=selected_portal()),
    actionButton(inputId="change_portal", label=strong("Change Project"), style="padding: 5px 5px 5px 5px", class="btn btn-success"),
    downloadButton(outputId="download_portal", style="padding: 5px 5px 5px 5px", class="my-btn btn btn-primary"),
    br(), br(),
    h4(class="title-1", "Description"),
    HTML(paste0("<p>", projectlist$Description[which(projectlist$Portal == selected_portal())], "</p>")),
    HTML(paste0("<p><b style='color: #3182bd;'>Cell-Line:</b> ", projectlist$Cell_Line[which(projectlist$Portal == selected_portal())], "</p>")),
    br(),
    tabsetPanel(
      id="main_page", selected=subtab, type="pills",
      
      ###Annotation#####
      tabPanel(
        title="Annotation", value="annotation",
        #"Hello World!"
        source("ui_annotation.R", local=TRUE)$value
      ),
      
      ###Chemical Explorer#####
      tabPanel(
        title = "Chemical Explorer", value="chemical_explorer",
        #"Hello World!"
        source("ui_chemical.R", local=TRUE)$value
      ),
      
      ###Marker Explorer####
      tabPanel(
        title = "Marker Explorer", value="marker_explorer",
        #"Hello World!"
        source("ui_marker.R", local=TRUE)$value
      ),
      
      ###Heatmap Explorer####
      tabPanel(
        title = "Heatmap Explorer", value="heatmap_explorer",
        #"Hello World!"
        source("ui_heatmap.R", local=TRUE)$value
      ),
      
      ###Taxonomic Clustering####
      navbarMenu(
        title = "Taxonomic Clustering",
        
        tabPanel(
          title = "K2 Taxonomer Results", value="k2_taxonomer_results",
          #"Hello World!"
          source("ui_taxonomic_clustering.R", local=TRUE)$value
        ),
        
        tabPanel(
          title = "Compare Multiple", value="compare_multiple",
          #"Hello World!"
          source("ui_compare_multiple.R", local=TRUE)$value
        )
      )
    )
  )  
  
})

# Go to the specific tab on the portal page
observeEvent(input$main_page, {
  
  if(input$main_page == "chemical_explorer"){
    if(is.null(input$chem) | input$chem == ""){
      updateQueryString(paste0("?page=", input$portal_id, "&tab=", input$main_page), mode="push")
    }else{
      updateQueryString(paste0("?page=", input$portal_id, "&tab=", input$main_page, "&chemical_id=", input$chem, "&stat=", input$chemical_tab), mode="push")
    }
  }else{
    updateQueryString(paste0("?page=", input$portal_id, "&tab=", input$main_page), mode="push")
  }
  
  if(input$main_page == "k2_taxonomer_results"){
    session$sendCustomMessage("ResizeK2Table", "clusteringTable")
  }
  
})

## Change the portal id ####
observeEvent(input$change_portal, {
    
  portal_id <- input$portal_id
  
  updateQueryString(paste0("?page=", input$portal_id, "&tab=annotation"), mode="push")
  updateTabsetPanel(session, inputId="main_page", selected="annotation")
  
  selected_portal(portal_id)
  
}, ignoreInit = FALSE)

# Download the portal data
output$download_portal <- downloadHandler(
  
  filename = function() {
    paste0(input$portal_id, "-Dataset.zip")
  },
  
  content = function(file) {
    
    withProgress(message = "Downloading: ", value = 0, {
      
      fs <- c()
      tmpdir <- tempdir()
      
      datasets <- listEntities("PortalDataset", portal=input$portal_id)
      
      # Sort by timestamp and extract most recent dataset to convenience object
      datasets <- datasets[order(sapply(datasets, slot, "timestamp"))]
      dataset <- datasets[[length(datasets)]]
      
      # increment the progress bar
      incProgress(1/6, detail = "profile annotation")
      
      # Read in the profile data ####
      profile_dat <- getWorkFileAsObject(
        hiveWorkFileID(dataset@ProfileAnnotationRDS)
      )
      
      saveRDS(profile_dat, file.path(tmpdir, "Profile_Annotation.RDS"))
      
      # increment the progress bar
      incProgress(2/6, detail = "chemical annotation")          
      
      # Read in the chemical data ####
      chemical_dat <- getWorkFileAsObject(
        hiveWorkFileID(dataset@ChemicalAnnotationRDS)
      )
      
      saveRDS(chemical_dat, file.path(tmpdir, "Chemical_Annotation.RDS"))
      
      # increment the progress bar
      incProgress(3/6, detail = "expression set")
      
      # Read in the expression data ####
      expression_dat <- getWorkFileAsObject(
        hiveWorkFileID(dataset@GeneExpressionRDS)
      )
      
      saveRDS(expression_dat, file.path(tmpdir, "Gene_Expression.RDS"))
      
      # increment the progress bar
      incProgress(4/6, detail = "connectivity map")
      
      # Read in the connectivity data ####
      connectivity_dat <- getWorkFileAsObject(
        hiveWorkFileID(dataset@ConnectivityRDS)
      )
      
      saveRDS(connectivity_dat, file.path(tmpdir, "Connectivity.RDS"))
      
      # increment the progress bar
      incProgress(5/6, detail = "gene set enrichment")
      
      # Read in the gs enrichment data ####
      gs_enrichment_dat <- getWorkFileAsObject(
        hiveWorkFileID(dataset@GeneSetEnrichmentRDS)
      )
      
      saveRDS(gs_enrichment_dat, file.path(tmpdir, "Gene_Set_Enrichment.RDS"))
      
      # increment the progress bar
      incProgress(6/6, detail = "K2-taxonomer") 
      
      K2summary <- getWorkFileAsObject(
        hiveWorkFileID(dataset@K2TaxonomerResultsRDS)
      )
      
      saveRDS(K2summary, file.path(tmpdir, "K2Taxonomer.RDS"))
      
      # zip the files
      file_names <- c("Profile_Annotation", "Chemical_Annotation", "Gene_Expression", "Gene_Set_Enrichment", "Connectivity", "K2Taxonomer") 
      
      for(names in file_names){
        path <- file.path(tmpdir, paste0(names, ".RDS"))
        fs <- c(fs, path)
      } 
      
      zip(zipfile=file, files=fs, flags = "-r9Xj")
      
    })
  },
  
  contentType = "application/zip"
)



