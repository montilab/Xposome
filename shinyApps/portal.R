
## portal page ####
output$portal_page <- renderUI({

  req(input$portal_tab == "portal", input$portal_id)
  
  div(
    class="portal-page",
    
    h4(class="highlight-title", HTML(paste0("<b style='color: #3182bd;'>Project</b> - ", projectlist$Project[which(projectlist$Portal == input$portal_id)], ", <b style='color: #3182bd;'>Cell-Line</b> - ", projectlist$Cell_Line[which(projectlist$Portal == input$portal_id)]))),
    br(),
    
    fluidRow(
      column(
        width=2,
        selectInput(inputId="portal_id", label=NULL, choices=projectlist$Portal, selected=input$portal_id, width="auto")
      ),
      column(
        width=8,
        downloadButton(outputId="download_portal", label="Download Portal Data", style="padding: 5px 10px 5px 10px", class="my-btn btn btn-primary")
      )
    ),
    h4(class="highlight-title", "Description"),
    HTML(paste0("<p>", projectlist$Description[which(projectlist$Portal == input$portal_id)], "</p>")),
    br(),
    uiOutput(outputId="portal_main_page")
  )  
  
})

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



