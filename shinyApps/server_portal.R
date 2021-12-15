

# Download the portal data
output$download_portal <- downloadHandler(
  
  filename = function() {
    paste0(input$portal_id, "-portal.zip")
  },
  
  content = function(file) {
      
    withProgress(message = "Downloading: ", value = 0, {
      
      fs <- c()
      tmpdir <- tempdir()
      file_names <- c()
      
      # increment the progress bar
      incProgress(1/6, detail = "profile annotation")
      
      # zip the files
      file_names <- c(file_names, "Profile_Annotation")
      
      profile_dat <- readRDS(paste0("www/data/", input$portal_id, "/Profile_Annotation.RDS"))
      
      saveRDS(profile_dat, file.path(tmpdir, "Profile_Annotation.RDS"))
      
      # increment the progress bar
      incProgress(2/6, detail = "chemical annotation")
      
      # zip the files
      file_names <- c(file_names, "Chemical_Annotation")
      
      chemical_dat <- readRDS(paste0("www/data/", input$portal_id, "/Chemical_Annotation.RDS"))
      
      saveRDS(chemical_dat, file.path(tmpdir, "Chemical_Annotation.RDS"))
      
      # increment the progress bar
      incProgress(3/6, detail = "expression set")
      
      # zip the files
      file_names <- c(file_names, "Gene_Expression")
      
      expression_dat <- readRDS(paste0("www/data/", input$portal_id, "/Gene_Expression.RDS"))
      
      saveRDS(expression_dat, file.path(tmpdir, "Gene_Expression.RDS"))
      
      # increment the progress bar
      incProgress(4/6, detail = "connectivity map")
      
      connectivity_dat <- tryCatch({
        
        readRDS(paste0("www/data/", input$portal_id, "/Connectivity.RDS"))
        
      }, error=function(e){
        
        data.frame(warning="There is no connectivity map for this portal.")
        
      })
      
      if(!is.null(connectivity_dat)){
        # zip the files
        file_names <- c(file_names, "Connectivity")
        saveRDS(connectivity_dat, file.path(tmpdir, "Connectivity.RDS"))
      }
      
      saveRDS(connectivity_dat, file.path(tmpdir, "Connectivity.RDS")) 
      
      # increment the progress bar
      incProgress(5/6, detail = "gene set enrichment")
      
      # zip the files
      file_names <- c(file_names, "Gene_Set_Enrichment")
      
      gs_enrichment_dat <- readRDS(paste0("www/data/", input$portal_id, "/Gene_Set_Enrichment.RDS"))
      
      saveRDS(gs_enrichment_dat, file.path(tmpdir, "Gene_Set_Enrichment.RDS"))
      
      # increment the progress bar
      incProgress(6/6, detail = "K2-taxonomer")
      
      # zip the files
      file_names <- c(file_names, "K2Taxonomer")
      
      # Read in K2 Taxonomer data ####
      K2summary <- readRDS(paste0("www/data/", input$portal_id, "/K2Taxonomer.RDS"))
      
      saveRDS(K2summary, file.path(tmpdir, "K2Taxonomer.RDS"))
      
      for(names in file_names){
        path <- file.path(tmpdir, paste0(names, ".RDS"))
        fs <- c(fs, path)
      }
      
      zip(zipfile=file, files=fs, flags = "-r9Xj")
      
    })
    
  },
  
  contentType = "application/zip"
  
)



