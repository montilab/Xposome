

logoutUI <- function(id, label="Log out", icon=NULL, class="btn-danger", style="color: white;") {
  
  ns <- shiny::NS(id)
  
  shiny::actionLink(inputId=ns("button"), class="button-link", icon=tags$i(class="fa fa-user-circle"), label="Sign Out")  
}

logout <- function(input, output, session, active) {
  
  shiny::observeEvent(active(), ignoreInit = TRUE, {
    
    ##remove warning message####
    project_table_message("")
    login_table_message("")
    
    if(nrow(projectdata())==0){
      
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
      
      write.csv(data, "data/Project_List.csv", row.names=FALSE, na="")
      
    }else{
      
      ##Read in the project list###
      data <- read_csv(paste0("data/Project_List.csv"))
      
      ##Get all files in the data folder###
      data_files <- list.files("data/")
      
      ##Create a list of wanted folders and files####
      wanted_files <- c("Connectivity Map", "Enrichment Gene Set", "Landmark", "Project_List.csv", "Template", "User_Login_List.csv") 
      
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
    }
    
    if(nrow(logindata())==0){
      
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
      
    }
    
    shinyjs::toggle(id = "button", anim = TRUE, time = 1, animType = "fade")
    
  })
  
  # return reactive logout button tracker
  shiny::reactive({ input$button })
  
}


