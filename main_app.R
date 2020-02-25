
# Preproccessing data #####
source("startup_1.R", local=TRUE)
source("startup_2.R", local=TRUE)
source("server_annotation.R", local=TRUE)
source("server_chemical.R", local=TRUE)
source("server_marker.R", local=TRUE)
source("server_heatmap.R", local=TRUE)
source("server_taxonomic_clustering.R", local=TRUE)
source("server_compare_multiple.R", local=TRUE)

# The main page layout ####
output$pageStub <- renderUI({
  
  fluidRow(
    id="navbar-page", style="background: white; padding-bottom: 20px",
    
    column(
      width=12,
      
      navbarPage(
        title=actionLink(inputId="main_link", label=strong(paste(substr(session$clientData$url_search, 2, nchar(session$clientData$url_search)), "Portal"))), id="main_page", position=c("static-top"), collapsible=TRUE, selected="About",
        
        ###About####
        tabPanel(
          title="About", value="About",
          source("ui_about.R", local=TRUE)$value
        ),
        
        ###Annotation#####
        tabPanel(
          title="Annotation", value="Annotation",
          source("ui_annotation.R", local=TRUE)$value
        ),
        
        ###Chemical Explorer#####
        tabPanel(
          title = "Chemical Explorer", value = "Chemical Explorer",
          #"hello"
          source("ui_chemical.R", local=TRUE)$value
        ),
        
        ###Marker Explorer####
        tabPanel(
          title = "Marker Explorer", value = "Marker Explorer",
          #"hello"
          source("ui_marker.R", local=TRUE)$value
        ),
        
        ###Heatmap Explorer####
        tabPanel(
          title = "Heatmap Explorer", value = "Heatmap Explorer",
          #"hello"
          source("ui_heatmap.R", local=TRUE)$value
        ),
        
        ###Taxonomic Clustering####
        navbarMenu(
          title = "Taxonomic Clustering",
          
          tabPanel(
            title = "Instructions", value = "Instructions",
            source("ui_taxonomic_clustering_instructions.R", local=TRUE)$value
          ),
          
          tabPanel(
            title = "K2 Taxanomer Results", value = "K2_Taxanomer_Results",
            #"Hello World!"
            source("ui_taxonomic_clustering.R", local=TRUE)$value
          ),
          
          tabPanel(
            title = "Compare Multiple", value = "Compare_Multiple",
            #"Hello World!"
            source("ui_compare_multiple.R", local=TRUE)$value
          )
        )
      )
    )
  )
    
})

# Go back to home page when the logo link is clicked on ####
observeEvent(input$main_link, {
  updateNavbarPage(session, inputId="main_page", selected = "About")
}, ignoreInit=TRUE)



  