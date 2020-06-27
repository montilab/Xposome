
# The main page layout ####
output$pageStub <- renderUI({
  
  div(
    class="main-page",
    
    navbarPage(
      title=actionLink(inputId="main_link", label=strong(fname, "Portal")), id="main_page", position=c("static-top"), collapsible=TRUE, selected="About",
      
      ###About####
      tabPanel(
        title="About", value="About",
        source("R/ui_about.R", local=TRUE)$value
      ),
      
      ###Annotation#####
      tabPanel(
        title="Annotation", value="Annotation",
        source("R/ui_annotation.R", local=TRUE)$value
      ),

      ###Chemical Explorer#####
      tabPanel(
        title = "Chemical Explorer", value = "Chemical Explorer",
        #"Hello World!"
        source("R/ui_chemical.R", local=TRUE)$value
      ),

      ###Marker Explorer####
      tabPanel(
        title = "Marker Explorer", value = "Marker Explorer",
        #"Hello World!"
        source("R/ui_marker.R", local=TRUE)$value
      ),

      ###Heatmap Explorer####
      tabPanel(
        title = "Heatmap Explorer", value = "Heatmap Explorer",
        #"Hello World!"
        source("R/ui_heatmap.R", local=TRUE)$value
      ),

      ###Taxonomic Clustering####
      navbarMenu(
        title = "Taxonomic Clustering",

        tabPanel(
          title = "K2 Taxanomer Results", value = "K2 Taxanomer Results",
          #"Hello World!"
          source("R/ui_taxonomic_clustering.R", local=TRUE)$value
        ),

        tabPanel(
          title = "Compare Multiple", value = "Compare Multiple",
          #"Hello World!"
          source("R/ui_compare_multiple.R", local=TRUE)$value
        )
      )
    )
  )
  
})

# Go back to home page when the logo link is clicked on ####
observeEvent(input$main_link, {
  updateNavbarPage(session, inputId="main_page", selected = "About")
}, ignoreInit=TRUE)

# # Go back to home page when the logo link is clicked on ####
observeEvent(input$main_page, {
  
  if(input$main_page == "K2 Taxanomer Results"){
    session$sendCustomMessage("ResizeK2Table", "clusteringTable")
  }
  
})
