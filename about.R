

#about page
output$pageStub <- renderUI({
  
  #####<!-- START ABOUT PAGE -->
  div(
    id="about-page",
    
    tags$section(
      class="header-section", style="background: white;",
      fluidRow(
        column(
          width=12,
          includeHTML("about.html")
        )
      )
    )
  )#####<!-- END ABOUT PAGE -->

})
