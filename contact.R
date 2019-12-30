

#contact page
output$pageStub <- renderUI({
  
  #####<!-- START CONTRACT PAGE -->
  div(
    id="home-page",
    
    tags$section(
      class="header-section", style="background: white;",
      fluidRow(
        column(
          width=12,
          includeHTML("contact.html")
        )
      )
    )
  )#####<!-- END CONTRACT PAGE -->

})
