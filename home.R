

#home page
output$pageStub <- renderUI({
  
  #####<!-- START HOME PAGE -->
  div(
    id="home-page", 
    
    fluidRow(
      class="header-section", style="background: white;",
      
      column(
        width=12,
        includeHTML("home.html")
      )
    )
  )#####<!-- END HOME PAGE -->

})
