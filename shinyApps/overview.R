

#about page
output$about_page <- renderUI({
  
  req(input$portal_tab == "overview")
  
  #####<!-- START ABOUT PAGE -->
  div(
    class="about-page",
    includeHTML("about.html")
  )
  #####<!-- END ABOUT PAGE -->
  
})
