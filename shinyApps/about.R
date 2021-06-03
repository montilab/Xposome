

#about page
output$about_page <- renderUI({
  
  req(input$portal_tab == "about")
  
  #####<!-- START ABOUT PAGE -->
  div(
    class="about-page",
    includeHTML("about.html")
  )
  #####<!-- END ABOUT PAGE -->
  
})
