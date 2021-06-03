

#contact page
output$contact_page <- renderUI({
  
  req(input$portal_tab == "contact")
  
  #####<!-- START CONTRACT PAGE -->
  div(
    class="contact-page",
    includeMarkdown(paste0("www/RMD/contact_page.Rmd")),
  )
  #####<!-- END CONTRACT PAGE -->

})
