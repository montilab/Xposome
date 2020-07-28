

#contact page
output$pageStub <- renderUI({
  
  #####<!-- START CONTRACT PAGE -->
  fluidRow(
    class="contact-page",
    
    column(
      width=12,
      
      includeMarkdown(paste0("www/RMD/contact_page.Rmd"))
    )
    
  )#####<!-- END CONTRACT PAGE -->

})
