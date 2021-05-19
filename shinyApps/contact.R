

#contact page
output$pageStub <- renderUI({
  
  #####<!-- START CONTRACT PAGE -->
  div(class="contact-page",
      
      includeMarkdown(paste0("www/RMD/contact_page.Rmd")),

  )
  #####<!-- END CONTRACT PAGE -->

})
