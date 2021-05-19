

## home page ####
output$pageStub <- renderUI({
  
  #####<!-- START HOME PAGE -->
  
  includeHTML("description.html")
  
  #####<!-- END HOME PAGE -->

})

output$portal_inputs <- renderUI({
  
  req(input$portal_choice, projectlist)
  
  choice <- input$portal_choice
  
  if(choice == "Project"){
    choices <- projectlist$Project[!is.na(projectlist$Project)]
  }else{
    choices <- projectlist$Cell_Line[!is.na(projectlist$Cell_Line)]
  }
  
  selectInput(
    inputId = "portal_input",
    label = NULL,
    choices = c("Search by" = "", choices)
  )
  
})

output$portal_choices <- renderUI({
  
  selectInput(inputId="portal_choice", label=NULL, choices=c("Project", "Cell-Line"), selected="Project", width="100px")
  
})


output$portal_search <- renderUI({

  choice <- input$portal_choice; input <- input$portal_input;
  
  if(length(input)==0){
    portal_id <- "ADIPO"
  }else{
    if(choice == "Project"){
      portal_id <- projectlist$Portal[which(projectlist$Project == input)]
    }else{
      portal_id <- projectlist$Portal[which(projectlist$Cell_Line == input)]
    }
  }
  
  # Select the portal link
  selected_portal(portal_id)
  
  # Update the url link
  HTML(paste0('<a onclick="curlinkFun(\'portal\')" href="?page=', portal_id,'" id="portal_submit" style="margin-left: 1px; padding: 5px 5px 5px 5px;" class="btn btn-success">Search</a>'))

})

