

## home page ####
output$home_page <- renderUI({
  
  req(input$portal_tab == "home")
  
  #####<!-- START HOME PAGE -->
  includeHTML("description.html")
  #####<!-- END HOME PAGE -->

})

## Add the portal selected input options
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

## Add the portal selected inputs
output$portal_choices <- renderUI({
  
  selectInput(inputId="portal_choice", label=NULL, choices=c("Project", "Cell-Line"), selected="Project", width="100px")
  
})

## Add the search button
output$portal_search <- renderUI({

  choice <- input$portal_choice; input <- input$portal_input;
  
  if(!is.null(input)){
    if(input==""){
      HTML(paste0('<button id="portal_submit" style="margin-left: 1px; padding: 5px 5px 5px 5px;" class="btn btn-success">Search</button>'))
    }else{
      if(choice == "Project"){
        portal_id <- projectlist$Portal[which(projectlist$Project == input)]
      }else{
        portal_id <- projectlist$Portal[which(projectlist$Cell_Line == input)]
      }
      
      # Update the url link
      HTML(paste0('<a href="?page=', portal_id, '&tab=annotation" id="portal_submit" style="margin-left: 1px; padding: 5px 5px 5px 5px;" class="btn btn-success">Search</a>'))
      
    }
  }
  
})

