

## home page ####
output$pageStub <- renderUI({
  
  #####<!-- START HOME PAGE -->
  fluidRow(
    class="home-page",
    
    column(
      width=12,
      
      DT::dataTableOutput(outputId = "main_table")
    )
    
  )#####<!-- END HOME PAGE -->

})

## output main table ####
output$main_table <- DT::renderDataTable({
  
  req(projectlist) 
  
  projectlist <- projectlist; Project <- NULL;
  
  for(i in 1:nrow(projectlist)){
    Project <- c(Project, paste0('<a onclick="curlinkFun(&#39;', projectlist$Portal[i], '&#39;)" href="?', projectlist$Portal[i], '" class="portal-link" id="', projectlist$Portal[i], '">', projectlist$Project[i], '</a>'))
  }
  
  table <- data.frame(
    Project=Project,
    Cell_line=projectlist$Cell_Line,
    Description=projectlist$Description
  )
  colnames(table) <- c("Project", "Cell line", "Description")
  
  return(table)

}, escape = FALSE, server = TRUE, rownames=FALSE, selection = "none", 
options = list(
  dom="T",
  columnDefs = list(
    list(className = 'dt-center', targets = "_all")
  )
))

