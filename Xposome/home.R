

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
  
  projectlist <- projectlist; Project <- NULL;
  
  #print(projectlist)
  
  if(all(!is.na(projectlist$Project))){
    
    for(i in 1:nrow(projectlist)){
      Project <- c(Project, paste0('<a onclick="curlinkFun(&#39;', projectlist$Portal[i], '&#39;)" href="?', projectlist$Portal[i], '" class="portal-link" id="', projectlist$Portal[i], '" value="', projectlist$Portal[i], '">', projectlist$Project[i], '</a>'))
    }
    
    table <- data.frame(
      Project=Project,
      Cell_line=projectlist$Cell_Line,
      Description=projectlist$Description
    )
    
  }else{
    
    table <- data.frame(
      Project=paste0("<br>"),
      Cell_line=paste0("<br>"),
      Description=paste0("<br>")
    )
    
  }
  
  colnames(table) <- c("Project", "Cell line", "Description")
  
  return(table)

}, escape = FALSE, server = TRUE, rownames=FALSE, selection = "none",
options = list(
  columnDefs = list(list(className = 'dt-center', targets = "_all")),
  deferRender = FALSE,
  paging = TRUE,
  searching = TRUE,
  ordering = TRUE,
  pageLength = 20,
  scrollX = TRUE,
  scrollY = 400,
  scrollCollapse = TRUE,
  dom = 'T<"clear">Blfrtip',
  buttons=c('copy','csv','print')
))

