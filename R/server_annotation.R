

#Annotation Table####
output$annotation_table <- DT::renderDataTable({
  
  selection = input$annot_table_selection
  
  if(selection == "Chemicals"){
    
    chemical_dat() %...>% data.table.round()
    
  }else if(selection == "Samples"){
    
    profile_dat() %...>% data.table.round()
    
  }
  
}, escape = FALSE, extensions = 'Buttons', server = TRUE, rownames=FALSE, selection = "none",
options = list(
  columnDefs = list(list(className = 'dt-left', targets = "_all")),
  paging = TRUE,
  searching = TRUE,
  pageLength = 20, 
  deferRender = FALSE,
  scrollX = TRUE, 
  scrollY = 400,
  scrollCollapse = TRUE,
  ordering = TRUE,
  dom = 'T<"clear">Blfrtip', 
  buttons=c('copy','csv','print'))
)
