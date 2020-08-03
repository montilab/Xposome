

#Annotation Table####
output$annotation_table <- DT::renderDataTable({
  
  selection = input$annot_table_selection
  
  if(selection == "Chemicals"){
    
    chemical_dat() %...>% data.table.round()
    
  }else if(selection == "Samples"){
    
    profile_dat() %...>% data.table.round()
    
  }
  
}, escape = FALSE, extensions = 'Buttons', server = TRUE, rownames=FALSE, selection = "single",
options = list(
  columnDefs = list(list(className = 'dt-left', targets = "_all")),
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
