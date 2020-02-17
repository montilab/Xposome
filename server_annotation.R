

#Annotation Table####
output$annotation_table <- DT::renderDataTable({
   
  req(input$annot_table_selection) 
  
  if(input$annot_table_selection == "Chemicals"){
   
      table <- dat[["Chemical Annotation"]]
      
  }else if(input$annot_table_selection == "Samples"){
    
      table <- dat[["Profile Annotation"]]
      
  }
  
  return(data.table.round(table))
  
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
