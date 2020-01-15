

#Annotation Table####
output$annotation_table <- DT::renderDataTable({
   
  req(input$annot_table_selection) 
  
  if(input$annot_table_selection == "Chemicals"){
   
    if("?ADIPO" %in% session$clientData$url_search){
      table <- chemical_info
    }else{
      table <- dat[["Chemical Annotation"]]
    }
    
  }else if(input$annot_table_selection == "Samples"){
    
    if("?ADIPO" %in% session$clientData$url_search){
      table <- profile_info
    }else{
      table <- dat[["Profile Annotation"]]
    }
    
  }
  
  return(data.table.round(table))
  
}, escape = FALSE, extensions = 'Buttons', server = TRUE, colnames = c('Entry'=1), class = "display",
options = list(
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
