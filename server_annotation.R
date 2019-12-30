

#Annotation Table####
observeEvent(input$annot_table_selection, {
  
  output$annotation_table <- DT::renderDataTable({
    
    if(input$annot_table_selection == "Chemicals"){
      table <- dat[["Chemical Annotation"]]
    }else if(input$annot_table_selection == "Samples"){
      table <- dat[["Profile Annotation"]]
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

}, ignoreInit=TRUE)
