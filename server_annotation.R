

#Annotation Table####
output$annotation_table <- DT::renderDataTable({
   
  req(input$annot_table_selection) 
  
  ## Hide loader and show content ####
  shinyjs::hide(id = "loading-annotation", anim = TRUE, animType = "fade")
  
  if(input$annot_table_selection == "Chemicals"){
    
    chemical_dat() 
    
  }else if(input$annot_table_selection == "Samples"){
    
    profile_dat()
    
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
