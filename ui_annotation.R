

##Annotation Page####
fluidRow(
  class="portal-annotation",
  
  column(
    width=12,
    radioButtons(
      inputId = "annot_table_selection",
      label = "Annotation Type:",
      choices = c("Chemicals", "Samples"),
      selected = "Chemicals",
      inline = TRUE
    )
  ),
  
  br(),
  
  column(
    width=12, 
    div(
      id="loading-annotation",
      div(class="loader"),
      h4("Loading...", id="loading_text")
    )
  ),
  
  column(
    width=12,
    DT::dataTableOutput("annotation_table") 
  )
)
  
  



