

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
  
  column(
    width=12,
      DT::dataTableOutput("annotation_table") %>% withSpinner(type=4, color="#0dc5c1")
  )
)





