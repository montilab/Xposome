

##The annotation page####
fluidRow(
  column(
    width=12,
    radioButtons(
      inputId = "annot_table_selection",
      label = "Annotation Type",
      choices = c("Chemicals", "Samples"),
      selected = "Chemicals",
      inline = TRUE
    )
  ),

  column(
    width=12,
      DT::dataTableOutput("annotation_table") %>% withSpinner(color="#0dc5c1", proxy.height="200px")
  )
)





