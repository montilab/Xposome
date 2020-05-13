

##Update chemical list####
observeEvent(chemical_dat(), {
  
  # Create a list of chemicals without duplicated names ####
  chemicals <- list(
    `Chemical Name`=sort(unique(chemical_dat()$Chemical_Name[which(!chemical_dat()$Chemical_Name %in% c(NA, ""))])),
    `BUID`=sort(unique(chemical_dat()$BUID[which(!chemical_dat()$BUID %in% c(NA, ""))])),
    `CAS`=sort(unique(chemical_dat()$CAS[which(!chemical_dat()$CAS %in% c(NA, ""))]))
  )

  updateSelectInput(session, inputId = "chem", choices = c("Please select an option below" = "", chemicals))

})

##Output the chemical table####
output$chemical_table <- DT::renderDataTable({
  
  req(input$chem, chemical_dat())

  pos <- get_chem_description(chemical_dat=chemical_dat(), chem=input$chem, BUID=FALSE)
  table <- chemical_dat()[pos,] %>% data.table.round()
  
  return(table)
  
}, escape = FALSE, extensions = 'Buttons', server = TRUE, rownames=FALSE, selection = "none",
options = list(
  columnDefs = list(list(className = 'dt-left', targets = "_all")),
  scrollX = TRUE,
  dom = 'T'
))

##Output the gene expression table####
output$gene_expression_table <- DT::renderDataTable({

  req(annot_var(), profile_dat(), chemical_dat(), expression_dat(), input$chem, input$filterbyinput_de, input$filterbyinput_de, input$range_de, input$numberthresleft_de, input$numberthresright_de, input$summarizefunc_de)

  table <- get_de(
    chem=input$chem, 
    annot_var=annot_var(),
    profile_dat=profile_dat(), 
    chemical_dat=chemical_dat(),
    expression_dat=expression_dat(),
    header = "ModZScore",
    summarize.func = input$summarizefunc_de,
    landmark = input$landmark_de, 
    do.nmarkers = "number" %in% input$filterbyinput_de,
    nmarkers = c(input$numberthresleft_de, input$numberthresright_de),
    do.scorecutoff = "score" %in% input$filterbyinput_de,
    scorecutoff = c(input$range_de[1], input$range_de[2])
  ) %>% data.table.round()
  
  return(table)
  
}, escape = FALSE, extensions = 'Buttons', server = TRUE, rownames=FALSE, selection = "none",
options = list(
  columnDefs = list(list(className = 'dt-center', targets = "_all")),
  rowCallback = JS( 'function(row, data) { $("td:eq(0)", row).css("text-align", "left"); }'),
  headerCallback =  JS( 'function(thead, data) { $(thead).find("th").eq(0).css("text-align", "left"); }'),
  deferRender = FALSE,
  paging = TRUE,
  searching = TRUE,
  ordering = TRUE,
  pageLength = 20,
  scrollX = TRUE,
  scrollY = 400,
  scrollCollapse = TRUE,
  dom = 'T<"clear">Blfrtip',
  buttons=c('copy','csv','print'))
)

#Output the gene set enrichment table####
output$gene_set_enrichment_table <- DT::renderDataTable({

  req(annot_var(), profile_dat(), chemical_dat(), gs_enrichment_dat(), input$chem, input$gsname, input$gsmethod, input$summarize_gs)
  
  table <- get_gsenrichment(
    chem=input$chem, 
    annot_var=annot_var(),
    profile_dat=profile_dat(), 
    chemical_dat=chemical_dat(),
    expression_dat=gs_enrichment_dat()[[paste0(dsmap()[[input$gsname]], "_", input$gsmethod)]],
    gsname = input$gsname,
    header="GS Score",
    summarize.func=input$summarize_gs
  ) %>% data.table.round()
  
  return(table)

}, escape = FALSE, extensions = 'Buttons', server = TRUE, rownames=FALSE, selection = "none",
options = list(
  columnDefs = list(list(className = 'dt-center', targets = "_all")),
  rowCallback = JS( 'function(row, data) { $("td:eq(0)", row).css("text-align", "left"); }'),
  headerCallback =  JS( 'function(thead, data) { $(thead).find("th").eq(0).css("text-align", "left"); }'),
  deferRender = FALSE,
  paging = TRUE,
  searching = TRUE,
  ordering = TRUE,
  pageLength = 20,
  scrollX = TRUE,
  scrollY = 400,
  scrollCollapse = TRUE,
  dom = 'T<"clear">Blfrtip',
  buttons=c('copy','csv','print'))
)


#Output connectivity table####
output$connectivity_table <- DT::renderDataTable({

  req(annot_var(), profile_dat(), chemical_dat(), connectivity_dat(), input$chem, input$conn_name, input$summarizefunc_conn)

  table <- get_connectivity(
    chem=input$chem, 
    annot_var=annot_var(),
    profile_dat=profile_dat(), 
    chemical_dat=chemical_dat(),
    expression_dat=connectivity_dat()[[input$conn_name]],
    header="Connectivity Score",
    summarize.func=input$summarizefunc_conn
  ) %>% data.table.round()
  
  return(table)

}, escape = FALSE, extensions = 'Buttons', server = TRUE, rownames=FALSE, selection = "none",
options = list(
  columnDefs = list(
    list(className = 'dt-center', targets = "_all")
  ),
  rowCallback = JS( 'function(row, data) { $("td:eq(0)", row).css("text-align", "left"); }'),
  headerCallback =  JS( 'function(thead, data) { $(thead).find("th").eq(0).css("text-align", "left"); }'),
  deferRender = FALSE,
  paging = TRUE,
  searching = TRUE,
  ordering = TRUE,
  pageLength = 20,
  scrollX = TRUE,
  scrollY = 400,
  scrollCollapse = TRUE,
  dom = 'T<"clear">Blfrtip',
  buttons=c('copy','csv','print'))
)

###################################################
#
# GENE EXPRESSION
#
###################################################

#observe when restore button is clicked####
observeEvent(input$de_restore, {
  updateCheckboxInput(session, inputId = "landmark_de", value = defaults[["landmark_de"]])
  updateSelectInput(session, inputId = "summarizefunc_de", selected = defaults[["summarizefunc_de"]])
  updateCheckboxGroupInput(session, inputId = "filterbyinput_de", selected = defaults[["filterbyinput_de"]])
  updateSelectInput(session, inputId = "range_de", selected = defaults[["range_de"]])
  updateSliderInput(session, inputId = "numberthresleft_de", value = defaults[["numberthresleft_de"]])
  updateSliderInput(session, inputId = "numberthresright_de", value = defaults[["numberthresright_de"]])
}, ignoreInit=TRUE)

##Observe when hide button is clicked####
observeEvent(input$de_hide, {
  updateCollapse(session, "de_opt_panel", close = "de_options")
}, ignoreInit=TRUE)


##Observe when show button is clicked####
observeEvent(input$de_show, {
  updateCollapse(session, "de_opt_panel", open = "de_options")
}, ignoreInit=TRUE)


###################################################
#
# GENE SET ENRICHMENT
#
###################################################

#observe when restore button is clicked####
observeEvent(input$es_restore, {
  updateSelectInput(session, inputId = "gsname", selected = "Hallmark")
  updateSelectInput(session, inputId = "gsmethod", selected = "gsva")
  updateSelectInput(session, inputId = "summarize_gs", selected = "median")
}, ignoreInit=TRUE)

##Observe when hide button is clicked####
observeEvent(input$es_hide, {
  updateCollapse(session, "es_opt_panel", close = "es_options")
}, ignoreInit=TRUE)


##Observe when show button is clicked####
observeEvent(input$es_show, {
  updateCollapse(session, "es_opt_panel", open = "es_options")
}, ignoreInit=TRUE)


###################################################
#
# CONNECTIVITY
#
###################################################

#observe when restore button is clicked####
observeEvent(input$conn_restore, {
  updateSelectInput(session, inputId = "conn_name", selected = "pcl")
  updateSelectInput(session, inputId = "summarizefunc_conn", selected = "median")
}, ignoreInit=TRUE)

##Observe when hide button is clicked####
observeEvent(input$conn_hide, {
  updateCollapse(session, "conn_opt_panel", close = "conn_options")
}, ignoreInit=TRUE)


##Observe when show button is clicked####
observeEvent(input$conn_show, {
  updateCollapse(session, "conn_opt_panel", open = "conn_options")
}, ignoreInit=TRUE)


