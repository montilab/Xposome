
##output chemical table####
output$chemical_table <- DT::renderDataTable({
  
  req(input$chem)
  
  chem <- input$chem
  
  chemical_dat() %...>% 
    get_chem_description(chemical_dat=., chem=chem, chemical_id=FALSE) %...>% 
    data.table.round() %...!% { return(NULL) }
  
}, escape = FALSE, extensions = 'Buttons', server = TRUE, rownames=FALSE, selection = "none",
options = list(
  columnDefs = list(list(className = 'dt-left', targets = "_all")),
  scrollX = TRUE,
  scrollCollapse = TRUE,
  paging = FALSE,
  searching = FALSE,
  dom = 'T<"clear">Blfrtip',
  buttons=c('copy','csv','print')
))


##Get the gene expression table####
expression_table <- reactive({
  
  chem = input$chem;

  promise_all(profile_dat=profile_dat(), chemical_dat=chemical_dat(), expression_dat=expression_dat()) %...>% 
    with({
      get_de(
        chem = chem,
        profile_dat = profile_dat,
        chemical_dat = chemical_dat,
        expression_dat = expression_dat,
        header = "ModZScore"
      ) %>% data.table.round()
    }) %...!% { return(NULL) }
  
}) %>% bindCache(input$portal_id, input$chem) %>% 
  bindEvent(input$portal_id, input$chem)


##Output the gene expression table####
output$gene_expression_table <- DT::renderDataTable({
  
  req(input$chem)
  
  landmark_de = input$landmark_de;
  summarizefunc_de = input$summarizefunc_de;
  filterbyinput_de = input$filterbyinput_de;
  range_de = input$range_de;
  numberthresleft_de = input$numberthresleft_de;
  numberthresright_de = input$numberthresright_de;
  
  expression_table() %...>%
    get_de_filter(
      de_dat = ., 
      landmark_dat = landmark_dat,
      summarize.func = summarizefunc_de,
      landmark = landmark_de,
      do.nmarkers = "number" %in% filterbyinput_de,
      nmarkers = c(numberthresleft_de, numberthresright_de),
      do.scorecutoff = "score" %in% filterbyinput_de,
      scorecutoff = c(range_de[1], range_de[2])
    ) %...!% { return(NULL) }
  
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

#Get the gene set enrichment table####
gs_enrichment_table <- reactive({
  
  chem = input$chem
  gsname = input$gsname
  gsmethod = input$gsmethod
  
  promise_all(profile_dat=profile_dat(), chemical_dat=chemical_dat(), gs_enrichment_dat=gs_enrichment_dat()) %...>%
    with({
      get_gsenrichment(
        chem = chem,
        profile_dat = profile_dat,
        chemical_dat = chemical_dat,
        expression_dat = gs_enrichment_dat[[paste0(gsname, "_", gsmethod)]],
        header = "GS Score"
      ) %>% data.table.round()
    }) %...!% { return(NULL) }
  
}) %>% bindCache(input$portal_id, input$chem, input$gsname, input$gsmethod) %>%
  bindEvent(input$portal_id, input$chem, input$gsname, input$gsmethod)


#Output the gene set enrichment table####
output$gene_set_enrichment_table <- DT::renderDataTable({
  
  req(input$chem)
  
  portal_id = input$portal_id; 
  gsname = input$gsname; 
  summarize.func = input$summarizefunc_gs;

  gs_enrichment_table() %...>%
    get_gsenrichment_filter(
      gs_dat = .,
      gsname = gsname,
      summarize.func = summarize.func
    ) %...!% { return(NULL) }
  
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


#Get connectivity table####
connectivity_table <- reactive({
  
  chem = input$chem
  conn_name = input$conn_name
  
  promise_all(profile_dat=profile_dat(), chemical_dat=chemical_dat(), connectivity_dat=connectivity_dat()) %...>%
    with({
      get_connectivity(
        chem = chem,
        profile_dat = profile_dat,
        chemical_dat = chemical_dat,
        expression_dat = connectivity_dat[[conn_name]],
        header = "Connectivity Score"
      ) %>% data.table.round()
    }) %...!% {
      warning <- data.frame(WARNING="<span>There is no connectivity map for this portal.</span>")
      return(warning)
    }
  
}) %>% bindCache(input$portal_id, input$chem, input$conn_name) %>%
  bindEvent(input$portal_id, input$chem, input$conn_name)


#Output connectivity table####
output$connectivity_table <- DT::renderDataTable({
  
  req(input$chem)
  
  summarize.func = input$summarizefunc_conn
  
  connectivity_table() %...>% (function(data){
    if("WARNING" %in% colnames(data)){
      colnames(data) <- paste0("<b style='color:red'>WARNING!!!</b>")
      return(data)
    }else{
      get_connectivity_filter(
        conn_dat = data,
        summarize.func = summarize.func
      )
    }
  }) %...!% { return(NULL) }
  
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

###################################################
#
# GENE EXPRESSION
#
###################################################

#observe when restore button is clicked####
observeEvent(input$de_restore, {
  
  updateCheckboxInput(session, inputId = "landmark_de", value = de_defaults[["landmark_de"]])
  updateSelectInput(session, inputId = "summarizefunc_de", selected = de_defaults[["summarizefunc_de"]])
  updateCheckboxGroupInput(session, inputId = "filterbyinput_de", selected = de_defaults[["filterbyinput_de"]])
  updateSelectInput(session, inputId = "range_de", selected = de_defaults[["range_de"]])
  updateSliderInput(session, inputId = "numberthresleft_de", value = de_defaults[["numberthresleft_de"]])
  updateSliderInput(session, inputId = "numberthresright_de", value = de_defaults[["numberthresright_de"]])
  
})

##Observe when hide button is clicked####
observeEvent(input$de_hide, {
  
  shinyjs::hide(id="de_opt_panel")
  
})


##Observe when show button is clicked####
observeEvent(input$de_show, {
  
  shinyjs::show(id="de_opt_panel")
  
})


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
  
})

##Observe when hide button is clicked####
observeEvent(input$es_hide, {
  
  shinyjs::hide(id="es_opt_panel")
  
})


##Observe when show button is clicked####
observeEvent(input$es_show, {
  
  shinyjs::show(id="es_opt_panel")
  
})


###################################################
#
# CONNECTIVITY
#
###################################################

#observe when restore button is clicked####
observeEvent(input$conn_restore, {
  
  updateSelectInput(session, inputId = "conn_name", selected = "pcl")
  updateSelectInput(session, inputId = "summarizefunc_conn", selected = "median")
  
})


##Observe when hide button is clicked####
observeEvent(input$conn_hide, {
  
  shinyjs::hide(id="conn_opt_panel")
  
})


##Observe when show button is clicked####
observeEvent(input$conn_show, {
  
  shinyjs::show(id="conn_opt_panel")
  
})


