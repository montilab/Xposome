

# Output the chemical selection ####
observeEvent(chemical_dat(), {
  
  chemical_dat() %...>% {
    
    dat <- .
    
    chemicals <- list(
      `Chemical Name`=sort(unique(dat$Chemical_Name[which(!dat$Chemical_Name %in% c(NA, ""))])),
      `BUID`=sort(unique(dat$BUID[which(!dat$BUID %in% c(NA, "") & !dat$Chemical_Name %in% c(NA, ""))])),
      `CAS`=sort(unique(dat$CAS[which(!dat$CAS %in% c(NA, "") & !dat$Chemical_Name %in% c(NA, ""))]))
    )
  
    updateSelectInput(session, inputId="chem", choices=c("Please select an option below"="", chemicals), selected=chemical_id)
    
  }
  
})

## Go back to home page when the logo link is clicked on ####
observeEvent({
  input$chem
}, {
  
  if(input$main_page == "chemical_explorer"){
    if(is.null(input$chem) | input$chem == ""){
      updateQueryString(paste0("?page=", fname, "&tab=", input$main_page), mode="push") 
    }else{
      updateQueryString(paste0("?page=", fname, "&tab=", input$main_page, "&chemical_id=", input$chem, "&stat=", input$chemical_tab), mode="push") 
    }
  }
  
})

## Go back to home page when the logo link is clicked on ####
observeEvent({
  input$chemical_tab
}, {
  
  if(input$main_page == "chemical_explorer"){
    if(is.null(input$chem) | input$chem == ""){
      updateQueryString(paste0("?page=", fname, "&tab=", input$main_page), mode="push") 
    }else{
      updateQueryString(paste0("?page=", fname, "&tab=", input$main_page, "&chemical_id=", input$chem, "&stat=", input$chemical_tab), mode="push") 
    }
  }
  
})

##output chemical table####
output$chemical_table <- DT::renderDataTable({
  
  req(input$chem)
  
  chem=input$chem
  
  chemical_dat() %...>% {
    chemical_dat <- .
    pos <- get_chem_description(chemical_dat=chemical_dat, chem=chem, chemical_id=FALSE)
    table <- chemical_dat[pos,] %>% data.table.round()
  }
  
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
  
  req(annot_var(), input$chem, input$summarizefunc_de, input$filterbyinput_de, input$range_de, input$numberthresleft_de, input$numberthresright_de)
  
  chem=input$chem
  landmark_de=input$landmark_de
  summarizefunc_de=input$summarizefunc_de
  filterbyinput_de=input$filterbyinput_de
  range_de=input$range_de
  numberthresleft_de=input$numberthresleft_de
  numberthresright_de=input$numberthresright_de
  
  promise_all(annot_var=annot_var(), profile_dat=profile_dat(), chemical_dat=chemical_dat(), expression_dat=expression_dat()) %...>% with({
    get_de(
      chem=chem,
      annot_var=annot_var,
      profile_dat=profile_dat,
      chemical_dat=chemical_dat,
      expression_dat=expression_dat,
      header = "ModZScore",
      summarize.func = summarizefunc_de,
      landmark = landmark_de,
      do.nmarkers = "number" %in% filterbyinput_de,
      nmarkers = c(numberthresleft_de, numberthresright_de),
      do.scorecutoff = "score" %in% filterbyinput_de,
      scorecutoff = c(range_de[1], range_de[2])
    ) %>% data.table.round()
    
  }) %...!% { return(NULL) }
  
})

##Output the gene expression table####
output$gene_expression_table <- DT::renderDataTable({

  req(expression_table())

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
  
  req(annot_var(), input$chem, input$gsname, input$gsmethod, input$summarize_gs)
  
  chem=input$chem
  gsname=input$gsname
  gsmethod=input$gsmethod
  summarize_gs=input$summarize_gs
  
  promise_all(annot_var=annot_var(), profile_dat=profile_dat(), chemical_dat=chemical_dat(), gs_enrichment_dat=gs_enrichment_dat()) %...>% with({
    
    get_gsenrichment(
      chem=chem,
      annot_var=annot_var,
      profile_dat=profile_dat,
      chemical_dat=chemical_dat,
      expression_dat=gs_enrichment_dat[[paste0(dsmap[[gsname]], "_", gsmethod)]],
      gsname = gsname,
      header="GS Score",
      summarize.func=summarize_gs
    ) %>% data.table.round()
    
  }) %...!% { return(NULL) }
  
})

#Output the gene set enrichment table####
output$gene_set_enrichment_table <- DT::renderDataTable({

  req(gs_enrichment_table())

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
  
  req(annot_var(), input$chem, input$conn_name, input$summarizefunc_conn)
  
  chem=input$chem
  conn_name=input$conn_name
  summarizefunc_conn=input$summarizefunc_conn

  promise_all(annot_var=annot_var(), profile_dat=profile_dat(), chemical_dat=chemical_dat(), connectivity_dat=connectivity_dat()) %...>% with({
    
    get_connectivity(
      chem=chem,
      annot_var=annot_var,
      profile_dat=profile_dat,
      chemical_dat=chemical_dat,
      expression_dat=connectivity_dat[[conn_name]],
      header="Connectivity Score",
      summarize.func=summarizefunc_conn
    ) %>% data.table.round()
    
  }) %...!% { return(data.frame(Connectivity_Id="NO CONNECTIVITY MAP")) }
  
})


#Output connectivity table####
output$connectivity_table <- DT::renderDataTable({
  
  req(connectivity_table()) %...>% {
    
    conn_table <- .
    
    if(all(!conn_table$Connectivity_Id %in% "NO CONNECTIVITY MAP")){
      
      conn_table %>% datatable(
        rownames = FALSE,
        escape = FALSE,
        extensions = 'Buttons',
        selection = "none",
        options = list(
          columnDefs = list(
            list(className = 'dt-left', targets = 0),
            list(className = 'dt-center', targets = 1:(ncol(conn_table)-1))
          ),
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
        )
      )
      
    }else{
      
      conn_table <- data.frame(WARNING="There is no connectivity map for this portal.");

      conn_table %>% datatable(
        rownames = FALSE,
        escape = FALSE,
        extensions = 'Buttons',
        selection = "single",
        options = list(
          columnDefs = list(list(className = 'dt-center', targets = "_all")),
          deferRender = FALSE,
          scrollX = TRUE,
          scrollCollapse = TRUE,
          dom = 'T'
        )
      )
      
    }
    
  }  
})

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


