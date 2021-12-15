
## Update tas selection ####
observeEvent({
  input$portal_id
}, {
  
  profile_dat() %...>% (function(data){
    max <- ceiling(max(data$TAS))  
    val <- round(max(data$TAS), 2) + 0.05
    updateSliderInput(session, inputId="marker_tas", label="TAS:", min=0, max=max, step=0.1, value=c(0, val))
  })
  
})


## Observe tas selection ####
observeEvent({
  input$marker_tas
}, {
  
  tas = input$marker_tas 
  
  profile_dat() %...>% (function(data){
    data <- data %>% filter(as.numeric(TAS) >= as.numeric(tas[1]) & as.numeric(TAS) <= as.numeric(tas[2]))
    if(nrow(data) == 0){
      shinyjs::hide(id="plot_content")
      shinyjs::show(id="warning_content")
    }else{
      shinyjs::hide(id="warning_content")
      shinyjs::show(id="plot_content")
    }
  })
  
})


## Update the gene expression selection ####
observeEvent({
  input$portal_id
  input$marker
}, {
  
  req(input$marker == 'Genes')
  
  selected_gene = input$marker_gene
  
  expression_dat() %...>% (function(eset){
    genelist <- sort(rownames(eset))
    updateSelectizeInput(session, inputId = "marker_gene", label = "Select a gene:", choices = c("Select an option below"="", genelist), selected=selected_gene)
  }) %...!% { return(NULL) }

})


## Update the gene set selection ####
observeEvent({
  input$portal_id
  input$marker
  input$marker_gsname
  input$marker_gsmethod
}, {

  req(input$marker == 'Gene Sets')
  
  selected_gs=input$marker_gs; gsname=input$marker_gsname; gsmethod=input$marker_gsmethod;
  
  gs_enrichment_dat() %...>% (function(eset){
    genesetlist <- sort(rownames(eset[[paste0(gsname, "_", gsmethod)]]))
    updateSelectizeInput(session, inputId = "marker_gs", label = "Select a gene set:", choices = c("Select an option below"="", genesetlist), selected=selected_gs)
  }) %...!% { return(NULL) }
  
})


## Update connectivity selection ####
observeEvent({
  input$portal_id
  input$marker
  input$marker_conn_name
}, {

  req(input$marker == 'CMap Connectivity')
  
  selected_conn=input$marker_conn; conn_name = input$marker_conn_name
    
  connectivity_dat() %...>% (function(eset){
    genesetlist <- sort(rownames(eset[[conn_name]]))
    updateSelectizeInput(session, inputId = "marker_conn", label = "Select a gene set:", choices = c("Select an option below"="", genesetlist), selected=selected_conn)
  }) %...!% { return(NULL) }    
  
})

###########################################################
#
# EXPRESSION PLOTS ####
#
###########################################################

## Get DE data for overall profiles ####
de_all <- reactive({
  
  req(input$marker == 'Genes')
  
  tas=input$marker_tas; view=input$marker_view;
  
  promise_all(profile_dat=profile_dat(), expression_dat=expression_dat()) %...>%
    with({
      
      #matching expression column names with profile annotation
      annot_var <- ifelse(all(colnames(expression_dat) %in% profile_dat$Sig_Id), "Sig_Id", "Chemical_Id")
      
      profile_dat <- profile_dat %>%
        filter(as.numeric(TAS) >= as.numeric(tas[1]) & as.numeric(TAS) <= as.numeric(tas[2])) %>% 
        transmute(Id=(!!!syms(annot_var))) %>%
        distinct(Id, .keep_all=TRUE)
      
      eset <- exprs(expression_dat)[,which(colnames(expression_dat) %in% profile_dat$Id)]
      all <- data.frame(x=as.numeric(eset))
      
      all_wrapper_df(df=all, view=view, marker_id="All")
      
    }) %...!% { return(NULL) }
  
}) %>% bindCache(input$portal_id, input$marker, input$marker_tas, input$marker_view) %>% 
  bindEvent(input$portal_id, input$marker, input$marker_tas, input$marker_view)


## Get DE data for selected chemical profiles ####
de_chemical <- reactive({

  req(input$marker == 'Genes')

  marker_id=input$marker_gene; tas=input$marker_tas; view=input$marker_view;

  promise_all(profile_dat=profile_dat(), expression_dat=expression_dat()) %...>%
    with({

      #matching expression column names with profile annotation
      annot_var <- ifelse(all(colnames(expression_dat) %in% profile_dat$Sig_Id), "Sig_Id", "Chemical_Id")
    
      profile_dat <- profile_dat %>%
        filter(as.numeric(TAS) >= as.numeric(tas[1]) & as.numeric(TAS) <= as.numeric(tas[2])) %>% 
        transmute(Id=(!!!syms(annot_var))) %>%
        distinct(Id, .keep_all=TRUE)

      rowid <- which(rownames(expression_dat) %in% marker_id)
      eset <- exprs(expression_dat)[rowid,]

      query <- profile_dat %>% mutate(Id=as.character(Id)) %>%
        left_join(data.frame(Id=as.character(names(eset)), x=as.numeric(eset))) %>%
        select(x)

      all_wrapper_df(df=query, view=view, marker_id=marker_id)

  }) %...!% { return(NULL) }

}) %>% bindCache(input$portal_id, input$marker, input$marker_gene, input$marker_tas, input$marker_view, "Chemical") %>% 
  bindEvent(input$portal_id, input$marker, input$marker_gene, input$marker_tas, input$marker_view)


## Output DE overall plot ####
output$de_marker_plot <- renderPlotly({

  req(input$marker == 'Genes')
  
  marker_id=input$marker_gene; view=input$marker_view; header="Moderated Z-scores";

  promise_all(de_all=de_all(), de_chemical=de_chemical()) %...>%
    with({
      
      data <- de_all %>% rbind(de_chemical)
      get_overall_marker_plot(data=data, marker_id=marker_id, view=view, header=header) %>% 
        ggplotly() %>% layout(showlegend=TRUE, hoverlabel=list(bgcolor="white"))
      
    }) %...!% { return(NULL) }

})


## Get DE exposure data ####
de_exposure <- reactive({
  
  portal_id = input$portal_id; marker_id = input$marker_gene; tas = input$marker_tas; view = input$marker_view;
  
  exposure_phenotype <- unlist(strsplit(as.character(projectlist$Exposure_Phenotype[which(projectlist$Portal == portal_id)]), ",", fixed=TRUE)) %>% trimws()
  
  promise_map(exposure_phenotype, function(exposure_variable){
    
    promise_all(profile_dat=profile_dat(), expression_dat=expression_dat()) %...>%
      with({
        
        #matching expression column names with profile annotation
        annot_var <- ifelse(all(colnames(expression_dat) %in% profile_dat$Sig_Id), "Sig_Id", "Chemical_Id")
        
        profile_dat <- profile_dat %>%
          filter(as.numeric(TAS) >= as.numeric(tas[1]) & as.numeric(TAS) <= as.numeric(tas[2])) %>% 
          transmute(Id=(!!!syms(annot_var)), exposure_value=(!!!syms(exposure_variable))) %>% 
          distinct(Id, .keep_all=TRUE)
        
        rowid <- which(rownames(expression_dat) %in% marker_id)
        eset <- exprs(expression_dat)[rowid,]
        
        df <- profile_dat %>% mutate(Id=as.character(Id)) %>% 
          left_join(data.frame(Id=as.character(names(eset)), x=as.numeric(eset))) %>% 
          select(x, exposure_value)
        
        exposure_wrapper_df(df=df, view=view, exposure_variable=exposure_variable)
        
      }) %...!% { return(NULL) }
    
  }) %...!% { return(NULL) }
  
}) %>% bindCache(input$portal_id, input$marker, input$marker_gene, input$marker_tas, input$marker_view, "Exposure") %>% 
  bindEvent(input$portal_id, input$marker, input$marker_gene, input$marker_tas, input$marker_view)


## Output DE exposure plot ####
output$de_exposure_phenotype_plot <- renderPlotly({

  req(input$marker == 'Genes')
  
  portal_id = input$portal_id; marker_id = input$marker_gene; view = input$marker_view; header = "Moderated Z-scores";
  exposure_phenotype <- unlist(strsplit(as.character(projectlist$Exposure_Phenotype[which(projectlist$Portal == portal_id)]), ",", fixed=TRUE)) %>% trimws()
  
  de_exposure() %...>% 
    do.call(rbind, .) %...>% 
    get_exposure_marker_plot(
      df = .,
      marker_id = marker_id,
      header = header,
      view = view
    ) %...>% 
    ggplotly(height=400*length(exposure_phenotype)) %...>% 
    layout(showlegend=TRUE, hoverlabel=list(bgcolor="white")) %...!% { return(NULL) }

})

## Get DE table ####
de_table <- reactive({
  
  req(input$marker == 'Genes')
  
  marker_id = input$marker_gene; tas = input$marker_tas; header = "Moderated <br> Z-scores";
  
  promise_all(profile_dat=profile_dat(), expression_dat=expression_dat()) %...>%
    with({
      
      get_marker_table(
        marker_id = marker_id,
        profile_dat = profile_dat,
        expression_dat = expression_dat,
        header = header,
        tas = tas
      ) %>% data.table.round()
      
    }) %...!% { return(NULL) }
  
}) %>% bindCache(input$portal_id, input$marker, input$marker_gene, input$marker_tas) %>% 
  bindEvent(input$portal_id, input$marker, input$marker_gene, input$marker_tas)


## Output DE marker table####
output$de_marker_table <-  DT::renderDataTable({

  req(input$marker == 'Genes')
  
  de_table()  %...!% { return(NULL) }

}, escape = FALSE, extensions = 'Buttons', server = TRUE, rownames=FALSE, selection = "none",
options = list(
  columnDefs = list(list(className = 'dt-left', targets = "_all")),
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


###########################################################
#
# GENE SETS PLOTS ####
#
###########################################################

## Get GS data for overall profiles ####
gs_all <- reactive({
  
  req(input$marker == 'Gene Sets')
  
  gsname=input$marker_gsname; gsmethod=input$marker_gsmethod; tas=input$marker_tas; view=input$marker_view;

  promise_all(profile_dat=profile_dat(), gs_enrichment_dat=gs_enrichment_dat()) %...>%
    with({
      
      expression_dat = gs_enrichment_dat[[paste0(gsname, "_", gsmethod)]]
      
      #matching expression column names with profile annotation
      annot_var <- ifelse(all(colnames(expression_dat) %in% profile_dat$Sig_Id), "Sig_Id", "Chemical_Id")
      
      profile_dat <- profile_dat %>%
        filter(as.numeric(TAS) >= as.numeric(tas[1]) & as.numeric(TAS) <= as.numeric(tas[2])) %>% 
        transmute(Id=(!!!syms(annot_var))) %>%
        distinct(Id, .keep_all=TRUE)
      
      eset <- exprs(expression_dat)[,which(colnames(expression_dat) %in% profile_dat$Id)]
      all <- data.frame(x=as.numeric(eset))
      
      all_wrapper_df(df=all, view=view, marker_id="All")
      
    }) %...!% { return(NULL) }
  
}) %>% bindCache(input$portal_id, input$marker, input$marker_gsname, input$marker_gsmethod, input$marker_tas, input$marker_view) %>% 
  bindEvent(input$portal_id, input$marker, input$marker_gsname, input$marker_gsmethod, input$marker_tas, input$marker_view)


## Get GS data for selected chemical profiles ####
gs_chemical <- reactive({
  
  req(input$marker == 'Gene Sets')
  
  marker_id=input$marker_gs; gsname=input$marker_gsname; gsmethod=input$marker_gsmethod; tas=input$marker_tas; view=input$marker_view;
  
  promise_all(profile_dat=profile_dat(), gs_enrichment_dat=gs_enrichment_dat()) %...>%
    with({
      
      expression_dat = gs_enrichment_dat[[paste0(gsname, "_", gsmethod)]]
      
      #matching expression column names with profile annotation
      annot_var <- ifelse(all(colnames(expression_dat) %in% profile_dat$Sig_Id), "Sig_Id", "Chemical_Id")
      
      profile_dat <- profile_dat %>%
        filter(as.numeric(TAS) >= as.numeric(tas[1]) & as.numeric(TAS) <= as.numeric(tas[2])) %>% 
        transmute(Id=(!!!syms(annot_var))) %>%
        distinct(Id, .keep_all=TRUE)
      
      rowid <- which(rownames(expression_dat) %in% marker_id)
      eset <- exprs(expression_dat)[rowid,]
      
      query <- profile_dat %>% mutate(Id=as.character(Id)) %>%
        left_join(data.frame(Id=as.character(names(eset)), x=as.numeric(eset))) %>%
        select(x)
      
      all_wrapper_df(df=query, view=view, marker_id=marker_id)
      
    }) %...!% { return(NULL) }
  
}) %>% bindCache(input$portal_id, input$marker, input$marker_gs, input$marker_gsname, input$marker_gsmethod, input$marker_tas, input$marker_view, "Chemical") %>% 
  bindEvent(input$portal_id, input$marker, input$marker_gs, input$marker_gsname, input$marker_gsmethod, input$marker_tas, input$marker_view)


## Output GS overall plot ####
output$gs_marker_plot <- renderPlotly({
  
  req(input$marker == 'Gene Sets')
  
  marker_id=input$marker_gs; view=input$marker_view; header="Moderated Z-scores";
  
  promise_all(gs_all=gs_all(), gs_chemical=gs_chemical()) %...>%
    with({
      
      data <- gs_all %>% rbind(gs_chemical)
      get_overall_marker_plot(data=data, marker_id=marker_id, view=view, header=header) %>% 
        ggplotly() %>% layout(showlegend=TRUE, hoverlabel=list(bgcolor="white"))
      
    }) %...!% { return(NULL) }
  
})


## Get GS exposure data ####
gs_exposure <- reactive({
  
  portal_id = input$portal_id; marker_id = input$marker_gs; gsname = input$marker_gsname; gsmethod = input$marker_gsmethod; tas = input$marker_tas; view = input$marker_view;
  
  exposure_phenotype <- unlist(strsplit(as.character(projectlist$Exposure_Phenotype[which(projectlist$Portal == portal_id)]), ",", fixed=TRUE)) %>% trimws()
  
  promise_map(exposure_phenotype, function(exposure_variable){
    
    promise_all(profile_dat=profile_dat(), gs_enrichment_dat=gs_enrichment_dat()) %...>%
      with({
        
        expression_dat = gs_enrichment_dat[[paste0(gsname, "_", gsmethod)]]
        
        #matching expression column names with profile annotation
        annot_var <- ifelse(all(colnames(expression_dat) %in% profile_dat$Sig_Id), "Sig_Id", "Chemical_Id")
        
        profile_dat <- profile_dat %>%
          filter(as.numeric(TAS) >= as.numeric(tas[1]) & as.numeric(TAS) <= as.numeric(tas[2])) %>% 
          transmute(Id=(!!!syms(annot_var)), exposure_value=(!!!syms(exposure_variable))) %>% 
          distinct(Id, .keep_all=TRUE)
        
        rowid <- which(rownames(expression_dat) %in% marker_id)
        eset <- exprs(expression_dat)[rowid,]
        
        df <- profile_dat %>% mutate(Id=as.character(Id)) %>% 
          left_join(data.frame(Id=as.character(names(eset)), x=as.numeric(eset))) %>% 
          select(x, exposure_value)
        
        exposure_wrapper_df(df=df, view=view, exposure_variable=exposure_variable)
        
      }) %...!% { return(NULL) }
    
  }) %...!% { return(NULL) }
  
}) %>% bindCache(input$portal_id, input$marker, input$marker_gs, input$marker_gsname, input$marker_gsmethod, input$marker_tas, input$marker_view, "Exposure") %>% 
  bindEvent(input$portal_id, input$marker, input$marker_gs, input$marker_gsname, input$marker_gsmethod, input$marker_tas, input$marker_view)


## Output GS exposure plot ####
output$gs_exposure_phenotype_plot <- renderPlotly({
  
  req(input$marker == 'Gene Sets')
  
  portal_id = input$portal_id; marker_id = input$marker_gs; view = input$marker_view; header = "Moderated Z-scores";
  exposure_phenotype <- unlist(strsplit(as.character(projectlist$Exposure_Phenotype[which(projectlist$Portal == portal_id)]), ",", fixed=TRUE)) %>% trimws()
  
  gs_exposure() %...>% 
    do.call(rbind, .) %...>% 
    get_exposure_marker_plot(
      df = .,
      marker_id = marker_id,
      header = header,
      view = view
    ) %...>% 
    ggplotly(height=400*length(exposure_phenotype)) %...>% 
    layout(showlegend=TRUE, hoverlabel=list(bgcolor="white")) %...!% { return(NULL) }
  
})

## Get GS table ####
gs_table <- reactive({
  
  req(input$marker == 'Gene Sets')
  
  marker_id = input$marker_gs; gsname=input$marker_gsname; gsmethod=input$marker_gsmethod; tas = input$marker_tas; header = "Moderated <br> Z-scores";
  
  promise_all(profile_dat=profile_dat(), gs_enrichment_dat=gs_enrichment_dat()) %...>%
    with({
      
      expression_dat = gs_enrichment_dat[[paste0(gsname, "_", gsmethod)]]
      
      get_marker_table(
        marker_id = marker_id,
        profile_dat = profile_dat,
        expression_dat = expression_dat,
        header = header,
        tas = tas
      ) %>% data.table.round()
      
    }) %...!% { return(NULL) }
  
}) %>% bindCache(input$portal_id, input$marker, input$marker_gs, input$marker_gsname, input$marker_gsmethod, input$marker_tas) %>% 
  bindEvent(input$portal_id, input$marker, input$marker_gs, input$marker_gsname, input$marker_gsmethod, input$marker_tas)


## Output GS marker table ####
output$gs_marker_table <-  DT::renderDataTable({
  
  req(input$marker == 'Gene Sets')
  
  gs_table() %...!% { return(NULL) }
  
}, escape = FALSE, extensions = 'Buttons', server = TRUE, rownames=FALSE, selection = "none",
options = list(
  columnDefs = list(list(className = 'dt-left', targets = "_all")),
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


###########################################################
#
# CONNECTIVITY PLOTS ####
#
###########################################################

## Get CONNECTIVITY data for overall profiles ####
conn_all <- reactive({
  
  req(input$marker == 'CMap Connectivity')
  
  conn_name = input$marker_conn_name; tas=input$marker_tas; view=input$marker_view;
  
  promise_all(profile_dat=profile_dat(), connectivity_dat=connectivity_dat()) %...>%
    with({
      
      expression_dat = connectivity_dat[[conn_name]]
      
      #matching expression column names with profile annotation
      annot_var <- ifelse(all(colnames(expression_dat) %in% profile_dat$Sig_Id), "Sig_Id", "Chemical_Id")
      
      profile_dat <- profile_dat %>%
        filter(as.numeric(TAS) >= as.numeric(tas[1]) & as.numeric(TAS) <= as.numeric(tas[2])) %>% 
        transmute(Id=(!!!syms(annot_var))) %>%
        distinct(Id, .keep_all=TRUE)
      
      eset <- exprs(expression_dat)[,which(colnames(expression_dat) %in% profile_dat$Id)]
      all <- data.frame(x=as.numeric(eset)) 
      
      all_wrapper_df(df=all, view=view, marker_id="All")
      
    }) %...!% { return(NULL) }
  
}) %>% bindCache(input$portal_id, input$marker, input$marker_conn_name, input$marker_tas, input$marker_view) %>% 
  bindEvent(input$portal_id, input$marker, input$marker_conn_name, input$marker_tas, input$marker_view)


## Get CONNECTIVITY data for selected chemical profiles ####
conn_chemical <- reactive({
  
  req(input$marker == 'CMap Connectivity')
  
  marker_id=input$marker_conn; conn_name = input$marker_conn_name; tas=input$marker_tas; view=input$marker_view;
  
  promise_all(profile_dat=profile_dat(), connectivity_dat=connectivity_dat()) %...>%
    with({
      
      expression_dat = connectivity_dat[[conn_name]]
      
      #matching expression column names with profile annotation
      annot_var <- ifelse(all(colnames(expression_dat) %in% profile_dat$Sig_Id), "Sig_Id", "Chemical_Id")
      
      profile_dat <- profile_dat %>%
        filter(as.numeric(TAS) >= as.numeric(tas[1]) & as.numeric(TAS) <= as.numeric(tas[2])) %>% 
        transmute(Id=(!!!syms(annot_var))) %>%
        distinct(Id, .keep_all=TRUE)
      
      rowid <- which(rownames(expression_dat) %in% marker_id)
      eset <- exprs(expression_dat)[rowid,]
      
      query <- profile_dat %>% mutate(Id=as.character(Id)) %>%
        left_join(data.frame(Id=as.character(names(eset)), x=as.numeric(eset))) %>%
        select(x)
      
      all_wrapper_df(df=query, view=view, marker_id=marker_id)
      
    }) %...!% { return(NULL) }
  
}) %>% bindCache(input$portal_id, input$marker, input$marker_conn, input$marker_conn_name, input$marker_tas, input$marker_view, "Chemical") %>% 
  bindEvent(input$portal_id, input$marker, input$marker_conn, input$marker_conn_name, input$marker_tas, input$marker_view)


## Output CONNECTIVITY overall plot ####
output$conn_marker_plot <- renderPlotly({
  
  req(input$marker == 'CMap Connectivity')
  
  marker_id = input$marker_conn; view = input$marker_view; header = "Moderated Z-scores";
  
  promise_all(conn_all=conn_all(), conn_chemical=conn_chemical()) %...>%
    with({
      
      data <- conn_all %>% rbind(conn_chemical)
      get_overall_marker_plot(data=data, marker_id=marker_id, view=view, header=header) %>% 
        ggplotly() %>% layout(showlegend=TRUE, hoverlabel=list(bgcolor="white"))
      
    }) %...!% { return(NULL) }
  
})


## Get CONNECTIVITY exposure data ####
conn_exposure <- reactive({
  
  portal_id = input$portal_id; marker_id = input$marker_conn; conn_name = input$marker_conn_name; tas = input$marker_tas; view = input$marker_view;
  
  exposure_phenotype <- unlist(strsplit(as.character(projectlist$Exposure_Phenotype[which(projectlist$Portal == portal_id)]), ",", fixed=TRUE)) %>% trimws()
  
  promise_map(exposure_phenotype, function(exposure_variable){
    
    promise_all(profile_dat=profile_dat(), connectivity_dat=connectivity_dat()) %...>%
      with({
        
        expression_dat = connectivity_dat[[conn_name]]
        
        #matching expression column names with profile annotation
        annot_var <- ifelse(all(colnames(expression_dat) %in% profile_dat$Sig_Id), "Sig_Id", "Chemical_Id")
        
        profile_dat <- profile_dat %>%
          filter(as.numeric(TAS) >= as.numeric(tas[1]) & as.numeric(TAS) <= as.numeric(tas[2])) %>% 
          transmute(Id=(!!!syms(annot_var)), exposure_value=(!!!syms(exposure_variable))) %>% 
          distinct(Id, .keep_all=TRUE)
        
        rowid <- which(rownames(expression_dat) %in% marker_id)
        eset <- exprs(expression_dat)[rowid,]
        
        df <- profile_dat %>% mutate(Id=as.character(Id)) %>% 
          left_join(data.frame(Id=as.character(names(eset)), x=as.numeric(eset))) %>% 
          select(x, exposure_value)
        
        exposure_wrapper_df(df=df, view=view, exposure_variable=exposure_variable)
        
      }) %...!% { return(NULL) }
    
  }) %...!% { return(NULL) }
  
}) %>% bindCache(input$portal_id, input$marker, input$marker_conn, input$marker_conn_name, input$marker_tas, input$marker_view, "Exposure") %>% 
  bindEvent(input$portal_id, input$marker, input$marker_conn, input$marker_conn_name, input$marker_tas, input$marker_view)


## Output CONNECTIVITY exposure plot ####
output$conn_exposure_phenotype_plot <- renderPlotly({
  
  req(input$marker == 'CMap Connectivity')
  
  portal_id = input$portal_id; marker_id = input$marker_conn; view = input$marker_view; header = "Moderated Z-scores";
  exposure_phenotype <- unlist(strsplit(as.character(projectlist$Exposure_Phenotype[which(projectlist$Portal == portal_id)]), ",", fixed=TRUE)) %>% trimws()
  
  conn_exposure() %...>% 
    do.call(rbind, .) %...>% 
    get_exposure_marker_plot(
      df = .,
      marker_id = marker_id,
      header = header,
      view = view
    ) %...>% 
    ggplotly(height=400*length(exposure_phenotype)) %...>% 
    layout(showlegend=TRUE, hoverlabel=list(bgcolor="white")) %...!% { return(NULL) }
  
})

## Get CONNECTIVITY table ####
conn_table <- reactive({
  
  req(input$marker == 'CMap Connectivity')
  
  marker_id = input$marker_conn; conn_name = input$marker_conn_name; tas = input$marker_tas; header = "Moderated <br> Z-scores";
  
  promise_all(profile_dat=profile_dat(), connectivity_dat=connectivity_dat()) %...>%
    with({
      
      expression_dat = connectivity_dat[[conn_name]]
      
      get_marker_table(
        marker_id = marker_id,
        profile_dat = profile_dat,
        expression_dat = expression_dat,
        header = header,
        tas = tas
      ) %>% data.table.round()
      
    }) %...!% { return(NULL) }
  
}) %>% bindCache(input$portal_id, input$marker, input$marker_conn, input$marker_conn_name, input$marker_tas) %>% 
  bindEvent(input$portal_id, input$marker, input$marker_conn, input$marker_conn_name, input$marker_tas)


## Output CONNECTIVITY marker table ####
output$conn_marker_table <-  DT::renderDataTable({
  
  req(input$marker == 'CMap Connectivity')
  
  conn_table() %...!% { return(NULL) }
  
}, escape = FALSE, extensions = 'Buttons', server = TRUE, rownames=FALSE, selection = "none",
options = list(
  columnDefs = list(list(className = 'dt-left', targets = "_all")),
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




