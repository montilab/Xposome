

# Output the chemical selection ####
observeEvent(profile_dat(), {
  
  profile_dat() %...>% {
    prof_dat <- .
    
    if("TAS" %in% colnames(prof_dat)){
      max <- floor(max(prof_dat$TAS)/0.1)*0.1    
      
      output$TAS_view <- renderUI({
        sliderInput(inputId="marker_tas", label="TAS:", min=0, max=max, step=0.1, value=c(0, 0.2))
      })
    }    
  }
  
})

# update the marker selection if there is no connectivity map ####
output$marker_option <- renderUI({
  
  connectivity_dat() %...>% {
    
    conn_dat <- .
    
    if(is.null(conn_dat)){
      selectInput(
        inputId = "marker",
        label = "Select a marker set:",
        choices = c("Please select an option below" = "", "Genes", "Gene Sets")
      )
    }else{
      selectInput(
        inputId = "marker",
        label = "Select a marker set:",
        choices = c("Please select an option below" = "", "Genes", "Gene Sets", "CMap Connectivity")
      )
    }
    
  }
  
})

# Output the gene selection ####
observeEvent(input$marker, {
  
  req(input$marker %in% "Genes")
  
  ##Getting the gene list####
  expression_dat() %...>% {
    eset <- .
    genelist <- sort(rownames(eset))
    updateSelectInput(session, inputId="marker_gene", choices=genelist)
    shinyjs::show(id="marker_gene")
    shinyjs::show(id="de_generate")
    shinyjs::show(id="cancel_de_generate")
  }
  
}, ignoreInit = TRUE)

 
# Output the gene set selection ###
observeEvent(input$marker_gsname, {
  
  ## Get gene set list
  gsname=input$marker_gsname; gsmethod=input$marker_gsmethod;
  
  ##Getting the gene list####
  gs_enrichment_dat() %...>% {
    eset <- .[[paste0(dsmap[[gsname]], "_", gsmethod)]]
    genesetlist <- sort(rownames(eset))
    updateSelectInput(session, inputId="marker_gs", choices=genesetlist)
    shinyjs::show(id="gs_generate")
    shinyjs::show(id="cancel_gs_generate")
  }
  
}, ignoreInit = TRUE)


observeEvent(input$marker_conn_name, {

  ## Get gene set list
  conn_name=input$marker_conn_name; 
  
  ##Getting the gene list####
  connectivity_dat() %...>% {
    eset <- .[[conn_name]]
    genesetlist <- sort(rownames(eset))
    updateSelectInput(session, inputId="marker_conn", choices=genesetlist)
    shinyjs::show(id="conn_generate")
    shinyjs::show(id="cancel_conn_generate")
  }
  
}, ignoreInit = TRUE)

##Output the exposure phenotype plots####
output$exposure_phenotype_plot <- renderUI({
  
  req(exposure_phenotype)
  
  TablePlot <- NULL;
  
  for(s in seq_along(exposure_phenotype)){
    TablePlot <- c(TablePlot, UIMarkerplot(outputId=s+1))
  }
  
  TablePLot <- paste0(TablePlot, collapse="\n<br>\n")

  HTML(TablePlot)
  
})

##Create reactive values####
marker_header <- reactiveVal(NULL);
create_marker_plot <- reactiveVal(NULL);

##Observe event when a button is clicked####
observeEvent(input$de_generate, {

  ##Disable the generate button
  shinyjs::disable(id="de_generate")
  
  ##regenerate plots again
  create_marker_plot(NULL)
  
  ##Get marker header
  marker_header("Mod-Zscores")

  #Get input values
  marker_id <- input$marker_gene;
  header <- marker_header();
  tas <- input$marker_tas;
  view <- input$marker_view;
  width <- input$dimension[1];
  
  ##Create new progress bar
  progress <- AsyncProgress$new(message=paste0("Generating ", ifelse(view %in% "Density", paste0("Density Plot"), view), "..."))

  results <- promise_all(annot_var=annot_var(), profile_dat=profile_dat(), expression_dat=expression_dat()) %...>% 
    with({
      
      ##Create a empty list to store figures####
      marker_fig <- list(); n=length(exposure_phenotype)+2;
      
      # throw errors that were signal (if Cancel was clicked)
      interruptor$execInterrupts()
      
      ##Create the overall plot####
      fig1 <- get_marker_plot(
        expression_dat = expression_dat,
        profile_dat = profile_dat,
        annot_var = annot_var,
        marker_id = marker_id,
        col_id = NA,
        header = header,
        tas = tas,
        view = view
      )  %>% ggplotly(width = width)
      
      marker_fig[["Overall"]] <- fig1
      
      progress$inc(1/n)
      
      ##Color palettes for qualitative data####
      col_palette <- c("Set3", "Set1", "Paired", "Accent", "Pastel1", "Dark2", "Pastel2", "Set2")
      
      #Create the exposure phenotype plots####
      for(s in seq_along(exposure_phenotype)){
        #s=1;
        col_id=exposure_phenotype[s]
        variable=profile_dat %>% select(!!exposure_phenotype[s]) %>% distinct()
        col_colors=brewer.pal(nrow(variable), col_palette[s])
        col_names=unique(variable[,exposure_phenotype[s]])
        
        fig <- get_marker_plot(
          expression_dat = expression_dat,
          profile_dat = profile_dat,
          annot_var = annot_var,
          marker_id = marker_id,
          col_id = col_id,
          col_names = col_names,
          col_colors = col_colors,
          header = header,
          tas = tas,
          view = view
        )  %>% ggplotly(width = width)
        
        marker_fig[[exposure_phenotype[s]]] <- fig
        
        progress$inc((s+1)/n)
      }
      
      ##Create the summary of table output####
      table <- get_de_by_gene_table(
        expression_dat = expression_dat,
        profile_dat = profile_dat,
        annot_var = annot_var,
        marker_id = marker_id,
        header = header,
        tas = tas
      ) %>% data.table.round()
      
      marker_fig[["Table"]] <- table
      
      progress$inc(n/n)
      
      return(marker_fig)
      
    }) %...>% create_marker_plot()
  
  ## Show notification on error or user interrupt
  results <- catch(
    results,
    function(e){
      create_marker_plot(NULL)
      print(e$message)
    })
  
  ## When done with analysis, remove progress bar
  results <- finally(results, function(){
    progress$close()
    shinyjs::enable(id="de_generate")
  })
  
  print(paste0("Generating ", ifelse(view %in% "Density", paste0("Density Plot"), view), "..."))
  
}, ignoreInit=TRUE)

# ##Observe event when a button is clicked####
observeEvent(input$gs_generate, {

  ##Disable the generate button
  shinyjs::disable(id="gs_generate")
  
  ##regenerate plots again
  create_marker_plot(NULL)
  
  ##Get marker header
  marker_header("Gene Set Scores")
  
  #Get input values
  marker_id <- input$marker_gs;
  gsname <- input$marker_gsname; 
  gsmethod <- input$marker_gsmethod;
  header <- marker_header();
  tas <- input$marker_tas;
  view <- input$marker_view;
  width <- input$dimension[1];
  
  ##Create new progress bar
  progress <- AsyncProgress$new(message=paste0("Generating ", ifelse(view %in% "Density", paste0("Density Plot"), view), "..."))
  
  results <- promise_all(annot_var=annot_var(), profile_dat=profile_dat(), gs_enrichment_dat=gs_enrichment_dat()) %...>% 
    with({
      
      ##Create a empty list to store figures####
      marker_fig <- list(); n=length(exposure_phenotype)+2;
      
      # throw errors that were signal (if Cancel was clicked)
      interruptor$execInterrupts()
      
      ##Create the overall plot####
      fig1 <- get_marker_plot(
        expression_dat = gs_enrichment_dat[[paste0(dsmap[[gsname]], "_", gsmethod)]],
        profile_dat = profile_dat,
        annot_var = annot_var,
        marker_id = marker_id,
        col_id = NA,
        header = header,
        tas = tas,
        view = view
      )  %>% ggplotly(width = width)
      
      marker_fig[["Overall"]] <- fig1
      
      progress$inc(1/n)
      
      ##Color palettes for qualitative data####
      col_palette <- c("Set3", "Set1", "Paired", "Accent", "Pastel1", "Dark2", "Pastel2", "Set2")
      
      #Create the exposure phenotype plots####
      for(s in seq_along(exposure_phenotype)){
        #s=1;
        col_id=exposure_phenotype[s]
        variable=profile_dat %>% select(!!exposure_phenotype[s]) %>% distinct()
        col_colors=brewer.pal(nrow(variable), col_palette[s])
        col_names=unique(variable[,exposure_phenotype[s]])
        
        fig <- get_marker_plot(
          expression_dat = gs_enrichment_dat[[paste0(dsmap[[gsname]], "_", gsmethod)]],
          profile_dat = profile_dat,
          annot_var = annot_var,
          marker_id = marker_id,
          col_id = col_id,
          col_names = col_names,
          col_colors = col_colors,
          header = header,
          tas = tas,
          view = view
        )  %>% ggplotly(width = width)
        
        marker_fig[[exposure_phenotype[s]]] <- fig
        
        progress$inc((s+1)/n)
      }
      
      ##Create the summary of table output####
      table <- get_de_by_gene_table(
        expression_dat = gs_enrichment_dat[[paste0(dsmap[[gsname]], "_", gsmethod)]],
        profile_dat = profile_dat,
        annot_var = annot_var,
        marker_id = marker_id,
        header = header,
        tas = tas
      ) %>% data.table.round()
      
      marker_fig[["Table"]] <- table
      
      progress$inc(n/n)
      
      return(marker_fig)
      
    }) %...>% create_marker_plot()
  
  ## Show notification on error or user interrupt
  results <- catch(
    results,
    function(e){
      create_marker_plot(NULL)
      print(e$message)
    })
  
  ## When done with analysis, remove progress bar
  results <- finally(results, function(){
    progress$close()
    ##Enable the generate button
    shinyjs::enable(id="gs_generate")
  })
  
  print(paste0("Generating ", ifelse(view %in% "Density", paste0("Density Plot"), view), "..."))
  
}, ignoreInit=TRUE)

##Observe event when a button is clicked####
observeEvent(input$conn_generate, {

  ##Disable the generate button
  shinyjs::disable(id="conn_generate")
  
  ##regenerate plots again
  create_marker_plot(NULL)
  
  ##Get marker header
  marker_header("Connectivity Score (Percentile)")
  
  #Get input values
  marker_id <- input$marker_conn;
  conn_name <- input$marker_conn_name; 
  header <- marker_header();
  tas <- input$marker_tas;
  view <- input$marker_view;
  width <- input$dimension[1];
  
  ##Create new progress bar
  progress <- AsyncProgress$new(message=paste0("Generating ", ifelse(view %in% "Density", paste0("Density Plot"), view), "..."))
  
  results <- promise_all(annot_var=annot_var(), profile_dat=profile_dat(), connectivity_dat=connectivity_dat()) %...>% 
    with({
      
      ##Create a empty list to store figures####
      marker_fig <- list(); n=length(exposure_phenotype)+2;
      
      # throw errors that were signal (if Cancel was clicked)
      interruptor$execInterrupts()
      
      ##Create the overall plot####
      fig1 <- get_marker_plot(
        expression_dat = connectivity_dat[[conn_name]],
        profile_dat = profile_dat,
        annot_var = annot_var,
        marker_id = marker_id,
        col_id = NA,
        header = header,
        tas = tas,
        view = view
      )  %>% ggplotly(width = width)
      
      marker_fig[["Overall"]] <- fig1
      
      progress$inc(1/n)
      
      ##Color palettes for qualitative data####
      col_palette <- c("Set3", "Set1", "Paired", "Accent", "Pastel1", "Dark2", "Pastel2", "Set2")
      
      #Create the exposure phenotype plots####
      for(s in seq_along(exposure_phenotype)){
        #s=1;
        col_id=exposure_phenotype[s]
        variable=profile_dat %>% select(!!exposure_phenotype[s]) %>% distinct()
        col_colors=brewer.pal(nrow(variable), col_palette[s])
        col_names=unique(variable[,exposure_phenotype[s]])
        
        fig <- get_marker_plot(
          expression_dat = connectivity_dat[[conn_name]],
          profile_dat = profile_dat,
          annot_var = annot_var,
          marker_id = marker_id,
          col_id = col_id,
          col_names = col_names,
          col_colors = col_colors,
          header = header,
          tas = tas,
          view = view
        )  %>% ggplotly(width = width)
        
        marker_fig[[exposure_phenotype[s]]] <- fig
        
        progress$inc((s+1)/n)
      }
      
      ##Create the summary of table output####
      table <- get_de_by_gene_table(
        expression_dat = connectivity_dat[[conn_name]],
        profile_dat = profile_dat,
        annot_var = annot_var,
        marker_id = marker_id,
        header = header,
        tas = tas
      ) %>% data.table.round()
      
      marker_fig[["Table"]] <- table
      
      progress$inc(n/n)
      
      return(marker_fig)

    }) %...>% create_marker_plot()
  
  ## Show notification on error or user interrupt
  results <- catch(
    results,
    function(e){
      create_marker_plot(NULL)
      print(e$message)
    })
  
  ## When done with analysis, remove progress bar
  results <- finally(results, function(){
    progress$close()
    ##Enable the generate button
    shinyjs::disable(id="conn_generate")
  })
  
  print(paste0("Generating ", ifelse(view %in% "Density", paste0("Density Plot"), view), "..."))
  
}, ignoreInit=TRUE)

##Styling the plots####
l <- function(title){
  list(
    orientation = 'v',
    title=list(
      text=paste0('<b>', title, ':</b></br>'),
      size=10,
      color="lightgray"
    ),
    font = list(
      family = "sans-serif",
      size = 10
    )
  )
}

##Output the overall plot ####
output$marker_plot_1 <- renderPlotly({

  req(create_marker_plot())

  fig <- create_marker_plot()[["Overall"]]
  fig <- fig %>% layout(legend = l("Overall"), hoverlabel = list(bgcolor="white"))
  fig

})

##Output exposure phenotype plots####
observeEvent(create_marker_plot(), {

  req(create_marker_plot())

  for(i in seq_along(exposure_phenotype)){
    local({

      plotname <- paste0("marker_plot_", i+1)
      legend_header <- exposure_phenotype[i]
      fig <- create_marker_plot()[[exposure_phenotype[i]]]
      
      output[[plotname]] <- renderPlotly({
        fig <- fig %>% layout(legend = l(legend_header), hoverlabel = list(bgcolor="white"))
        fig
      })

    })
  }

}, ignoreInit=TRUE)


##Output the marker table header####
output$marker_table_header <- renderUI({

  req(marker_header())

  h3(paste0("Table of Profiles Ranked by ", marker_header()))

})

##Output the marker table####
output$marker_table <-  DT::renderDataTable({

  req(create_marker_plot())

  table <- create_marker_plot()[["Table"]]
  return(table)

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
