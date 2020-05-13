

# Output the tas selection ####
observeEvent(maxTAS(), {
  
  updateSliderInput(session, inputId = "marker_tas", min = 0,  max = maxTAS(), value=c(0, 0.2))
  
})

# Output the gene set selection ####
observeEvent(expression_dat(), {

  eset <- expression_dat()
  updateSelectInput(session, inputId="marker_gene", choices=sort(rownames(eset)))
  shinyjs::enable(id="de_generate")
  
},)

# Create the gene set list ####
observe({
  
  req(gs_enrichment_dat(), input$marker_gsname, input$marker_gsmethod)

  eset <- gs_enrichment_dat()[[paste0(dsmap()[[input$marker_gsname]], "_", input$marker_gsmethod)]]
  updateSelectInput(session, inputId="marker_gs", choices=sort(rownames(eset)))
  shinyjs::enable(id="gs_generate")
  
})


##Output the gene set selection####
observe({
  
  req(input$marker_conn_name, connectivity_dat())

  eset <- connectivity_dat()[[input$marker_conn_name]]
  updateSelectInput(session, inputId="marker_conn", choices=sort(rownames(eset)))
  shinyjs::enable(id="conn_generate")
  
})

##Create reactive marker data#####
marker_data <- reactiveVal(NULL);
marker_id <- reactiveVal(NULL);
marker_header <- reactiveVal(NULL);
marker_key <- reactiveVal(NULL);
marker_tas <- reactiveVal(NULL); 
marker_view <- reactiveVal(NULL);

##Observe event when a button is clicked####
observeEvent(input$de_generate, {

  req(expression_dat(), input$marker, input$marker_tas, input$marker_view, input$marker_gene)

  eset <- expression_dat(); 
  marker_gene <- isolate({ input$marker_gene });
  tas <- isolate({ input$marker_tas }); 
  plot_view <- isolate({ input$marker_view });

  marker_data(eset)
  marker_id(marker_gene)
  marker_header("Mod Zscores")
  marker_tas(tas)
  marker_view(plot_view)
  
}, ignoreInit=TRUE)

##Observe event when a button is clicked####
observeEvent(input$gs_generate, {

  req(gs_enrichment_dat(), input$marker, input$marker_tas, input$marker_view, input$marker_gsname, input$marker_gsmethod, input$marker_gs)

  eset <- gs_enrichment_dat()[[paste0(dsmap()[[input$marker_gsname]], "_", input$marker_gsmethod)]]
  marker <- isolate({ input$marker }); 
  marker_gs <- isolate({ input$marker_gs });
  tas <- isolate({ input$marker_tas }); 
  plot_view <- isolate({ input$marker_view });
  
  marker_data(eset)
  marker_id(marker_gs)
  marker_header("Gene Set Scores")
  marker_tas(tas)
  marker_view(plot_view)

}, ignoreInit=TRUE)

##Observe event when a button is clicked####
observeEvent(input$conn_generate, {

  req(connectivity_dat(), input$marker, input$marker_tas, input$marker_view, input$marker_conn_name, input$marker_conn)

  eset <- connectivity_dat()[[input$marker_conn_name]]
  marker_conn <- isolate({ input$marker_conn });
  tas <- isolate({ input$marker_tas }); 
  plot_view <- isolate({ input$marker_view });
  
  marker_data(eset)
  marker_id(marker_conn)
  marker_header("Connectivity Score (Percentile)")
  marker_tas(tas)
  marker_view(plot_view)

}, ignoreInit=TRUE)

##Output the first plot#####
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

output$marker_plot_1 <- renderPlotly({

  req(profile_dat(), marker_data(), marker_id(), marker_header(), marker_tas(), marker_view())

  fig <- get_de_by_gene_hist(
    marker_id = marker_id(),
    expression_dat = marker_data(),
    profile_dat = profile_dat(),
    annot_var = annot_var(),
    col_id = NA,
    header = marker_header(),
    marker_tas = marker_tas(),
    plot = marker_view(),
    replace_na = if(fname == "ADIPO"){ "Vehicle" }else{ "N/A" }
  ) %>% ggplotly() 
  
  fig <- fig %>% layout(legend = l("Overall"))
  
})


##Output the second plot#####
output$marker_plot_2 <- renderPlotly({

  req(profile_dat(), marker_data(), marker_id(), marker_header(), marker_tas(), marker_view())

  fig <- get_de_by_gene_hist(
    marker_id = marker_id(),
    expression_dat = marker_data(),
    profile_dat = profile_dat(),
    annot_var = annot_var(),
    col_id = if(fname == "ADIPO"){ "PPARg_Mod" }else{ "Carcinogenicity" },
    col_colors = if(fname == "ADIPO"){ c("grey", "green", "orange", "purple") }else{ c("grey", "green", "orange") },
    col_names = if(fname == "ADIPO"){ c("Vehicle", "Yes", "No", "Suspected") }else{ c("N/A", "-", "+") },
    header = marker_header(),
    marker_tas = marker_tas(),
    plot = marker_view(),
    replace_na = if(fname == "ADIPO"){ "Vehicle" }else{ "N/A" }
  ) %>% ggplotly()

  fig <- fig %>% layout(legend = l("Carcinogenicity"))
  
})

##Output the third plot#####
output$marker_plot_3 <- renderPlotly({

  req(profile_dat(), fname != "ADIPO", marker_data(), marker_id(), marker_header(), marker_tas(), marker_view())

  fig <- get_de_by_gene_hist(
    marker_id = marker_id(),
    expression_dat = marker_data(),
    profile_dat = profile_dat(),
    annot_var = annot_var(),
    col_id = "Genotoxicity",
    col_colors = c("grey","pink", "purple"),
    col_names = c("N/A", "-", "+"),
    header = marker_header(),
    marker_tas = marker_tas(),
    plot = marker_view(),
    replace_na = if(fname == "ADIPO"){ "Vehicle" }else{ "N/A" }
  ) %>% ggplotly()

  fig <- fig %>% layout(legend = l("Genotoxicity"))
  
})


##Output the marker table header####
output$marker_table_header <- renderUI({

  req(marker_header())

  h3(paste0("Table of Profiles Ranked by ", marker_header()))

})

##Output the marker table####
output$marker_table <-  DT::renderDataTable({

  req(marker_data(), marker_id(), marker_header(), input$marker_tas)

  get_de_by_gene_table(
    marker_id = marker_id(),
    expression_dat = marker_data(),
    profile_dat = profile_dat(),
    annot_var = annot_var(),
    header = marker_header(),
    marker_tas = input$marker_tas
  ) %>% data.table.round()

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
