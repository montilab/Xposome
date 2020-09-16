
# update hm marker selection if there is no connectivity map ####
output$marker_hm_option <- renderUI({
  
  connectivity_dat() %...>% {
    
    conn_dat <- .
    
    if(is.null(conn_dat)){
      selectInput(
        inputId = "marker_hm", 
        label = "Select a marker set:",   
        choices = if(landmark %in% TRUE){ c("Please select an option below" = "", "Genes (Landmark)", "Gene Sets") }else{  c("Please select an option below" = "", "Genes", "Gene Sets") }
      )
    }else{
      selectInput(
        inputId = "marker_hm", 
        label = "Select a marker set:",   
        choices = if(landmark %in% TRUE){ c("Please select an option below" = "", "Genes (Landmark)", "Gene Sets", "CMap Connectivity") }else{  c("Please select an option below" = "", "Genes", "Gene Sets", "CMap Connectivity") }
      ) 
    }
    
  }
})

##Observe event when a button is clicked####
output$hm_de_button <- renderUI({
  
  req(input$marker_hm)
  
  if(input$marker_hm == "Genes"){
    htmlfile <- paste0("JSON/", fname, "/gene_expression.html")
  }else{
    htmlfile <- paste0("JSON/", fname, "/landmark_gene.html")
  }
  
  a(onclick="heatmapFun('hm_de_generate');", class="mybuttons", href=htmlfile, target="_blank", alt="heatmap", width = "100%", height="auto", span(icon("fas fa-arrow-circle-right"), "Generate heatmap"))
  
})

##Observe event when a button is clicked####
output$hm_es_button <- renderUI({
  
  req(input$marker_gsname_hm, input$marker_gsmethod_hm)
  
  htmlfile <- paste0("JSON/", fname, "/", dsmap[[input$marker_gsname_hm]], "_", input$marker_gsmethod_hm, ".html")
  a(onclick="heatmapFun('hm_es_generate');", class="mybuttons", href=htmlfile, target="_blank", alt="heatmap", width = "100%", height="auto", span(icon("fas fa-arrow-circle-right"), "Generate heatmap"))
  
})

##Observe event when a button is clicked####
output$hm_conn_button <- renderUI({
  
  req(input$marker_conn_name_hm)
  
  htmlfile <- paste0("JSON/", fname, "/", input$marker_conn_name_hm, "_connectivity.html")
  a(onclick="heatmapFun('hm_conn_generate');", class="mybuttons", href=htmlfile, target="_blank", alt="heatmap", width = "100%", height="auto", span(icon("fas fa-arrow-circle-right"), "Generate heatmap"))
  
})
