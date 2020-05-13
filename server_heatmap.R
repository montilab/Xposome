
##Observe event when a button is clicked####
output$hm_de_button <- renderUI({
  
  req(input$marker_hm)
  
  htmlfile <- paste0("http://montilab.bu.edu/Xposome/JSON/", fname, "/gene_expression.html")
  
  a(onclick="heatmapFun('hm_de_generate');", class="mybuttons", href=htmlfile, target="_blank", alt="heatmap", width = "100%", height="auto", span(icon("fas fa-arrow-circle-right"), "Generate heatmap"))
  
})

##Observe event when a button is clicked####
output$hm_es_button <- renderUI({
  
  req(input$marker_hm, input$marker_gsname_hm, input$marker_gsmethod_hm)
  
  htmlfile <- paste0("http://montilab.bu.edu/Xposome/JSON/", fname, "/", dsmap()[[input$marker_gsname_hm]], "_", input$marker_gsmethod_hm, ".html")
  a(onclick="heatmapFun('hm_es_generate');", class="mybuttons", href=htmlfile, target="_blank", alt="heatmap", width = "100%", height="auto", span(icon("fas fa-arrow-circle-right"), "Generate heatmap"))
  
})

##Observe event when a button is clicked####
output$hm_conn_button <- renderUI({
  
  req(input$marker_hm, input$marker_conn_name_hm)
  
  htmlfile <- paste0("http://montilab.bu.edu/Xposome/JSON/", fname, "/", input$marker_conn_name_hm, "_connectivity.html")
  a(onclick="heatmapFun('hm_conn_generate');", class="mybuttons", href=htmlfile, target="_blank", alt="heatmap", width = "100%", height="auto", span(icon("fas fa-arrow-circle-right"), "Generate heatmap"))
  
})
