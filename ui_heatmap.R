
#Heatmap Explorer Page####
tagList(
  
  bsCollapse(
    id = "heatmap_opt_panel", open = "heatmap_options",
    
    bsCollapsePanel(
      title = "Options", value = "heatmap_options",
      
      fluidRow(
        column(
          width=4, 
          selectInput(
            inputId = "marker_hm", 
            label = "Select a marker set:",   
            choices = if(isolate(session$clientData$url_search) == "?ADIPO"){ c("Please select an option below" = "", "Genes", "Gene Sets", "CMap Connectivity") }else{ c("Please select an option below" = "", "Genes (Landmark)", "Gene Sets", "CMap Connectivity") }
          )
        ),
        
        column(
          width=4, 
          sliderInput(
            inputId = "marker_tas_hm", label = "TAS range:", min = 0, max = maxTAS, value = 0.2, step = 0.01
          ),
          helpText(style="color: red;", "Warning: TAS < 0.2 is slow to load!")
        ),
        
        column(
          width=4, 
          if(isolate(session$clientData$url_search) == "?ADIPO"){
            conditionalPanel(
              condition = "input.marker_hm == 'Genes'",
              sliderInput(inputId = "numberthreshold", label = "Top number of genes (by MAD):", min = 0, max = 1000, value = 500, ticks = FALSE, step = 10)
            )
          }
        )
      ),
      
      conditionalPanel(
        condition = "input.marker_hm == 'Genes' | input.marker_hm == 'Genes (Landmark)'",
        
        fluidRow(
          column(
            width=12,
            actionButton(inputId = "hm_de_generate", label = "Generate heatmap", icon=icon("fas fa-arrow-circle-right"), class="mybuttons")
          )
        )
      ),
      
      conditionalPanel(
        condition = "input.marker_hm == 'Gene Sets'",
        
        fluidRow(
          column(
            width=4,
            selectInputWithTooltip(
              inputId = "marker_gsname_hm",
              label = "Gene set name", 
              bId= "Bgsname_marker_hm",
              helptext = helptext_geneset,
              choices = names(dsmap)
            )
          ),
          
          column(
            width=4,
            selectInputWithTooltip(
              inputId = "marker_gsmethod_hm", 
              label ="Projection method", 
              bId = "Bgsmethod_marker_hm",
              helptext = helptext_method,
              choices = names(dsmap_method)
            )
          )
        ),
        
        fluidRow(
          column(
            width=12,
            actionButton(inputId = "hm_es_generate", label = "Generate heatmap", icon=icon("fas fa-arrow-circle-right"), class="mybuttons")
          )
        )
      ),
      
      conditionalPanel(
        condition = "input.marker_hm == 'CMap Connectivity'",
        
        fluidRow(
          column(
            width=4,
            selectizeInput(
              inputId = "marker_conn_name_hm",
              label = "Connectivity Level", 
              choices = c("Perturbagen Classes", "Perturbagens")
            )
          )
        ),
        
        fluidRow(
          column(
            width=12,
            actionButton(inputId = "hm_conn_generate", label = "Generate heatmap", icon=icon("fas fa-arrow-circle-right"), class="mybuttons")
          )
        )
      )
    )
  ),
  
  conditionalPanel(
    condition = "input.hm_de_generate >= 1 | input.hm_es_generate >= 1 | input.hm_conn_generate >= 1",
    
    fluidRow(
      column(
        width=6,
        div(class="text-md-left",
            div(class="morpheus-site-link", uiOutput("morpheus_result_link")),
        )
      ),
      
      column(
        width = 6,
        div(class="text-md-right",
            downloadButton(outputId = "hm_download_pdf", label = "Download pdf", class="mybuttons"),
            downloadButton(outputId = "hm_download_png", label = "Download png", class="mybuttons")
        )
      )
    ),
      
    br(), 
    
    fluidRow(
      column(
        width=12,
        uiOutput(outputId = "heatmap_holder") %>% withSpinner(type=4, color="#0dc5c1", proxy.height="400px")
      )
    )
  )
)

