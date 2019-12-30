

tagList(
  
  fluidRow(
    column(
      width=4, 
      selectizeInput(
        inputId = "marker_hm", 
        label = "Select marker set:",   
        choices = c("Genes (Landmark)", "Gene Sets", "CMap Connectivity"),
        selected = "Genes (Landmark)"
      )
    ),
    
    column(
      width=4, 
      sliderInput(
        inputId = "marker_tas_hm", label = "TAS range", min = 0, max = maxTAS, value = 0.2, step = 0.01
      ),
      helpText(style="color: red;", "Warning: TAS < 0.2 is slow to load!")
    )
  ),
  
  conditionalPanel(
    "input.marker_hm == 'Gene Sets'",
    
    fluidRow(
      column(
        width=4,
        selectInputWithTooltip(
          inputId = "marker_gsname_hm",
          label = "Gene set name", 
          bId= "Bgsname_marker_hm",
          helptext = paste(
            "Hallmark: MSigDB Hallmark Pathways (v5.0)",
            "C2: MSigDB C2 reactome Pathways (v5.0)",
            "NURSA: Nuclear Receptor Signaling Atlas, consensome data for human",
            sep = "<br>"
          ),
          choices = c("Hallmark", "C2", "NURSA")
        )
      ),
      
      column(
        width=4,
        selectInputWithTooltip(
          inputId = "marker_gsmethod_hm", 
          label ="Projection method", 
          bId = "Bgsmethod_marker_hm",
          helptext = paste(
            "gsva, ssgea, zscore: from R Bioconductor package GSVA",
            "gsproj: GeneSetProjection for R package montilab:CBMRtools",
            sep = "<br>"
          ),
          choices = c("gsva", "ssgsea", "zscore", "gsproj")
        )
      )
    )
  ),
  
  conditionalPanel(
    "input.marker_hm == 'CMap Connectivity'",
    
    fluidRow(
      column(
        width=4,
        selectInput(
          inputId = "marker_conn_name_hm",
          label = "Connectivity Level", 
          choices = c("Perturbagen Classes", "Perturbagens")
        )
      )
    )
  ),
  
  conditionalPanel(
    condition = "$('html').hasClass('shiny-busy')",
    withSpinner(color="#0dc5c1", div(style="height: 200px"))
  ),  
  
  conditionalPanel(
    condition = "!$('html').hasClass('shiny-busy')",
    
    fluidRow(
      column(
        width=12,
        downloadButton(outputId = "hm_download_pdf", label = "Download pdf", class="mybuttons"),
        downloadButton(outputId = "hm_download_png", label = "Download png", class="mybuttons")
      )
    ),
    
    conditionalPanel(
      "input.marker_hm == 'Gene Sets'",
      
      
      fluidRow(
        column(
          width=4,
          br(), 
          uiOutput("morpheus_result_link")
        )
      )
    ),
    
    fluidRow(
      column(
        width=12,
        plotOutput(outputId = "hm_plot", height = 3000, width = "100%") %>% withSpinner(color="#0dc5c1", proxy.height = "100px")
      )
    )
  )
  
)
  
