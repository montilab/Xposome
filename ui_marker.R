

tagList(
  
  fluidRow(
    column(
      width=4,
      selectizeInput(
        inputId = "marker",
        label = "Select marker set:",
        choices = c("Genes", "Gene Sets", "CMap Connectivity"),
        selected = "Genes"
      )
    ),
    
    column(
      width=4,
      sliderInput(
        inputId = "marker_tas", label = "TAS range:", min = 0,  max = maxTAS, value = 0.2, step = 0.1
      )
    ),
    
    column(
      width=4,
      radioButtons(
        inputId = "marker_view", 
        label = "Plot Type:",
        choices = c("Density", "Boxplot"), 
        selected = "Density", inline=TRUE
      )
    )
  ),
  
  conditionalPanel(
    condition = "input.marker == 'Genes'",
    
    fluidRow(
      column(
        width=4,
        selectInput(
          inputId = "marker_gene",
          label = "Select a gene:",
          choices = sort(rownames(dat[["Gene Expression"]]))
        )
      )
    )
  ),
  
  conditionalPanel(
    "input.marker == 'Gene Sets'",
    
    fluidRow(
      column(
        width=4,
        selectInputWithTooltip(
          inputId = "marker_gsname",
          label = "Gene set name", 
          bId= "Bgsname_marker",
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
          inputId = "marker_gsmethod", 
          label ="Projection method", 
          bId = "Bgsmethod_marker",
          helptext = paste(
            "gsva, ssgea, zscore: from R Bioconductor package GSVA",
            "gsproj: GeneSetProjection for R package montilab:CBMRtools",
            sep = "<br>"
          ),
          choices = c("gsva", "ssgsea", "zscore", "gsproj")
        )
      ),
      
      column(
        width=4,
        uiOutput("marker_gs_selection")
      )
    )
  ),
    
  conditionalPanel(
    "input.marker == 'CMap Connectivity'",
    
    fluidRow(
      column(
        width=4,
        selectInput(
          inputId = "marker_conn_name",
          label = "Connectivity Level", 
          choices = c("Perturbagen Classes", "Perturbagens")
        )
      ),
      
      column(
        width=4,
        uiOutput("marker_conn_selection")
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
        downloadButton(outputId = "marker_download_pdf", label = "Download pdf", class="mybuttons"),
        downloadButton(outputId = "marker_download_png", label = "Download png", class="mybuttons")
      )
    ),
    
    fluidRow(
      column(
        width=12,
        plotOutput(outputId = "marker_plot_1")  
      ),
      
      column(
        width=12,
        plotOutput(outputId = "marker_plot_2") 
      ),
      
      column(
        width=12,
        plotOutput(outputId = "marker_plot_3") 
      )
    ),
    
    fluidRow(
      column(
        width=12,
        uiOutput(outputId = "marker_table_header"),
        DT::dataTableOutput(outputId = "marker_table") 
      )
    )
  )
  
)  

