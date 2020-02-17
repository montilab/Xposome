

#Taxonomic Clustering Page####
tagList(
  fluidRow(
    class="col-container-2",
    
    column(
      width=6,
      div(class="header-3",
          div(class="title", "Click and hold nodes to select sub-groups"),
          div(class="content", 
              visNetworkOutput(outputId = "dendroSelect", width="100%", height = "550px")
          )
      )
    ),
    
    column(
      width=6, 
      div(class="header-3",
          div(class="title", "Selections"),
          div(class="content", 
              uiOutput(outputId = "compare"),
              br(), br(),
              uiOutput(outputId = "groupSel")
          )
      )
    )
  ),
  
  fluidRow(
    column(
      width=12,
      bsCollapse(
        id = "de_analysis_panel", open = "de_analysis_options",
        
        bsCollapsePanel(
          title = "Differential Analysis Results", value = "de_analysis_options",
          DT::dataTableOutput(outputId = "DGEmulti")
        )
      )
    )
  ),
  
  fluidRow(
    column(
      width=12,
      bsCollapse(
        id = "ge_analysis_panel", open = "ge_analysis_options",
        
        bsCollapsePanel(
          title = "Gene Expression", value = "ge_analysis_options",
          plotly::plotlyOutput(outputId = "genePlotCluster")
        )
      )
    )
  ),
  
  fluidRow(
    column(
      width=12,
      bsCollapse(
        id = "enrichment_analysis_panel", open = "enrichment_analysis_options",
        
        bsCollapsePanel(
          title = "Enrichment Results", value = "enrichment_analysis_options",
          DT::dataTableOutput(outputId = "HEmulti")
        )
      )
    )
  ),
  
  fluidRow(
    column(
      width=12,
      bsCollapse(
        id = "single_analysis_panel", open = "single_analysis_options",
        
        bsCollapsePanel(
          title = "Single-Sample Enrichment", value = "single_analysis_options",
          plotly::plotlyOutput(outputId = "hePlotCluster")
        )
      )
    )
  )
)

