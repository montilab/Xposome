

#Taxonomic Clustering Page####
tagList(
  fluidRow(
    class="col-container-2",
    
    column(
      width=6,
      
      div(class="header-3",
          div(class="title", "Click and hold nodes to select sub-groups"),
          div(class="content", 
              h2("Column 1"),
              p("Hello World!")
          )
      )
    ),
    
    column(
      width=6, 
      
      div(class="header-3",
          div(class="title", "Selections"),
          div(class="content", 
              h2("Column 2"),
              p("Hello World!")
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
          h2("Column 3"),
          p("Hello World!")
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
          h2("Column 4"),
          p("Hello World!")
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
          h2("Column 5"),
          p("Hello World!")
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
          h2("Column 6"),
          p("Hello World!")
        )
      )
    )
  )
)

