
## portal page ####
div(
  class="portal-page",
  
  fluidRow(
    column(
      width=12,
      div(id='portal_title')
    )
  ),
  
  fluidRow(
    column(
      width=12,
      div(
        id="portal-description", style="display: none;", 
        div(id='portal_description')
      )
    )
  ),
  
  fluidRow(
    column(
      width=4,
      selectizeInput(inputId="portal_id", label=NULL, choices=projectlist$Portal, width="100%", multiple=FALSE)
    ),
    column(
      width=8,
      downloadButton(outputId="download_portal", label="Download Project Data", icon=shiny::icon("download"), class="download-btn")
    )
  ),
  
  fluidRow(
    column(
      width=12,
      tabsetPanel(
        id="main_page", type="pills",
        
        ###Annotation#####
        tabPanel(
          title="Annotation", value="annotation",
          #"Hello World!"
          source("ui_annotation.R", local=TRUE)$value
        ),
        
        ###Chemical Explorer#####
        tabPanel(
          title="Chemical Explorer", value="chemical_explorer",
          #"Hello World!"
          source("ui_chemical.R", local=TRUE)$value
        ),
        
        ###Marker Explorer####
        tabPanel(
          title="Marker Explorer", value="marker_explorer",
          #"Hello World!"
          source("ui_marker.R", local=TRUE)$value
        ),
        
        ###Heatmap Explorer####
        tabPanel(
          title="Heatmap Explorer", value="heatmap_explorer",
          #"Hello World!"
          source("ui_heatmap.R", local=TRUE)$value
        ),
        
        ###Taxonomic Clustering####
        tabPanel(
          title="K2 Taxonomer Results", value="k2_taxonomer_results",
          #"Hello World!"
          source("ui_taxonomic_clustering.R", local=TRUE)$value
        ),
        
        tabPanel(
          title="K2 Compare Multiple", value="k2_compare_multiple",
          #"Hello World!"
          source("ui_compare_multiple.R", local=TRUE)$value
        )
      )
    )
  )
)

  