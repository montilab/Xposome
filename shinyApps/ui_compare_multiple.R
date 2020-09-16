

#Taxonomic Compare Multiple Page####
div(
  class="portal-compare-multiple",
  
  fluidRow(
    class="col-container-3",
    
    column(
      width=6,
      
      div(
        class="header-3",
        div(class="title", "Click and hold nodes to select sub-groups"),
        div(class="content", 
            visNetworkOutput(outputId="dendroSelect", width="auto", height="600px") %>% withSpinner(type=4, color="#0dc5c1") 
        )
      )
    ),
    
    column(
      width=6, 
      
      div(
        class="header-3",
        div(class="title", "Selections"),
        div(class="content", 
            actionButton(inputId = "compareReset", label = strong("Reset"), class = "mybuttons"),
            div(style="display: inline-block", 
                uiOutput(outputId = "compare")
            ),
            br(), br(),
            uiOutput(outputId = "groupSel")
        )
      )
    )
  ),
  
  fluidRow(
    class="col-container-3",
    
    column(
      width=12, 
      
      div(
        class="header-3",
        div(class="title", "Differential Analysis Results"),
        div(class="content",
            DT::dataTableOutput(outputId = "DGEmulti") %>% withSpinner(type=4, color="#0dc5c1")
        )
      )
    )
  ),
  
  fluidRow(
    class="col-container-3",
    
    column(
      width=12, 
      
      div(
        class="header-3",
        div(class="title", "Gene Expression"),
        div(class="content",
            plotly::plotlyOutput(outputId = "genePlotCluster", width = "auto") %>% withSpinner(type=4, color="#0dc5c1")
        )
      )
    )
  ),
  
  fluidRow(
    class="col-container-3",
    
    column(
      width=12, 
      
      div(
        class="header-3",
        div(class="title", "Enrichment Results"),
        div(class="content",
            DT::dataTableOutput(outputId = "HEmulti") %>% withSpinner(type=4, color="#0dc5c1")
        )
      )
    )
  ),
  
  fluidRow(
    class="col-container-3",
    
    column(
      width=12,
      
      div(
        class="header-3",
        div(class="title", "Single-Sample Enrichment"),
        div(class="content",
            plotly::plotlyOutput(outputId = "hePlotCluster", width = "auto") %>% withSpinner(type=4, color="#0dc5c1")
        )
      )
    )
  )
)

