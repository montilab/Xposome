

#Taxonomic Compare Multiple Page####
div(
  class="portal-compare-multiple",
  
  tags$table(
    id="comparingTable",
    
    tags$tbody(
      tags$tr(
        tags$td(
          fluidRow(
            class="col-container-3",
            column(
              width=12,
              div(
                class="header-1",
                div(class="title", HTML("Click and hold nodes for 2 seconds to <span style='color: green'>select</span> or <span style='color: green'>unselect</span> sub-groups")),
                div(class="content", 
                    visNetworkOutput(outputId="dendroSelect", width="auto", height="600px")  %>% withSpinner(type=4, color="#0dc5c1")
                )
              )
            )
          )
        ),
    
        tags$td(
          fluidRow(
            column(
              width=12, 
              div(
                class="header-1",
                div(class="title", HTML("Select 2 or more nodes to compare")),
                div(class="content", 
                    actionButton(inputId = "compareReset", label = strong("Reset"), class = "mybuttons"),
                    shinyjs::disabled(
                      actionButton(inputId = "compareGo", label = strong("Compare Nodes"), class = "mybuttons")
                    ),
                    DT::dataTableOutput(outputId = "groupSel") %>% withSpinner(type=4, color="#0dc5c1")
                )
              )
            )
          )
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
            shinyBS::bsModal(
              id = "geneMultiHelpShow",
              title = "Help",
              trigger = "geneHelpMulti",
              size = "large",
              div(HTML("Use ^SEARCHTERM$ to filter for exact matches in columns. Used | to combine multiple terms.")),
              div(HTML("For example: search for ^FABP4$|^ADIPOQ$|^CD36$ in the <b>Gene</b> column"))
            ),
            shinyBS::bsModal(
              id = "geneMultiTabDL",
              title = "Download",
              trigger = "geneDLMulti",
              size = "large",
              downloadButton("downloadGeneCSVMulti", "Download Table as CSV file")
            ),
            DT::dataTableOutput(outputId = "DGEmulti")  %>% withSpinner(type=4, color="#0dc5c1")
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
        div(
          class="content",
          plotly::plotlyOutput(outputId = "genePlotCluster", width = "100%", height="400px") %>% withSpinner(type=4, color="#0dc5c1"),
          br(),
          shinyjs::hidden(
            div(id="multiGene-legend",
                div(class="Greys-CompareMultiple",
                    div(class="q0-text", "Vehicle"),
                    div(class="q0")
                )
            )
          )
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
            shinyBS::bsModal(
              id = "hyperMultiHelpShow",
              title = "Help",
              trigger = "hyperHelpMulti",
              size = "large",
              div(HTML("Use ^SEARCHTERM$ to filter for exact matches in columns. Used | to combine multiple terms.")),
              div(HTML("For example: search for ^FABP4$|^ADIPOQ$|^CD36$ in the <b>Gene</b> column"))
            ),
            shinyBS::bsModal(
              id = "hyperMultiTabDL",
              title = "Download",
              trigger = "hyperDLMulti",
              size = "large",
              downloadButton("downloadHyperCSVMulti", "Download Table as CSV file")
            ),
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
        div(
          class="content",
          plotly::plotlyOutput(outputId = "hePlotCluster", width = "100%", height="400px") %>% withSpinner(type=4, color="#0dc5c1"),
          br(),
          shinyjs::hidden(
            div(
              id="multiHE-legend",
              div(class="Greys-CompareMultiple",
                  div(class="q0-text", "Vehicle"),
                  div(class="q0")
              )
            )
          )
        )
      )
    )
  )
)


