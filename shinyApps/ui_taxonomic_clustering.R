

#Taxonomic Clustering Page####
div(
  class="portal-taxonomer",
  
  fluidRow(
    class="col-container-1",
    
    column(
      width=12,
      div(
        class="header-1",
        div(class="title", "Member Search"),
        div(
          class="content",
          fluidRow(
            column(
              width=6,
              strong("Enter a search string"),
              div(
                class = "search_member",
                textInput(inputId = "mstring", label=NULL, value="", width = "auto"),
                actionButton(inputId = "search_string", label = "Search", class="mybuttons"),
                actionButton(inputId = "reset_string", label = "Reset", class="mybuttons")
              )
            ), 
            
            column(
              width = 6,
              shinyjs::hidden(
                selectizeInput(inputId = "mVal", label = "Results", choices = c("Select from an option below"=""), multiple=FALSE, width = "auto")
              )
            )
          )
        )
      )
    )
  ),
  
  tags$table(
    id="clusteringTable",
    
    tags$tbody(
      tags$tr(
        tags$td(
          fluidRow(
            class="col-container-2", 
            column(
              width=12,
              div(
                class="header-2",
                div(class="title", "K2Taxonomer Results"),
                div(class="content", 
                   visNetworkOutput(outputId="dendro", width="100%", height="650px") %>% withSpinner(type=4, color="#0dc5c1"),
                   shinyjs::hidden(
                     div(id="qvalues-legend",
                       div(class="BuGn-text",
                           div(class="q-7-text", "Q-Values:"), 
                           div(class="q0-7-text", "1"),
                           div(class="q1-7-text", "0.25"),
                           div(class="q2-7-text", "0.1"),
                           div(class="q3-7-text", "0.05"),
                           div(class="q4-7-text", "0.01"),
                           div(class="q5-7-text", "0.001"),
                           div(class="q6-7-text", "< 0.001")
                       ),
                       div(class="BuGn",
                           div(class="q-7"),
                           div(class="q0-7"),
                           div(class="q1-7"),
                           div(class="q2-7"),
                           div(class="q3-7"),
                           div(class="q4-7"),
                           div(class="q5-7"),
                           div(class="q6-7")
                       )
                     )
                   )
                )
              )
            )
          )
        ),
        
        tags$td(
          fluidRow(
            class="col-container-2", 
            
            column(
              width=12, 
              
              div(
                class="header-2",
                div(class="title", HTML("Cluster Information (&#8593; = Group 1; &#8595; = Group 2)")),
                div(
                  class="content", 
                  
                  tabsetPanel(
                    id="taxonomer_tab", selected = "Stability",
                    
                    tabPanel(
                      title = "Stability", value="Stability",
                      
                      fluidRow(
                        column(
                          width = 8, offset = 2,
                          shinyjs::hidden(
                            selectizeInput(inputId = "selCov", label = NULL, choices=c("Add Annotation to Heatmap" = ""), multiple=TRUE, width="100%")
                          )
                        ),
                        column(
                          width = 12,
                          plotlyOutput(outputId = "heatmapPlot", width = "auto", height = "auto") %>% withSpinner(type=4, color="#0dc5c1")
                        ),
                        column(
                          width = 12,
                          DT::dataTableOutput(outputId = "stabStats")
                        )
                      )
                    ),
                    
                    tabPanel(
                      title = "Group Members", value="Group Membership",
                      
                      fluidRow(
                        column(
                          width=12,
                          DT::dataTableOutput(outputId = "infoTab") %>% withSpinner(type=4, color="#0dc5c1"),
                          checkboxInput(inputId = "viewAll", label = "Display all columns", value = FALSE, width = "100%")
                        )
                      )
                    ),
                    
                    tabPanel(
                      title = "Meta-Variable Results", value = "Meta Variable Results",
                      
                      fluidRow(
                        column(
                          width=12,
                          DT::dataTableOutput(outputId = "metaVarTab") %>% withSpinner(type=4, color="#0dc5c1")
                        ),
                        
                        column(
                          width=12,
                          shinyjs::hidden(
                            checkboxInput(inputId = "visualizeQvalues", label = "Visualize Q-values", value = FALSE, width = "100%")
                          )
                        )
                      )
                    )
                  )
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
              id = "geneHelpShow",
              title = "Help",
              trigger = "geneHelp",
              size = "large",
              div(HTML("Use ^SEARCHTERM$ to filter for exact matches in columns. Used | to combine multiple terms.")),
              div(HTML("For example: search for ^FABP4$|^ADIPOQ$|^CD36$ in the <b>Gene</b> column"))
            ),
            shinyBS::bsModal(
              id = "geneTabDL",
              title = "Download",
              trigger = "geneDL",
              size = "large",
              downloadButton("downloadGeneCSV", "Download Table as CSV file")
            ),
            DT::dataTableOutput(outputId = "DGE") %>% withSpinner(type=4, color="#0dc5c1")
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
          plotly::plotlyOutput(outputId = "genePlot", width = "100%", height="400px") %>% withSpinner(type=4, color="#0dc5c1"),
          br(),
          shinyjs::hidden(
            div(
              id="Gene-legend",
              div(id="Gene-Group1", class="Greys-K2Taxonomer",
                  div(class="q0-3-text", "Group 1"),
                  div(class="q0-3")
              ),
              div(id="Gene-Vehicle", class="Greys-K2Taxonomer",
                  div(class="q1-3-text", "Vehicle"),
                  div(class="q1-3")
              ),
              div(id="Gene-Group2", class="Greys-K2Taxonomer",
                  div(class="q2-3-text", "Group 2"),
                  div(class="q2-3")
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
              id = "hyperHelpShow",
              title = "Help",
              trigger = "hyperHelp",
              size = "large",
              div(HTML("Use ^SEARCHTERM$ to filter for exact matches in columns. Used | to combine multiple terms.")),
              div(HTML("For example: search for ^NURSA ERS ESTROGENS$|^REACTOME AXON GUIDANCE$ in the <b>Gene Set</b> column"))
            ),
            shinyBS::bsModal(
              id = "hyperTabDL",
              title = "Download",
              trigger = "hyperDL",
              size = "large",
              downloadButton("downloadHyperCSV", "Download Table as CSV file")
            ),
            DT::dataTableOutput(outputId = "HE") %>% withSpinner(type=4, color="#0dc5c1")
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
          plotly::plotlyOutput(outputId = "pathwayPlot", width = "100%", height="400px")  %>% withSpinner(type=4, color="#0dc5c1"),
          br(),
          shinyjs::hidden(
            div(
              id="HE-legend",
              div(id="HE-Group1", class="Greys-K2Taxonomer",
                  div(class="q0-3-text", "Group 1"),
                  div(class="q0-3")
              ),
              div(id="HE-Vehicle", class="Greys-K2Taxonomer",
                  div(class="q1-3-text", "Vehicle"),
                  div(class="q1-3")
              ),
              div(id="HE-Group2", class="Greys-K2Taxonomer",
                  div(class="q2-3-text", "Group 2"),
                  div(class="q2-3")
              )
            )
          )
        )
      )
    )
  )
)

