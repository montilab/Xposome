

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
        div(class="content",
            fluidRow(
              column(
                width=6,
                textInput(inputId = "mstring", label="Enter a String:", width = "100%"),
                actionButton(inputId = "search_string", label = "Search", class="mybuttons"),
                actionButton(inputId = "Helpgo", label = "Help", class="mybuttons")
              ), 
              
              column(
                width = 6,
                uiOutput("Match")
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
                div(class="title", "Taxonomer Results"),
                div(class="content", 
                   visNetworkOutput(outputId="dendro", width="auto", height="650px") %>% withSpinner(type=4, color="#0dc5c1")
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
                          width = 10, offset = 1,
                          br(),
                          selectInput(inputId = "selCov", label = NULL, choices=c("Add Annotation to Heatmap:" = ""))
                        ),
                        column(
                          width = 12,
                          plotlyOutput(outputId = "heatmapPlot", width = "auto") %>% withSpinner(type=4, color="#0dc5c1")
                        ),
                        column(
                          width = 10, offset = 1,
                          uiOutput(outputId = "stabStats"),
                          br(), br()
                        )
                      )
                    ),
                    
                    tabPanel(
                      title = "Group Members", value="Group Members",
                      
                      fluidRow(
                        column(
                          width=12,
                          br(),
                          DT::dataTableOutput(outputId = "infoTab") %>% withSpinner(type=4, color="#0dc5c1"),
                          checkboxInput(inputId = "viewAll", label = "Display all columns", value = FALSE, width = "100%"),
                          br(), br()
                        )
                      )
                    ),
                    
                    tabPanel(
                      title = "Meta-Variable Results", value="Meta Variable Results",
                      
                      fluidRow(
                        column(
                          width=12,
                          br(),
                          DT::dataTableOutput(outputId = "metaVarTab") %>% withSpinner(type=4, color="#0dc5c1")
                        ),
                        
                        column(
                          width=12,
                          br(), 
                          actionButton(inputId = "visualizeQvalues", label = "Visualize Q-values", class="mybuttons"),
                          actionButton(inputId = "resetQvalues", label = "Reset", class="mybuttons"),
                          br(), br()
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
        div(class="content",
            plotly::plotlyOutput(outputId = "genePlot", width = "auto") %>% withSpinner(type=4, color="#0dc5c1")
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
        div(class="content",
            plotly::plotlyOutput(outputId = "pathwayPlot", width = "auto") %>% withSpinner(type=4, color="#0dc5c1")
        )
      )
    )
  )
)

