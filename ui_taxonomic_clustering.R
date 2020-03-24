

#Taxonomic Clustering Page####
tagList(
  fluidRow(
    class="col-container-1",
    
    column(
      width=3, style="background-color: #fec44f; width: 30%; border: 1px solid lightgray;",
      
      div(class="header-1",
          div(class="title", "Member Search"),
          div(class="content",
              textInput(inputId = "mstring", label="Enter a String:", width = "100%"),
              uiOutput("Match"),
              actionButton(inputId = "Helpgo", label = "Help", class="mybuttons")
          )
      )
    ),
    
    column(
      width=9, style="background: lightgray; width: 70%;",
      
      fluidRow(
        class="col-container-2",
        
        column(
          width=6, style="background: white; width: 100%;",
          
          div(class="header-2",
            div(class="title", "Taxonomer Results"),
            div(class="content", 
                visNetworkOutput(outputId = "dendro", width="100%", height = "600px") %>% withSpinner(type=4, color="#0dc5c1", proxy.height="200px")
            )
          )
        ),
        
        column(
          width=6, style="background: white; width: 100%;",
          
          div(class="header-2",
              div(class="title", "Cluster Information (↑ = Group 1; ↓ = Group 2)"),
              div(class="content", 
                  tabsetPanel(
                    id="de_tab", selected = "Stability",
                    tabPanel(
                      title = "Stability", value="Stability",
                      fluidRow(
                        column(
                          width = 10, offset = 1,
                          br(),
                          selectInput(inputId = "selCov", label = NULL, choices=varOptions)
                        ),
                        column(
                          width = 12,
                          plotlyOutput(outputId = "heatmapPlot") %>% withSpinner(type=4, color="#0dc5c1", proxy.height="200px")
                        ),
                        column(
                          width = 10, offset = 1,
                          uiOutput(outputId = "stabStats")
                        )
                      )
                    ),
                    
                    tabPanel(
                      title = "Group Members", value="Group_Members",
                      DT::dataTableOutput(outputId = "infoTab") %>% withSpinner(type=4, color="#0dc5c1", proxy.height="200px")
                    ),
                    
                    tabPanel(
                      title = "Meta-Variable Results", value="Meta_Variable_Results",
                      column(
                        width=12,
                        DT::dataTableOutput(outputId = "metaVarTab") %>% withSpinner(type=4, color="#0dc5c1", proxy.height="200px")
                      ),
                      
                      column(
                        width=12,
                        br(), 
                        actionButton(inputId = "visualizeQvalues", label = "Visualize Q-values", class="mybuttons"),
                        actionButton(inputId = "resetQvalues", label = "Reset", class="mybuttons")
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
          width=12, style="background: white; width: 100%;",
          
          div(class="header-3",
              div(class="title", "Differential Analysis Results"),
              div(class="content",
                  DT::dataTableOutput(outputId = "DGE") %>% withSpinner(type=4, color="#0dc5c1", proxy.height="200px")
              )
          )
        )
      ),
      
      fluidRow(
        class="col-container-3",
        
        column(
          width=12, style="background: white; width: 100%;",
          
          div(class="header-3",
              div(class="title", "Gene Expression"),
              div(class="content",
                  plotly::plotlyOutput(outputId = "genePlot") %>% withSpinner(type=4, color="#0dc5c1", proxy.height="200px")
              )
          )
        )
      ),
      
      fluidRow(
        class="col-container-2",
        
        column(
          width=12, style="background: white; width: 100%;",
          
          div(class="header-3",
              div(class="title", "Enrichment Results"),
              div(class="content",
                  DT::dataTableOutput(outputId = "HE") %>% withSpinner(type=4, color="#0dc5c1", proxy.height="200px")
              )
          )
        )
      ),
      
      fluidRow(
        class="col-container-3",
        
        column(
          width=12, style="background: white; width: 100%;",
          
          div(class="header-3",
              div(class="title", "Single-Sample Enrichment"),
              div(class="content",
                  plotly::plotlyOutput(outputId = "pathwayPlot") %>% withSpinner(type=4, color="#0dc5c1", proxy.height="200px")
              )
          )
        )
      )
    )
  )
)

