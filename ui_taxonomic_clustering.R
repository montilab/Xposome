

#Taxonomic Clustering Page####
tagList(
  fluidRow(
    class="col-container-1",
    
    column(
      width=3, style="background-color: #fec44f;",
      div(class="header-1",
          div(class="title", "Member Search"),
          div(class="content",
              textInput(inputId = "mstring", label="Enter a String:"),
              uiOutput("Match"),
              actionButton(inputId = "Helpgo", label = "Help", class="mybuttons")
          )
      )
    ),
    
    column(
      width=9, style="background: lightgray;",
      
      fluidRow(
        class="col-container-2",
        
        column(
          width=6,
          
          div(class="header-2",
            div(class="title", "Taxonomer Results"),
            div(class="content", 
                visNetworkOutput(outputId = "dendro", width="100%", height = "600px")
            )
          )
        ),
        
        column(
          width=6, 
          
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
                          plotlyOutput(outputId = "heatmapPlot")
                        ),
                        column(
                          width = 10, offset = 1,
                          uiOutput(outputId = "stabStats")
                        )
                      )
                    ),
                    
                    tabPanel(
                      title = "Group Members", value="Group_Members",
                      DT::dataTableOutput(outputId = "infoTab")
                    ),
                    
                    tabPanel(
                      title = "Meta-Variable Results", value="Meta_Variable_Results",
                      column(
                        width=12,
                        DT::dataTableOutput(outputId = "metaVarTab")
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
        column(
          width=12,
          bsCollapse(
            id = "de_analysis_panel", open = "de_analysis_options",
            
            bsCollapsePanel(
              title = "Differential Analysis Results", value = "de_analysis_options",
              DT::dataTableOutput(outputId = "DGE")
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
              plotly::plotlyOutput(outputId = "genePlot")
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
              DT::dataTableOutput(outputId = "HE")
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
              plotly::plotlyOutput(outputId = "pathwayPlot")
            )
          )
        )
      )
    )
  )
)

