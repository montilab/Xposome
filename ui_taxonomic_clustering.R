

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
              selectInput(inputId = "mVal", label="Select a Match:", choices="No Matches", selectize = TRUE)
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
                h2("Column 2"),
                p("Hello World!")
            )
          )
        ),
        
        column(
          width=6, 
          
          div(class="header-2",
              div(class="title", "Cluster Information (↑ = Group 1; ↓ = Group 2)"),
              div(class="content", 
                  tabsetPanel(
                    id="de_tab", 
                    tabPanel(
                      title = "Stability", value="Stability",
                      h2("Column 3"),
                      p("Stability"),
                      p("Hello World!")
                    ),
                    tabPanel(
                      title = "Group Members", value="Group_Members",
                      h2("Column 3"),
                      p("Group Members"),
                      p("Hello World!")
                    ),
                    tabPanel(
                      title = "Meta-Variable Results", value="Meta_Variable_Results",
                      h2("Column 3"),
                      p("Meta-Variable Results"),
                      p("Hello World!")
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
            id = "ge_analysis_panel", open = "ge_analysis_options",
            
            bsCollapsePanel(
              title = "Gene Expression", value = "ge_analysis_options",
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
            id = "enrichment_analysis_panel", open = "enrichment_analysis_options",
            
            bsCollapsePanel(
              title = "Enrichment Results", value = "enrichment_analysis_options",
              h2("Column 6"),
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
              h2("Column 7"),
              p("Hello World!")
            )
          )
        )
      )
    )
  )
)

