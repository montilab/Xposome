
tagList(
  
  fluidRow(
    column(
      width=4,
      selectizeInput(
        inputId = "chem",
        label = "Select chemical:",
        choices = chemicals,
        width = "100%"
      )
    ),
    
    column(
      width=8,
        DT::dataTableOutput("chemical_table")
    )
  ),
  
  br(),
  
  fluidRow(
    column(
      width=12,
      
      tabsetPanel(
        id="chemical_tab", type="pills",
          
        tabPanel(
          title="Gene Expression",
          
          bsCollapse(
            id = "de_opt_panel", open = "Options",
            
            bsCollapsePanel(
              "Options", 
              
              fluidRow(
                column(
                  width=2, 
                  checkboxInput(inputId = "landmark_de", label = "Landmark only", value=FALSE)
                ),
                
                column(
                  width=2, 
                  selectInput(inputId = "summarizefunc_de", label = "Summarization:", choices=c("max", "median", "mean", "min", "Q1", "Q3"), selected = "median")
                ),
                
                column(
                  width=2,
                  checkboxGroupInput(inputId = "filterbyinput_de", label = "Filter by:", choices=c("score" = "score", "number" = "number"), selected = c("score", "number"))
                ),
                
                column(
                  width=2,
                  sliderInput("range_de", "Score Threshold", min = -10, max = 10, value = c(-2, 2), step = 0.01)
                ),
                
                column(
                  width=2,
                  sliderInput("numberthresleft_de", "Num +", min = 0, max = 1000, value = 10, ticks = FALSE, step = 10)
                ),
                
                column(
                  width=2,
                  sliderInput("numberthresright_de", "Num -", min = 0, max = 1000, value = 10, ticks = FALSE, step = 10)
                ) 
              )
            )
          ),
          
          fluidRow(
            column(
              width=12,
              actionButton(inputId = "restore", label = "Restore Defaults", class="mybuttons"),
              actionButton(inputId = "de_hide", label = "Hide", class="mybuttons"),
              actionButton(inputId = "de_show", label = "Show", class="mybuttons")
            )
          ),
          
          br(),
          
          fluidRow(
            column(
              width=12,
              DT::dataTableOutput("gene_expression_table") %>% withSpinner(color="#0dc5c1", proxy.height="200px")
            )
          )
        ),
        
        tabPanel(
          title="Gene Set Enrichment",
          
          fluidRow(
            column(
              width=4,
              selectInputWithTooltip(
                inputId = "gsname",
                label = "Gene set name", 
                bId= "Bgsname",
                helptext = paste(
                  "Hallmark: MSigDB Hallmark Pathways (v5.0)",
                  "C2: MSigDB C2 reactome Pathways (v5.0)",
                  "NURSA: Nuclear Receptor Signaling Atlas, consensome data for human",
                  sep = "<br>"
                ),
                choices = c("Hallmark", "C2", "NURSA")
              )
            ),
            
            column(
              width=4,
              selectInputWithTooltip(
                inputId = "gsmethod", 
                label ="Projection method", 
                bId = "Bgsmethod",
                helptext = paste(
                  "gsva, ssgea, zscore: from R Bioconductor package GSVA",
                  "gsproj: GeneSetProjection for R package montilab:CBMRtools",
                  sep = "<br>"
                ),
                choices = c("gsva", "ssgsea", "zscore", "gsproj")
              )
            ),
            
            column(
              width=4,
              selectInput(
                inputId = "summarize_gs", 
                label = "Sort by:", 
                choices = c("max", "median", "mean", "min", "Q1", "Q3"),
                selected = "median"
              )
            )
          ),
          
          fluidRow(
            column(
              width=12,
              DT::dataTableOutput("gene_set_enrichment_table") %>% withSpinner(color="#0dc5c1", proxy.height="200px")
            )
          )
        ),
        
        tabPanel(
          "Connectivity",
          
          fluidRow(
            column(
              width=4,
              selectInput(
                inputId = "conn_name",
                label = "Connectivity Level", 
                choices = c("Perturbagen Classes" = "pcl", "Perturbagens" = "pert")
              )
            ),
            
            column(
              width=4, 
              selectInput(
                inputId = "summarizefunc_conn", 
                label = "Sort by:", 
                choices = c("max", "median", "mean", "min", "Q1", "Q3"),
                selected = "median"
              )
            )
          ),
          
          fluidRow(
            column(
              width=12,
              DT::dataTableOutput("connectivity_table") %>% withSpinner(color="#0dc5c1", proxy.height="200px")
            )
          )
        )
      )
    )
  )
)



