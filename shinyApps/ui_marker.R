
#Marker Explorer Page####
fluidRow(
  class="portal-marker",
  
  column(
    width=12,
    
    bsCollapse(
      id = "marker_opt_panel", open = "marker_options",
      
      bsCollapsePanel(
        title = "Options", value = "marker_options",
        
        fluidRow(
          column(
            width=4,
            uiOutput(outputId = "marker_option") %>% withSpinner(type=4, color="#0dc5c1", proxy.height="80px")
          ),
          
          column(
            width=4,
            radioButtons(
              inputId = "marker_view",  label = "Plot Type:", choices = c("Density", "Boxplot"), selected = "Density", inline=TRUE
            )
          ),
          
          column(
            width=4,
            uiOutput(outputId = "TAS_view")
          )
        ),
        
        conditionalPanel(
          condition = "input.marker == 'Genes'",
          
          fluidRow(
            column(
              width=4,
              uiOutput(outputId = "marker_gene_options") %>% withSpinner(type=4, color="#0dc5c1", proxy.height="80px")
            )
          )
        ),
        
        conditionalPanel(
          condition = "input.marker == 'Gene Sets'",
          
          fluidRow(
            column(
              width=4,
              selectInputWithTooltip(
                inputId = "marker_gsname",
                label = "Gene set name", 
                bId= "Bgsname_marker",
                helptext = helptext_geneset(),
                choices = names(dsmap())
              )
            ),
            
            column(
              width=4,
              selectInputWithTooltip(
                inputId = "marker_gsmethod", 
                label ="Projection method", 
                bId = "Bgsmethod_marker",
                helptext = helptext_method,
                choices = names(dsmap_method)
              )
            ),
            
            column(
              width=4,
              uiOutput(outputId = "marker_geneset_options") %>% withSpinner(type=4, color="#0dc5c1", proxy.height="80px")
            )
          ),
          
          fluidRow(
            column(
              width=12,
              uiOutput(outputId = "marker_geneset_btn")
            )
          )
        ),
        
        conditionalPanel(
          condition = "input.marker == 'CMap Connectivity'",
          
          fluidRow(
            column(
              width=4,
              selectInput(
                inputId = "marker_conn_name",
                label = "Connectivity Level:", 
                choices = connmap
              )
            ),
            
            column(
              width=4,
              uiOutput(outputId = "marker_conn_options") %>% withSpinner(type=4, color="#0dc5c1", proxy.height="80px")
            )
          ),
          
          fluidRow(
            column(
              width=12,
              uiOutput(outputId = "marker_conn_btn")
            )
          )
        )
      )
    ),
    
    conditionalPanel(
      condition = "input.de_generate >= 1 | input.gs_generate >= 1 | input.conn_generate >= 1",
      
      fluidRow(
        column(
          width=12,
          div(
            class="header-3",
            div(class="content",
                plotlyOutput(outputId = "marker_plot_1", width="auto") %>% withSpinner(type=4, color="#0dc5c1")
            )
          )
        )
      ),
      
      br(),
      
      uiOutput(outputId = "exposure_phenotype_plot"),
      
      br(),
      
      fluidRow(
        column(
          width=12,
          uiOutput(outputId = "marker_table_header"),
          DT::dataTableOutput(outputId = "marker_table") %>% withSpinner(type=4, color="#0dc5c1")
        )
      )
    )
  )
)
  