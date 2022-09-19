
#Marker Explorer Page####
fluidRow(
  class="portal-marker",
  
  column(
    width=12,
    
    div(
      class="header",
      div(class="title", "Options"),
      div(
        class="content",
        fluidRow(
          column(
            width=4,
            selectizeInput(
              inputId = "marker",
              label = "Select a marker set",
              choices = NULL,
              multiple = FALSE,
              width="auto"
            )
          ),
          
          column(
            width=4,
            radioButtons(
              inputId = "marker_view",  
              label = "Plot Type", 
              choices = c("Density", "Boxplot"), 
              selected = "Density", 
              inline=TRUE
            )
          ),
          
          column(
            width=4,
            sliderInput(inputId="marker_tas", label="TAS", min=0, max=1, step=0.1, value=c(0, 1))
          )
        ),
          
        conditionalPanel(
          condition = "input.marker == 'Genes'",
          fluidRow(
            column(
              width=4,
              selectizeInput(
                inputId = "marker_gene", 
                label = "Select a gene", 
                choices = c("Select an option below" = ""),
                multiple = FALSE,               
                width="auto"
              )
            )
          )
        ),
          
        conditionalPanel(
          condition = "input.marker == 'Gene Sets'",
          fluidRow(
            column(
              width=4,
              div(id = "marker_gsname_label"),
              selectizeInput(
                inputId = "marker_gsname",
                label = NULL,
                choices = NULL,
                multiple = FALSE,
                width="auto"
              )
            ),
            
            column(
              width=4,
              selectizeInput(
                inputId = "marker_gsmethod",
                label = HTML("Projection method", paste0('<button type="button" class="tooltip-txt" data-html="true" data-tooltip-toggle="tooltip" data-placement="top" title=\"', helptext_gsva_method, '\">?</button>')),
                choices = gsva_method,
                multiple = FALSE,
                width="auto"
              )
            ),
            
            column(
              width=4,
              selectizeInput(
                inputId="marker_gs", 
                label="Select a gene set", 
                choices = c("Select an option below"=""),
                multiple = FALSE,
                width="auto"
              )
            )
          )
        ),
        
        conditionalPanel(
          condition = "input.marker == 'CMap Connectivity'",
          fluidRow(
            column(
              width=4,
              selectizeInput(
                inputId = "marker_conn_name",
                label = "Connectivity Level", 
                choices = connmap,
                multiple = FALSE,
                width="auto"
              )
            ),
            
            column(
              width=4,
              selectizeInput(
                inputId = "marker_conn", 
                label = "Select a gene set", 
                choices = c("Select an option below"=""),
                multiple = FALSE,
                width="auto"
              )
            )
          )
        ),
        
        shinyjs::hidden(
          fluidRow(
            id="warning_content",
            h4(id="tas_warning_msg", HTML("None of the TAS lies within the selected boundary. Please select another zone!"))
          )
        ),
        
        conditionalPanel(
          condition = "input.marker == 'Genes' && input.marker_gene !== ''",
          fluidRow(
            id="plot_content",
            
            column(
              width=12,
              plotlyOutput(outputId = "de_marker_plot", width="auto", height="auto")
            ),
            
            column(
              width=12,
              plotlyOutput(outputId = "de_exposure_phenotype_plot", width="auto", height="auto")
            ),
            
            column(
              width=12,
              uiOutput(outputId = "de_marker_tbl_header"),
              DT::dataTableOutput(outputId = "de_marker_table")
            )
          )
        ),
        
        conditionalPanel(
          condition = "input.marker == 'Gene Sets' && input.marker_gs !== ''",
          fluidRow(
            id="plot_content",
            
            column(
              width=12,
              plotlyOutput(outputId = "gs_marker_plot", width="auto", height="auto")
            ),
            
            column(
              width=12,
              plotlyOutput(outputId = "gs_exposure_phenotype_plot", width="auto", height="auto")
            ),
            
            column(
              width=12,
              uiOutput(outputId = "gs_marker_tbl_header"),
              DT::dataTableOutput(outputId = "gs_marker_table")
            )
          )
        ),
        
        conditionalPanel(
          condition = "input.marker == 'CMap Connectivity' && input.marker_conn !== ''",
          fluidRow(
            id="plot_content",
            
            column(
              width=12,
              plotlyOutput(outputId = "conn_marker_plot", width="auto", height="auto")
            ),
            
            column(
              width=12,
              plotlyOutput(outputId = "conn_exposure_phenotype_plot", width="auto", height="auto")
            ),
            
            column(
              width=12,
              uiOutput(outputId = "conn_marker_tbl_header"),
              DT::dataTableOutput(outputId = "conn_marker_table")
            )
          )
        )
      )
    )
  )
)

