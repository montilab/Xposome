
#Marker Explorer Page####
fluidRow(
  class="portal-marker",
  
  bsCollapse(
    id = "marker_opt_panel", open = "marker_options",
    
    bsCollapsePanel(
      title = "Options", value = "marker_options",
      
      fluidRow(
        column(
          width=4,
          selectInput(
            inputId = "marker",
            label = "Select a marker set:",
            choices = c("Please select an option below" = "", "Genes", "Gene Sets", "CMap Connectivity")
          )
        ),
        
        column(
          width=4,
          sliderInput(
            inputId = "marker_tas", label = "TAS range:", min = 0,  max = 1, value = c(0, 0.2), step = 0.1
          )
        ),
        
        column(
          width=4,
          radioButtons(
            inputId = "marker_view", 
            label = "Plot Type:",
            choices = c("Density", "Boxplot"), 
            selected = "Density", inline=TRUE
          )
        )
      ),
      
      conditionalPanel(
        condition = "input.marker == 'Genes'",
        
        fluidRow(
          column(
            width=4,
            shinyjs::disabled(
              selectInput(inputId = "marker_gene", label = "Select a gene:", choices = "")
            )
          )
        ),
        
        fluidRow(
          column(
            width=12,
            br(),
            actionButton(inputId = "de_generate", label = "Generate plot", icon=icon("fas fa-arrow-circle-right"), class="mybuttons")
          )
        )
      ),
      
      conditionalPanel(
        "input.marker == 'Gene Sets'",
        
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
            shinyjs::disabled(
              selectInput(inputId="marker_gs", label="Select a gene set:", choices = "")
            )
          )
        ),
        
        fluidRow(
          column(
            width=12,
            br(),
            actionButton(inputId = "es_generate", label = "Generate plot", icon=icon("fas fa-arrow-circle-right"), class="mybuttons")
          )
        )
      ),
      
      conditionalPanel(
        "input.marker == 'CMap Connectivity'",
        
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
            shinyjs::disabled(
              selectInput(inputId = "marker_conn", label = "Select a gene set:", choices = "")
            )
          )
        ),
        
        fluidRow(
          column(
            width=12,
            br(),
            actionButton(inputId = "conn_generate", label = "Generate plot", icon=icon("fas fa-arrow-circle-right"), class="mybuttons")
          )
        )
      )
    )
  ),
  
  conditionalPanel(
    condition = "input.de_generate >= 1 | input.es_generate >= 1 | input.conn_generate >= 1",
    
    fluidRow(
      column(
        width=12,
        div(class="text-md-right",
            downloadButton(outputId = "marker_download_pdf", label = "Download pdf", class="mybuttons"),
            downloadButton(outputId = "marker_download_png", label = "Download png", class="mybuttons")
        )
      )
    ),
    
    br(),
    
    fluidRow(
      column(
        width=12,
        plotOutput(outputId = "marker_plot_1") %>% withSpinner(type=4, color="#0dc5c1", proxy.height="200px")
      ),
      
      column(
        width=12,
        plotOutput(outputId = "marker_plot_2")
      ),
      
      column(
        width=12,
        if(experimental_design() == "Multiple replicates"){
          plotOutput(outputId = "marker_plot_3")
        }
      ),
      
      column(
        width=12,
        uiOutput(outputId = "marker_table_header"),
        DT::dataTableOutput(outputId = "marker_table") %>% withSpinner(type=4, color="#0dc5c1", proxy.height="200px")
      )
    )
  )
)

