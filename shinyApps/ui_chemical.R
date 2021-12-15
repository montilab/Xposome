
##Chemical Explorer Page####
fluidRow(
  class="portal-chemical",
  
  column(
    width=6, offset=6,
    selectizeInput(
      inputId = "chem",
      label = "Select a Chemical ID/CAS ID",
      choices = NULL,
      width = "100%",
      multiple = FALSE
    )
  ),
  
  column(
    width=12,
    DT::dataTableOutput("chemical_table")
  ),
  
  column(  
    width=12, 
    
    tabsetPanel(
      id="chemical_tab", type="pills", 
      
      #Gene expression tab####
      tabPanel(
        title="Gene Expression", value="gene_expression",
        
        div(
          class="header", id="de_opt_panel",
          div(class="title", "Options"),
          div(
            class="content",
            fluidRow(
              column(
                width=2,
                checkboxInput(inputId = "landmark_de", label = "Landmark only", value = de_defaults[["landmark_de"]])
              ),
              
              column(
                width=2,
                selectInput(inputId = "summarizefunc_de", label = "Summarization", choices=c("max", "median", "mean", "min", "Q1", "Q3"), selected = de_defaults[["summarizefunc_de"]])
              ),
              
              column(
                width=2, 
                checkboxGroupInput(inputId = "filterbyinput_de", label = "Filter by", choices=c("score" = "score", "number" = "number"), selected = de_defaults[["filterbyinput_de"]])
              ),
              
              column(
                width=2, 
                sliderInput(inputId = "range_de", label = "Score Threshold", min = -10, max = 10, step = 0.01, value = de_defaults[["range_de"]])
              ),
              
              column(
                width=2,
                sliderInput(inputId = "numberthresleft_de", label = "Num -", min = 0, max = 1000, ticks = FALSE, step = 10, value = de_defaults[["numberthresleft_de"]])
              ),
              
              column(
                width=2,
                sliderInput(inputId = "numberthresright_de", label = "Num +", min = 0, max = 1000, ticks = FALSE, step = 10, value = de_defaults[["numberthresright_de"]])
              )
            )
          )
        ),
        
        fluidRow(
          column(
            width=12,
            actionButton(inputId = "de_restore", label = "Restore Defaults", class="mybuttons"),
            actionButton(inputId = "de_hide", label = "Hide", class="mybuttons"),
            actionButton(inputId = "de_show", label = "Show", class="mybuttons")
          )
        ),
        
        br(),
        
        fluidRow(
          column(
            width=12,
            DT::dataTableOutput("gene_expression_table") %>% withSpinner(type=4, color="#0dc5c1")
          )
        )
      ),
      
      #Gene set enrichment tab####
      tabPanel(
        title="Gene Set Enrichment", value="gene_set_enrichment",
        
        div(
          class="header", id="es_opt_panel",
          div(class="title", "Options"),
          div(
            class="content",
            fluidRow(
              column(
                width=4,
                div(id = "gsname_label"),
                selectizeInput(
                  inputId = "gsname",
                  label = NULL,
                  choices = NULL,
                  multiple = FALSE
                )
              ),
              
              column(
                width=4,
                selectizeInput(
                  inputId = "gsmethod",
                  label = HTML("Projection method", paste0('<button type="button" class="tooltip-txt" data-html="true" data-tooltip-toggle="tooltip" data-placement="top" title=\"', helptext_gsva_method, '\">?</button>')),
                  choices = gsva_method,
                  multiple = FALSE
                )
              ),
              
              column(
                width=4,
                selectizeInput(
                  inputId = "summarizefunc_gs",
                  label = "Sort by",
                  choices = c("max", "median", "mean", "min", "Q1", "Q3"),
                  selected = "median",
                  multiple = FALSE
                )
              )
            )
          )
        ),
        
        fluidRow(
          column(
            width=12,
            actionButton(inputId = "es_restore", label = "Restore Defaults", class="mybuttons"),
            actionButton(inputId = "es_hide", label = "Hide", class="mybuttons"),
            actionButton(inputId = "es_show", label = "Show", class="mybuttons")
          )
        ),
        
        br(),
        
        fluidRow(
          column(
            width=12,
            DT::dataTableOutput("gene_set_enrichment_table") %>% withSpinner(type=4, color="#0dc5c1")
          )
        )
      ),
      
      #Connectivity tab####
      tabPanel(
        title="Connectivity", value="connectivity",
        
        div(
          class="header",
          div(class="title", "Options"), id="conn_opt_panel",
          div(
            class="content",
            fluidRow(
              column(
                width=4,
                selectizeInput(
                  inputId = "conn_name",
                  label = "Connectivity Level",
                  choices = connmap,
                  multiple = FALSE
                )
              ),
              
              column(
                width=4,
                selectizeInput(
                  inputId = "summarizefunc_conn",
                  label = "Sort by",
                  choices = c("max", "median", "mean", "min", "Q1", "Q3"),
                  selected = "median",
                  multiple = FALSE
                )
              )
            )
          )
        ),
        
        fluidRow(
          column(
            width=12,
            actionButton(inputId = "conn_restore", label = "Restore Defaults", class="mybuttons"),
            actionButton(inputId = "conn_hide", label = "Hide", class="mybuttons"),
            actionButton(inputId = "conn_show", label = "Show", class="mybuttons")
          )
        ),
        
        br(),
        
        fluidRow(
          column(
            width=12,
            DT::dataTableOutput("connectivity_table") %>% withSpinner(type=4, color="#0dc5c1")
          )
        )
      )
    )
  )
) 




