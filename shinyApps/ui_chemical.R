
##Chemical Explorer Page####
fluidRow(
  class="portal-chemical",
  
  column(
    width=4, offset = 4,
    selectInput(
      inputId = "chem",
      label = "Select a Chemical Name/BUID/CAS:",
      choices = c("Please select an option below" = ""),
      width = "100%"
    )
  ),
  
  column(
    width=12,
    DT::dataTableOutput("chemical_table") %>% withSpinner(type=4, color="#0dc5c1", proxy.height="100px")
  ),
  
  column(  
    width=12, style="padding-top: 20px;",
    
    tabsetPanel(
      id="chemical_tab", type="pills", selected=chemical_tab,
      
      #Gene expression tab####
      tabPanel(
        title="Gene Expression", value="gene_expression",
        
        bsCollapse(
          id = "de_opt_panel", open = "de_options",
          
          bsCollapsePanel(
            title = "Options", value = "de_options",
            
            fluidRow(
              column(
                width=2,
                if(landmark == FALSE){
                  shinyjs::disabled(
                    checkboxInput(inputId = "landmark_de", label = "Landmark only", value=FALSE)
                  )
                }else{
                  checkboxInput(inputId = "landmark_de", label = "Landmark only", value=TRUE)
                }
              ),
              
              column(
                width=2,
                selectInput(inputId = "summarizefunc_de", label = "Summarization", choices=c("max", "median", "mean", "min", "Q1", "Q3"), selected = defaults[["summarizefunc_de"]])
              ),
              
              column(
                width=2,
                if(landmark == FALSE){
                  div(
                    checkboxGroupInput(inputId = "filterbyinput_de", label = "Filter by", choices=c("score" = "score"), selected = "score"),
                    shinyjs::disabled(
                      checkboxInput(inputId = "filterbyinput_de_number", label = "number", value=FALSE)
                    )
                  )
                }else{
                  checkboxGroupInput(inputId = "filterbyinput_de", label = "Filter by", choices=c("score" = "score", "number" = "number"), selected = defaults[["filterbyinput_de"]])
                }
              ),
              
              column(
                width=2,
                sliderInput(inputId = "range_de", label = "Score Threshold", min = -10, max = 10, value = defaults[["range_de"]], step = 0.01)
              ),
              
              column(
                width=2,
                if(landmark == FALSE){
                  shinyjs::disabled(
                    sliderInput(inputId = "numberthresleft_de", label = "Num +", min = 0, max = 1000, value = defaults[["numberthresleft_de"]], ticks = FALSE, step = 10)
                  )
                }else{
                  sliderInput(inputId = "numberthresleft_de", label = "Num +", min = 0, max = 1000, value = defaults[["numberthresleft_de"]], ticks = FALSE, step = 10)
                }
              ),
              
              column(
                width=2,
                if(landmark == FALSE){
                  shinyjs::disabled(
                    sliderInput(inputId = "numberthresright_de", label = "Num -", min = 0, max = 1000, value = defaults[["numberthresright_de"]], ticks = FALSE, step = 10)
                  )
                }else{
                  sliderInput(inputId = "numberthresright_de", label = "Num -", min = 0, max = 1000, value = defaults[["numberthresright_de"]], ticks = FALSE, step = 10)
                }
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
        
        bsCollapse(
          id = "es_opt_panel", open = "es_options",
          
          bsCollapsePanel(
            title = "Options", value = "es_options",
            
            fluidRow(
              column(
                width=4,
                selectInputWithTooltip(
                  inputId = "gsname",
                  label = "Gene set name",
                  bId= "Bgsname",
                  helptext = helptext_geneset,
                  choices = names(dsmap)
                )
              ),
              
              column(
                width=4,
                selectInputWithTooltip(
                  inputId = "gsmethod",
                  label ="Projection method",
                  bId = "Bgsmethod",
                  helptext = helptext_method,
                  choices = names(dsmap_method)
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
        
        bsCollapse(
          id = "conn_opt_panel", open = "conn_options",
          
          bsCollapsePanel(
            title = "Options", value = "conn_options",
            
            fluidRow(
              column(
                width=4,
                selectInput(
                  inputId = "conn_name",
                  label = "Connectivity Level:",
                  choices = connmap
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




