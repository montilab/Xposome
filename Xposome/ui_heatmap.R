
#Heatmap Explorer Page####
fluidRow(
  class="portal-heatmap",
  
  column(
    width=12,
    
    bsCollapse(
      id = "heatmap_opt_panel", open = "heatmap_options",
      
      bsCollapsePanel(
        title = "Options", value = "heatmap_options",
        
        fluidRow(
          column(
            width=4, 
            uiOutput(outputId="marker_hm_option") %>% withSpinner(type=4, color="#0dc5c1", proxy.height="80px")
          ), 
          
          conditionalPanel(
            condition = "input.marker_hm == 'Genes' | input.marker_hm == 'Genes (Landmark)'",
            
            column(
              width=12,
              br(),
              uiOutput(outputId = "hm_de_button")
            )
          ),
          
          conditionalPanel(
            condition = "input.marker_hm == 'Gene Sets'",
            
            column(
              width=4,
              selectInputWithTooltip(
                inputId = "marker_gsname_hm",
                label = "Gene set name", 
                bId= "Bgsname_marker_hm",
                helptext = helptext_geneset,
                choices = names(dsmap)
              )
            ),
            
            column(
              width=4,
              selectInputWithTooltip(
                inputId = "marker_gsmethod_hm", 
                label ="Projection method", 
                bId = "Bgsmethod_marker_hm",
                helptext = helptext_method,
                choices = names(dsmap_method)
              )
            ),
            
            column(
              width=12,
              br(),
              uiOutput(outputId = "hm_es_button")
            )
          ),
          
          conditionalPanel(
            condition = "input.marker_hm == 'CMap Connectivity'",
            
            column(
              width=4,
              selectizeInput(
                inputId = "marker_conn_name_hm",
                label = "Connectivity Level", 
                choices = connmap
              )
            ), 
            
            column(
              width=12,
              br(),
              uiOutput(outputId = "hm_conn_button")
            )
          )
        ),
        
        br(), 
        
        fluidRow(
          column(
            width=12,
            helpText(em("Note: heatmap is generated using the interactive tools in Morpheus, https://software.broadinstitute.org/morpheus"))
          )
        )
      )
    ),
    
    conditionalPanel(
      condition = "input.hm_de_generate >= 1 | input.hm_es_generate >= 1 | input.hm_conn_generate >= 1",
      
      fluidRow(
        column(
          width=12,
          uiOutput(outputId = "heatmap_holder")
        )
      )
    )
  )
)

