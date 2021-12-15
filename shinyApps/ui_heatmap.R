
#Heatmap Explorer Page####
fluidRow(
  class="portal-heatmap",
  
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
              inputId = "marker_hm", 
              label = "Select a marker set",   
              choices = NULL,
              multiple = FALSE
            ) 
          ), 
          
          conditionalPanel(
            condition = "input.marker_hm == 'Genes' | input.marker_hm == 'Genes (Landmark)'",
            
            column(
              width=12,
              actionButton(inputId = "hm_de_button", class="mybuttons", label = "Generate heatmap", onclick = "de_morpheusHM();")
            )
          ),
          
          conditionalPanel(
            condition = "input.marker_hm == 'Gene Sets'",
            
            column(
              width=4,
              div(id = "marker_gsname_hm_label"),
              selectizeInput(
                inputId = "marker_gsname_hm",
                label = NULL,
                choices = NULL,
                multiple = FALSE
              )
            ),
            
            column(
              width=4,
              selectizeInput(
                inputId = "marker_gsmethod_hm",
                label = HTML("Projection method", paste0('<button type="button" class="tooltip-txt" data-html="true" data-tooltip-toggle="tooltip" data-placement="top" title=\"', helptext_gsva_method, '\">?</button>')),
                choices = gsva_method,
                multiple = FALSE
              )
            ),
            
            column(
              width=12,
              actionButton(inputId = "hm_gs_button", class="mybuttons", label = "Generate heatmap", onclick = "gs_morpheusHM();")
            )
          ),
          
          conditionalPanel(
            condition = "input.marker_hm == 'CMap Connectivity'",
            
            column(
              width=4,
              selectizeInput(
                inputId = "marker_conn_name_hm",
                label = "Connectivity Level", 
                choices = connmap,
                multiple = FALSE
              )
            ), 
            
            column(
              width=12,
              actionButton(inputId = "hm_conn_button", class="mybuttons", label = "Generate heatmap", onclick = "conn_morpheusHM();")
            )
          )
        )
      )
    )
  ),
  
  column(
    width=12,
    helpText(HTML("<em>Note: heatmap is generated using <a href='https://software.broadinstitute.org/morpheus' target='blank'><b>Morpheus</b></a> interactive tool</em>"))
  )
)

