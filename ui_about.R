

#About page#####
fluidRow(
  column(
    width=12,
    includeMarkdown(paste0("introduction_", substr(session$clientData$url_search, 2, nchar(session$clientData$url_search)), ".Rmd")) 
  ),
  
  column(
    width=12,
    img(src="IMAGES/logo.png", align = "left", width = "100%")
  )
)



