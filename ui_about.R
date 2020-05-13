

#Portal About Page#####
fluidRow(
  class="portal-about",
  
  column(
    width=12,
    includeMarkdown(paste0("www/RMD/introduction_", fname, ".Rmd")) 
  )
)


