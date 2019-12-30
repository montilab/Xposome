
#Obtain the column dose for the dataset####
col_id = ifelse(dat[["title"]]=="MCF10A Portal", "unique_ID_by_chem", "dose (uM)")

#Function to create a list of chemicals without duplicated names####
get_ids_pdat <- function(pdat, cols = c("Chemical Name", "CAS", "BUID"), col.unique = "BUID", val.ignore = c("", " ", NA, "NA", "NOCAS")){
  tab <- unique(pdat[, cols])
  
  res <- lapply(cols, function(i){
    x <- as.character(tab[,i])
    x.uniq <- setdiff(unique(x), union(x[duplicated(x)], val.ignore))
    x.uniq <- sort(x.uniq)
  })
  
  names(res)<-cols
  return(res)
}

#A list of chemicals without duplicated names####
chemicals <- get_ids_pdat(pdat = dat[["Chemical Annotation"]])

#Get the tas value from the profile annotation#####
tas <- dat[["Profile Annotation"]]$TAS

#Create a tas range menu for marker explorer and heatmap explorer#####
minProf <- 3
maxTAS <- tas[order(tas, decreasing = TRUE)][minProf]
maxTAS <- floor(maxTAS/0.1)*0.1

#Create a list of gene enrichment sets and methods####
dsmap <- list(
  Hallmark="gsscores_h.all.v5.0",
  C2="gsscores_c2.cp.reactome.v5.0", 
  NURSA="gsscores_nursa_consensome_Cbyfdrvalue_0.01.gmt"
)

##Create names of the connectivity mapping#####
connmap <- list(PCL = "pcl", PERT = "pert")
names(connmap) <- c("Perturbagen Classes", "Perturbagens")

##Define the webpage domain####
domain <- paste0("https://carcinogenome.org/data/", sub(" .*", "", dat$title))

##Function to extract BUID for each chemical#####
get_BUID <- function(input, tab){
  as.character(tab[which(apply(tab, 1, function(i) any(i %in% input)))[1], "BUID"])
}

##function to round the data table values####
data.table.round <- function(dt, digits = 3){
  
  cols <- sapply(colnames(dt), function(i) is.numeric(dt[,i]))
  cols <- names(which(cols))
  
  for(i in cols)
    dt[,i] <- round(dt[,i], digits)
  
  dt <- data.table(dt)
  return(dt)
  
}

#Function to create an select input with tooltip####
selectInputWithTooltip <- function(inputId, label, bId,helptext, choices){
  selectInput(
    inputId = inputId,
    label = tags$span(
      label, 
      tipify(
        el = bsButton(inputId = bId, label = "?", style = "inverse", size = "extra-small"), 
        title = HTML(helptext),
        placement = "bottom",
        trigger = "hover"
      )
    ),
    choices = choices
  )
}

#The main page layouts####
output$pageStub <- renderUI({
  
  fluidRow(
    id="navbar-page", style="background: white; padding-bottom: 20px",
    
    column(
      width=12,
      
      navbarPage(
        title=actionLink(inputId="main_link", label=strong(paste(substr(fname, 1, nchar(fname)-2), "Portal"))), id="main_page", position=c("static-top"), collapsible=TRUE, selected="About",
        
        ###About####
        tabPanel(
          title="About", value="About",
          source("ui_about.R", local=TRUE)$value
        ),
        
        ###Annotation#####
        tabPanel(
          title="Annotation", value="Annotation",
          source("ui_annotation.R", local=TRUE)$value
        ),
        
        ###Chemical Explorer#####
        tabPanel(
          title = "Chemical Explorer", value = "Chemical Explorer",
          source("ui_chemical.R", local=TRUE)$value
        ),
        
        ###Marker Explorer####
        tabPanel(
          title = "Marker Explorer", value = "Marker Explorer",
          source("ui_marker.R", local=TRUE)$value
        ),
        
        ###Heatmap Explorer####
        tabPanel(
          title = "Heatmap Explorer", value = "Heatmap Explorer",
          source("ui_heatmap.R", local=TRUE)$value
        ),
        
        ###Taxonomic Clustering####
        tabPanel(
          title = "Taxonomic Clustering", value = "Taxonomic Clustering",
          HTML("THIS TAB WILL INCLUDE THE INFORMATION ABOUT TAXOMONIC CLUSTERING")
        )
        
      )
    )
  )
    
})

##Go back to home page when logo link is clicked####
observeEvent(input$main_link, {
  
  updateNavbarPage(session, inputId="main_page", selected = "About")
  
}, ignoreInit=TRUE)

  
  