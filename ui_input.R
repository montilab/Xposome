
# Function to create an select input with tooltip ####
selectInputWithTooltip <- function(inputId, label, bId, helptext, choices, selected=NULL){
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
    choices = choices,
    selected = selected
  )
}


# Function to create an radio buttons with tooltip ####
radioButtonsWithTooltip <- function(inputId, label, bId, helptext, choices, selected=NULL, inline=TRUE){
  radioButtons(
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
    choices = choices,
    selected = selected,
    inline = inline
  )
}

# Function to create an radio buttons with tooltip ####
fileInputRadioButtonsWithTooltip <- function(inputId, label, bId, helptext, choices, selected=NULL, inline=TRUE){
  radioButtons(
    inputId = inputId,
    label = tags$span(
      label, 
      tipify(
        el = div( 
          downloadLink(outputId = paste0(bId, "_csv"), label = em(style="font-size: 11px", "template.csv")),
          downloadLink(outputId = paste0(bId, "_rds"), label = em(style="font-size: 11px", "template.RDS")),
        ),
        title = HTML(helptext),
        placement = "bottom",
        trigger = "hover"
      )
    ),
    choices = choices,
    selected = selected,
    inline = inline
  )
}

