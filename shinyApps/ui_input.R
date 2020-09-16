

#Check a checkbox input####
CheckBoxInputFunction <- function(inputId, label, value=TRUE, disabled=FALSE){
  HTML(
    paste0(
      '<div class="form-group shiny-input-container">', '\n',
      '<div class="checkbox">', '\n',
      '<label>', '\n',
      '<input id="', inputId, '" type="checkbox" ', ifelse(value==TRUE, 'checked="checked" ', ''), ifelse(disabled==TRUE, 'disabled', ''), '/>', '\n',
      '<span>', label, '</span>', '\n',
      '</label>', '\n',
      '</div>', '\n',
      '</div>'
    )
  )
}

###Create selected inputs####
SelectInputFunction <- function(id, label, choices, selected){
  
  Body <- NULL; Label <- paste0("<b>", label, "</b>\n");
  Head <- paste0('<select id="metavar_', id, '" style="width: auto; padding: 5px; border-radius: 5px;">\n')
  
  for(i in seq_along(choices)){
    #i=1;
    if(is.null(selected)){
      if(i==1){
        Body <- c(Body, paste0('<option value="', choices[i], '">', choices[i], '</option>\n'))
      }else{
        Body <- c(Body, paste0('<option value="', choices[i], '">', choices[i], '</option>\n'))
      }
    }else{
      if(selected %in% c(NA, "")){
        if(i==1){
          Body <- c(Body, paste0('<option value="" selected>Please select an option below</option>\n'))
          Body <- c(Body, paste0('<option value="', choices[i], '">', choices[i], '</option>\n'))
        }else{
          Body <- c(Body, paste0('<option value="', choices[i], '">', choices[i], '</option>\n'))
        }
      }else{
        if(i == 1 & choices[i] == selected){
          Body <- c(Body, paste0('<option value="', choices[i], '" selected>', choices[i], '</option>\n'))
        }else if(i == 1 & choices[i] != selected){
          Body <- c(Body, paste0('<option value="', choices[i], '">', choices[i], '</option>\n'))
        }else if(i != 1 & choices[i] == selected){
          Body <- c(Body, paste0('<option value="', choices[i], '" selected>', choices[i], '</option>\n'))
        }else if(i != 1 & choices[i] != selected){
          Body <- c(Body, paste0('<option value="', choices[i], '">', choices[i], '</option>\n'))
        }
      }
    }
  }
  
  return(HTML("<div>\n", ifelse(is.null(label), "", Label), Head, paste0(Body, collapse=""), "</select>\n", "</div>"))
  
}


# Function to create an select input with tooltip ####
selectInputWithTooltip <- function(inputId, label, bId, bId_label="?", helptext, choices, selected=NULL, multiple=FALSE){
  selectInput(
    inputId = inputId,
    label = tags$span(
      label, 
      tipify(
        el = bsButton(inputId = bId, label = bId_label, style = "inverse", size = "extra-small"), 
        title = HTML(helptext),
        placement = "bottom",
        trigger = "click"
      )
    ),
    choices = choices,
    selected = selected,
    multiple = multiple
  )
}


# Function to create an radio buttons with tooltip ####
radioButtonsWithTooltip <- function(inputId, label, bId, bId_label="?", helptext, choices, selected=NULL, inline=TRUE){
  radioButtons(
    inputId = inputId,
    label = tags$span(
      label, 
      tipify(
        el = bsButton(inputId = bId, label = bId_label, style = "inverse", size = "extra-small"), 
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

# Function to create an radio buttons with downloadlink ####
radioButtonsWithDownloadLink <- function(inputId, label, bId, bId_label="?", helptext, choices, selected=NULL, inline=TRUE){
  radioButtons(
    inputId = inputId,
    label = tags$span(
      label, 
      tipify(
        el = div( 
          downloadLink(outputId = bId, label = em(style="font-size: 11px", bId_label))
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

##UI for exposure plots####
UIMarkerplot <- function(outputId){
  
  paste0(
    '<div class="row">',
      '<div class="col-sm-12">',
        '<div class="header-3">',
          '<div class="content">',
            '<div id="marker_plot_', outputId, '" style="width:auto; height:400px;" class="plotly html-widget html-widget-output shiny-report-size"></div>',
          '</div>',
        '</div>',
      '</div>',
    '</div>'
  )
  
}



