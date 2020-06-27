

## Create reactive values ####
projectdata <- reactiveVal(NULL)
logindata <- reactiveVal(NULL)

## Warning message for main project and login table ####
project_table_message <- reactiveVal(NULL)
login_table_message <- reactiveVal(NULL)

## Warning message for add project function####
addprojectwarningmsg <- reactiveVal(NULL)
addinputwarningmsg <- reactiveVal(NULL)
addcompoundvarwarningmsg <- reactiveVal(NULL)
addexposurevarwarningmsg <- reactiveVal(NULL)
addexposurephenotypevarwarningmsg <- reactiveVal(NULL)
addenrichmentversionwarningmsg <- reactiveVal(NULL)

## Warning message for edit project function####
editprojectwarningmsg <- reactiveVal(NULL)

## Warning message for add and edit user function###
adduserwarningmsg <- reactiveVal(NULL)
edituserwarningmsg <- reactiveVal(NULL)

## keep track of who logged in ####
UserLog <- reactiveValues(Logged=FALSE);
LogInMessage <- reactiveValues(Msg="");

#sign in page
output$pageStub <- renderUI({
  
  #####<!-- START SIGN IN PAGE -->
  if(UserLog$Logged == FALSE){

    uiOutput(outputId = "uiLogin")
    
  }else if(UserLog$Logged == TRUE){
    
    uiOutput(outputId = "uiMain")
    
  }#####<!-- END SIGN IN PAGE -->
  

})

##IF USER LOG IS FALSE, THEN SHOW THE LOGIN PANNEL####
output$uiLogin <- renderUI({
  
  req(UserLog$Logged == FALSE)
  
  fluidRow(
    class="signin-page",
    
    column(
      width=4, offset=4, class="login-container",
      
      div(class="login-form", 
          div(class="login-form-title", strong('Sign In'))
      ),
      
      div(
        class="login-validate-form",
        
        div(class="wrap-input",
            HTML(paste0("<input class='logininput' id='userName' type='text' name='username' placeholder='Enter username' value='Reina' onkeypress=\"loginfunction(event)\" style=\"width: 100%;\"/>")),
            tags$script('document.getElementById("userName").focus();')
        ),
        
        div(class="wrap-input",
            HTML(paste0("<input class='logininput' id='passWord' type='password' name='password' placeholder='Enter password' value='Chau' onkeypress=\"loginfunction(event)\" style=\"width: 100%;\"/>"))
        ),
        
        div(class="login-error-message",
            uiOutput(outputId="LogInErrorMessage")
        ),
        
        p(style="text-align: center",
          actionButton(class="login-btn", inputId="SignInButton", label=strong("Sign In"), onkeypress="loginfunction(event)", width="auto")
        )
      )
    )
  )

})

##OBSERVE THE SIGN IN BUTTON#####
##If user role is user, shows the user app,
##else shows the moderator app
observeEvent(input$SignInButton, {
  
  ## Obtain the username and password ####
  userName <- input$userName; passWord <- input$passWord;
  
  ## Read in the log in sheet ####
  logInfo <- read.csv(paste0("data/User_Login_List.csv"), header = TRUE)
  
  ## Verify username and password ###
  Log <- logInfo %>% filter(Username %in% userName, Password %in% passWord)
  
  ## Read in the project list ####
  projectInfo <- read.csv(paste0("data/Project_List.csv"), header = TRUE) 
  
  ## Check the login message ####
  if(nrow(Log) > 0){
    
    projectdata(projectInfo)
    logindata(logInfo)
    LogInMessage$Msg <- paste0("")
    UserLog$Logged <- TRUE;
    
  }else{
    
    LogInMessage$Msg <- paste0("Incorrect username or password.") 
    UserLog$Logged <- FALSE;
    
  }

}, ignoreInit=TRUE, ignoreNULL = TRUE)

##Show the error message if login info is not correct####
output$LogInErrorMessage <- renderUI({
  
  p(id="LoginMessage", LogInMessage$Msg)
  
})


###IF LOGGED IS TRUE THEN SHOW THE MAIN PAGE#####
output$uiMain <- renderUI({
  
  req(UserLog$Logged == TRUE)
  
  ###<!-- Header top area start -->
  div(
    class="moderator-page", 
    
    ###the header#####
    fluidRow(
      class="moderator-top-banner",
      
      column(
        width=6,
        
        div(class="text-md-left",
            div(class="button-link", "Moderator Page")
        )
      ),
      
      column(
        width=6,
        
        div(class="text-md-right",
            actionLink(inputId="LogOut", class="button-link", icon=tags$i(class="fa fa-user-circle"), label="Log out")
        )
      )
    ),
    
    fluidRow(
      class="moderator-body",
      
      column(
        width=12,
        
        h3("Project Table"),
        br(),
        DT::dataTableOutput(outputId = "projecttable"),
        br(),
        uiOutput("ProjectTableMessage"),
        br(),
        actionButton(class="mybuttons", inputId="AddProject", label=strong("Add"), width="70px"),
        actionButton(class="mybuttons", inputId="RemoveProject", label=strong("Remove"), width="70px"),
        actionButton(class="mybuttons", inputId="EditProject", label=strong("Edit"), width="70px"),
        actionButton(class="mybuttons", inputId="SaveProject", label=strong("Save"), width="70px"),
        helpText(em("Note: you must save your changes to get the updated data and tables.", style="font-size: 9pt;"))
      )
    ),

    fluidRow(
      class="moderator-body",
      
      column(
        width=12,
        
        h3("Login Table"),
        br(),
        DT::dataTableOutput(outputId = "logintable"),
        br(),
        uiOutput("LoginTableMessage"),
        br(),
        actionButton(class="mybuttons", inputId="AddUser", label=strong("Add"), width="70px"),
        actionButton(class="mybuttons", inputId="RemoveUser", label=strong("Remove"), width="70px"),
        actionButton(class="mybuttons", inputId="EditUser", label=strong("Edit"), width="70px"),
        actionButton(class="mybuttons", inputId="SaveUser", label=strong("Save"), width="70px"),
        helpText(em("Note: you must save your changes to get the updated data and tables.", style="font-size: 9pt;"))
      )
    )
  )##<!-- Header top area end -->

})


#######################################################
#
# PROJECT TABLE ####
#
#######################################################

## Output the project table ####
output$projecttable <- DT::renderDataTable({
  
  req(projectdata())
  
  table <- projectdata()
  colnames(table) <- gsub("_", " ", colnames(table))
  
  return(table)
  
}, escape = FALSE, server = TRUE, rownames=FALSE, selection = "single")


## Output project table message ####
output$ProjectTableMessage <- renderUI({
  
  req(project_table_message())
  
  p(style="color:red;", HTML(project_table_message()))
  
})

## Functions to add new project #####
AddProject <- function() {
  div(
    id = "addProjectData",
    
    modalDialog(
      size = "l", title = "Add Project", footer = NULL,
      fluidRow(
        column(
          width=4,
          textInput(inputId = "Add_Project_Name", label=strong(span(style="color:red;", "*"), "Project name:"), value=""),
        ),
        column(
          width=4,
          textInput(inputId = "Add_Cell_Line_Name", label=strong(span(style="color:red;", "*"), "Cell line name:"), value="")
        ),
        column(
          width=4,
          textInput(inputId = "Add_Portal_Name", label=strong(span(style="color:red;", "*"), "Portal name:"), value="")
        )
      ),
      fluidRow(
        column(
          width=12,
          strong(span(style="color:red;", "*"), "Description:", style="text-align: left;"),
          HTML(paste0("<textarea style='width: 100%; height: 150px; padding: 10px; margin-top: 5px;' id='Add_Description' placeholder='Write a short description about the project...'></textarea>")),
          helpText(em("Note: you can use HTML tags inside the description box for styling and formatting text", style="font-size: 9pt;"))
        )
      ),
      br(),
      fluidRow(
        column(
          width=12,
          uiOutput("AddProjectWarningMessage")        
        )
      ),
      br(),
      fluidRow(
        column(
          width=12,
          shinyjs::hidden(
            div(
              id = "loading-content",
              div(class="loader"),
              h4("Processing...", id="loading_text")
            )
          )
        )
      ),
      br(),
      fluidRow(
        column(
          width = 6,
          
          ## Import files ####
          h4("Import Files:", style="padding-bottom: 10px;"),
          fileInput(inputId = "add_intro_file", label = strong(span(style="color:red;", "*"), "Choose an introduction file ", downloadLink(outputId = "add_intro_template_rmd", label = em(style="font-size: 11px", "template.rmd")))),
          uiOutput(outputId='add_intro_file_msg'),
          br(),
          
          fileInput(inputId = "add_pro_file", label = strong(span(style="color:red;", "*"), "Choose a profile annotation file")),
          fileInputRadioButtonsWithTooltip(
            inputId="add_pro_file_type", label="File type:", bId="add_pro_template", helptext="", choices=c(".csv", ".RDS"), selected=NULL, inline=TRUE
          ),
          uiOutput(outputId='add_pro_file_msg'),
          br(),
          
          fileInput(inputId = "add_chem_file", label = strong(span(style="color:red;", "*"), "Choose a chemical annotation file")),
          fileInputRadioButtonsWithTooltip(
            inputId="add_chem_file_type", label="File type:", bId="add_chem_template", helptext="", choices=c(".csv", ".RDS"), selected=NULL, inline=TRUE
          ),
          uiOutput(outputId='add_chem_file_msg'),
          br(),
          
          fileInput(inputId = "add_ge_file", label = strong(span(style="color:red;", "*"), "Choose a gene expression file")),
          fileInputRadioButtonsWithTooltip(
            inputId="add_ge_file_type", label="File type:", bId="add_ge_template", helptext="", choices=c(".csv", ".RDS"), selected=NULL, inline=TRUE
          ),
          uiOutput(outputId='add_ge_file_msg'),
          br(),
          
          radioButtons(inputId = "add_conn_option", label = "Add connectivity map?", choices = c("Yes", "No"), inline = TRUE),
          br(),
          conditionalPanel(
            condition = "input.add_conn_option == 'Yes'",
            fileInput(inputId = "add_conn_pcl_file", label = strong(span(style="color:red;", "*"), "Choose a connectivity map file (Perturbagen Classes)")),
            fileInputRadioButtonsWithTooltip(
              inputId="add_conn_pcl_file_type", label="File type:", bId="add_conn_pcl_template", helptext="", choices=c(".csv", ".RDS"), selected=NULL, inline=TRUE
            ),
            uiOutput(outputId='add_conn_pcl_file_msg'),
            br(),
            
            fileInput(inputId = "add_conn_pert_file", label = strong(span(style="color:red;", "*"), "Choose a connectivity map file (Perturbagens)")),
            fileInputRadioButtonsWithTooltip(
              inputId="add_conn_pert_file_type", label="File type:", bId="add_conn_pert_template", helptext="", choices=c(".csv", ".RDS"), selected=NULL, inline=TRUE
            ),
            uiOutput(outputId='add_conn_pert_file_msg')
          )
        ),
        
        column(
          width=6,
          
          ##Getting the exposure variables ####
          selectInputWithTooltip(inputId = "add_variable_compound", label=strong(span(style="color:red;", "*"), "Select a compound variable:"), bId="add_var_compound", helptext="Some description here", choices=c("Please import a profile annotation file" = ""), multiple = FALSE),
          uiOutput(outputId = 'add_variable_compound_msg'),
          selectInputWithTooltip(inputId = "add_variable_exposure", label=strong(span(style="color:red;", "*"), "Select a list of additional exposure variables:"), bId="add_var_exposure", helptext="Some description here", choices=c("Please import a profile annotation file" = ""), multiple = TRUE),
          uiOutput(outputId = 'add_variable_exposure_msg'),
          
          ## Calculations ####
          h4("Calculations:", style="padding-bottom: 10px;"),
          checkboxInput(inputId = "Add_Landmark", label = "Landmark Gene", value=FALSE), 
          conditionalPanel(
            condition="input.Add_Landmark == true",
            uiOutput(outputId = "Add_Tas_Modz")
          ),
          br(),
          
          #uiOutput(outputId  = 'Add_Taxonomer_Analysis')
          ## Gene set enrichment ####
          h4("GSVA Gene Set Enrichment:", style="padding-bottom: 10px;"),
          radioButtonsWithTooltip(
            inputId = "add_cur_enrichment_option",
            label = "Use existed gene set",
            bId = "add_cur_enrichment",
            helptext = "Gene sets include the MSigDB collections (Hallmark, C2 reactome pathways), and gene targets of various nuclear receptors (NURSA)",
            choices = c("Yes", "No")
          ),
          conditionalPanel(
            condition = "input.add_cur_enrichment_option == 'No'",
            numericInput(inputId = "Add_New_Enrichment_Version", label = strong(span(style="color:red;", "*"), "New version", span(style="color:red;", "(# must greater than 0)")), value = NULL),
            uiOutput(outputId = 'add_enrichment_version_msg'),
            fileInput(inputId = "add_hallmark_file", label = strong(span(style="color:red;", "*"), "Choose a hallmark gene set ", downloadLink(outputId = "add_hallmark_template_gmt", label = em(style="font-size: 11px", "template.gmt")))),
            uiOutput(outputId = "add_hallmark_file_msg"),
            fileInput(inputId = "add_c2_file", label = strong(span(style="color:red;", "*"), "Choose a C2 reactome pathways gene set", downloadLink(outputId = "add_c2_template_gmt", label = em(style="font-size: 11px", "template.gmt")))),
            uiOutput(outputId = "add_c2_file_msg"),
            fileInput(inputId = "add_nursa_file", label = strong(span(style="color:red;", "*"), "Choose a NURSA gene set ", downloadLink(outputId = "add_nursa_template_gmt", label = em(style="font-size: 11px", "template.gmt")))),
            uiOutput(outputId = "add_nursa_file_msg")
          ),
          conditionalPanel(
            condition = "input.add_cur_enrichment_option == 'Yes'",
            uiOutput(outputId = "Enrichment_Cur_Version_Option")
          ),
          shinyjs::disabled(radioButtons(inputId = "add_gsva_method", label = "GSVA methods:", choices = c("All", "gsva"="gsva", "ssGSEA"="ssgsea", "zscore"="zscore"), inline = TRUE)),
          br(),
          
          ## Gene set enrichment ####
          h4("K2Taxonomer Analysis:", style="padding-bottom: 10px;"),
          selectInputWithTooltip(inputId = "add_variable_exposure_phenotype", label=strong(span(style="color:red;", "*"), "Select a list of exposure phenotype:"), bId="add_var_exposure_phenotype", helptext="Some description here", choices=c("Please import a chemical annotation file" = ""), multiple = TRUE),
          uiOutput(outputId = 'add_variable_exposure_phenotype_msg'),
          DT::dataTableOutput(outputId = "add_metavar_variable_test"),
          br(),
          
          conditionalPanel(
            condition = "input.add_conn_option == 'Yes'",
            shinyjs::hidden(checkboxInput(inputId = "add_connectivity_var", label = "Add Connectivity Variables", value = FALSE))
          ),
          
          ## Additional parameters ####
          h4("Additional Parameters:"),
          selectInputWithTooltip(inputId = "add_feature_metric", label="Feature filtering:", bId="add_feature_filtering_metric", helptext="Some description here", choices = c("sd"="sd", "mad"="mad", "Sn"="Sn", "Qn"="Qn", "F"="F", "square"="square"), selected = "sd"),       
          radioButtons(inputId = "add_ssGSEA_method", label = "ssGSEA methods:", choices = c("gsva"="gsva", "ssGSEA"="ssgsea", "zscore"="zscore", "plage"="plage"), inline = TRUE)
        )
      ),
      br(),
      fluidRow(
        column(
          width=12,
          uiOutput("add_input_message")
        )
      ),
      br(),
      fluidRow(
        column(
          width=4,
          actionButton("Add_Project_Add_Button", label=strong("Add"), class="mybuttons", width="70px"),
          actionButton("Add_Project_Cancel_Button", label=strong("Cancel"), class="mybuttons", width="70px")
        )
      )
    )
  )
}

# add current existing gs enrichment version ####
output$Enrichment_Cur_Version_Option <- renderUI({
  
  version <- unique(projectdata()$Enrichment_Version)
  
  selectInput(inputId = "Add_Enrichment_Version", label = paste0("GS enrichment version:"), choices = version)
  
})

# Pop-up for add ####
observeEvent(input$AddProject, {
  
  project_table_message("")
  showModal(AddProject())
  
}, ignoreInit = TRUE)

## Output the add warning message ###
output$AddProjectWarningMessage <- renderUI({
  
  req(addprojectwarningmsg())
  
  p(style="color:red;", HTML(addprojectwarningmsg()))
  
})

## Output the enrichment version warning message ###
output$add_enrichment_version_msg <- renderUI({
  
  req(addenrichmentversionwarningmsg())
  
  p(style="color:red;", HTML(addenrichmentversionwarningmsg()))
  
})

##Create message for variable selection
output$add_variable_compound_msg <- renderUI({
  
  req(addcompoundvarwarningmsg())
  
  p(style="color:red;", HTML(addcompoundvarwarningmsg()))
  
})

##Create message for variable selection
output$add_variable_exposure_msg <- renderUI({
  
  req(addexposurevarwarningmsg())
  
  p(style="color:red;", HTML(addexposurevarwarningmsg()))
  
})

##Create message for variable selection
output$add_variable_exposure_phenotype_msg <- renderUI({
  
  req(addexposurephenotypevarwarningmsg())
  
  p(style="color:red;", HTML(addexposurephenotypevarwarningmsg()))
  
})

##Create message for variable selection
output$add_input_message <- renderUI({
  
  req(addinputwarningmsg())
  
  p(style="color:red; text-align:center;", HTML(addinputwarningmsg()))
  
})

## Observe when cancel button is clicked
observeEvent(input$Add_Project_Cancel_Button, {
  removeModal()
})

# Functions to edit table #####
EditProject <- function(table) {
  div(
    id = "editProjectData",
    
    modalDialog(
      size = "l", title = "Edit Project", footer = NULL,
      fluidRow(
        column(
          width=3,
          textInput(inputId = "Edit_Project_Name", label=strong(span(style="color:red;", "*"), "Project name:"), value=table$Project),
        ),
        column(
          width=3,
          textInput(inputId = "Edit_Cell_Line_Name", label=strong(span(style="color:red;", "*"), "Cell line name:"), value=table$Cell_Line)
        ),
        column(
          width=3,
          textInput(inputId = "Edit_Portal_Name", label=strong(span(style="color:red;", "*"), "Portal name:"), value=table$Portal)
        ),
        column(
          width=3,
          numericInput(inputId = "Edit_Enrichment_Version", label=strong(span(style="color:red;", "*"), "GS enrichment version:"), value=table$Enrichment_Version)
        ), 
        column(
          width=12,
          helpText(em("Note: GS enrichment version must be greater than 0", style="font-size: 9pt;"))
        )
      ),
      fluidRow(
        column(
          width=12,
          strong(span(style="color:red;", "*"), "Description:", style="text-align: left;"),
          HTML(paste0("<textarea style='width: 100%; height: 150px; padding: 10px; margin-top: 5px;' id='Edit_Description' placeholder='Write a short description about the project...'>", table$Description,"</textarea>")),
          helpText(em("Note: you can use HTML tags inside the description box for styling and formatting text", style="font-size: 9pt;"))
        )
      ),
      br(),
      fluidRow(
        column(
          width=6,

          ## Modify input files ####
          h4("Modify Input Files:", style="padding-bottom: 10px;"),
          radioButtons(inputId = "edit_files", label = NULL, choices = c("None", "All", "Introduction Page", "Profile Annotation", "Chemical Annotation", "Gene Expression", "Connectivity Map")),
          br(),
          
          conditionalPanel(
            condition = "input.edit_files == 'Introduction Page' | input.edit_files == 'All'",
            fileInput(inputId = "edit_intro_file", label = strong(span(style="color:red;", "*"), "Choose an introduction file ", downloadLink(outputId = "edit_intro_template", label = em(style="font-size: 11px", "template.RDS"))))
          ),
          
          conditionalPanel(
            condition = "input.edit_files == 'Profile Annotation' | input.edit_files == 'All'",
            fileInput(inputId = "edit_pro_file", label = strong(span(style="color:red;", "*"), "Choose a profile annotation file")),
            fileInputRadioButtonsWithTooltip(
              inputId="edit_pro_file_type", label="File type:", bId="edit_pro_template", helptext="", choices=c(".csv", ".RDS"), selected=NULL, inline=TRUE
            ),
            br(),
            
            conditionalPanel(
              condition = "input.edit_files == 'Profile Annotation'",
              radioButtons(inputId = "edit_pro_chem_option", label = "Does chemical annotation change?", choices = c("Yes", "No"), inline = TRUE),
              conditionalPanel(
                condition = "input.edit_pro_chem_option == 'Yes'",
                fileInput(inputId = "edit_pro_chem_file", label = strong(span(style="color:red;", "*"), "Choose a chemical annotation file")),
                fileInputRadioButtonsWithTooltip(
                  inputId="edit_pro_chem_file_type", label="File type:", bId="edit_pro_chem_template", helptext="", choices=c(".csv", ".RDS"), selected=NULL, inline=TRUE
                ),
                br()
              ),
              radioButtons(inputId = "edit_pro_ge_option", label = "Does gene expression change?", choices = c("Yes", "No"), inline = TRUE),
              conditionalPanel(
                condition = "input.edit_pro_ge_option == 'Yes'",
                fileInput(inputId = "edit_pro_ge_file", label = strong(span(style="color:red;", "*"), "Choose a gene expression file")),
                fileInputRadioButtonsWithTooltip(
                  inputId="edit_pro_ge_file_type", label="File type:", bId="edit_pro_ge_template", helptext="", choices=c(".csv", ".RDS"), selected=NULL, inline=TRUE
                ),
                br()
              )
            )
          ),
          
          conditionalPanel(
            condition = "input.edit_files == 'Chemical Annotation' | input.edit_files == 'All'",
            fileInput(inputId = "edit_chem_file", label = strong(span(style="color:red;", "*"), "Choose a chemical annotation file")),
            fileInputRadioButtonsWithTooltip(
              inputId="edit_chem_file_type", label="File type:", bId="edit_chem_template", helptext="", choices=c(".csv", ".RDS"), selected=NULL, inline=TRUE
            ),
            br(),
            
            conditionalPanel(
              condition = "input.edit_files == 'Chemical Annotation'",
              radioButtons(inputId = "edit_chem_pro_option", label = "Does profile annotation change?", choices = c("Yes", "No"), inline = TRUE),
              conditionalPanel(
                condition = "input.edit_chem_pro_option == 'Yes'",
                fileInput(inputId = "edit_chem_pro_file", label = strong(span(style="color:red;", "*"), "Choose a profile annotation file")),
                fileInputRadioButtonsWithTooltip(
                  inputId="edit_chem_pro_file_type", label="File type:", bId="edit_chem_pro_template", helptext="", choices=c(".csv", ".RDS"), selected=NULL, inline=TRUE
                ),
                br()
              ),
              radioButtons(inputId = "edit_chem_ge_option", label = "Does gene expression change?", choices = c("Yes", "No"), inline = TRUE),
              conditionalPanel(
                condition = "input.edit_chem_ge_option == 'Yes'",
                fileInput(inputId = "edit_chem_ge_file", label = strong(span(style="color:red;", "*"), "Choose a gene expression file")),
                fileInputRadioButtonsWithTooltip(
                  inputId="edit_chem_ge_file_type", label="File type:", bId="edit_chem_ge_template", helptext="", choices=c(".csv", ".RDS"), selected=NULL, inline=TRUE
                ),
                br()
              )
            )
          ),
          
          conditionalPanel(
            condition = "input.edit_files == 'Gene Expression' | input.edit_files == 'All'",
            fileInput(inputId = "edit_ge_file", label = strong(span(style="color:red;", "*"), "Choose a gene expression file")),
            fileInputRadioButtonsWithTooltip(
              inputId="edit_ge_file_type", label="File type:", bId="edit_ge_template", helptext="", choices=c(".csv", ".RDS"), selected=NULL, inline=TRUE
            ),
            br(),
            
            conditionalPanel(
              condition = "input.edit_files == 'Gene Expression'",
              radioButtons(inputId = "edit_ge_pro_option", label = "Does profile annotation change?", choices = c("Yes", "No"), inline = TRUE),
              conditionalPanel(
                condition = "input.edit_ge_pro_option == 'Yes'",
                fileInput(inputId = "edit_ge_pro_file", label = strong(span(style="color:red;", "*"), "Choose a profile annotation file")),
                fileInputRadioButtonsWithTooltip(
                  inputId="edit_ge_pro_file_type", label="File type:", bId="edit_ge_pro_template", helptext="", choices=c(".csv", ".RDS"), selected=NULL, inline=TRUE
                ),
                br()
              ),
              radioButtons(inputId = "edit_ge_chem_option", label = "Does chemical annotation change?", choices = c("Yes", "No"), inline = TRUE),
              conditionalPanel(
                condition = "input.edit_ge_chem_option == 'Yes'",
                fileInput(inputId = "edit_ge_chem_file", label = strong(span(style="color:red;", "*"), "Choose a chemical annotation file")),
                fileInputRadioButtonsWithTooltip(
                  inputId="edit_ge_chem_file_type", label="File type:", bId="edit_ge_chem_template", helptext="", choices=c(".csv", ".RDS"), selected=NULL, inline=TRUE
                ),
                br()
              )
            )
          ),
          
          conditionalPanel(
            condition = "input.edit_files == 'All' | input.edit_files == 'Profile Annotation' | input.edit_files == 'Chemical Annotation' | input.edit_files == 'Gene Expression'",
            radioButtons(inputId = "edit_conn_option", label = "Does connectivity map change?", choices = c("Yes", "No"), inline = TRUE),
            conditionalPanel(
              condition = "input.edit_conn_option == 'Yes'",
              fileInput(inputId = "edit_all_conn_file", label = strong(span(style="color:red;", "*"), "Choose a connectivity map file")),
              fileInputRadioButtonsWithTooltip(
                inputId="edit_all_conn_file_type", label="File type:", bId="edit_all_conn_template", helptext="", choices=c(".csv", ".RDS"), selected=NULL, inline=TRUE
              ),
              br()
            )
          ),
          
          conditionalPanel(
            condition = "input.edit_files == 'Connectivity Map'",
            fileInput(inputId = "edit_conn_file", label = strong(span(style="color:red;", "*"), "Choose a connectivity map file")),
            fileInputRadioButtonsWithTooltip(
              inputId="edit_conn_file_type", label="File type:", bId="edit_conn_template", helptext="", choices=c(".csv", ".RDS"), selected=NULL, inline=TRUE
            ),
            br()
          )
        ),
        
        column(
          width=6,
          
          conditionalPanel(
            condition = "(input.edit_files == 'Profile Annotation' && input.edit_pro_ge_option == 'Yes') | (input.edit_files == 'Chemical Annotation' && input.edit_chem_ge_option == 'Yes') | input.edit_files == 'Gene Expression' | input.edit_files == 'All'",
            
            ## Recalculation ####
            h4("Redo Calculations:", style="padding-bottom: 10px;"),
            checkboxInput(inputId = "Edit_TAS", label = "TAS", value = FALSE),
            checkboxInput(inputId = "Edit_ModZscores", label = "Mod-Zscores", value = FALSE),
            br(),
            
            ## Redo enrichment analysis ####
            h4("Modify Enrichment Analysis:", style="padding-bottom: 10px;"),
            radioButtons(inputId = "edit_enrichment_option", label = NULL, choices = c("None", "Current version", "New version"), inline=TRUE),
            conditionalPanel(
              condition = "input.edit_enrichment_option == 'New version'",
              numericInput(inputId = "edit_enrichment_version", label = strong(span(style="color:red;", "*"), "New version", span(style="color:red;", "(# must be greater than 0)")), value = NULL)
            ),
            conditionalPanel(
              condition = "input.edit_enrichment_option == 'Current version'",
              selectInputWithTooltip(
                inputId = "edit_enrichment_set",
                label = "Gene set name",
                bId= "enrichment_set",
                helptext = "Gene sets include the MSigDB collections (Hallmark, C2 reactome pathways), and gene targets of various nuclear receptors (NURSA)",
                choices = c("All"="All", "Hallmark"="Hallmark", "C2 reactome pathways"="C2", "NURSA"="NURSA")
              )
            ),
            conditionalPanel(
              condition = "input.edit_enrichment_option == 'New version' | (input.edit_enrichment_option == 'Current version' && input.edit_enrichment_set == 'Hallmark') | (input.edit_enrichment_option == 'Current version' && input.edit_enrichment_set == 'All')",
              fileInput(inputId = "edit_hallmark_file", label = strong(span(style="color:red;", "*"), "Choose a hallmark gene set ", downloadLink(outputId = "edit_hallmark_template_gmt", label = em(style="font-size: 11px", "template.gmt"))))
            ),
            conditionalPanel(
              condition = "input.edit_enrichment_option == 'New version' | (input.edit_enrichment_option == 'Current version' && input.edit_enrichment_set == 'C2') | (input.edit_enrichment_option == 'Current version' && input.edit_enrichment_set == 'All')",
              fileInput(inputId = "edit_C2_file", label = strong(span(style="color:red;", "*"), "Choose a C2 reactome pathways gene set ", downloadLink(outputId = "edit_c2_template_gmt", label = em(style="font-size: 11px", "template.gmt"))))
            ),
            conditionalPanel(
              condition = "input.edit_enrichment_option == 'New version' | (input.edit_enrichment_option == 'Current version' && input.edit_enrichment_set == 'NURSA') | (input.edit_enrichment_option == 'Current version' && input.edit_enrichment_set == 'All')",
              fileInput(inputId = "edit_NURSA_file", label = strong(span(style="color:red;", "*"), "Choose a NURSA gene set ", downloadLink(outputId = "edit_nursa_template_gmt", label = em(style="font-size: 11px", "template.gmt"))))
            ),
            conditionalPanel(
              condition = "input.edit_enrichment_option == 'Current version'",
              radioButtons(inputId = "edit_gsva_method", label = "GSVA methods:", choices = c("All", "gsva", "ssGSEA", "zscore"), inline = TRUE)
            ),
            conditionalPanel(
              condition = "input.edit_enrichment_option == 'New version'",
              shinyjs::disabled(
                radioButtons(inputId = "edit_new_gsva_method", label = "GSVA methods:", choices = c("All", "gsva", "ssGSEA", "zscore"), inline = TRUE)
              )
            )
          )
        )
      ),
      br(),
      fluidRow(
        column(
          width=12,
          uiOutput("EditProjectWarningMessage")        
        )
      ),
      br(),
      fluidRow(
        column(
          width=4,
          actionButton("Edit_Project_Update_Button", label=strong("Update"), class="mybuttons", width="70px"),
          actionButton("Edit_Project_Dismiss_Button", label=strong("Dismiss"), class="mybuttons", width="70px")
        )
      )
    )
  )
}

## Output the add warning message ###
output$EditProjectWarningMessage <- renderUI({
  
  req(editprojectwarningmsg())
  
  p(style="color:red;", HTML(editprojectwarningmsg()))
  
})

# Pop-up for edit ####
observeEvent(input$EditProject, {
  
  row <- input$projecttable_rows_selected

  if(length(input$projecttable_rows_selected) > 0){
    project_table_message("")
    table <- projectdata()
    showModal(EditProject(table=table[row,]))
  }else{
    project_table_message("Please select a project to make changes")
  }
  
}, ignoreInit = TRUE)

## Observe when change button is clicked ####
observeEvent(input$Edit_Project_Update_Button, {
  
  req(projectdata())
  
  row <- input$projecttable_rows_selected
  proj_dat <- data.frame(projectdata())
  
  Project=trimws(input$Edit_Project_Name);
  Cell_Line=trimws(input$Edit_Cell_Line_Name);
  Portal=trimws(input$Edit_Portal_Name);
  Description=trimws(input$Edit_Description);  
  
  if(!input$edit_files %in% c("None", "Introduction", "Connectivity Map")){
    Enrichment_Version=trimws(input$Edit_Enrichment_Version)
  }else{
    if(input$edit_enrichment_option == 'New version'){
      Enrichment_Version=trimws(input$edit_enrichment_version)
    }else{
      Enrichment_Version=trimws(input$Edit_Enrichment_Version)
    }
  }
  
  if(Project=="" | Cell_Line=="" | Portal=="" | is.na(Enrichment_Version) | is.character(as.numeric(Enrichment_Version)) | as.numeric(Enrichment_Version) <= 0 | Description==""){
    
    editprojectwarningmsg("Please fill in the required (*) fields.")
    
  }else{
    
    proj_dat[row,] <- c(Project, Cell_Line, Portal, Enrichment_Version, Description)
    projectdata(proj_dat)
    editprojectwarningmsg("")
    project_table_message(paste0("Project ", Project, ' has been updated.'))
    removeModal()
    
  }
  
})

## Observe when dismiss button is clicked ####
observeEvent(input$Edit_Project_Dismiss_Button, {
  removeModal()
})

## Remove project function ####
RemoveProject <- function(){
  div(
    id = "removeProjectData", 
    
    modalDialog(footer = NULL, size= "l", title = NULL,
      fluidRow(
        column(
          width=12,
          h4("Are you sure you want to remove this project?")
        ),
      ),
      br(),
      fluidRow(
        column(
          width=4,
          actionButton("Remove_Project_Yes_Button", label=strong("Yes"), class="mybuttons", width="50px"),
          actionButton("Remove_Project_No_Button", label=strong("No"), class="mybuttons", width="50px")
        )
      )
    )
  )
}

## Observe when remove data is clicked ####
observeEvent(input$RemoveProject, {
  
  if(length(input$projecttable_rows_selected) > 0){
    project_table_message("")
    showModal(RemoveProject())
  }else{
    project_table_message("Please select a project to remove.")
  }
  
})

## Observe when yes button is clicked ####
observeEvent(input$Remove_Project_Yes_Button, {
  
  row <- input$projecttable_rows_selected
  proj_dat <- data.frame(projectdata())
  projectdata(proj_dat[-row,])
  project_table_message(paste0("Project ", proj_dat$Project[row], " has been removed. Click 'Save' to implement this changes."))
  removeModal()
  
})

## Observe when no button is clicked ####
observeEvent(input$Remove_Project_No_Button, {
  removeModal()
})


##pop-up for save project ####
SaveProject <- function(){
  div(
    id = "saveProjectData",
    
    modalDialog(
      footer = NULL, size= "l", title = NULL,
      fluidRow(
        column(
          width=12,
          h4("Are you sure you want to save the changes?")
        ),
      ),
      br(),
      fluidRow(
        column(
          width=4,
          actionButton("Save_Project_Yes_Button", label=strong("Yes"), class="mybuttons", width="50px"),
          actionButton("Save_Project_No_Button", label=strong("No"), class="mybuttons", width="50px")
        )
      )
    )
  )
}

## CLICKING THE SAVE PROJECT BUTTON ####
observeEvent(input$SaveProject, {
  
  project_table_message("")
  showModal(SaveProject())
  
})

## Observe when save project yes button is clicked ####
observeEvent(input$Save_Project_Yes_Button, {
  
  data <- projectdata()
  write.csv(data, paste0("data/Project_List.csv"), row.names = FALSE)
  project_table_message("Project list has been saved.")
  removeModal()
  
})

## Observe when save project no button is clicked ####
observeEvent(input$Save_Project_No_Button, {
  data <- read.csv(paste0("data/Project_List.csv"), header = TRUE)
  projectdata(data)
  removeModal()
})

#######################################################
#
# USER TABLE ####
#
#######################################################

# Functions to add new data for user login #####
AddUser <- function() {
  div(
    id = "addUserData",
    
    modalDialog(
      size = "l", title = "Add User", footer = NULL,
      fluidRow(
        column(
          width=3,
          textInput(inputId = "addfirstname", label=strong(span(style="color:red;", "*"), "First name:"), value=""),
        ),
        column(
          width=3,
          textInput(inputId = "addlastname", label=strong(span(style="color:red;", "*"), "Last name:"), value="")
        ),
        column(
          width=3,
          textInput(inputId = "addusername", label=strong(span(style="color:red;", "*"), "Username:"), value="")
        ),
        column(
          width=3,
          textInput(inputId = "addpassword", label=strong(span(style="color:red;", "*"), "Password:"), value="")
        )
      ),
      br(),
      fluidRow(
        column(
          width=12,
          uiOutput("AddUserWarningMessage")        
        )
      ),
      br(),
      fluidRow(
        column(
          width=4,
          actionButton("Add_User_Add_Button", label=strong("Add"), class="mybuttons", width="70px"),
          actionButton("Add_User_Cancel_Button", label=strong("Cancel"), class="mybuttons", width="70px")
        )
      )
    )
  )
}

## Output add user login message ####
output$AddUserWarningMessage <- renderUI({
  
  req(adduserwarningmsg())
  
  p(style="color:red;", HTML(adduserwarningmsg()))

})

## Observe when cancel button is clicked ####
observeEvent(input$AddUser, {
  
  login_table_message("")
  showModal(AddUser())
  
})

## Observe the add user button is clicked #####
observeEvent(input$Add_User_Add_Button, {
  
  Firstname=trimws(input$addfirstname);
  Lastname=trimws(input$addlastname);
  Username=trimws(input$addusername);
  Password=trimws(input$addpassword);
  Status="Moderator";
  
  login_dat <- data.frame(logindata())
  new_user <- data.frame(
    Firstname=Firstname, 
    Lastname=Lastname, 
    Username=Username, 
    Password=Password, 
    Status=Status
  )
  
  if(Firstname=="" | Lastname=="" | Username=="" | Password==""){
    
    adduserwarningmsg("Please fill in the required (*) fields.")
    
  }else{
    
    validate_user <- login_dat %>% 
      filter(
        Username %in% !!Username,
        Password %in% !!Password
      )
    
    if(nrow(validate_user) > 0){
      adduserwarningmsg("This username is already existed. Please enter another username.")
    }else{
      adduserwarningmsg("")
      newlogin <- login_dat %>% rbind(new_user)
      logindata(newlogin)
      login_table_message(paste0(Username, ' ', Password, ' has been added.'))
      removeModal()
    }
    
  }
  
})

## Observe the cancel user button is clicked #####
observeEvent(input$Add_User_Cancel_Button, {
  removeModal()
})

# Functions to add new data for user login #####
EditUser <- function(table) {
  
  div(
    id = "editUserData",
    modalDialog(
      size = "l", title = "Edit User", footer = NULL, 
      fluidRow(
        column(
          width=3,
          textInput(inputId = "editfirstname", label=strong(span(style="color:red;", "*"), "First name:"), value=table$Firstname),
        ),
        column(
          width=3,
          textInput(inputId = "editlastname", label=strong(span(style="color:red;", "*"), "Last name:"), value=table$Lastname)
        ),
        column(
          width=3,
          textInput(inputId = "editusername", label=strong(span(style="color:red;", "*"), "Username:"), value=table$Username)
        ),
        column(
          width=3,
          textInput(inputId = "editpassword", label=strong(span(style="color:red;", "*"), "Password:"), value=table$Password)
        )
      ),
      br(),
      fluidRow(
        column(
          width=12,
          uiOutput("EditUserWarningMessage")        
        )
      ),
      br(),
      fluidRow(
        column(
          width=4,
          actionButton("Edit_User_Update_Button", label=strong("Update"), class="mybuttons", width="70px"),
          actionButton("Edit_User_Dismiss_Button", label=strong("Dismiss"), class="mybuttons", width="70px")
        )
      )
    )
  )
}

# Pop-up for edit ####
observeEvent(input$EditUser, {
  
  row <- input$logintable_rows_selected
  
  if(length(input$logintable_rows_selected) > 0){
    login_table_message("")
    table <- logindata()
    showModal(EditUser(table=table[row,]))
  }else{
    login_table_message("Please select a user to make changes")
  }
  
}, ignoreInit = TRUE)

## Observe when remove user yes button is clicked ####
observeEvent(input$Edit_User_Update_Button, {
  
  Firstname=trimws(input$editfirstname);
  Lastname=trimws(input$editlastname);
  Username=trimws(input$editusername);
  Password=trimws(input$editpassword);
  Status="Moderator";
  
  row <- input$logintable_rows_selected
  login_dat <- data.frame(logindata())
  
  if(Firstname=="" | Lastname=="" | Username=="" | Password==""){
    
    edituserwarningmsg("Please fill in the required (*) fields.")
    
  }else{
    
    login_dat[row,] <- c(Firstname, Lastname, Username, Password, "Moderator")
    logindata(login_dat)
    edituserwarningmsg("")
    login_table_message(paste0("User ", Username, " has been modified."))
    removeModal()
    
  }
  
})

## Observe when remove user no button is clicked ####
observeEvent(input$Edit_User_Dismiss_Button, {
  removeModal()
})

## Output edit user login message ####
output$EditUserWarningMessage <- renderUI({
  
  req(edituserwarningmsg())
  
  p(style="color:red;", HTML(edituserwarningmsg()))
  
})

## Remove user modal dialog ####
RemoveUser <- function(){
  div(
    id = "removeUserData", 
    
    modalDialog(
      footer = NULL, size= "l", title = NULL,
      fluidRow(
        column(
          width=12,
          h4("Are you sure you want to remove this user?")
        ),
      ),
      br(),
      fluidRow(
        column(
          width=4,
          actionButton("Remove_User_Yes_Button", label=strong("Yes"), class="mybuttons", width="50px"),
          actionButton("Remove_User_No_Button", label=strong("No"), class="mybuttons", width="50px")
        )
      )
    )
  )
}

## Observe when remove user button is clicked ####
observeEvent(input$RemoveUser, {
  
  if(length(input$logintable_rows_selected) > 0){
    login_table_message("")
    showModal(RemoveUser())
  }else{
    login_table_message("Please select a user to remove.")
  }
  
})

## Observe when remove user yes button is clicked ####
observeEvent(input$Remove_User_Yes_Button, {
  
  row <- input$logintable_rows_selected
  login_dat <- data.frame(logindata())
  logindata(login_dat[-row,])
  removeModal()
  
})

## Observe when remove user no button is clicked ####
observeEvent(input$Remove_User_No_Button, {
  removeModal()
})

##pop-up for save user ####
SaveUser <- function(){
  div(
    id = "saveUserData",
    
    modalDialog(
      footer = NULL, size= "l", title = NULL,
      fluidRow(
        column(
          width=12,
          h4("Are you sure you want to save the changes?")
        ),
      ),
      br(),
      fluidRow(
        column(
          width=4,
          actionButton("Save_User_Yes_Button", label=strong("Yes"), class="mybuttons", width="50px"),
          actionButton("Save_User_No_Button", label=strong("No"), class="mybuttons", width="50px")
        )
      )
    )
  )
}

## CLICKING THE USER SAVE BUTTON ####
observeEvent(input$SaveUser, {
  
  login_table_message("")
  showModal(SaveUser())
  
})

## Observe when save user yes button is clicked ####
observeEvent(input$Save_User_Yes_Button, {
  
  data <- logindata()
  write.csv(data, paste0("data/User_Login_List.csv"), row.names = FALSE)
  login_table_message("Login list has been saved.")
  removeModal()
  
})

## Observe when save user no button is clicked ####
observeEvent(input$Save_User_No_Button, {
  removeModal()
})

## Login table warning message ####
output$LoginTableMessage <- renderUI({
  
  req(login_table_message())
  
  p(style="color:red;", HTML(login_table_message()))
  
})

## Output the user log in table ####
output$logintable <- DT::renderDataTable({
  
  req(logindata())
  
  logindata()
  
}, escape = FALSE, server = TRUE, rownames=FALSE, selection = "single", options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"))))

## CLICKING THE LOG-OUT BUTTON ####
observeEvent(input$LogOut, {
  
  ##remove warning message####
  project_table_message("")
  login_table_message("")
  
  ##Change the log status to FALSE
  ##AND go back to the login screen
  UserLog$Logged <- FALSE
  
})

#######################################################
#
# ADD DOWNLOAD HANDLER ####
#
#######################################################

##Download introduction template#####
output$add_intro_template_rmd <- downloadHandler(
  
  filename = paste0("introduction_template.Rmd"),
  
  content = function(file){
    file.copy("data/Template/introduction.Rmd", file)
  },
  
  contentType = "application"
  
)

##Download profile annotation template#####
output$add_pro_template_csv <- downloadHandler(
  
  filename = paste0("profile_annotation_template.csv"),
  
  content = function(file){
    file.copy("data/Template/profile_annotation.csv", file)
  },
  
  contentType = "application"
  
)

##Download profile annotation template#####
output$add_pro_template_rds <- downloadHandler(
  
  filename = paste0("profile_annotation_template.RDS"),
  
  content = function(file){
    file.copy("data/Template/profile_annotation.RDS", file)
  },
  
  contentType = "application"
  
)

##Download chemical annotation template#####
output$add_chem_template_csv <- downloadHandler(
  
  filename = paste0("chemical_annotation_template.csv"),
  
  content = function(file){
    file.copy("data/Template/chemical_annotation.csv", file)
  },
  
  contentType = "application"
  
)

##Download chemical annotation template#####
output$add_chem_template_rds <- downloadHandler(
  
  filename = paste0("chemical_annotation_template.RDS"),
  
  content = function(file){
    file.copy("data/Template/chemical_annotation.RDS", file)
  },
  
  contentType = "application"
  
)

##Download gene expression template#####
output$add_ge_template_csv <- downloadHandler(
  
  filename = paste0("gene_expression_template.csv"),
  
  content = function(file){
    file.copy("data/Template/gene_expression.csv", file)
  },
  
  contentType = "application"
  
)

##Download gene expression template#####
output$add_ge_template_rds <- downloadHandler(
  
  filename = paste0("gene_expression_template.RDS"),
  
  content = function(file){
    file.copy("data/Template/gene_expression.RDS", file)
  },
  
  contentType = "application"
  
)

##Download connectivity template (pertubargens class)#####
output$add_conn_pcl_template_csv <- downloadHandler(
  
  filename = paste0("connectivity_pertubargen_class_template.csv"),
  
  content = function(file){
    file.copy("data/Template/connectivity_pcl.csv", file)
  },
  
  contentType = "application"
  
)

output$add_conn_pcl_template_rds <- downloadHandler(
  
  filename = paste0("connectivity_pertubargen_class_template.RDS"),
  
  content = function(file){
    file.copy("data/Template/connectivity_pcl.RDS", file)
  },
  
  contentType = "application"
  
)

##Download connectivity template (pertubargens)#####
output$add_conn_pert_template_csv <- downloadHandler(
  
  filename = paste0("connectivity_pertubargens_template.csv"),
  
  content = function(file){
    file.copy("data/Template/connectivity_pert.csv", file)
  },
  
  contentType = "application"
  
)

output$add_conn_pert_template_rds <- downloadHandler(
  
  filename = paste0("connectivity_pertubargens_template.RDS"),
  
  content = function(file){
    file.copy("data/Template/connectivity_pert.RDS", file)
  },
  
  contentType = "application"
  
)

##Download hallmark template#####
output$add_hallmark_template_gmt <- downloadHandler(
  
  filename = paste0("hallmark_template.gmt"),
  
  content = function(file){
    file.copy("data/Template/hallmark.gmt", file)
  },
  
  contentType = "application"
  
)

##Download c2 template#####
output$add_c2_template_gmt <- downloadHandler(
  
  filename = paste0("c2_template.gmt"),
  
  content = function(file){
    file.copy("data/Template/c2.gmt", file)
  },
  
  contentType = "application"
  
)

##Download nursa template#####
output$add_nursa_template_gmt <- downloadHandler(
  
  filename = paste0("nursa_template.gmt"),
  
  content = function(file){
    file.copy("data/Template/nursa.gmt", file)
  },
  
  contentType = "application"
  
)

#######################################################
#
# EDIT DOWNLOAD HANDLER ####
#
#######################################################

##Download introduction template#####
output$edit_intro_template_rmd <- downloadHandler(
  
  filename = paste0("introduction_template.Rmd"),
  
  content = function(file){
    file.copy("data/Template/introduction.Rmd", file)
  },
  
  contentType = "application"
  
)

##Download profile annotation template#####
output$edit_pro_template_csv <- downloadHandler(
  
  filename = paste0("profile_annotation_template.csv"),
  
  content = function(file){
    file.copy("data/Template/profile_annotation.csv", file)
  },
  
  contentType = "application"
  
)

##Download profile annotation template#####
output$edit_pro_template_rds <- downloadHandler(
  
  filename = paste0("profile_annotation_template.RDS"),
  
  content = function(file){
    file.copy("data/Template/profile_annotation.RDS", file)
  },
  
  contentType = "application"
  
)

##Download chemical annotation template#####
output$edit_chem_template_csv <- downloadHandler(
  
  filename = paste0("chemical_annotation_template.csv"),
  
  content = function(file){
    file.copy("data/Template/chemical_annotation.csv", file)
  },
  
  contentType = "application"
  
)

##Download chemical annotation template#####
output$edit_chem_template_rds <- downloadHandler(
  
  filename = paste0("chemical_annotation_template.RDS"),
  
  content = function(file){
    file.copy("data/Template/chemical_annotation.RDS", file)
  },
  
  contentType = "application"
  
)

##Download gene expression template#####
output$edit_ge_template_csv <- downloadHandler(
  
  filename = paste0("gene_expression_template.csv"),
  
  content = function(file){
    file.copy("data/Template/gene_expression.csv", file)
  },
  
  contentType = "application"
  
)

##Download gene expression template#####
output$edit_ge_template_rds <- downloadHandler(
  
  filename = paste0("gene_expression_template.RDS"),
  
  content = function(file){
    file.copy("data/Template/gene_expression.RDS", file)
  },
  
  contentType = "application"
  
)

##Download connectivity template (pertubargens class)#####
output$edit_conn_pcl_template_csv <- downloadHandler(
  
  filename = paste0("connectivity_pertubargen_class_template.csv"),
  
  content = function(file){
    file.copy("data/Template/connectivity_pcl.csv", file)
  },
  
  contentType = "application"
  
)

output$edit_conn_pcl_template_rds <- downloadHandler(
  
  filename = paste0("connectivity_pertubargen_class_template.RDS"),
  
  content = function(file){
    file.copy("data/Template/connectivity_pcl.RDS", file)
  },
  
  contentType = "application"
  
)

##Download connectivity template (pertubargens)#####
output$edit_conn_pert_template_csv <- downloadHandler(
  
  filename = paste0("connectivity_pertubargens_template.csv"),
  
  content = function(file){
    file.copy("data/Template/connectivity_pert.csv", file)
  },
  
  contentType = "application"
  
)

output$edit_conn_pert_template_rds <- downloadHandler(
  
  filename = paste0("connectivity_pertubargens_template.RDS"),
  
  content = function(file){
    file.copy("data/Template/connectivity_pert.RDS", file)
  },
  
  contentType = "application"
  
)

##Download hallmark template#####
output$edit_hallmark_template_gmt <- downloadHandler(
  
  filename = paste0("hallmark_template.gmt"),
  
  content = function(file){
    file.copy("data/Template/hallmark.gmt", file)
  },
  
  contentType = "application"
  
)

##Download c2 template#####
output$edit_c2_template_gmt <- downloadHandler(
  
  filename = paste0("c2_template.gmt"),
  
  content = function(file){
    file.copy("data/Template/c2.gmt", file)
  },
  
  contentType = "application"
  
)

##Download nursa template#####
output$edit_nursa_template_gmt <- downloadHandler(
  
  filename = paste0("nursa_template.gmt"),
  
  content = function(file){
    file.copy("data/Template/nursa.gmt", file)
  },
  
  contentType = "application"
  
)
