
## Create reactive values ####
projectdata <- reactiveVal(projectlist)
logindata <- reactiveVal(loginlist)

## Warning message for main project and login table ####
LogInMessage <- reactiveVal(NULL)
project_table_message <- reactiveVal(NULL)
login_table_message <- reactiveVal(NULL)
about_file_msg <- reactiveVal(NULL)
contact_file_msg <- reactiveVal(NULL)
forgotpasswordwarningmsg <- reactiveVal(NULL)
changepwdwarningmsg <- reactiveVal(NULL)

## Create reactive import data####
reset <- reactiveVal(TRUE)
portal <- reactiveVal(NULL)
user <- reactiveVal(NULL)
cohorts <- reactiveVal(NULL)
intro_file <- reactiveVal(NULL)
pro_file <- reactiveVal(NULL)
chem_file <- reactiveVal(NULL)
ge_file <- reactiveVal(NULL)
conn_pcl_file <- reactiveVal(NULL)
conn_pert_file <- reactiveVal(NULL)
gs_collection_file <- reactiveVal(NULL)

## Create reactuve import data message####
intro_file_msg <- reactiveVal(NULL)
pro_file_msg <- reactiveVal(NULL)
ge_file_msg <- reactiveVal(NULL)
conn_pcl_file_msg <- reactiveVal(NULL)
conn_pert_file_msg <- reactiveVal(NULL)
gs_collection_file_msg <- reactiveVal(NULL)

## Warning message for add project function####
addprojectwarningmsg <- reactiveVal(NULL)
addinputwarningmsg <- reactiveVal(NULL)
addcompoundvarwarningmsg <- reactiveVal(NULL)
addexposurevarwarningmsg <- reactiveVal(NULL)
addexposurephenotypevarwarningmsg <- reactiveVal(NULL)
addenrichmentgswarningmsg <- reactiveVal(NULL)
addenrichmentlinkwarningmsg <- reactiveVal(NULL)

## Warning message for edit project function####
editprojectwarningmsg <- reactiveVal(NULL)
editinputwarningmsg <- reactiveVal(NULL)
editcompoundvarwarningmsg <- reactiveVal(NULL)
editexposurevarwarningmsg <- reactiveVal(NULL)
editexposurephenotypevarwarningmsg <- reactiveVal(NULL)
editenrichmentgswarningmsg <- reactiveVal(NULL)
editenrichmentlinkwarningmsg <- reactiveVal(NULL)

## Warning message for add and edit user function###
adduserwarningmsg <- reactiveVal(NULL)
edituserwarningmsg <- reactiveVal(NULL)

#sign in page
output$pageStub <- renderUI({
  
  #####<!-- START SIGN IN PAGE -->
  shiny::div(
    uiOutput(outputId = "uiLogin"),
    uiOutput(outputId = "uiMain")
  )#####<!-- END SIGN IN PAGE -->

})

# call the logout module with reactive trigger to hide/show
logout_init <- callModule(
  logout, 
  id = "logout", 
  active = reactive(credentials()$user_auth)
)

# call login module supplying data frame, user and password cols
# and reactive trigger
credentials <- callModule(
  login, 
  id = "login", 
  log_out = reactive(logout_init())
)

##IF USER LOG IS FALSE, THEN SHOW THE LOGIN PANNEL####
output$uiLogin <- renderUI({
  
  req(credentials()$user_auth== FALSE)
  
  loginUI(id="login")
  
})

## Functions to forgot password #####
ForgotPassword <- function() {
  div(
    id = "Forgot_Password",
    
    modalDialog(
      size = "s", title = "Forgot your password?", footer = NULL,
      
      fluidRow(
        column(
          width=12,
          p(strong("To access your account, please enter your information.")),
          textInput(inputId="FG_Firstname", label=strong(span(style="color:red;", "*"), "First name"), value=""),
          textInput(inputId="FG_Lastname", label=strong(span(style="color:red;", "*"), "Last name"), value=""),
          textInput(inputId="FG_Username", label=strong(span(style="color:red;", "*"), "Username"), value=""),
          uiOutput("FG_Message"), 
          br(),
          actionButton(class="mybuttons", inputId="FG_Button", label=strong("Submit")),
          actionButton(class="mybuttons", inputId="FG_Back", label=strong("Back"))
        )
      )
    )
  )
}

##OBSERVE THE FORGOT BUTTON#####
observeEvent(input$ForgetPassword, {
  
  #Show the add modal
  showModal(ForgotPassword())
  
})

##OBSERVE THE SUBMIT BUTTON#####
observeEvent(input$FG_Button, {
  
  Firstname=trimws(input$FG_Firstname);
  Lastname=trimws(input$FG_Lastname);
  Username=trimws(input$FG_Username);
  
  login_dat <- read.csv(paste0("data/User_Login_List.csv"), header = TRUE, stringsAsFactors = FALSE)
  #print(login_dat)

  if(Firstname=="" | Lastname=="" | Username==""){
    
    forgotpasswordwarningmsg("Please fill in the required (*) fields.")
    
  }else{
    
    row <- which(login_dat$Username == Username)
    #print(row); print(Username);
    
    if(length(row) > 0){
      tmp_pwd <- password(n = 10, numbers = TRUE, case = TRUE, special = c("?", "!", "&", "%", "$"))
      login_dat$Password[row[1]] <- sodium::password_store(as.character(tmp_pwd))
      sendpassword(
	 from_sender="rchau88@bu.edu",
	 to_recipient="lilychau999@gmail.com", 
  	 recipient_first=Firstname, 
  	 recipient_last=Lastname, 
  	 recipient_account=Username, 
 	 tmp_pwd=tmp_pwd
      )
      write.csv(login_dat, paste0("data/User_Login_List.csv"), row.names = FALSE)
      forgotpasswordwarningmsg("Thank you for your submission! A temporary password has been sent to your email.")
      
    }else{
      
      forgotpasswordwarningmsg("This username does not exist in our database. Please enter another username.")
      
    }
    
  }
})

##OBSERVE THE BACK BUTTON#####
observeEvent(input$FG_Back, {
  
  forgotpasswordwarningmsg("")
  removeModal()
  
})

## forgot password warning message ####
output$FG_Message <- renderUI({
  
  req(forgotpasswordwarningmsg())
  
  p(style="color:red;", HTML(forgotpasswordwarningmsg()))
  
})

###IF LOGGED IS TRUE THEN SHOW THE MAIN PAGE#####
output$uiMain <- renderUI({
  
  req(credentials()$user_auth == TRUE)
  
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
            logoutUI(id="logout")
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
        width=6,
        
        h3("About Page"),
        br(),
        fileInput(inputId = "add_about_file", label = strong(span(style="color:red;", "*"), "Choose an about file ", downloadLink(outputId = "add_about_template_rmd", label = em(style="font-size: 11px", "about_page.Rmd")))),
        uiOutput(outputId='add_about_file_msg'),
        actionButton(class="mybuttons", inputId="About_Page_Add_Button", label=strong("Add"), width="70px")
      ),
      column(
        width=6,
        
        h3("Contact Page"),
        br(),
        fileInput(inputId = "add_contact_file", label = strong(span(style="color:red;", "*"), "Choose a contact file ", downloadLink(outputId="add_contact_template_rmd", label = em(style="font-size: 11px", "contact_page.Rmd")))),
        uiOutput(outputId='add_contact_file_msg'),
        actionButton(class="mybuttons", inputId="Contact_Page_Add_Button", label=strong("Add"), width="70px"),
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
table_colnames <- c("Project", "Cell_Line", "Portal", "Landmark_Gene", "Exposure_Levels", "Exposure_Phenotype", "Description")

## Output the project table ####
output$projecttable <- DT::renderDataTable({
  
  if(all(!is.na(projectdata()$Project))){
    table <- projectdata() %>% select(all_of(table_colnames))
  }else{
    table <- projectdata() %>% mutate_all(~replace(., is.na(.), "<br>")) %>% select(all_of(table_colnames))
  }  
  
  colnames(table) <- gsub("_", " ", colnames(table))
  
  return(table)
  
}, escape = FALSE, extensions = 'Buttons', server = TRUE, rownames = FALSE, selection = "single",
options = list(
  columnDefs = list(list(width='400px', targets=-1)),
  deferRender = FALSE,
  paging = TRUE,
  searching = TRUE,
  ordering = TRUE,
  pageLength = 20,
  scrollX = TRUE,
  scrollY = 400,
  scrollCollapse = TRUE,
  dom = 'T<"clear">Blfrtip',
  buttons=c('copy','csv','print')
))

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
          uiOutput("addprojectwarningmessage")        
        )
      ),
      br(),
      fluidRow(
        column(
          width=12,
          shinyjs::hidden(
            div(
              id = "add-loading-content",
              div(class="loader"),
              h4("Processing...", id="loading_text")
            )
          )
        )
      ),
      fluidRow(
        column(
          width = 6,
          
          ## Include L1000 landmark genes?####
          radioButtonsWithTooltip(inputId="Add_Landmark", label="L1000 Landmark Gene", bId="add_landmark_tooltip", helptext="some helptext here", choices=c("Yes"=TRUE, "No"=FALSE), selected=FALSE, inline=TRUE), 
          
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
          
          fileInput(inputId = "add_ge_file", label = strong(span(style="color:red;", "*"), "Choose a gene expression file")),
          fileInputRadioButtonsWithTooltip(
            inputId="add_ge_file_type", label="File type:", bId="add_ge_template", helptext="", choices=c(".csv", ".RDS"), selected=NULL, inline=TRUE
          ),
          uiOutput(outputId='add_ge_file_msg'),
          br(),
          
          radioButtons(inputId = "add_conn_option", label = "Include connectivity map?", choices = c("Yes", "No"), inline = TRUE),
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
          
          ##Get compound and exposure variables ####
          h4("Exposure Variables:", style="padding-bottom: 10px;"),
          selectInputWithTooltip(inputId = "add_variable_compound", label=strong(span(style="color:red;", "*"), "Select a compound variable:"), bId="add_var_compound", helptext="Some description here", choices=c("Import a profile annotation" = ""), multiple = FALSE),
          uiOutput(outputId = 'add_variable_compound_msg'),
          selectInputWithTooltip(inputId = "add_variable_exposure", label=strong(span(style="color:red;", "*"), "Select a list of additional exposure variables:"), bId="add_var_exposure", helptext="Some description here", choices=c("Import a profile annotation" = ""), multiple = TRUE),
          uiOutput(outputId = 'add_variable_exposure_msg'),
          
          ## Calculations ####
          uiOutput(outputId = "Add_Tas_Modz"),
          br(),
          
          ## Gene set enrichment ####
          h4("Gene Set Enrichment:", style="padding-bottom: 10px;"),
          radioButtonsWithTooltip(
            inputId = "add_cur_enrichment_option",
            label = "Use existing gene set",
            bId = "add_cur_enrichment",
            helptext = "Gene sets include the MSigDB collections (Hallmark, C2 reactome pathways), and gene targets of various nuclear receptors (NURSA)",
            choices = c("Yes", "No")
          ),
          conditionalPanel(
            condition = "input.add_cur_enrichment_option == 'No'",
            textInput(inputId = "Add_New_Enrichment_GS", label = strong(span(style="color:red;", "*"), "Enter a Name for Gene Set Collection:"), value = ""),
            uiOutput(outputId = "add_enrichment_gs_msg"),
            textInput(inputId = "Add_New_Enrichment_Link", label = strong(span(style="color:red;", "*"), "Provide a Link to the GS Collection:"), value = ""),
            uiOutput(outputId = "add_enrichment_link_msg"),
            fileInput(inputId = "add_gs_collection_file", label = strong(span(style="color:red;", "*"), "Choose a Gene Set Collection", downloadLink(outputId = "add_gs_collection_template_gmt", label = em(style="font-size: 11px", "template.gmt")))),
            uiOutput(outputId = "add_gs_collection_file_msg")
          ),
          shinyjs::disabled(radioButtons(inputId="add_ssGSEA_method", label="Choose a ssGSEA method:", choices=c("gsva"="gsva", "ssgsea", "zscore", "plage"), inline=TRUE)),
          br(),
          
          ## K2Taxonomer Analysis ####
          h4("K2Taxonomer Analysis:", style="padding-bottom: 10px;"),
          selectInputWithTooltip(inputId = "add_variable_exposure_phenotype", label=strong(span(style="color:red;", "*"), "Select a list of exposure phenotype:"), bId="add_var_exposure_phenotype", helptext="Some description here", choices=c("Import a profile annotation" = ""), multiple = TRUE),
          uiOutput(outputId = 'add_variable_exposure_phenotype_msg'),
          DT::dataTableOutput(outputId = "add_metavar_variable_test"),
          br(),
          
          conditionalPanel(
            condition = "input.add_conn_option == 'Yes'",
            checkboxInput(inputId = "add_connectivity_var", label = "Add Connectivity Variables", value = FALSE),
            conditionalPanel(
              condition = "input.add_connectivity_var == true",
              selectInput(inputId="add_connectivity_test", label="Choose a statistical test:", choices=c("1-sided Wilcox RS test", "2-sided Wilcox RS test", "1-sided t test", "2-sided t test"))
            )
          ),
           
          ## Additional parameters ####
          h4("Additional Parameters:"),
          selectInputWithTooltip(inputId="add_feature_metric", label="Feature filtering:", bId="add_feature_filtering_metric", helptext="Some description here", choices=c("sd"="sd", "mad"="mad"), selected="sd") 
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

## Show pop-up for add project ####
observeEvent(input$AddProject, {
  
  #Reset file input####
  session$sendCustomMessage("ResetFileInput", "add_intro_file")
  session$sendCustomMessage("ResetFileInput", "add_pro_file")
  session$sendCustomMessage("ResetFileInput", "add_ge_file")
  session$sendCustomMessage("ResetFileInput", "add_conn_pcl_file")
  session$sendCustomMessage("ResetFileInput", "add_conn_pert_file")
  session$sendCustomMessage("ResetFileInput", "add_gs_collection_file")

  ##Remove error messages
  addprojectwarningmsg(NULL)
  addinputwarningmsg(NULL)
  addcompoundvarwarningmsg(NULL)
  addexposurevarwarningmsg(NULL)
  addexposurephenotypevarwarningmsg(NULL)
  addenrichmentgswarningmsg(NULL)
  addenrichmentlinkwarningmsg(NULL)
  
  ##Remove data messages
  portal(NULL)
  cohorts(NULL)
  intro_file_msg(NULL)
  pro_file_msg(NULL)
  ge_file_msg(NULL)
  conn_pcl_file_msg(NULL)
  conn_pert_file_msg(NULL)
  gs_collection_file_msg(NULL)
  project_table_message(NULL)
  
  ##Remove data
  intro_file(NULL)
  pro_file(NULL)
  chem_file(NULL)
  ge_file(NULL)
  conn_pcl_file(NULL)
  conn_pert_file(NULL)
  gs_collection_file(NULL)

  #Show the add modal
  showModal(AddProject())
  
}, ignoreInit = TRUE)

## Output the add project warning message ###
output$addprojectwarningmessage <- renderUI({
  
  req(addprojectwarningmsg())
  
  p(style="color:red;", HTML(addprojectwarningmsg()))
  
})

## Observe the gs collection####
observeEvent(input$Add_New_Enrichment_GS, {
  
  req(input$Add_New_Enrichment_GS)
  
  gs_collection=trimws(input$Add_New_Enrichment_GS)
  
  if(gs_collection==""){
    addenrichmentgswarningmsg("Please enter a valid name.")
  }else{
    addenrichmentgswarningmsg("")
  }
  
})

## Output gs collection warning message ####
output$add_enrichment_gs_msg <- renderUI({
  
  req(addenrichmentgswarningmsg())
  
  p(style="color:red;", HTML(addenrichmentgswarningmsg()))
  
})

## Observe the gs enrichment link####
observeEvent(input$Add_New_Enrichment_Link, {
  
  req(input$Add_New_Enrichment_Link)
  
  gs_collection_link=trimws(input$Add_New_Enrichment_Link)
  
  if(gs_collection_link==""){
    addenrichmentlinkwarningmsg("Please enter a valid link.")
  }else{
    addenrichmentlinkwarningmsg("")
  }
  
})

## Output gs enrichment link warning message ####
output$add_enrichment_link_msg <- renderUI({
  
  req(addenrichmentlinkwarningmsg())
  
  p(style="color:red;", HTML(addenrichmentlinkwarningmsg()))
  
})

##Observe when variable compound changed####
observeEvent(input$add_variable_compound, {
  
  req(input$add_variable_compound)
  
  addcompoundvarwarningmsg(NULL)
  
})

##Create message for compound variable selection###
output$add_variable_compound_msg <- renderUI({
  
  req(addcompoundvarwarningmsg())
  
  p(style="color:red;", HTML(addcompoundvarwarningmsg()))
  
})

##Observe when variable exposure changed####
observeEvent(input$add_variable_exposure, {
  
  req(input$add_variable_exposure)
  
  addexposurevarwarningmsg(NULL)
  
})

##Create message for exposure variable selection####
output$add_variable_exposure_msg <- renderUI({
  
  req(addexposurevarwarningmsg())
  
  p(style="color:red;", HTML(addexposurevarwarningmsg()))
  
})

##Observe when variable exposure phenotype changed####
observeEvent(input$add_variable_exposure_phenotype, {
  
  req(input$add_variable_exposure_phenotype)
  
  addexposurephenotypevarwarningmsg(NULL)
  
})

##Create message for exposure phenotype variable selection####
output$add_variable_exposure_phenotype_msg <- renderUI({
  
  req(addexposurephenotypevarwarningmsg())
  
  p(style="color:red;", HTML(addexposurephenotypevarwarningmsg()))
  
})

## Create message for all inputs selection####
output$add_input_message <- renderUI({
  
  req(addinputwarningmsg())
  
  p(style="color:red; text-align:center;", HTML(addinputwarningmsg()))
  
})

# Functions to edit project #####
EditProject <- function(table) {
  div(
    id = "editProjectData",
    
    modalDialog(
      size = "l", title = "Edit Project", footer = NULL,
      fluidRow(
        column(
          width=4,
          textInput(inputId = "Edit_Project_Name", label=strong(span(style="color:red;", "*"), "Project name:"), value=table$Project),
        ),
        column(
          width=4,
          textInput(inputId = "Edit_Cell_Line_Name", label=strong(span(style="color:red;", "*"), "Cell line name:"), value=table$Cell_Line)
        ),
        column(
          width=4,
          textInput(inputId = "Edit_Portal_Name", label=strong(span(style="color:red;", "*"), "Portal name:"), value=table$Portal)
        )
      ),
      fluidRow(
        column(
          width=12,
          strong(span(style="color:red;", "*"), "Description:", style="text-align: left;"),
          HTML(paste0("<textarea style='width: 100%; height: 150px; padding: 10px; margin-top: 5px;' id='Edit_Description' placeholder='Write a short description about the project...'>", ifelse(is.na(table$Description), "", as.character(table$Description)), "</textarea>")),
          helpText(em("Note: you can use HTML tags inside the description box for styling and formatting text", style="font-size: 9pt;"))
        )
      ),
      br(),
      fluidRow(
        column(
          width=12,
          uiOutput("editprojectwarningmessage")        
        )
      ),
      br(),
      fluidRow(
        column(
          width=12,
          shinyjs::hidden(
            div(
              id = "edit-loading-content",
              div(class="loader"),
              h4("Processing...", id="loading_text")
            )
          )
        )
      ),
      fluidRow(
        column(
          width=6,
          
          ## Include L1000 landmark genes####
          radioButtonsWithTooltip(inputId="Edit_Landmark", label="L1000 Landmark Gene", bId="edit_landmark_tooltip", helptext="some helptext here", choices=c("Yes"=TRUE, "No"=FALSE), selected=ifelse(is.na(table$Landmark_Gene), "No", table$Landmark_Gene), inline=TRUE), 

          ## Modify input files ####
          h4("Modify Input Files:", style="padding-bottom: 10px;"),
          radioButtons(inputId="edit_files", label=NULL, choices=c("None", "All", "Introduction Page", "Profile Annotation", "Gene Expression", "Connectivity Map", "GS Enrichment", "K2Taxonomer")),
          br(),
          
          conditionalPanel(
            condition = "input.edit_files == 'Introduction Page' | input.edit_files == 'All'",
            fileInput(inputId="edit_intro_file", label=strong(span(style="color:red;", "*"), "Choose an introduction file ", downloadButton(outputId="edit_intro_download_file", label=em(style="font-size: 11px", "Introduction_page.Rmd")))),
            uiOutput(outputId='edit_intro_file_msg'),
            br()
          ),
          
          conditionalPanel(
            condition = "input.edit_files == 'Profile Annotation' | input.edit_files == 'All' | input.edit_files == 'Gene Expression'| input.edit_files == 'Connectivity Map'",
            conditionalPanel(
              condition = "input.edit_files == 'Gene Expression'",
              radioButtons(inputId="edit_ge_pro_option", label="Does profile annotation change?", choices=c("Yes", "No"), selected="No", inline=TRUE)
            ),
            conditionalPanel(
              condition = "input.edit_files == 'Connectivity Map'",
              radioButtons(inputId="edit_conn_pro_option", label="Does profile annotation change?", choices=c("Yes", "No"), selected="No", inline=TRUE)
            ),
            conditionalPanel(
              condition = "input.edit_files == 'Profile Annotation' | input.edit_files == 'All' | (input.edit_files == 'Gene Expression' && input.edit_ge_pro_option=='Yes')  | (input.edit_files == 'Connectivity Map' && input.edit_conn_pro_option=='Yes')",
              fileInput(inputId="edit_pro_file", label=strong(span(style="color:red;", "*"), "Choose a profile annotation file", downloadButton(outputId="edit_pro_download_file", label=em(style="font-size: 11px", "Profile_annotation.RDS")))),
              fileInputRadioButtonsWithTooltip(
                inputId="edit_pro_file_type", label="File type:", bId="edit_pro_template", helptext="", choices=c(".csv", ".RDS"), selected=NULL, inline=TRUE
              ),
              uiOutput(outputId='edit_pro_file_msg'),
              br(),
              conditionalPanel(
                condition = "input.edit_files == 'Profile Annotation'",
                radioButtons(inputId="edit_pro_ge_option", label="Does gene expression change?", choices=c("Yes", "No"), inline=TRUE)
              )
            )
          ),

          conditionalPanel(
            condition = "input.edit_files == 'Gene Expression' | input.edit_files == 'All' | (input.edit_files == 'Profile Annotation' && input.edit_pro_ge_option=='Yes') | (input.edit_files == 'Connectivity Map' && input.edit_conn_pro_option=='Yes')",
            fileInput(inputId = "edit_ge_file", label = strong(span(style="color:red;", "*"), "Choose a gene expression file", downloadButton(outputId="edit_ge_download_file", label=em(style="font-size: 11px", "Expression_Set.RDS")))),
            fileInputRadioButtonsWithTooltip(
              inputId="edit_ge_file_type", label="File type:", bId="edit_ge_template", helptext="", choices=c(".csv", ".RDS"), selected=NULL, inline=TRUE
            )
          ),
          uiOutput(outputId='edit_ge_file_msg'),
          br(),
          
          conditionalPanel(
            condition = "input.edit_files == 'All' | input.edit_files == 'Connectivity Map' | input.edit_files == 'Profile Annotation' | input.edit_files == 'Gene Expression'",
            conditionalPanel(
              condition = "input.edit_files == 'All' | input.edit_files == 'Profile Annotation' | input.edit_files == 'Gene Expression'",
              radioButtons(inputId = "edit_conn_option", label = "Does connectivity map change?", choices = c("Yes", "No"), inline = TRUE),
            ),
            conditionalPanel(
              condition = "(input.edit_files == 'All'  && input.edit_conn_option == 'Yes') | input.edit_files == 'Connectivity Map' | (input.edit_files == 'Profile Annotation' && input.edit_conn_option == 'Yes') | (input.edit_files == 'Gene Expression' && input.edit_conn_option == 'Yes')",
              fileInput(inputId = "edit_conn_pcl_file", label = strong(span(style="color:red;", "*"), "Choose a connectivity map file (Perturbagen Classes)")),
              fileInputRadioButtonsWithTooltip(
                inputId="edit_conn_pcl_file_type", label="File type:", bId="edit_conn_pcl_template", helptext="", choices=c(".csv", ".RDS"), selected=NULL, inline=TRUE
              )
            ),
            uiOutput(outputId='edit_conn_pcl_file_msg'),
            br(),
            conditionalPanel(
              condition = "(input.edit_files == 'All'  && input.edit_conn_option == 'Yes') | input.edit_files == 'Connectivity Map' | (input.edit_files == 'Profile Annotation' && input.edit_conn_option == 'Yes') | (input.edit_files == 'Gene Expression' && input.edit_conn_option == 'Yes')",
              fileInput(inputId = "edit_conn_pert_file", label = strong(span(style="color:red;", "*"), "Choose a connectivity map file (Perturbagens)")),
              fileInputRadioButtonsWithTooltip(
                inputId="edit_conn_pert_file_type", label="File type:", bId="edit_conn_pert_template", helptext="", choices=c(".csv", ".RDS"), selected=NULL, inline=TRUE
              )
            ),
            uiOutput(outputId='edit_conn_pert_file_msg')
          )
        ),
        
        column(
          width=6,
          
          conditionalPanel(
            condition = "input.edit_files == 'Profile Annotation' | input.edit_files == 'Gene Expression' | input.edit_files == 'All' | input.edit_files == 'GS Enrichment' | input.edit_files == 'K2Taxonomer' | (input.edit_files == 'Connectivity Map' && input.edit_conn_pro_option == 'Yes')",
            
            ##Getting the exposure variables ####
            h4("Exposure Variables:", style="padding-bottom: 10px;"),
            selectInputWithTooltip(inputId = "edit_variable_compound", label=strong(span(style="color:red;", "*"), "Select a compound variable:"), bId="edit_var_compound", helptext="Some description here", choices=c("Import a profile annotation" = ""), multiple = FALSE),
            uiOutput(outputId = 'edit_variable_compound_msg'),
            selectInputWithTooltip(inputId = "edit_variable_exposure", label=strong(span(style="color:red;", "*"), "Select a list of additional exposure variables:"), bId="edit_var_exposure", helptext="Some description here", choices=c("Import a profile annotation" = ""), multiple = TRUE),
            uiOutput(outputId = 'edit_variable_exposure_msg'),
            
            ## Calculation ####
            uiOutput(outputId = "Edit_Tas_Modz"),
            br(),
            
            ## Redo enrichment analysis ####
            h4("Gene Set Enrichment:", style="padding-bottom: 10px;"),
            radioButtonsWithTooltip(
              inputId = "edit_cur_enrichment_option",
              label = "Use existing gene set",
              bId = "edit_cur_enrichment",
              helptext = "Gene sets include the MSigDB collections (Hallmark, C2 reactome pathways), and gene targets of various nuclear receptors (NURSA)",
              choices = c("Yes", "No"),
              selected = ifelse(table$GS_Collection=="Default", "Yes", "No")
            ),
            conditionalPanel(
              condition = "input.edit_cur_enrichment_option == 'No'",
              textInput(inputId = "Edit_New_Enrichment_GS", label = strong(span(style="color:red;", "*"), "Enter a Name for Gene Set Collection:"), value = ifelse(table$GS_Collection=="Default", "", as.character(table$GS_Collection))),
              uiOutput(outputId = "edit_enrichment_gs_msg"),
              textInput(inputId = "Edit_New_Enrichment_Link", label = strong(span(style="color:red;", "*"), "Provide a Link to the GS Collection:"), value = ifelse(table$GS_Collection=="Default", "", as.character(table$GS_Collection_Link))),
              uiOutput(outputId = "edit_enrichment_link_msg"),
              if(!table$GS_Collection %in% c("Default", "", NA)){
                radioButtons(inputId = "edit_gs_collection_file_option", label = strong(span(style="color:red;", "*"), "Change gene set collection?"), choices = c("Yes", "No"), selected = "No", inline=TRUE)
              }else{
                shinyjs::hidden(radioButtons(inputId = "edit_gs_collection_file_option", label = strong(span(style="color:red;", "*"), "Change the collection file:"), choices = c("Yes", "No"), selected = "Yes", inline=TRUE))
              }
            ),
            
            conditionalPanel(
              condition = "input.edit_cur_enrichment_option == 'No' & input.edit_gs_collection_file_option == 'Yes'",
              fileInput(inputId = "edit_gs_collection_file", label = strong(span(style="color:red;", "*"), "Choose a Gene Set Collection", downloadLink(outputId = "edit_gs_collection_template_gmt", label = em(style="font-size: 11px", "template.gmt")))),
              uiOutput(outputId = "edit_gs_collection_file_msg")
            ),
            
            conditionalPanel(
              condition = "input.edit_cur_enrichment_option == 'No' & input.edit_gs_collection_file_option == 'No'",
              downloadButton(outputId = "download_gs_collection_file", label = "Download gene set collection"),
              br(), br()
            ),
            
            conditionalPanel(
              condition = "input.edit_cur_enrichment_option == 'Yes'",
              uiOutput(outputId = "edit_enrichment_cur_version_option")
            ),
            shinyjs::disabled(radioButtons(inputId="edit_ssGSEA_method", label="Choose a ssGSEA method:", choices=c("gsva"="gsva", "ssgsea", "zscore", "plage"), inline=TRUE)),
            br(),
            
            ## K2Taxonomer ####
            h4("K2Taxonomer Analysis:", style="padding-bottom: 10px;"),
            selectInputWithTooltip(inputId = "edit_variable_exposure_phenotype", label=strong(span(style="color:red;", "*"), "Select a list of exposure phenotype:"), bId="edit_var_exposure_phenotype", helptext="Some description here", choices=c("Import a profile annotation" = ""), multiple = TRUE),
            uiOutput(outputId = 'edit_variable_exposure_phenotype_msg'),
            DT::dataTableOutput(outputId = "edit_metavar_variable_test"),
            br(),
            
            shinyjs::hidden(
              checkboxInput(inputId = "edit_connectivity_var", label = "Add Connectivity Variables", value = FALSE),
              
              conditionalPanel(
                condition = "input.edit_connectivity_var == true",
                selectInput(inputId="edit_connectivity_test", label="Choose a statistical test:", choices=c("1-sided Wilcox RS test", "2-sided Wilcox RS test", "1-sided t test", "2-sided t test"))
              )
            ),
            
            ## Additional parameters ####
            h4("Additional Parameters:"),
            selectInputWithTooltip(inputId="edit_feature_metric", label="Feature filtering:", bId="edit_feature_filtering_metric", helptext="Some description here", choices=c("sd"="sd", "mad"="mad"), selected="sd")
          )
        )
      ),
      br(),
      fluidRow(
        column(
          width=12,
          uiOutput("edit_input_message")        
        )
      ),
      br(),
      fluidRow(
        column(
          width=4,
          actionButton("Edit_Project_Add_Button", label=strong("Update"), class="mybuttons", width="70px"),
          actionButton("Edit_Project_Cancel_Button", label=strong("Cancel"), class="mybuttons", width="70px")
        )
      )
    )
  )
}

# Show pop-up for edit project ####
observeEvent(input$EditProject, {
  
  row <- input$projecttable_rows_selected
  
  if(length(input$projecttable_rows_selected) > 0){
    
    if(all(!is.na(projectdata()))){
      
      #Reset file input####
      session$sendCustomMessage("ResetFileInput", "edit_intro_file")
      session$sendCustomMessage("ResetFileInput", "edit_pro_file")
      session$sendCustomMessage("ResetFileInput", "edit_ge_file")
      session$sendCustomMessage("ResetFileInput", "edit_conn_pcl_file")
      session$sendCustomMessage("ResetFileInput", "edit_conn_pert_file")
      session$sendCustomMessage("ResetFileInput", "edit_gs_collection_file")

      ##Remove error messages
      editprojectwarningmsg(NULL)
      editinputwarningmsg(NULL)
      editcompoundvarwarningmsg(NULL)
      editexposurevarwarningmsg(NULL)
      editexposurephenotypevarwarningmsg(NULL)
      editenrichmentgswarningmsg(NULL)
      editenrichmentlinkwarningmsg(NULL)
      
      ##Remove data messages
      cohorts(NULL)
      intro_file_msg(NULL)
      pro_file_msg(NULL)
      ge_file_msg(NULL)
      conn_pcl_file_msg(NULL)
      conn_pert_file_msg(NULL)
      gs_collection_file_msg(NULL)
      project_table_message(NULL)
      
      ##Remove data
      intro_file(NULL)
      pro_file(NULL)
      chem_file(NULL)
      ge_file(NULL)
      conn_pcl_file(NULL)
      conn_pert_file(NULL)
      gs_collection_file(NULL)

      ##Show the edit modal
      table <- projectdata()
      portal(table[row,])
      showModal(EditProject(table=table[row,]))
      
    }else{
      
      project_table_message("There is no project to edit.")
      
    }
      
  }else{
    
    project_table_message("Please select a project to make changes")
    
  }
  
}, ignoreInit = TRUE)

## Output the edit project warning message ###
output$editprojectwarningmessage <- renderUI({
  
  req(editprojectwarningmsg())
  
  p(style="color:red;", HTML(editprojectwarningmsg()))
  
})

## Observe the enrichment version####
observeEvent(input$Edit_New_Enrichment_GS, {
  
  req(input$Edit_New_Enrichment_GS)
  
  gs_collection=trimws(input$Edit_New_Enrichment_GS)
  
  if(gs_collection==""){
    editenrichmentgswarningmsg("Please enter a valid name.")
  }else{
    editenrichmentgswarningmsg("")
  }
  
})

## Output the enrichment version warning message ###
output$edit_enrichment_gs_msg <- renderUI({
  
  req(editenrichmentgswarningmsg())
  
  p(style="color:red;", HTML(editenrichmentgswarningmsg()))
  
})

## Observe the enrichment version####
observeEvent(input$Edit_New_Enrichment_Link, {
  
  req(input$Edit_New_Enrichment_Link)
  
  gs_collection_link=trimws(input$Edit_New_Enrichment_Link)
  
  if(gs_collection_link==""){
    editenrichmentlinkwarningmsg("Please enter a valid link.")
  }else{
    editenrichmentlinkwarningmsg("")
  }
  
})

## Output the enrichment version warning message ###
output$edit_enrichment_link_msg <- renderUI({
  
  req(editenrichmentlinkwarningmsg())
  
  p(style="color:red;", HTML(editenrichmentlinkwarningmsg()))
  
})

##Observe when variable compound changed####
observeEvent(input$edit_variable_compound, {
  
  req(input$edit_variable_compound)
  
  editcompoundvarwarningmsg(NULL)
  
})

##Create message for compound variable selection####
output$edit_variable_compound_msg <- renderUI({
  
  req(editcompoundvarwarningmsg())
  
  p(style="color:red;", HTML(editcompoundvarwarningmsg()))
  
})

##Observe when variable exposure changed####
observeEvent(input$edit_variable_exposure, {
  
  req(input$edit_variable_exposure)
  
  editexposurevarwarningmsg(NULL)
  
})

##Create message for exposure variable selection####
output$edit_variable_exposure_msg <- renderUI({
  
  req(editexposurevarwarningmsg())
  
  p(style="color:red;", HTML(editexposurevarwarningmsg()))
  
})

##Observe when variable exposure phenotype changed####
observeEvent(input$edit_variable_exposure_phenotype, {
  
  req(input$edit_variable_exposure_phenotype)
  
  editexposurephenotypevarwarningmsg(NULL)
  
})

##Create message for exposure phenotype variable selection####
output$edit_variable_exposure_phenotype_msg <- renderUI({
  
  req(editexposurephenotypevarwarningmsg())
  
  p(style="color:red;", HTML(editexposurephenotypevarwarningmsg()))
  
})

##Create message for all inputs selection####
output$edit_input_message <- renderUI({
  
  req(editinputwarningmsg())
  
  p(style="color:red; text-align:center;", HTML(editinputwarningmsg()))
  
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
    if(all(!is.na(projectdata()))){
      project_table_message("")
      showModal(RemoveProject())
    }else{
      project_table_message("There is no project to remove.")
    }
  }else{
    project_table_message("Please select a project to remove.")
  }
  
})

## Observe when yes button is clicked ####
observeEvent(input$Remove_Project_Yes_Button, {
  
  portal(NULL)
  row <- input$projecttable_rows_selected
  proj_dat <- data.frame(projectdata())
  project_table_message(paste0("Project ", proj_dat$Project[row], " has been removed. Click 'Save' to implement this changes."))
  projectdata(proj_dat[-row,])
  removeModal()
  
})

## Observe when no button is clicked ####
observeEvent(input$Remove_Project_No_Button, {
  
  portal(NULL)
  removeModal()
  
})

## Function for save project ####
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

## CLICK ON THE SAVE PROJECT BUTTON ####
observeEvent(input$SaveProject, {
  
  project_table_message("")
  showModal(SaveProject())
  
})

## Observe when save project yes button is clicked ####
observeEvent(input$Save_Project_Yes_Button, {
  
  data <- projectdata(); portal <- portal();
  
  #print(portal)
  
  if(!is.null(portal)){
    if(!portal$Portal %in% data$Portal){
      unlink(paste0('data/', portal$Portal), recursive=TRUE, force=TRUE)
      unlink(paste0("www/JSON/", portal$Portal), recursive=TRUE, force=TRUE)
      unlink(paste0("www/RMD/introduction_", portal$Portal, ".Rmd"), recursive=TRUE, force=TRUE)
    }
  }
  
  write.csv(data, paste0("data/Project_List.csv"), row.names = FALSE)
  project_table_message("Project list has been saved.")
  removeModal()
  
})

## Observe when save project no button is clicked ####
observeEvent(input$Save_Project_No_Button, {
  
  data <- read.csv(paste0("data/Project_List.csv"), header = TRUE)
  project_table_message("")
  projectdata(data)
  removeModal()
  
})

#######################################################
#
# USER TABLE ####
#
#######################################################

## Output the user log in table ####
output$logintable <- DT::renderDataTable({
  
  req(logindata())
  
  logindata()
  
}, escape = FALSE, extensions = 'Buttons', server = TRUE, rownames = FALSE, selection = "single",
options = list(
  columnDefs = list(list(className = 'dt-center', targets = "_all")),
  deferRender = FALSE,
  paging = TRUE,
  searching = TRUE,
  ordering = TRUE,
  pageLength = 20,
  scrollX = TRUE,
  scrollY = 400,
  scrollCollapse = TRUE,
  dom = 'T<"clear">Blfrtip',
  buttons=c('copy','csv','print')
))

# Functions to add new data for user login #####
AddUser <- function() {
  div(
    id = "addUserData",
    
    modalDialog(
      size = "l", title = "Add User", footer = NULL,
      fluidRow(
        column(
          width=3,
          textInput(inputId="addfirstname", label=strong(span(style="color:red;", "*"), "First name:"), value=""),
        ),
        column(
          width=3,
          textInput(inputId="addlastname", label=strong(span(style="color:red;", "*"), "Last name:"), value="")
        ),
        column(
          width=3,
          textInput(inputId="addusername", label=strong(span(style="color:red;", "*"), "Username:"), value="")
        ),
        column(
          width=3,
          passwordInput(inputId="addpassword", label=strong(span(style="color:red;", "*"), "Password:"), value="")
        )
      ),
      br(),
      fluidRow(
        column(
          width=12,
          uiOutput(outputId="AddUserWarningMessage")        
        )
      ),
      br(),
      fluidRow(
        column(
          width=4,
          actionButton(inputId="Add_User_Add_Button", label=strong("Add"), class="mybuttons", width="70px"),
          actionButton(inputId="Add_User_Cancel_Button", label=strong("Cancel"), class="mybuttons", width="70px")
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
  
  #print(Password)
  
  login_dat <- data.frame(logindata(), stringsAsFactors=FALSE)
  new_user <- data.frame(
    Firstname=Firstname, 
    Lastname=Lastname, 
    Username=Username, 
    Password=sodium::password_store(Password), 
    Status=Status
  )
  
  if(Firstname=="" | Lastname=="" | Username=="" | Password==""){
    
    adduserwarningmsg("Please fill in the required (*) fields.")
    
  }else{
    
    validate_user <- login_dat %>% 
      filter(
        Username %in% !!Username
      )
    
    if(nrow(validate_user) > 0){
      adduserwarningmsg("This username is already existed. Please enter another username.")
    }else{
      adduserwarningmsg("")
      login_table_message(paste0('User ', Username, ' has been added. Click "Save" to keep the changes.'))
      newlogin <- login_dat %>% rbind(new_user)
      logindata(newlogin)
      removeModal()
    }
    
  }
})

## Observe the cancel user button is clicked #####
observeEvent(input$Add_User_Cancel_Button, {
  adduserwarningmsg(NULL)
  removeModal()
})

# Functions to add new data for user login #####
EditUser <- function(table) {
  
  div(
    id = "editUserData",
    modalDialog(
      size="l", title="Edit User", footer=NULL, 
      
      fluidRow(
        column(
          width=3,
          textInput(inputId="editfirstname", label=strong(span(style="color:red;", "*"), "First name:"), value=table$Firstname),
        ),
        column(
          width=3,
          textInput(inputId="editlastname", label=strong(span(style="color:red;", "*"), "Last name:"), value=table$Lastname)
        ),
        column(
          width=3,
          textInput(inputId="editusername", label=strong(span(style="color:red;", "*"), "Username:"), value=table$Username)
        ),
        column(
          width=3,
          shinyjs::disabled(
            passwordInput(inputId="editpassword", label=strong(span(style="color:red;", "*"), "Password:"), value=table$Password)
          )
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
          width=12,
          actionButton(inputId="Edit_User_Add_Button", label=strong("Update"), class="mybuttons", width="70px"),
          actionButton(inputId="Edit_User_Cancel_Button", label=strong("Cancel"), class="mybuttons", width="70px"),
          actionButton(inputId="Edit_Change_Pwd", label=strong("Change Password?"), class="mybuttons")
        )
      )
    )
  )
}

## Output edit user login message ####
output$EditUserWarningMessage <- renderUI({
  
  req(edituserwarningmsg())
  
  p(style="color:red;", HTML(edituserwarningmsg()))
  
})

# Pop-up for edit ####
observeEvent(input$EditUser, {
  
  row <- input$logintable_rows_selected
  
  if(length(input$logintable_rows_selected) > 0){
    login_table_message("")
    table <- logindata()
    user(table[row,])
    showModal(EditUser(table=table[row,]))
  }else{
    user(NULL)
    login_table_message("Please select a user to make changes")
  }
  
}, ignoreInit = TRUE)

## Observe when remove user yes button is clicked ####
observeEvent(input$Edit_User_Add_Button, {
  
  Firstname=trimws(input$editfirstname);
  Lastname=trimws(input$editlastname);
  Username=trimws(input$editusername);
  Password=trimws(input$editpassword);
  Status="Moderator";

  row <- input$logintable_rows_selected
  login_dat <- data.frame(logindata(), stringsAsFactors=FALSE)
  user <- user()
  
  #print(row); print(login_dat);
  
  if(Firstname=="" | Lastname=="" | Username=="" | Password==""){
    
    edituserwarningmsg("Please fill in the required (*) fields.")
    
  }else{
    
    #Validate to see user exists
    validate_user <- which(Username %in% login_dat$Username[which(!login_dat$Username %in% user$Username)])
    
    if(length(validate_user)){
      
      edituserwarningmsg("This username is already existed. Please enter another username.")

    }else{
      
      login_table_message(paste0("User ", Username, " has been modified. Click 'Save' to keep the changes."))
      edituserwarningmsg("")
      login_dat <- login_dat[-row,] 
      edit_user <- data.frame(Firstname=Firstname, Lastname=Lastname, Username=Username, Password=Password, Status="Moderator")
      user(edit_user)
      login_dat <- login_dat %>% rbind(edit_user)
      logindata(login_dat)
      removeModal()
      
    }
  }
})

## Observe when remove user no button is clicked ####
observeEvent(input$Edit_User_Cancel_Button, {
  
  edituserwarningmsg("")
  row <- input$logintable_rows_selected
  login_dat <- read_csv(paste0("data/User_Login_List.csv"))
  user(login_dat[row,])
  logindata(login_dat)
  removeModal()
  
})

# Functions to add new data for user login #####
ChangePwd <- function() {
  div(
    id="changePwd",
    
    modalDialog(
      size="s", title="Enter New Password", footer=NULL,
      
      fluidRow(
        column(
          width=12,
          textInput(inputId="newpassword", label=strong(span(style="color:red;", "*"), "New Password:"), value=""),
        ),
        column(
          width=12,
          uiOutput("ChangePwdWarningMessage")
        ),       
        column(
          width=12,
          actionButton(inputId="Submit_Pwd", label=strong("Submit"), class="mybuttons"),
          actionButton(inputId="Cancel_Pwd", label=strong("Cancel"), class="mybuttons")
        )
      )
    )
  )
}

## Output add user login message ####
output$ChangePwdWarningMessage <- renderUI({
  
  req(changepwdwarningmsg())
  
  p(style="color:red;", HTML(changepwdwarningmsg()))
  
})

# Pop-up for change password ####
observeEvent(input$Edit_Change_Pwd, {
  
  showModal(ChangePwd())
  
}, ignoreInit = TRUE)

# Pop-up for submit password ####
observeEvent(input$Submit_Pwd, {
  
  Password=trimws(input$newpassword);
  
  if(Password==""){
    
    changepwdwarningmsg("Please fill in the required (*) fields.")
    
  }else{
    
    changepwdwarningmsg("")
    row <- input$logintable_rows_selected
    login_dat <- logindata()
    login_dat$Password[row] <- sodium::password_store(Password)
    user(login_dat[row,])
    showModal(EditUser(table=login_dat[row,]))
    
  }

}, ignoreInit = TRUE)

# Pop-up for cancel password ####
observeEvent(input$Cancel_Pwd, {
  
  changepwdwarningmsg("")
  row <- input$logintable_rows_selected
  table <- logindata()
  user(table[row,])
  showModal(EditUser(table=table[row,]))
  
}, ignoreInit = TRUE)

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
  login_dat <- data.frame(logindata(), stringsAsFactors=FALSE)
  login_table_message(paste0("Username ", login_dat$Username[row], " has been removed. Click 'Save' to implement this changes."))
  logindata(login_dat[-row,])
  removeModal()
  
})

## Observe when remove user no button is clicked ####
observeEvent(input$Remove_User_No_Button, {
  row <- input$logintable_rows_selected
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
  
  data <- read.csv(paste0("data/User_Login_List.csv"), header = TRUE)
  logindata(data)
  removeModal()
  
})

## Login table warning message ####
output$LoginTableMessage <- renderUI({
  
  req(login_table_message())
  
  p(style="color:red;", HTML(login_table_message()))
  
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

##Download gene set collection template#####
output$add_gs_collection_template_gmt <- downloadHandler(
  
  filename = paste0("gs_collection_template.gmt"),
  
  content = function(file){
    file.copy("data/Template/hallmark.gmt", file)
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

##Download gene set collection template#####
output$edit_gs_collection_template_gmt <- downloadHandler(
  
  filename = paste0("gs_collection_template.gmt"),
  
  content = function(file){
    file.copy("data/Template/hallmark.gmt", file)
  },
  
  contentType = "application"
  
)

##About page #####
observeEvent(input$About_Page_Add_Button, {
  
  inputfile <- input$add_about_file;
  
  if(is.null(inputfile)){
    about_file_msg("Please choose a file to import.")
    return(NULL)
  }
  
})

observeEvent({
  input$add_about_file
}, {
  
  inputfile <- input$add_about_file
  
  if(is.null(inputfile)){
    about_file_msg("")
    return(NULL)
  }
  
  tryCatch({
    
    extension <- grep(toupper(".Rmd"), toupper(substr(inputfile$datapath, nchar(inputfile$datapath)-4, nchar(inputfile$datapath))), fixed = TRUE)
    
    if(length(extension) == 0){
      about_file_msg("Incorrect file format. Please check your file again.")
      return(NULL)
    }else{
      about_file_msg("")
      file.copy(from=inputfile$datapath, to=paste0("www/RMD/about_page.Rmd"), overwrite=TRUE)
    }
    
  }, error=function(err){
    about_file_msg("Import failed. Please check your file again.")
    return(NULL)
  }, warning=function(war){
    about_file_msg("Import failed. Please check your file again.")
    return(NULL)
  })
  
})

##Output about page warning message###
output$add_about_file_msg <- renderUI({
  
  req(about_file_msg())
  
  p(class="fileInputMsg",  HTML(about_file_msg()))
  
})

##Download about page template#####
output$add_about_template_rmd <- downloadHandler(
  
  filename = paste0("about_page.Rmd"),
  
  content = function(file){
    file.copy("www/RMD/about_page.Rmd", file)
  },
  
  contentType = "application"
  
)

##contact page #####
observeEvent(input$Contact_Page_Add_Button, {
  
  inputfile <- input$add_contact_file;
  
  if(is.null(inputfile)){
    contact_file_msg("Please choose a file to import.")
    return(NULL)
  }
  
})

observeEvent({
  input$add_contact_file
}, {
  
  inputfile <- input$add_contact_file
  
  if(is.null(inputfile)){
    contact_file_msg("")
    return(NULL)
  }
  
  tryCatch({
    
    extension <- grep(toupper(".Rmd"), toupper(substr(inputfile$datapath, nchar(inputfile$datapath)-4, nchar(inputfile$datapath))), fixed = TRUE)
    
    if(length(extension) == 0){
      contact_file_msg("Incorrect file format. Please check your file again.")
      return(NULL)
    }else{
      contact_file_msg("")
      file.copy(from=inputfile$datapath, to=paste0("www/RMD/contact_page.Rmd"), overwrite=TRUE)
    }
    
  }, error=function(err){
    contact_file_msg("Import failed. Please check your file again.")
    return(NULL)
  }, warning=function(war){
    contact_file_msg("Import failed. Please check your file again.")
    return(NULL)
  })
  
})

##Output contract page warning message###
output$add_contact_file_msg <- renderUI({
  
  req(contact_file_msg())
  
  p(class="fileInputMsg",  HTML(contact_file_msg()))
  
})

##Download contact page template#####
output$add_contact_template_rmd <- downloadHandler(
  
  filename = paste0("contact_page.Rmd"),
  
  content = function(file){
    file.copy("www/RMD/contact_page.Rmd", file)
  },
  
  contentType = "application"
  
)

##Download introduction page 
output$edit_intro_download_file <- downloadHandler(
  
  filename = paste0("Introduction_Page.Rmd"),
  
  content = function(file){
    file.copy(paste0("www/RMD/introduction_", portal()$Portal, ".Rmd"), file)
  },
  
  contentType = "application"
  
)

##Download profile annotation file
output$edit_pro_download_file <- downloadHandler(
  
  filename = paste0("Profile_Annotation.RDS"),
  
  content = function(file){
    file.copy(paste0("data/", portal()$Portal, "/Profile_Annotation.RDS"), file)
  },
  
  contentType = "application"
  
)

##Download gene expression file
output$edit_ge_download_file <- downloadHandler(
  
  filename = paste0("Expression_Set.RDS"),
  
  content = function(file){
    file.copy(paste0("data/", portal()$Portal, "/Expression_Set.RDS"), file)
  },
  
  contentType = "application"
  
)

##Download gs collection file
output$download_gs_collection_file <- downloadHandler(
  
  filename = paste0(portal()$GS_Collection, ".gmt"),
  
  content = function(file){
    file.copy(paste0("data/Enrichment Gene Set/", portal()$GS_Collection, ".gmt"), file)
  },
  
  contentType = "application"
  
)
  
  
  
  
