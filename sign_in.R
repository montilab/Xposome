
## keep track of who logged in ####
UserLog <- reactiveValues(Logged=FALSE);
UserRole <- reactiveValues(Status=NA);
LogInMessage <- reactiveValues(Msg="");

#sign in page
output$pageStub <- renderUI({
  
  #####<!-- START HOME PAGE -->
  div(
    id="sign-in-page", 
    
    fluidRow(
      class="header-section", style="background: white; padding: 0 0 0 0;",
      
      column(
        width=12,
        uiOutput(outputId = "uiLogin")
      ),
      
      column(
        width=12,
        uiOutput(outputId = "uiMain")
      )
    )
  )#####<!-- END HOME PAGE -->
  
})

##IF USER LOG IS FALSE, THEN SHOW THE LOGIN PANNEL####
output$uiLogin <- renderUI({
  
  req(UserLog$Logged == FALSE)
  
  div(
    class="limiter",
    div(
      class="container-login100",
      
      div(
        class="wrap-login100",
        div(class="login100-form-title", 
            span(class="login100-form-title-1", strong('Sign In'))
        ),
        
        tags$form(
          class="login100-form validate-form",
          div(class="wrap-input100 validate-input m-b-26",
              span(class="label-input100", "Username"),
              HTML(paste0("<input class='input100' id='userName' type='text' name='username' placeholder='Enter username' value='Reina' onkeypress=\"loginfunction(event)\" style=\"width: 100%;\"/>")),
              tags$script('document.getElementById("userName").focus();')
          ),
          
          div(class="wrap-input100 validate-input m-b-18",
              span(class="label-input100", "Password"),
              HTML(paste0("<input class='input100' id='passWord' type='password' name='pass' placeholder='Enter password' value='Chau' onkeypress=\"loginfunction(event)\" style=\"width: 100%;\"/>"))
          ),
          
          div(class="flex-sb-m w-full p-b-30",
              uiOutput(outputId="LogInErrorMessage")
          ),
          
          div(class="container-login100-form-btn",
              actionButton(class="login100-form-btn", inputId="SignInButton", label=strong("Sign In"), onkeypress="loginfunction(event)", width="auto")
          )
        )
      )
    )
  )
})

##OBSERVE THE SIGN IN BUTTON#####
##If user role is user, shows the user app,
##else shows the moderator app
observeEvent(input$SignInButton, {
  
  userName <- input$userName; passWord <- input$passWord;
  
  LogInLog <- read.csv(paste0("data/User_Login_List.csv"), header=TRUE, stringsAsFactors=FALSE)
  
  Log <- LogInLog %>% filter(Username %in% userName, Password %in% passWord)
  
  if(nrow(Log) > 0){
    
    LogInMessage$Msg <- paste0("")
    UserLog$Logged <- TRUE;
    
  }else{
    
    LogInMessage$Msg <- paste0("Incorrect username or password.") 
    UserLog$Logged <- FALSE;
    
  }
  
})

##Show the error message if login info is not correct####
output$LogInErrorMessage <- renderUI({
  
  p(id="LoginMessage", LogInMessage$Msg)
  
})

###IF LOGGED IS TRUE THEN SHOW THE MAIN PAGE#####
output$uiMain <- renderUI({
  
  req(UserLog$Logged == TRUE)
  
  ###<!-- Header top area start -->
  div(class="Main-page", 
      
      ###the header#####
      tags$header(
        class="header-top-area",
        div(class="container",
            div(class="row",
                div(class="col-lg-6 col-md-6 col-sm-12 col-xs-12",
                    div(class="text-md-left",
                        p(class="button-link", "Moderator Page")
                    )
                ),
                div(class="col-lg-6 col-md-6 col-sm-12 col-xs-12",
                    div(class="text-md-right",
                        actionLink(inputId="LogOut", class="button-link", icon=tags$i(class="fa fa-user-circle"), label="Log out")
                    )
                )
            )
        )
      ),
      
      div(class="sidebar-pro",
          div(class="container",
              div(class="row",
                  div(class="col-lg-12 col-md-12 col-sm-12 col-xs-12",
                      DT::dataTableOutput(outputId = "projecttable"),
                      br(),
                      uiOutput("TableMessage"),
                      br(),
                      actionButton(class="MyButtons", inputId="AddButton", label=strong("Add"), width="auto"),
                      actionButton(class="MyButtons", inputId="RemoveButton", label=strong("Remove"), width="auto")
                  )
              )
          )
      )
  ) ##<!-- Header top area end -->
})

## Output table message
table_message <- reactiveVal(NULL)

output$TableMessage <- renderUI({
  
  req(table_message())
  
  p(style="color:red;", table_message())
  
})

# Functions to add new data #####
AddProject <- function() {
  div(
    id = "addData",
    modalDialog(
      fluidRow(
        column(
          width=4,
          textInput(inputId = "Project_Name", label=p(span(style="color:red;", "*"), "Enter project name:"), value=""),
        ),
        column(
          width=4,
          textInput(inputId = "Cell_Line_Name", label=p(span(style="color:red;", "*"), "Enter cell line name:"), value="")
        ),
        column(
          width=4,
          textInput(inputId = "Data_Name", label=p(span(style="color:red;", "*"), "Enter data name:"), value="")
        )
      ),
      fluidRow(
        column(
          width=12,
          uiOutput("WarningMessage")        
        )
      ),
      br(),
      fluidRow(
        column(
          width=4,
          actionButton("Add_Data", label="Add", class="MyButtons")
        )
      ),
      title = "Add Project"
    )
  )
}

# Pop-up for add ####
observeEvent(input$AddButton, {
  
  showModal(AddProject())
  
}, ignoreInit = TRUE)


##CLICKING THE LOG-OUT BUTTON####
observeEvent(input$LogOut, {
  
  ##Change the log status to FALSE
  ##AND go back to the login screen
  UserLog$Logged <- FALSE
  
})

## Observe when sign in button is clicked
observeEvent(input$SignInButton, {
  
  req(UserLog$Logged == TRUE)
  
  projectdata(data)
  
})

## Read in the project list
data <- read_csv(paste0("data/Project_List.csv")) 

## Create reactive values
projectdata <- reactiveVal(NULL)
warningmsg <- reactiveVal(NULL)

## Output the warning message
output$WarningMessage <- renderUI({
  
  req(warningmsg())
  
  p(style="color:red;", warningmsg())
  
})

## Observe when add data is clicked
observeEvent(input$Add_Data, {
  
  proj_dat <- data.frame(projectdata())
  new_proj <- data.frame(Project=trimws(input$Project_Name), `Cell Line`=trimws(input$Cell_Line_Name), Data=trimws(input$Data_Name))

  if(input$Project_Name=="" | input$Cell_Line_Name=="" | input$Data_Name==""){
    warningmsg("Please fill in the required (*) fields.")
  }else{
    
    validate_proj <- proj_dat %>% 
      filter(
        Project %in% trimws(input$Project_Name),
        Cell.Line %in% trimws(input$Cell_Line_Name),
        Data %in% trimws(input$Data_Name)
      )
    
    if(nrow(validate_proj) > 0){
      warningmsg("This project is already existed. Please enter another project name.")
    }else{
      warningmsg("")
      table_message("")
      newproject <- proj_dat %>% rbind(new_proj)
      projectdata(newproject)
      removeModal()
    }
  }
  
})

## Remove project function
RemoveProject <- function(){
  div(
    id = "removeData",
    modalDialog(
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
          actionButton("Yes_Button", label="Yes", class="MyButtons"),
          actionButton("Cancel_Button", label="Cancel", class="MyButtons")
        )
      )
    )
  )
}

## Observe when remove data is clicked
observeEvent(input$RemoveButton, {
  
  if(length(input$projecttable_cell_clicked) > 0){
    table_message("")
    showModal(RemoveProject())
  }else{
    table_message("Please select a project to remove.")
  }
  
})

## Observe when yes button is clicked
observeEvent(input$Yes_Button, {
  
  row <- input$projecttable_cell_clicked$row
  proj_dat <- data.frame(projectdata())
  projectdata(proj_dat[-row,])
  removeModal()
  
})

## Observe when cancle button is clicked
observeEvent(input$Cancel_Button, {
  removeModal()
})

## Output the project table
output$projecttable <- DT::renderDataTable({
  
  req(projectdata())
  
  projectdata()
  
}, escape = FALSE, extensions = 'Buttons', server = TRUE, rownames=FALSE, selection = "single",
options = list(
  columnDefs = list(
    list(className = 'dt-left', targets = 0),
    list(className = 'dt-center', targets = 1:2)
  ),
  scrollX = TRUE, 
  dom = 'T'
))
