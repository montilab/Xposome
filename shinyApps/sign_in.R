
fluidRow(
  class="sign-in-page", 
  
  column(
    width=12, id="uiSignIn",
    
    div(
      class="login-container",

      div(class="login-form", 
          div(class="login-form-title", strong('Sign In'))
      ),
      
      div(
        class="login-validate-form",
        
        HTML(
          paste0(
            '<div class="form-group shiny-input-container" style="width:auto;">',
              '<i class="fa fa-user" role="presentation" aria-label="user icon"></i>',
              '<label class="control-label login-label" id="username-label" for="username">',
                'User Name',
              '</label>',
              '<input id="username" type="text" onkeypress="loginfunction(event)" class="form-control" value=""/>',
            '</div>'
          )
        ),
        
        br(), 
        
        HTML(
          paste0(
            '<div class="form-group shiny-input-container" style="width:auto;">',
              '<i class="fa fa-lock" role="presentation" aria-label="unlock-alt icon"></i>',
              '<label class="control-label login-label" id="password-label" for="password">',
                'Password',
              '</label>',
              '<input id="password" type="password" onkeypress="loginfunction(event)" class="form-control" value=""/>',
            '</div>'
          )
        ),
        
        br(),
        
        shinyjs::hidden(
          div(
            id = "error",
            tags$p("Invalid username or password!", style="color: red; font-weight: bold; padding-top: 5px;", class="text-center")
          )
        ),
        
        br(),
        
        div(
          style = "text-align: center;",
          actionButton(inputId="sign_in_btn", class="login-btn", label=strong("Sign In"), onkeypress="loginfunction(event)", width="auto")
        ),
        
        br(), br(),
        
        tags$p(
          style="text-align: center",
          actionLink(inputId="ForgetPassword", label=strong("Forgot Password?"), width="auto")
        )
      )
    )
  ),
  
  column(
    width= 12,
    
    shinyjs::hidden(
      div(
        id = "uiModeratorPage",
        uiOutput(outputId = "uiModerator") %>% withSpinner(type=4, color="#0dc5c1")
      )
    )
  )
  
)

