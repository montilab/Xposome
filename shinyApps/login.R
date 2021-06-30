
login <- function(username="", password="") {

  users <- "Username"; pwds <- "Password";
  
  ##Read in the data
  data <- read_csv(paste0("data/User_Login_List.csv"))
  
  # ensure all text columns are character class
  data <- dplyr::mutate_if(data, is.factor, as.character)
  
  # check for match of input username to username column in data
  row_username <- which(dplyr::pull(data, !!users) == trimws(username))
  
  if (length(row_username) > 0) {
    row_password <- dplyr::filter(data,dplyr::row_number() == row_username[1])
    row_password <- dplyr::pull(row_password, !!pwds)
    # create a sodium hash for password
    password_match <- sodium::password_verify(row_password, password)
  } else {
    password_match <- FALSE
  }
  
  # if user name row and password name row are same, credentials are valid
  if (password_match) {
    shinyjs::hide(id = "error")
    credentials$user_auth <- TRUE
    credentials$username <- username
    credentials$password <- password
  } else { # if not valid temporarily show error message to user
    shinyjs::show(id = "error")
    credentials$user_auth <- FALSE
    credentials$username <- ""
    credentials$password <- ""
  }

}

sendpassword <- function(from_sender="montilab@bu.edu", to_recipient="montilab@bu.edu", recipient_first="Montilab", recipient_last="Montilab", recipient_account="Montilab", tmp_pwd){
  
  recipient=paste(recipient_first, recipient_last)
  
  msg <- mime_part(
    paste0(
      '<!DOCTYPE>',
      '<html>',
      '<head>',
      '<meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>',
      '<meta name="viewport" content="width=device-width, initial-scale=1.0"/>',
      '<title>HTML MESSAGE</title>',
      '<style type="text/css">',
      '</style>',
      '</head>',
      '<body>',
      '<p>Hi <strong>', recipient_first, ',</strong></p>',
      '<p>The password for your Xposome account has changed.</p>',
      '<p></p>',
      '<p>Username: <strong>', recipient_account, '</strong></p>',
      '<p>Temporary password: <strong>', tmp_pwd, '</strong></p>',
      '<br>',
      '<p>Log back in? Follow this link, <strong>https://montilab.bu.edu/Xposome/?page=sign_in</strong></p>',
      '<br>',
      '<p>Best,</p>',
      '<p>Montilab Team</p>',
      '</body>',
      '</html>' 
    )
  )
  
  ## Override content type.
  msg[["headers"]][["Content-Type"]] <- "text/html"
  
  from <- paste0("\"Montilab Team\"<", from_sender, ">")
  to <- paste0("\"", recipient, "\"<", to_recipient, ">", collapse="")
  subject <- "Temporary password for Xposome"
  body <- list(msg)
  sendmail(from, to, subject, body, control=list(smtpServer="smtp.bu.edu", smtpPort="25"))
  
}

