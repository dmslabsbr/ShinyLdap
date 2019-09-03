# app.R
# Shiny LDAP demonstration
# by DMS 2019


# devtools::install_github('dmslabsbr/ShinyLdap', ref='v.0.24', force = TRUE)

library (shiny)
library (readr)
library (shinyldap)


# VARS

secrets.ldap.url <<- 'ldap://jacarta.,,,....:389'  # LDAP URL:port
secrets.ldap.dc <<- 'dc=ldapserver,dc=com' # LDAP DC
secrets.ldap.filtro <<- 'sAMAccountName'  # use this filter for WINDOWS AD
secrets.ldap.dominio <<- 'intranet' # LDAP DOMAIN
secrets.ldap.campos <<- c('dn:', 'cn:', 'sn:', 'title:','displayName:',
                'name:', 'employeeID:', 'sAMAccountName:', 'mail:',
                'title:')  # LDAP FIELDS TO SHOW

secrets.path <- paste0(find.package('shinyldap'), '/shiny_ldap_demo/secrets.R')
secrets.path.2 <- paste0(getwd(), '/secrets.R')


react.res <<- reactiveValues(data = NULL)


# if exist load secrets.R
message('Trying to load : ', secrets.path)
if (file.exists(secrets.path)) {
  message('loading file: ', secrets.path)
  source(secrets.path, echo=TRUE) # CONFIG FILE WITH PASSWORD
} else if (file.exists(secrets.path.2)) {
  message('loading file: ', secrets.path.2)
  source(secrets.path.2, echo=TRUE) # CONFIG FILE WITH PASSWORD
}

# Callback function

ldap.callback.return <- function(res) {
  message('Callback table_data: ',res$table_data )
  message('* data: ', res$data)
  message('* BTN: ', res$btn)
  message('* ldap: ', res$ldap)
  message('* user: ', res$user)
  message('* error: ', res$stderr)
  message('* err.cod: ', res$err.cod)
  message('* err.msg: ', res$err.msg)
  message('* status: ', res$status)

  react.res$data <- res$data
  react.res$table_data <- res$table_data
  react.res$btn <- res$btn
  react.res$ldap <- res$ldap
  react.res$user <- res$user
  react.res$stderr <- res$stderr
  react.res$err.cod <- unlist(res$err.cod)
  react.res$err.msg <- unlist(res$err.msg)
  react.res$status <- unlist(res$status)
  ret_msg <- 'nda'
  ret_wait <- 0

  if (unlist(res$status) == 100) {
    ret_msg <- "Error: Ldap search command not found! Is 'ldap-utils' installed?"
    #ret_wait <- time2wait + 2
    #time2wait <<- ret_wait
  }
  if (unlist(res$status) == 49) {
    ret_msg <- "Error: Incorrect username or password!"
    ret_wait <- time2wait * 2
    time2wait <<- ret_wait
  }
  message('end callback _: ',ret_msg)

  ret <- list(msg = ret_msg, wait = ret_wait)

  return(ret) # ret_msg
}


# server
server <- function(input, output, session) {


  session$onSessionEnded(stopApp)

  react.res$data <- 'react.res@data'

  time2wait <<- 10     #time to wait after wrong login

  shinyldap::ldap_login(input, output,
                        ui_name = 'ui_login',
                        modal = TRUE,
                        ldap.url = secrets.ldap.url,
                        ldap.dc = secrets.ldap.dc,
                        ldap.filtro = secrets.ldap.filtro,
                        ldap.dominio = secrets.ldap.dominio,
                        ldap.campos = secrets.ldap.campos,
                        label.user = 'User',
                        label.pass = 'Pass',
                        label.button.go = 'Login',
                        label.button.cancel = 'Cancel',
                        label.button.modal = 'Close',
                        label.title = 'Shiny LDAP Login',
                        show.button.cancel = TRUE,
                        show.button.modal = TRUE,
                        msg.list = list(empty = 'These fields cannot be empty.',
                                        time = 'Please! Wait a moment before login again.'),
                        callback.return = ldap.callback.return)


  output$table_res <- renderTable(react.res$table_data);
  output$information <- renderText (react.res$data)
  output$btn <- renderText(react.res$btn)
  output$ldap <- renderText(react.res$ldap)
  output$user <- renderText(react.res$user)
  output$stderr <- renderText(react.res$stderr)
  #
  output$status <- renderText(react.res$status)
  output$err_msg <- renderText(react.res$err.msg)
  output$err_cod <- renderText(react.res$err.cod)
  output$timeout <- renderText(react.res$timeout)
}



# ui


ui <- fluidPage(
  h3('R Shiny LDAP Demo v.1.0.5'),
  br('LDAP URL: ', secrets.ldap.url),
  uiOutput('ui_login'),hr(),br('Results: '),br(),

  h1( 'information:', verbatimTextOutput("information"),br(),
      'Button: ', verbatimTextOutput("btn"),
      'Ldap: ', verbatimTextOutput("ldap"),
      'User: ', verbatimTextOutput("user"),
      'stderr: ', verbatimTextOutput("stderr"),
      'err_msg: ', verbatimTextOutput('err_msg'),
      'err_code: ', verbatimTextOutput('err_cod'),
      'status: ',verbatimTextOutput("status"),
      'timeout: ', verbatimTextOutput("timeout")), h1('Table_Res:'),
  tableOutput('table_res')
)


# RUN

shinyApp(ui = ui, server = server)
