# app.R
# Shiny LDAP demonstration
# by DMS 2019


# devtools::install_github('dmslabsbr/ShinyLdap', force = TRUE)

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
                'G_MPTV_MEMBROS', 'G_MPTV_Users','title:')  # LDAP FIELDS TO SHOW

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
  message('Callback table_data: ', res$table_data)
  message('Callback data: ', res$data)
  message('Callback BTN: ', res$btn)
  message('Callback ldap: ', res$ldap)
  message('Callback user: ', res$user)
  message('Callback error: ', res$err)

  react.res$data <- res$data
  react.res$table_data <- res$table_data
  react.res$btn <- res$btn
  react.res$ldap <- res$ldap
  react.res$user <- res$user
  react.res$err <- res$err

  ret_msg <- ''

  if (react.res$err == 100) {
    ret_msg <- "Error: Ldap search command not found! Is 'ldap-utils' installed?"
  }
  if (react.res$err == 49) {
    ret_msg <- "Error: Incorrect username or password!"
  }

  return(ret_msg)
}


# server
server <- function(input, output, session) {


  session$onSessionEnded(stopApp)

  react.res$data <- 'react.res@data'

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
                        label.title = 'Shiny LDAP Login',
                        callback.return = ldap.callback.return)


  output$table_res <- renderTable(react.res$table_data);
  output$information <- renderText (react.res$data)
  output$btn <- renderText(react.res$btn)
  output$ldap <- renderText(react.res$ldap)
  output$user <- renderText(react.res$user)
  output$err <- renderText(react.res$err)

}



# ui


ui <- fluidPage(
  h3('R Shiny LDAP Demo'),
  br('LDAP URL: ', secrets.ldap.url),
  uiOutput('ui_login'),hr(),br('Results: '),br(),

  h1( verbatimTextOutput("information"),br(),
      'Button: ', verbatimTextOutput("btn"),
      'Ldap: ', verbatimTextOutput("ldap"),
      'User: ', verbatimTextOutput("user"),
      'Error: ', verbatimTextOutput("err")),
  tableOutput('table_res')
)


# RUN

shinyApp(ui = ui, server = server)