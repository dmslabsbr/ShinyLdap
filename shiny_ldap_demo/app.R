# app.R
# Shiny LDAP demonstration
# by DMS 2019


# devtools::install_github('dmslabsbr/ShinyLdap', force = TRUE)

library (shiny)
library (readr)
library (ShinyLdap)


# VARS

secrets.ldap.url <<- 'ldap://jacarta.,,,....:389'  # LDAP URL:port
secrets.ldap.dc <<- 'dc=ldapserver,dc=com' # LDAP DC
secrets.ldap.filtro <<- 'sAMAccountName'  # use this filter for WINDOWS AD
secrets.ldap.dominio <<- 'intranet' # LDAP DOMAIN
secrets.ldap.campos <<- c('dn:', 'cn:', 'sn:', 'title:','displayName:',
                'name:', 'employeeID:', 'sAMAccountName:', 'mail:',
                'G_MPTV_MEMBROS', 'G_MPTV_Users','title:')  # LDAP FIELDS TO SHOW

secrets.path <- paste0(find.package('ShinyLdap'), '/shiny_ldap_demo/secrets.R')
secrets.path.2 <- paste0(getwd(), '/secrets.R')


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
  message('Result callback: ', res)
  message('Result callback: ', res$data)
}


# server
server <- function(input, output, session) {

  session$onSessionEnded(stopApp)

  ShinyLdap::ldap_login(input, output,
                        ui_name = 'ui_login',
                        modal = TRUE,
                        ldap.url = secrets.ldap.url,
                        ldap.dc = secrets.ldap.dc,
                        ldap.filtro = secrets.ldap.filtro,
                        ldap.dominio = secrets.ldap.dominio,
                        ldap.campos = secrets.ldap.campos,
                        label.user = 'Usuário',
                        label.pass = 'Senha',
                        label.button.go = 'Login',
                        label.button.cancel = 'Cancel',
                        label.title = 'Shiny LDAP Login',
                        callback.return = ldap.callback.return)


}



# ui


ui <- fluidPage(
  shiny::h3('R Shiny LDAP Demo'),
  shiny::br('LDAP URL: ', secrets.ldap.url),
  shiny::uiOutput('ui_login')
)


# RUN

shinyApp(ui = ui, server = server)
