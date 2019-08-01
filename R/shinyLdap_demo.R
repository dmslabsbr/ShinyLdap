#' Run a shiny app showing how the ShinyLdap function works.
#'
#' @export
shinyLdap_demo <- function() {
  dir2 <- paste0(find.package('shinyldap'), '/shiny_ldap_demo')  #'/shiny_ldap_demo'
  if (!dir.exists(dir2)) {
    dir2 <- paste0(find.package('shinyldap'), '/inst/shiny_ldap_demo')
  }
  if (!dir.exists(dir2)) {
    dir2 <- paste0(getwd(), '/shiny_ldap_demo')
  }
  message(paste0("Running shiny app from ", dir2))
  shiny::runApp(appDir = dir2)
}
