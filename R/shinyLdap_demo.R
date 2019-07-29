#' Run a shiny app showing how the ShinyLdap function works.
#'
#' @export
shinyLdap_demo <- function() {
  dir <- paste0(find.package('shinyLdap'), '/')  #'/shiny_demo'
  message(paste0("Running shiny app from ", dir))
  shiny::runApp(appDir = dir)
}
