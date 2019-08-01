library(devtools)
usethis::use_package_doc()
devtools::load_all('.')
devtools::document()
devtools::build()
devtools::build(binary = TRUE, args = c('--preclean'))
devtools::install()
devtools::check(cran = TRUE)

library(shinyldap)

# Test with shiny app
shinyldap::shinyLdap_demo()
