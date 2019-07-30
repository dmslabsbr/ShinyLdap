#' A ldap_functions Function
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' ldap_login()

ldap_login <- function(input, output, ui_name, modal = FALSE,
              ldap.url,
              ldap.dc,
              ldap.filtro = 'sAMAccountName', # for AD LDAP Server
              ldap.dominio,
              ldap.campos,
              label.user = 'Usuário',
              label.pass = 'Senha',
              label.button.go = 'Login',
              label.title = 'Shiny LDAP Login'

) {

  message('R Shiny Ldap function v.: ', '0.0.7')
  message('Ldap.url: ', ldap.url)

  #TODO verificar parametros

  if (ldap.url=='') {
    stop ('You need to inform ldap.url like "https://ldap.domain.com:port"')
  }
  if (ldap.dc=='') {
    stop ('You need to inform ldap.dc like "dc=ldapserver,dc=com"')
  }
  if (ldap.filtro=='') {
    stop ('You need to inform ldap.filtro like "sAMAccountName"')
  }
  if (ldap.filtro=='') {
    stop ('You need to inform ldap.dominio like "intranet"')
  }
  if (ldap.campos=='') {
    stop ("You need to inform ldap.campos like c('dn:', 'cn:', 'sn:', 'title:','displayName:', 'name:', 'employeeID:', 'sAMAccountName:'")
  }


  # VARS

  comando <- 'ldapsearch -H ldap_url -x -D "ldap_dominio\\ldap_user" -w ldap_pass -b "ldap_dc" "(&(ldap_filtro=ldap_user))"'

  # prog

  ui_txtUser <- paste0(ui_name, 'txtuser')
  ui_txtPass <- paste0(ui_name, 'txtsenha')
  ui_actBtn <- paste0(ui_name,'_GO')
  ui_txtInfo <- paste0(ui_name,'txtInfo')
  ui_table <- paste0(ui_name,'table')

  result <- shiny::reactiveValues(data = NULL)

  temLdap = temComando('ldapsearch')
  cat(file=stderr(), "have ldap:", temLdap,'\n')
  if (!temLdap) {
    # No LDAPSEARCH COMMAND
    stop ('You need to install LDAP-UTILS (ldapsearch command)')
  }

  if (modal) {
    shiny::showModal(modal_ui)
  }

  go_click <- observeEvent(input[[ui_actBtn]], {

    if (temLdap) {
      result$data <- consultaLdap(input[[ui_txtUser]],input[[ui_txtPass]])
      result$table_data <- userLdap(input[[ui_txtUser]],input[[ui_txtPass]])

    } else {
      result$data <- 'LDAP not found!'
    }
  })


  output[[ui_txtInfo]] <- shiny::renderPrint(result$data);
  output[[ui_table]] <- shiny::renderTable(result$table_data);


  # FUNCTIONS

  #tem comando
  temComando <- function(comando) {
    tmp <- Sys.which(comando)
    return (!(paste0(tmp)==''))
  }

  #consulta LDAP
  consultaLdap <- function(usuario, senha) {
    newC <- sub('ldap_url', ldap.url, comando)
    newC <- sub('ldap_dominio', ldap.dominio,newC)
    newC <- gsub('ldap_user',usuario,newC)
    newC <- sub('ldap_pass',senha,newC)
    newC <- sub('ldap_filtro',ldap.filtro,newC)
    newC <- sub('ldap_dc',ldap.dc,newC)
    cat(file=stderr(),"Comando: ", newC, "\n")
    tmp <- system(paste0(newC),intern = TRUE)
    cat(file=stderr(),"tmp: ", tmp, "\n")
    atributos <- attr(tmp,which = "status")
    cat(file=stderr(),"atributos: ", atributos, "\n")

    if (is.null(atributos)) atributos = 0

    if (atributos == 49) {
      # erro de usuário / senha
      message('invalid user / password')
      res <- 49
    } else {
      res <- tmp;
    }
    return (res)
  }

  #userLdap
  userLdap <- function(usuario, senha) {
    resLdap <- consultaLdap(usuario, senha)
    cat(file=stderr(),"resLdap: ", resLdap, "\n")
    if (!is.numeric(resLdap)) {
      dt_usuario <- unique (grep(paste(ldap.campos,collapse="|"), resLdap, value=TRUE))
      dados <- data.frame('id' = separaTxt(dt_usuario)[1],
                          'dados' = separaTxt(dt_usuario)[2])
      return (dados[2])
    } else {
      return (resLdap)
    }
  }

  # LOGIN UI
  login_ui <- shiny::renderUI({
    shiny::fluidPage(
      shiny::titlePanel(label.title), # 'Login Shiny'
      shiny::fluidRow(shiny::textInput(ui_txtUser,label.user,""), #USER
                      shiny::passwordInput(ui_txtPass,label.pass,"")), #PASS
      shiny::actionButton(ui_actBtn, label.button.go), #BTN GO
      shiny::h2( shiny::verbatimTextOutput(ui_txtInfo),
                 shiny::tableOutput('table'))
    )
  })

  modal_ui <- shiny::modalDialog(title = label.title,
                                 shiny::div(
                                   shiny::textInput(ui_txtUser,label.user,""),
                                   shiny::passwordInput(ui_txtPass,label.pass,"")),
                                 shiny::actionButton(ui_actBtn, label.button.go),
                                 shiny::h2( shiny::verbatimTextOutput(ui_txtInfo),
                                            shiny::tableOutput('table'))
                                 )

  #build LOGIN UI

  output[[ui_name]] <- login_ui


  # for create
  if (1==2) {
    # cmd clear and rebuild
    devtools::document()
    devtools::install()
    usethis::use_package_doc()
    devtools::document()
    devtools::build()
    devtools::build(binary = TRUE, args = c('--preclean'))

  }





  # THE END
  return(result)

}







dogs_over_cats <- function(agree=TRUE){
  if(agree==TRUE){
    print("Woof woof! 2")
  }
  else {
    print("Try again. 2")
  }
}



