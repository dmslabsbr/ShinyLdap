#' A ldap_functions Function
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' ldap_login()

ldap_login <- function(input, output, ui_name,
              ldap.url,
              ldap.dc,
              ldap.filtro,
              ldap.dominio,
              ldap.campos,
              label.user = 'Usuário',
              label.pass = 'Senha',
              label.button.go = 'Login'

) {

  message('R Shiny Ldap function v.: ', '0.0.6')
  message('Ldap.url: ', ldap.url)

  #TODO verificar parametros

  if (1==2) {
    stop ('erro','1=2')
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

  go_click <- observeEvent(input[[ui_actBtn]], {
    temLdap = temComando('ldapsearch')
    cat(file=stderr(), "temldap:", temLdap,'\n')
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
    newC <- sub('ldap_url', ldap_url, comando)
    newC <- sub('ldap_dominio', ldap_dominio,newC)
    newC <- gsub('ldap_user',usuario,newC)
    newC <- sub('ldap_pass',senha,newC)
    newC <- sub('ldap_filtro',ldap_filtro,newC)
    newC <- sub('ldap_dc',ldap_dc,newC)
    cat(file=stderr(),"Comando: ", newC, "\n")
    tmp <- system(paste0(newC),intern = TRUE)
    cat(file=stderr(),"tmp: ", tmp, "\n")
    atributos <- attr(tmp,which = "status")
    cat(file=stderr(),"atributos: ", atributos, "\n")

    if (is.null(atributos)) atributos = 0

    if (atributos == 49) {
      # erro de usuário / senha
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
      dt_usuario <- unique (grep(paste(ldap_campos,collapse="|"), resLdap, value=TRUE))
      dados <- data.frame('id' = separaTxt(dt_usuario)[1],
                          'dados' = separaTxt(dt_usuario)[2])
      return (dados[2])
    } else {
      return (resLdap)
    }
  }





  #build LOGIN UI

  output[[ui_name]] <- shiny::renderUI({
    shiny::fluidPage(
      shiny::titlePanel('Login Shiny'),
      shiny::fluidRow(shiny::textInput(ui_txtUser,label.user,""), #USER
                      shiny::passwordInput(ui_txtPass,label.pass,"")), #PASS
      shiny::actionButton(ui_actBtn, label.button.go), #BTN GO
      shiny::h2( shiny::verbatimTextOutput(ui_txtInfo),
                 shiny::tableOutput('table'))
    )
  })

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



