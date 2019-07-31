#' A ldap_login function
#'
#' This function allows you to verify a username and password on an LDAP server.
#' @param input Shiny input object passed from the server.
#' @param output Shiny output object passed from the server.
#' @param ui_name the name of the UI output. That is, put \code{uiOutput(ui_name)} where
#'        you want the Login Dialog in \code{ui.R}.
#' @param modal boolean (TRUE / FALSE) indicating the form mode.
#' @param ldap.url --
#' @param ldap.dc --
#' @param ldap.filtro = 'sAMAccountName', # for AD LDAP Server
#' @param ldap.dominio --
#' @param ldap.campos --
#' @param label.user = 'Usuário',
#' @param label.pass = 'Senha',
#' @param label.button.go = 'Login',
#' @param label.button.cancel = 'Cancel',
#' @param label.title = 'Shiny LDAP Login',
#' @param callback.return a function called when the user click a response button. This function can
#'        return a error message.
#'
#'
#' @keywords ldap
#' @export
#' @examples
#'   ShinyLdap::ldap_login(input, output,
#'      ui_name = 'ui_login',
#'      modal = TRUE,
#'      ldap.url = secrets.ldap.url,
#'      ldap.dc = secrets.ldap.dc,
#'      ldap.filtro = secrets.ldap.filtro,
#'      ldap.dominio = secrets.ldap.dominio,
#'      ldap.campos = secrets.ldap.campos,
#'      label.user = 'Usuário',
#'      label.pass = 'Senha',
#'      label.button.go = 'Login',
#'      label.button.cancel = 'Cancel',
#'      label.title = 'Shiny LDAP Login',
#'      callback.return = ldap.callback.return)

ldap_login <- function(input, output, ui_name, modal = FALSE,
              ldap.url,
              ldap.dc,
              ldap.filtro = 'sAMAccountName', # for AD LDAP Server
              ldap.dominio,
              ldap.campos,
              label.user = 'Usuário',
              label.pass = 'Senha',
              label.button.go = 'Login',
              label.button.cancel = 'Cancel',
              label.title = 'Shiny LDAP Login',
              callback.return = function (result) {}

) {

  message('* R Shiny Ldap function v.: ', '0.0.20a', ' - - - - ', Sys.time(), ' - - - -')
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
  if (ldap.dominio =='') {
    stop ('You need to inform ldap.dominio like "intranet"')
  }
  if (length(ldap.campos) == 0) {
    stop ("You need to inform ldap.campos like c('dn:', 'cn:', 'sn:', 'title:','displayName:', 'name:', 'employeeID:', 'sAMAccountName:'")
  }


  # VARS

  comando <- 'ldapsearch -H ldap_url -x -D "ldap_dominio\\ldap_user" -w ldap_pass -b "ldap_dc" "(&(ldap_filtro=ldap_user))"'

  # prog

  ui_txtUser <- paste0(ui_name, 'txtuser')
  ui_txtPass <- paste0(ui_name, 'txtsenha')
  ui_actBtn <- paste0(ui_name,'_GO')
  ui_closeBtn <- paste0(ui_name,'_CLOSE')
  ui_txtInfo <- paste0(ui_name,'txtInfo')

  result <- list()
  result$ldap <- FALSE
  result$btn <-''
  result$table_data <-''
  result$user <- ''
  result$err <- FALSE


  temLdap = temComando('ldapsearch')
  result$ldap <- temLdap
  cat(file=stderr(), "ldapsearch: ", temLdap,'\n')
  if (!temLdap) {
    # No LDAPSEARCH COMMAND
    # stop ('You need to install LDAP-UTILS (ldapsearch command)')
    # just test

  }

  modal_ui <- shiny::modalDialog(title = label.title,
                                 shiny::div(
                                   shiny::textInput(ui_txtUser,label.user,""),
                                   shiny::passwordInput(ui_txtPass,label.pass,"")),
                                 shiny::h2( shiny::verbatimTextOutput(ui_txtInfo), ''),
                                 footer = shiny::column(
                                   shiny::actionButton(ui_actBtn, label.button.go),
                                   shiny::actionButton(ui_closeBtn, label.button.cancel),
                                   width = 12) # cancel
  )

  if (modal) {
    shiny::showModal(modal_ui)
  }

  # functions

  #consulta LDAP
  consultaLdap <- function(usuario, senha) {
    newC <- sub('ldap_url', ldap.url, comando)
    newC <- sub('ldap_dominio', ldap.dominio,newC)
    newC <- gsub('ldap_user',usuario,newC)
    newC <- sub('ldap_pass',senha,newC)
    newC <- sub('ldap_filtro',ldap.filtro,newC)
    newC <- sub('ldap_dc',ldap.dc,newC)
    cmd_nopass <- sub(senha,'******',newC)
    cat(file=stderr(),"Comando: ", cmd_nopass, "\n")
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
  userLdap <- function(resLdap) {
    cat(file=stderr(),"resLdap: ", resLdap, "\n")
    if (!is.numeric(resLdap)) {
      dt_usuario <- unique (grep(paste(ldap.campos,collapse="|"), resLdap, value=TRUE))

      message('dt_usuário: ', dt_usuario)

      lista_sep <- separaTxt(dt_usuario)

      message('lista_SEP')
      print(lista_sep)

      return (lista_sep)
    } else {
      return (resLdap)
    }
  }



  go_click <- shiny::observeEvent(input[[ui_actBtn]], {
    message('go_click')
    result$btn <- 'GO'
    result$user <- input[[ui_txtUser]]
    if (temLdap) {
      result$data <- consultaLdap(input[[ui_txtUser]],input[[ui_txtPass]])
      result$table_data <- userLdap(result$data)

      if (is.numeric(result$data)) {
        result$err <- result$data
      }
    } else {
      result$data <- 'LDAP not found!'
      result$err <- '100'
    }
    chama <- callback.return(result)
    message("callback_chama: ", chama )
    if (modal) {
      if (chama == '') {
        shiny::removeModal()
      } else {
        output[[ui_txtInfo]] <- shiny::renderPrint(paste0(chama));
      }
    }
  })


  close_click <- shiny::observeEvent(input[[ui_closeBtn]], {
    message('close_click')
    result$btn <- 'CANCEL'
    chama <- callback.return(result)
    if (modal) {shiny::removeModal()}
  })




  # LOGIN UI
  login_ui <- shiny::renderUI({
    message('login_ui')
    shiny::fluidPage(
      shiny::titlePanel(label.title), # 'Login Shiny'
      shiny::fluidRow(shiny::textInput(ui_txtUser,label.user,""), #USER
                      shiny::passwordInput(ui_txtPass,label.pass,"")), #PASS
      shiny::actionButton(ui_actBtn, label.button.go), #BTN GO
      shiny::actionButton(ui_closeBtn, label.button.cancel),
      shiny::h2( shiny::verbatimTextOutput(ui_txtInfo),
                 '')
    )
  })

  if (modal) {
    login_ui <- shiny::renderUI('')
  }

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

# functions extenal

#tem comando
temComando <- function(comando) {
  tmp <- Sys.which(comando)
  return (!(paste0(tmp)==''))
}
# split txt
separaTxt <- function(info) {
  lst <- list()
  for (i in info) {
    elem <- strsplit(i,': ')[[1]]
    lst[elem[1]] <- elem[2]
  }
  ret <- lst
  return (ret)
}


