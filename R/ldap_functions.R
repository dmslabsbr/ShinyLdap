#' Show dtedit version
#'
#' @return version
#'
#' @export
version <- function() {
  res <- '0.0.25b'
  return(res)
}


#' Create a ui for modal shinyldap
#'
#' login_ui - user-interface function
#'
#' Use in conjunction with \code{callModule} and \code{dtedit} to create
#' editable datatables. \code{dteditUI} is used in the 'user interface' component
#' of the shiny app.
#'
#' @param id the namespace of the module
#' @family Datatable Edit functions
#' @example inst/shiny_ldap_demo/app-module.R
#' @export
loginUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("loginUi"))
  )
}




#' A ldap_login function
#'
#' This function allows you to verify a username and password on an LDAP server.
#' @param input Shiny input object passed from the server.
#' @param output Shiny output object passed from the server.
#' @param ui_name the name of the UI output. That is, put `uiOutput(ui_name)` where
#'        you want the Login Dialog in `ui.R`.
#' @param modal boolean (TRUE / FALSE) indicating the form mode.
#' @param ldap.url ldap server url
#' @param ldap.dc ldap server DC
#' @param ldap.filtro 'sAMAccountName', # for AD LDAP Server
#' @param ldap.dominio ldap server domain
#' @param ldap.campos ldap fields to get
#' @param label.user user label
#' @param label.pass = password label
#' @param label.button.go = login button label
#' @param label.button.cancel = cancel button label
#' @param label.button.modal = modal button label
#' @param label.title = Login dialog title
#' @param show.button.cancel = Show the cancel button
#' @param show.button.modal = Show the modal button
#' @param msg.list = Message list
#' @param callback.return a function called when the user click a response button. This function can
#'        return a error message.
#'
#'
#' @keywords ldap
#' @export
#' @examples
#'   shinyldap::ldap_login(input, output,
#'      ui_name = 'ui_login',
#'      modal = TRUE,
#'      ldap.url = secrets.ldap.url,
#'      ldap.dc = secrets.ldap.dc,
#'      ldap.filtro = secrets.ldap.filtro,
#'      ldap.dominio = secrets.ldap.dominio,
#'      ldap.campos = secrets.ldap.campos,
#'      label.user = 'User',
#'      label.pass = 'Password',
#'      label.button.go = 'Login',
#'      label.button.cancel = 'Cancel',
#'      label.button.modal = 'Close',
#'      label.title = 'Shiny LDAP Login',
#'      show.button.cancel = TRUE,
#'      show.button.modal = FALSE,
#'      msg.list = list(empty = 'These fields cannot be empty!', time = 'Please! Wait a moment before login again.' ),
#'      callback.return = ldap.callback.return)

ldap_login <- function(input, output, session,
              ui_name, modal = FALSE,
              ldap.url,
              ldap.dc,
              ldap.filtro = 'sAMAccountName', # for AD LDAP Server
              ldap.dominio,
              ldap.campos,
              label.user = 'User',
              label.pass = 'Password',
              label.button.go = 'Login',
              label.button.cancel = 'Cancel',
              label.button.modal = 'Close',
              label.title = 'Shiny LDAP Login',
              show.button.cancel = TRUE,
              show.button.modal = FALSE,
              msg.list = list(empty = 'These fields cannot be empty!',
                              time = 'Please! Wait a moment before login again.'),
              callback.return = function (result) {}

) {

  message('Ldap.url: ', ldap.url)

  login_space <- 'loginUi'

  mod.active <- shiny::reactiveVal(FALSE)
  mod.timer <- shiny::reactiveVal(10)

  browser()

  #TODO verificar todos os parametros

  if (ldap.url=='') {
    stop ('You need to inform ldap.url like "https://ldap.domain.com:port"')
  }
  if (ldap.dc=='')  {
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

  if (!all(grepl(c(':'), ldap.campos))) {
    stop ('All ldap.campos elements must have ":" in name')
  }

  # VARS

  comando <- 'ldapsearch -H ldap_url -x -D "ldap_dominio\\ldap_user" -w ldap_pass -b "ldap_dc" "(&(ldap_filtro=ldap_user))"'


  # prog

  ui_txtUser <- paste0(ui_name, 'txtuser')
  ui_txtPass <- paste0(ui_name, 'txtsenha')
  ui_actBtn <- paste0(ui_name,'_GO')
  ui_closeBtn <- paste0(ui_name,'_CLOSE')
  ui_txtInfo <- paste0(ui_name,'txtInfo')
  ui_txtclock <- paste0(ui_name,'txtclock')

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

  modal_clock <- function() {

  }


  modal_ui <- function(show.bt = TRUE) {
    # try to isolate
    ns <- session$ns

    #shiny::isolate({
      ui_act <- shiny::actionButton(ns(ui_actBtn), label.button.go)
      ui_clo <- shiny::actionButton(ns(ui_closeBtn), label.button.cancel)
      ui_mod <- shiny::modalButton(label.button.modal)
      ui_u.1 <- shiny::textInput(ns(ui_txtUser),label.user,"")
      ui_u.2 <- shiny::passwordInput(ns(ui_txtPass),label.pass,"")
    #})
    ui_clock <- ''
    col1 <- 7
    col2 <- 5
    if (!show.bt) {
      shiny::isolate({
        ui_act <- ''
        ui_clo <- ''
        ui_mod <- ''
        ui_u.1 <- shiny::h2(msg.list$time, align = 'CENTER')
        col1 <- 12
      })
      #ui_clock <- shiny::column(1, #5
      #                          shiny::h2(
      #                            shiny::textOutput(ui_txtclock), align = 'CENTER', style = "color:red"
      #                          ))
      ui_u.2 <- shiny::h2(shiny::textOutput(ns(ui_txtclock)),
                          align = 'CENTER', style = "color:red")
    }
    if (!show.button.cancel) {ui_clo <- shiny::div()}
    if (!show.button.modal) {ui_mod <- shiny::div()}
    shiny::modalDialog(title = label.title,
                                 shiny::div(
                                   shiny::fluidPage(
                                     shiny::fluidRow(
                                       shiny::column(col1, #7
                                                     ui_u.1,
                                                     ui_u.2),
                                       ui_clock
                                       )
                                       )),
                                 shiny::h2( shiny::verbatimTextOutput(ns(ui_txtInfo)), style = "color:red", ''),
                                 footer = shiny::column(
                                   ui_act,
                                   ui_clo,
                                   ui_mod,
                                   width = 12)
    )
  }


  if (modal) {
    shiny::showModal(modal_ui())
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

    #TODO passwords with space \ " '

    newC <- c('-H',ldap.url,
           '-x', '-D',paste0(usuario,'@', ldap.dominio),
           '-w', senha,
           '-b', ldap.dc,
           paste0('(&(sAMAccountName=', usuario, '))'))

    tmp <- processx::run('ldapsearch', newC, error_on_status = FALSE)
    return (tmp)
  }

  #userLdap
  userLdap <- function(resLdap) {
    cat(file=stderr(),"resLdap: ", resLdap, "\n")
    if (!is.numeric(resLdap)) {
      dados <- unlist(strsplit(resLdap, split = '\n'))
      dt_usuario <- unique (grep(paste(ldap.campos,collapse="|"), dados, value=TRUE))

      message('dt_usuario: ', dt_usuario)

      lista_sep <- separaTxt(dt_usuario)

      message('lista_SEP')
      print(lista_sep)

      return (lista_sep)
    } else {
      return (resLdap)
    }
  }


  errosLdap <- function (erro) {
    lista_err <- c('525','52e','52f','530','531','532','533','568','701','773','775')
    e <- list()
    e$e525 <- 'Entry does not exist.'
    e$e52e <- 'Username is valid but password/credential is invalid.'
    e$e52f -> 'Account Restrictions are preventing this user from signing in.'
    e$e530 -> 'Time Restriction:Entry logon time restriction violation'
    e$e531 <- 'Device Restriction:Entry not allowed to log on to this computer.'
    e$e532 <- 'Password Expiration: Entry password has expired LDAP User-Account-Control Attribute - ERROR_PASSWORD_EXPIRED'
    e$e533 -> 'Administratively Disabled: LDAP User-Account-Control Attribute - ACCOUNTDISABLE'
    e$e568 <- "During a logon attempt, the user's security context accumulated too many security Identifiers. (ie Group-AD)"
    e$e701 <- 'LDAP Password Expiration: User-Account-Control Attribute - ACCOUNTEXPIRED'
    e$e773 -> "Password Expiration: Entry's password must be changed before logging on LDAP pwdLastSet: value of 0 indicates admin-required password change - MUST_CHANGE_PASSWD"
    e$e775 -> 'Intruder Detection:Entry is currently locked out and may not be logged on to LDAP User-Account-Control Attribute - LOCKOUT'
    e$e999 -> "Undefined error!"
    # "Can't contact LDAP server"

    lista2 <- paste0('data ',lista_err,',')

    ret <- list(cod = 'nda', msg = 'nda')

    for (i in lista2) {
      #message('ldap-i: ',i)
      if (any(grep(i,erro))) {
        po <- grep(i,lista2)
        ret$cod <- lista_err[po]
        ret$msg <- unlist(e[paste0('e',ret$cod)])
        message('return errorLdap: ', ret$msg)
        return (ret)
      }
    }
    ret$msg <- e$e999
    ret$cod <- 999
    return (ret)
  }

  go_click <- shiny::observeEvent(input[[ui_actBtn]], {
    message('go_click')
    i.user <- input[[ui_txtUser]]
    i.pass <- input[[ui_txtPass]]
    if ((i.user == '') | (i.pass == '')) {
      output[[ui_txtInfo]] <- shiny::renderPrint(paste0(msg.list$empty));
      return (TRUE)
    }
    result$btn <- 'GO'
    result$user <- i.user
    result$token <- session$token
    shiny::isolate({
      result$err.cod <- 'nda'
      result$err.msg <- 'nda'
    })
    if (temLdap) {
      dadosRaw <- consultaLdap(i.user,i.pass)
      message('status: ',dadosRaw$status)
      message('stdout: ',dadosRaw$stdout)
      message('stderr: ',dadosRaw$stderr)
      result$status <- dadosRaw$status
      result$data <- dadosRaw$stdout
      result$stderr <- dadosRaw$stderr
      result$timeout <- dadosRaw$timeout
      result$table_data <- userLdap(result$data)

      if (dadosRaw$status == 0) {
        result$err.msg <- 'ok'
        result$err.cod <- 0
      } else {
        tmp <- errosLdap(dadosRaw$stderr)
        result$err.msg <- tmp$msg
        result$err.cod <- tmp$cod
      }
    } else {
      result$data <- 'LDAP not found!'
      result$err.cod <- '100'
      result$err.msg <- result$data
      result$status <- '100'
    }
    chama <- callback.return(result)  # token
    chama.msg <- unlist(chama$msg)
    chama.wait <- unlist(chama$wait)
    message("callback_chama: ", chama.msg)
    message("* WAIT: ", chama.wait)
    if (modal) {
      if (chama.msg == 'nda') {
        shiny::removeModal()
        message("removeModal: ", 'modal' )
      } else {
        message("output: ", 'ui_txtInfo' )
        output[[ui_txtInfo]] <- shiny::renderPrint(paste0(chama$msg));
        if (chama.wait>0) {
          mod.timer(chama.wait)
          shiny::showModal(modal_ui(FALSE))
          mod.active(TRUE)
        }
      }
    }
  })


  #time counter
  shiny::observe({
    shiny::invalidateLater(1001, shiny::getDefaultReactiveDomain())  # session
    shiny::isolate({
      if(mod.active()) {
        mod.timer(mod.timer()-1)
        if(mod.timer()<1) {
          mod.active(FALSE)
          if (modal) {
            shiny::showModal(modal_ui())
          }
        }
      }
    })
  })

  if (modal) {
    output[[ui_txtclock]] <- shiny::renderText(format(as.difftime(mod.timer(), units="secs")))
  }

  close_click <- shiny::observeEvent(input[[ui_closeBtn]], {
    message('close_click')
    result$btn <- 'CANCEL'
    result$err.cod <- 0
    result$token <- session$token
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


