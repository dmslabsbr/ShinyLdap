# GLOBAL.R


# instala os pacotes faltando.
list.of.packages <- c("shiny", "readr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library('shiny')
library('readr')


ldap_url <- 'ldap://jacarta.intranet....:389'
ldap_dc <- 'dc=intranet,dc=mpgo'
ldap_filtro <- 'sAMAccountName'
ldap_dominio <- 'intranet'
ldap_dc <- 'dc=intranet,dc=mpgo'
ldap_campos <- c('dn:', 'cn:', 'sn:', 'title:','displayName:', 'name:', 'employeeID:', 'sAMAccountName:', 'mail:','G_MPTV_MEMBROS', 'G_MPTV_Users','title:' )
comando <- 'ldapsearch -H ldap_url -x -D "ldap_dominio\\ldap_user" -w ldap_pass -b "ldap_dc" "(&(ldap_filtro=ldap_user))"'

debug_mode = 1; # 0 sem debug.

#tem comando
temComando <- function(comando) {
  tmp <- Sys.which(comando)
  return (!(paste0(tmp)==''))
}

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

separaTxt <- function(info) {
  return (c(strsplit(info,': '))[[1]])
}


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