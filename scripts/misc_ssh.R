# <!-- coding: utf-8 -->
#
# les traitements sur un serveur accessible en ssh
#
# https://cran.r-project.org/web/packages/ssh/vignettes/intro.html#Execute_Script_or_Command
#
ssh_user_old <- FALSE

ssh_session <- function(user = "birdnet") {
  library(ssh)
  if (ssh_user_old == user) {
    return(invisible(ssh_con))
  }
#  ssh_disconnect(ssh_con)
  mga_ssh(user)
  a <- sprintf("%s@%s", ssh_user, ssh_host)
  carp("a: %s", a)
  ssh_con <<- ssh_connect(a, passwd = ssh_password, verbose = 1)
  ssh_user_old <<- user
  return(invisible(ssh_con))
}
ssh_disc <- function() {
  library(ssh)
  if(ssh_user_old != FALSE) {
    ssh_disconnect(ssh_con)
  }
}