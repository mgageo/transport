# <!-- coding: utf-8 -->
#
# utilisation de level0
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
# auteur : Marc Gauthier
#
#
# pour mettre à jour OpenStreetMap en mode texte
#
level0_session <- FALSE
level0_host <- "http://level0.osmz.ru/"
# source("geo/scripts/transport.R"); level0_connect()
level0_connect <- function(force = TRUE) {
  library(tidyverse)
  library(rvest)
  library(httr)
  if ( class(level0_session) == "rvest_session" && force == FALSE) {
    return(invisible(level0_session))
  }
  url <- level0_host
  carp("url: %s", url)
  level0_session <- rvest::session(url)
  return(invisible(level0_session))
}
# source("geo/scripts/transport.R"); level0_get()
level0_get <- function(type = "relation", id = "4754448", force = TRUE) {
  library(tidyverse)
  library(rvest)
  library(httr)
  level0_session <- level0_connect()
# http://level0.osmz.ru/?url=relation/7086059
  url <- sprintf("%s/?url=%s/%s", level0_host, type, id)
  edit <- level0_session %>%
    session_jump_to(url)
  data <- edit %>%
    html_element("textarea")%>%
    html_text() %>%
    glimpse()
}