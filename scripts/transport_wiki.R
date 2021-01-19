# <!-- coding: utf-8 -->
#
# utilisation des données d'OpenStreetMap
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
# auteur : Marc Gauthier
#
#
# pour mettre à jour le wiki
#
# https://medium.com/@kyleake/wikipedia-data-scraping-with-r-rvest-in-action-3c419db9af2d
# https://awesomeopensource.com/project/yusuzech/r-web-scraping-cheat-sheet#rvest7.3
# https://github.com/yusuzech/r-web-scraping-cheat-sheet/blob/master/README.md#rvest3
#
# source("geo/scripts/transport.R");config_xls('surf');wiki_init()
wiki_init <- function(force = FALSE) {
  library(tidyverse)
  library(rvest)
  host <- 'wiki.openstreetmap.org';
  url <- sprintf("https://%s/w/index.php?title=Special:UserLogin", host)
  page <- "User:Mga_geo/Transports_publics/Saint-Brieuc/route"
  session <- html_session(url)
  unfilled_forms <- html_form(session)
  login_form <- unfilled_forms[[1]]
  username <- mes_options("osm_wiki_username")
  password <- mes_options("osm_wiki_password")
#  carp("%s/%s", username, password)
  form <- set_values(login_form, wpName=username, wpPassword=password)
  session2 <- submit_form(session, form)
  session2 %>%
    html_form()
# https://wiki.openstreetmap.org/w/index.php?title=User:Mga_geo/Transports_publics/Saint-Brieuc/route&action=edit
  url <- sprintf("https://wiki.openstreetmap.org/w/index.php?title=%s&action=edit", page);
  carp("url: %s", url)
#  return()
  edit <- session2 %>%
    jump_to(url)
  edit_forms <- edit %>%
    html_form()
  edit_form <- edit_forms[[1]]
  form <- set_values(edit_form, wpTextbox1="titi")
  session3 <- submit_form(edit, form)
  html <- content(session3$response,as = "text")
  dsn <- sprintf("%s/wiki.html", transportDir)
  write(html, file = dsn, append = FALSE)
  carp("dsn: %s", dsn)
}
