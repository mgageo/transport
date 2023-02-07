# <!-- coding: utf-8 -->
# le réseau de bus
# utilisation des données d'OpenStreetMap
# auteur : Marc Gauthier
#
# https://osmose.openstreetmap.fr/en/byuser/mga_geo.rss?
# https://wiki.openstreetmap.org/wiki/FR:Osmose
#
# source("geo/scripts/transport.R");osmose_jour()
#
osmose_jour <- function() {
  carp()
  osm <- osmose_user_lire()
}
# pour lire un fichier osmose
# source("geo/scripts/transport.R");osm <- osmose_user_lire()
osmose_user_lire <- function(user = "mga_geo") {
  library(tidyRSS)
  library(tidyverse)
  url <- sprintf("https://osmose.openstreetmap.fr/en/byuser/%s.rss?", user)
  df <- tidyfeed(url)
  return(invisible(df))
}
