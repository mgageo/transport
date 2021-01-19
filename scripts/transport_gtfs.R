# <!-- coding: utf-8 -->
# le réseau de bus
# utilisation des données d'OpenStreetMap
# auteur : Marc Gauthier
#
#
# source("geo/scripts/transport.R");gtfs_jour()
gtfs_jour <- function() {
  carp()
  config_xls('tilt');
  gtfs_files_lire() %>% glimpse()
  gtfs_trips_stops()
}
