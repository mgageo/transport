# <!-- coding: utf-8 -->
#
# le réseau de bus de Fougères
#
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
#
# source("geo/scripts/transport.R");rmat_gtfs()
rmat_gtfs <- function() {
  library(tidyverse)
  library(rio)
  carp()
}
#
# pour les shapes : la conversion en gpx/geojson
# source("geo/scripts/transport.R");rmat_gtfs_shapes()
rmat_gtfs_shapes <- function(tt) {
  carp()
  config_xls('rmat');txt_gtfs_shapes_sf()
}
#
# pour les stops
# source("geo/scripts/transport.R");rmat_gtfs_stops()
rmat_gtfs_stops <- function() {
  carp()
  config_xls('rmat');txt_gtfs_stops_sf()
}
