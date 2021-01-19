# <!-- coding: utf-8 -->
# le réseau de bus de Lamballe
# utilisation des données opendata
# auteur : Marc Gauthier
#
# source("geo/scripts/transport.R");tibus_gtfs()
tibus_gtfs <- function() {
  library(tidyverse)
  library(rio)
  carp()
}
#
# pour les shapes : la conversion en gpx/geojson
# source("geo/scripts/transport.R");tibus_gtfs_shapes()
tibus_gtfs_shapes <- function(tt) {
  carp()
  config_xls('tibus');txt_gtfs_shapes_sf()
}
#
# pour les stops
# source("geo/scripts/transport.R");tibus_gtfs_stops()
tibus_gtfs_stops <- function() {
  carp()
  config_xls('tibus');txt_gtfs_stops_sf()
}
