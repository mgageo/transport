# <!-- coding: utf-8 -->
# le réseau de bus de Brest
# utilisation des données opendata
# auteur : Marc Gauthier
#
# source("geo/scripts/transport.R");bibus_jour()
bibus_jour <- function() {
  library(tidyverse)
  library(rio)
  carp()
  config_xls('bibus')
# pour les shapes : la conversion en gpx/geojson
  txt_gtfs_shapes_sf()
# pour les stops
  txt_gtfs_stops_sf()
}

