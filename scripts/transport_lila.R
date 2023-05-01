# <!-- coding: utf-8 -->
#
# le réseau de bus de Nantes
#
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
# source("geo/scripts/transport.R");lila_gtfs_jour()
lila_gtfs_jour <- function() {
  library(tidyverse)
  carp()
  config_xls('star')
  odDir <- sprintf("%s/web/geo/TRANSPORT/LILA", Drive)
  dsn <- 'D:/web/geo/TRANSPORT/LILA/LILA.gtfs.zip'
  webDir <- odDir
#  tt <- tidytransit_zip_lire(dsn)
  tidytransit_gtfs_shapes_sf()
#  tidytransit_gtfs_stops_sf()
  dsn <- sprintf("%s/shapes.geojson", webDir)
  transport_gtfs_shapes_geojson(dsn)
#  transport_gtfs_stops_geojson()
}
