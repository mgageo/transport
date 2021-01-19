# <!-- coding: utf-8 -->
# le réseau de bus de Nantes
# utilisation des données opendata
# auteur : Marc Gauthier
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
