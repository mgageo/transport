# <!-- coding: utf-8 -->
#
# le réseau de bus de Ploërmel
#
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
# source("geo/scripts/transport.R");ploermel_jour()
ploermel_jour <- function() {
  carp()
  config_xls('ploermel');
  mobibreizh_gtfs_reseau(Config[1, "reseau"], Config[1, "agency_id"], Config[1, "gtfs_dir"])
  txt_gtfs_stops_sf()
}
#
# source("geo/scripts/transport.R");ploermel_osrm(ref = "12299675")
# source("geo/scripts/transport.R");ploermel_osrm(ref = "12299677")
# source("geo/scripts/transport.R");ploermel_osrm(ref = "12299678")
ploermel_osrm <- function(ref, force = TRUE) {
  carp()
  library(tidyverse)
  library(stringr)
  library(httr)
  library(sp)
  library(rgdal)
  library(rgeos)
  config_xls('ploermel');
#
# par interrogation de la relation avec l'api, détermination des platforms
  polyline1 <- osrm_platforms_polyline(ref = ref, force = force)
  sfc1 <- osrm_polyline_sfc(polyline1)
  plot(sfc1, add = FALSE, col = "blue", lwd = 2)
  res <- osrm_get_json(ref = ref, type = "relation", service = "route", polyline = polyline1, force = TRUE)
  polyline4 <- res$routes[[1]]$geometry
  sfc4 <- osrm_polyline_sfc(polyline4)
  plot(sfc4, add = TRUE, col = "red", lwd = 2)
  ways.df <- osrm_polyline_ways(ref = ref, type = "relation", polyline = polyline1, force = force)
}