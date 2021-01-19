# <!-- coding: utf-8 -->
# le réseau de bus
# utilisation des données d'OpenStreetMap
# auteur : Marc Gauthier
#
# https://github.com/oscarperpinan/spacetime-vis/blob/master/osmar.R
#
#
# les traitements journaliers
# source("geo/scripts/transport.R");config_xls('surf');osm_jour( routes = FALSE, stops = TRUE)
osm_jour <- function(routes=TRUE, stops=TRUE) {
  carp()
  if( routes == TRUE ) {
    osm_routes_get()
    osm_routes_save()
    osm.sf <- osm_routes_read() %>%
      glimpse()
  }
  if( stops == TRUE ) {
    osm_stops_get()
    osm_stops_save()
    osm.sf <- osm_stops_read() %>%
      glimpse()
  }
}
# les routes
#
osm_routes_get <- function(fic='relations_route_bus') {
  requete <- sprintf("(relation[network='%s'][type=route][route=bus];>>;);out meta;", config[1, 'network'])
  dsn <- sprintf("%s.osm", fic)
  oapi_requete_get(requete, dsn)
}
# lecture avec sf
# https://github.com/r-spatial/sf/blob/master/tests/read.Rout.save
osm_routes_save <- function(fic='relations_route_bus') {
  library(sf)
  library(tidyverse)
  carp('fic: %s', fic)
  dsn <- sprintf("%s/%s.osm", osmDir, fic)
  osm.sf <- oapi_osmdata_lire_sf(dsn)
  dsn <- sprintf("%s/%s.Rds", transportDir, fic)
  saveRDS(osm.sf, dsn)
  return(invisible(osm.sf))
}
osm_routes_read <- function(fic='relations_route_bus') {
  library(sf)
  library(tidyverse)
  dsn <- sprintf("%s/%s.Rds", transportDir, fic)
  osm.sf <- readRDS(dsn)
  carp('dsn: %s', dsn)
  return(invisible(osm.sf$osm_multilines))
}
#
# les stops
#
osm_stops_get <- function(fic='nodes_stop') {
  requete <- "relation[network='%s'][type=route][route=bus]->.a;node(r.a);out meta;"
  requete <- '
relation[type=route][route=bus][network="%s"]->.a;
(
  node[highway=bus_stop](r.a);
  node[public_transport=platform](r.a);
);
out meta;'
  requete <- sprintf(requete, config[1, 'network'])
  fic <- sprintf("%s.osm", fic)
  oapi_requete_get(requete, fic)
}
# lecture avec sf
# https://github.com/r-spatial/sf/blob/master/tests/read.Rout.save
osm_stops_save <- function(fic='nodes_stop') {
  library(sf)
  library(tidyverse)
  dsn <- sprintf("%s/%s.osm", osmDir, fic)
  nc <- st_read(dsn, layer = "points") %>%
    glimpse()
  osm.sf <- oapi_osmdata_lire_sf(dsn) %>%
    glimpse()
  dsn <- sprintf("%s/%s.Rds", transportDir, fic)
  saveRDS(osm.sf, dsn)
  return(invisible(osm.sf))
}
osm_stops_read <- function(fic='nodes_stop') {
  library(sf)
  library(tidyverse)
  dsn <- sprintf("%s/%s.Rds", transportDir, fic)
  osm.sf <- readRDS(dsn)
  carp('dsn: %s', dsn)
  return(invisible(osm.sf$osm_points))
}