# <!-- coding: utf-8 -->
# le réseau de bus
# utilisation des données d'OpenStreetMap
# auteur : Marc Gauthier
#
# https://github.com/oscarperpinan/spacetime-vis/blob/master/osmar.R
#
#
# les traitements journaliers
# source("geo/scripts/transport.R");config_xls('breizhgo');osm_jour(routemaster = TRUE, route = FALSE, stop = FALSE)
osm_jour <- function(routemaster=TRUE, route=TRUE, stop=TRUE) {
  carp()
  if( routemaster == TRUE ) {
    osm_relations_routemaster_bus_get()
    osm_relations_routemaster_bus_save()
    osm.sf <- osm_relation_sroutemaster_bus_read() %>%
      glimpse()
  }
  if( route == TRUE ) {
    osm_routes_get()
    osm_routes_save()
    osm.sf <- osm_routes_read() %>%
      glimpse()
  }
  if( stop == TRUE ) {
    osm_stops_get()
    osm_stops_save()
    osm.sf <- osm_stops_read() %>%
      glimpse()
  }
}
# les routes
#
osm_relations_route_bus_get <- function(fic='relations_route_bus') {
  requete <- sprintf("(relation[network='%s'][type=route][route=bus];>>;);out meta;", Config[1, 'network'])
  dsn <- sprintf("%s.osm", fic)
  oapi_requete_get(requete, dsn)
}
# lecture avec sf
# https://github.com/r-spatial/sf/blob/master/tests/read.Rout.save
osm_relations_route_bus_save <- function(fic='relations_route_bus') {
  library(sf)
  library(tidyverse)
  carp('fic: %s', fic)
  dsn <- sprintf("%s/%s.osm", osmDir, fic)
  osm.sf <- oapi_osmdata_lire_sf(dsn)
  dsn <- sprintf("%s/%s.Rds", transportDir, fic)
  saveRDS(osm.sf, dsn)
  return(invisible(osm.sf))
}
osm_relations_route_bus_read <- function(fic='relations_route_bus') {
  library(sf)
  library(tidyverse)
  dsn <- sprintf("%s/%s.Rds", transportDir, fic)
  osm.sf <- readRDS(dsn)
  carp('dsn: %s', dsn)
  return(invisible(osm.sf$osm_multilines))
}
# les route_master
#
osm_relations_routemaster_bus_get <- function(fic='relations_routemaster_bus') {
  requete <- sprintf("(relation[network='%s'][type=route_master][route_master=bus];>>;);out meta;", Config[1, 'network'])
  dsn <- sprintf("%s.osm", fic)
  oapi_requete_get(requete, dsn)
}
osm_relations_routemaster_bus_csv <- function(fic='relations_routemaster_bus', force = FALSE) {
  requete <- sprintf('[out:csv(::type,::id,network,name,ref,"ref:network";true;"|")];relation[route_master=bus][network~"%s"];out center;', Config[1, 'network'])
  overpass_query_csv(requete, fic, force = force)
}
# lecture avec sf
# https://github.com/r-spatial/sf/blob/master/tests/read.Rout.save
osm_relations_routemaster_bus_save <- function(fic='relations_routemaster_bus') {
  library(sf)
  library(tidyverse)
  carp('fic: %s', fic)
  dsn <- sprintf("%s/%s.osm", osmDir, fic)
  osm.sf <- oapi_osmdata_lire_sf(dsn)
  dsn <- sprintf("%s/%s.Rds", transportDir, fic)
  saveRDS(osm.sf, dsn)
  return(invisible(osm.sf))
}
osm_relations_routemaster_bus_read <- function(fic='relations_routemaster_bus') {
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
  requete <- "relation[network~'%s'][type=route][route=bus]->.a;node(r.a);out meta;"
  requete <- '
relation[type=route][route=bus][network="%s"]->.a;
(
  node[highway=bus_stop](r.a);
  node[public_transport=platform](r.a);
);
out meta;'
  requete <- sprintf(requete, Config[1, 'network'])
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