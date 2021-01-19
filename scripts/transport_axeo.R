# <!-- coding: utf-8 -->
# le réseau de bus de Guingamp-Paimpol agglomération
# utilisation des données opendata
# auteur : Marc Gauthier
#
# source("geo/scripts/transport.R");axeo_jour()
axeo_jour <- function() {
  carp()
  config_xls('axeo');
  mobibreizh_gtfs_reseau(config[1, "reseau"], config[1, "agency_id"])
}
# source("geo/scripts/transport.R");axeo_stops_diff()
axeo_stops_diff <- function() {
  library(tidyverse)
  library(rio)
  carp()
  config_xls('axeo');
#  agency.df <- mobibreizh_agency_lire('axeo')
  mobi.df <- gtfs_stops_verif() %>%
#    filter(grepl('^axeo', stop_id)) %>%
    glimpse()
  mobi.sf <- st_as_sf(mobi.df, coords=c("lon", "lat"), crs = 4326) %>%
    dplyr::select(stop_name) %>%
    st_transform(2154)
  osm.sf <- axeo_osm_busstop_sf() %>%
#    filter(is.na(name)) %>%
    dplyr::select(name, osm_id) %>%
    st_transform(2154)
  df <- axeo_proches(osm.sf, mobi.sf) %>%
    dplyr::select(-geometry.x, -geometry.y) %>%
    dplyr::select(osm_id, stop_name) %>%
    glimpse()
  dsn <- sprintf("%s/stops_diff.csv", cfgDir);
  export(df, dsn);
  carp('dsn: %s', dsn)
  carp('distance des arrêts mobi')
  df <- axeo_proches(mobi.sf, osm.sf) %>%
    dplyr::select(-geometry.x, -geometry.y) %>%
    glimpse()
  View(df)
}
#
# axeo_proche
axeo_proches <- function(nc1, nc2) {
  library(sf)
  library(tidyverse)
  carp('nc1: %d', nrow(nc1))
  nc2$no <- 1:nrow(nc2)
  nc1$proche <- st_nearest_feature(nc1, nc2)
  df <- dplyr::left_join(nc1 %>% as.data.frame(), nc2 %>% as.data.frame(), by=c('proche'='no')) %>%
    glimpse()
#  stop('***')
  df$d <- st_distance(df$geometry.x, df$geometry.y, by_element = TRUE)
  units(df$d) <- 'm'
  return(invisible(df))
}
# source("geo/scripts/transport.R");axeo_osm_busstop_sf()
axeo_osm_busstop_sf <- function(force = FALSE) {
  carp()
  dsn <- 'network_busstop'
  requete <- sprintf("relation[network='%s']->.a;node(r.a);out meta;", config[1, "network"])
  dsn <- overpass_query(query = requete, fic = dsn, force = force)
  nc <- oapi_sf_lire(fic = dsn)
  return(invisible(nc))
}
axeo_osm_get_xml <- function(force = FALSE) {
  carp()
  dsn <- 'axeo_busstop.osm'
  requete <- "relation[network='axeo']->.a;node(r.a);out meta;"
  oapi_requete_get(requete=requete, fic=dsn, force = force)
  od <- oapi_osmdata_lire_xml(fic=dsn) %>%
    glimpse()
}
# source("geo/scripts/transport.R");axeo_nodes_busstop_valid()
axeo_nodes_busstop_valid <- function(force = TRUE) {
  library(tidyverse)
  library(rio)
  carp()
  config_xls('axeo');
  dsn <- 'network_nodes_busstop_valid'
  requete <- sprintf("relation[network='%s']->.a;node[!'public_transport'](r.a)->.b;(.b;way(bn.b););out meta;", config[1, "network"])
  dsn <- overpass_query(query = requete, fic = dsn, force = force)
  nc <- oapi_sf_lire(fic = dsn)
  return(invisible(nc))
}
#
# source("geo/scripts/transport.R");config_xls('axeo');axeo_stops_diff()
axeo_stops_diff <- function(force = FALSE) {
  library(tidyverse)
  library(janitor)
  gtfs.sf <- txt_gtfs_stops_sf(force = force) %>%
    filter(location_type == 0) %>%
    glimpse()
#  gtfs2osm_stops_create(gtfs.sf);return()
  gtfs.df <- gtfs.sf %>%
    st_drop_geometry()
  osm.sf <- osm_area_busstop_get(force = force)
  k_ref <- config[1, "k_ref"]
  carp("k_ref: %s", k_ref)
  if (k_ref %in% names(osm.sf)) {
    osm.sf <- osm.sf %>%
    dplyr::select(ref = k_ref, name, osm_id) %>%
    mutate(stop_id = as.numeric(str_extract(ref, "\\d+")))
  } else {
    carp("pas de bus_stop")
    osm.sf$ref <- ""
    osm.sf$stop_id <- ""
  }
  gtfs2osm_stops(gtfs.sf, osm.sf)
  return()
  osm.df <- osm.sf %>%
    st_drop_geometry() %>%
    filter(! is.na(`ref:FR:TUB`)) %>%
    dplyr::select(name, `ref:FR:TUB`) %>%
    mutate(stop_id = as.numeric(str_extract(`ref:FR:TUB`, "\\d+"))) %>%
    glimpse()
  df1 <- gtfs.df %>%
    full_join(osm.df, by = c("stop_id"), suffix = c(".gtfs", ".osm")) %>%
    filter(is.na(name)) %>%
    arrange(stop_id) %>%
    glimpse()
  nc1 <- st_join(gtfs.sf, osm.sf, join = st_nearest_feature) %>%
    glimpse()
}