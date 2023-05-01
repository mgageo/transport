# <!-- coding: utf-8 -->
#
# le réseau de bus de Fougères
#
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
#
#
## avec les données GéoBretagne
#
# source("geo/scripts/transport.R");surf_gb_osm_jour()
surf_gb_osm_stops <- function() {
  carp()
  config_xls("surf")
  surf_osm_stops()
  surf_gb_osm_stops()
}
#
# pour les stops
# source("geo/scripts/transport.R");surf_gb_osm_stops()
surf_gb_osm_stops <- function() {
  carp()
  library(sf)
  library(tidyverse)
  library(knitr)
  library(kableExtra)
  config_xls("surf")
  gb.sf <- surf_gb_stops() %>%
    dplyr::select(code, nom, stop_lon, stop_lat) %>%
    st_transform(2154) %>%
    glimpse()
  cc <- sf::st_coordinates(gb.sf)
  gb.sf <- cbind(gb.sf, cc) %>%
    glimpse()
  osm.sf <- osm_stops_read() %>%
    st_transform(2154) %>%
#    glimpse() %>%
    dplyr::select(code = ref.FR.SURF, name, osm_id)
  Encoding(osm.sf$name) = "UTF-8"
  cc <- sf::st_coordinates(osm.sf)
  osm.sf <- cbind(osm.sf, cc) %>%
    glimpse()
  df <- dplyr::full_join(gb.sf %>%  st_drop_geometry(), osm.sf %>%  st_drop_geometry(), by=c('code'='code')) %>%
    arrange(code) %>%
    glimpse()

  df1 <- df %>%
    filter(is.na(name))
  print(knitr::kable(df1, format = "pipe"))
  df2 <- df %>%
    filter(is.na(nom))
  print(knitr::kable(df2, format = "pipe"))
  df3 <- df %>%
    filter(! is.na(nom)) %>%
    filter(! is.na(name)) %>%
    glimpse()
  df4 <<- df3 %>%
    filter(nom != name) %>%
    glimpse()
  print(knitr::kable(df4, format = "pipe"))
  df4 <- df3 %>%
    mutate(distance = round(sqrt((X.x - X.y)^2 + (Y.x - Y.y)^2), 0)) %>%
    filter(distance > 50) %>%
    glimpse()
  print(knitr::kable(df4, format = "pipe"))
  df11 <- gb.sf %>%
    st_join(osm.sf, join = st_nearest_feature) %>%
    mutate(distance = round(sqrt((X.x - X.y)^2 + (Y.x - Y.y)^2), 0)) %>%
    dplyr::select(code.x, nom, code.y, name, osm_id, distance) %>%
    st_drop_geometry() %>%
    arrange(name, code.x)
  print(knitr::kable(df11, format = "pipe"))
}
#
# le shapefile de GéoBretagne
surf_gb_stops <- function() {
  carp()
  library(sf)
  library(tidyverse)

  dsn <- sprintf("%s/GeoBretagne/mobilite_arrets.shp", transportDir)
  nc <- st_read(dsn, stringsAsFactors=FALSE, options = "ENCODING=windows-1252", quiet = TRUE)
  return(invisible(nc))
}
#
# récupération des arrêts osm via les routes
surf_osm_stops <- function() {
  carp()
  library(sf)
  library(tidyverse)
  osm_jour(routes = FALSE, stops = TRUE)
}
#
## avec les données GTFS
#
# source("geo/scripts/transport.R");surf_gtfs()
surf_gtfs <- function() {
  library(tidyverse)
  library(rio)
  carp()
  tt <- gtfs_files_lire()
#  gtfs_shapes_sf(tt$shapes)
#  gtfs_stops_sf(tt$stops)
  surf_gtfs_trips(tt)
}
#
# la version avec tidytransit
surf_gtfs_tt <- function() {
  library(tidyverse)
  library(rio)
  carp()
  dsn <- 'D:/web/geo/TRANSPORT/SURF/GTFS/gtfs-20200827-02.zip'
  tt <- tidytransit_unzip_lire(dsn) %>%
    glimpse()
#  surf_gtfs_routes(tt)
  surf_gtfs_shapes(tt)
#  surf_gtfs_stops(tt)
#  surf_gtfs_trips(tt)
}
#
# pour les routes : le wiki
surf_gtfs_routes <- function(tt) {
  wiki <- gtfs_wiki_routes(tt$routes)
  carp('wiki: %s', wiki)
}
#
# pour les shapes : la conversion en gpx/geojson
surf_gtfs_shapes <- function(tt) {
  carp()
  gtfs_shapes_verif(tt$shapes)
  gtfs_shapes2kml(tt$shapes)
}
#
# pour les stops
surf_gtfs_stops <- function(tt) {
  carp()
#  plot(tt);  return()
  glimpse(tt$stops)
}
#
# pour les trips : le lien route shape
surf_gtfs_trips <- function(tt) {
  carp()
#  plot(tt);  return()
  glimpse(tt$trips)
  ids <- tt$trips %>%
    select(route_id, shape_id) %>%
    distinct() %>%
    arrange(route_id) %>%
    left_join(tt$routes) %>%
    View()
}