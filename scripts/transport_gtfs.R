# <!-- coding: utf-8 -->
#
# le réseau de bus
#
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
#
# source("geo/scripts/transport.R");gtfs_jour()
gtfs_jour <- function() {
  carp()
}
#
# source("geo/scripts/transport.R");gtfs_stops_diff()
gtfs_stops_diff <- function() {
  carp()
  dsn1 <- sprintf("%s/stops.txt", gtfsDir)
  carp("dsn1: %s", dsn1)
  df <- rio::import(dsn1, encoding = "UTF-8")
  stops1.df <- df %>%
    mutate(lat = as.numeric(stop_lat)) %>%
    mutate(lon = as.numeric(stop_lon)) %>%
    glimpse()
  dsn2 <- sprintf("%s/gtfs_20250121/stops.txt", gtfsDir)
  carp("dsn2: %s", dsn2)
  df <- rio::import(dsn2, encoding = "UTF-8")
  stops2.df <- df %>%
    mutate(lat = as.numeric(stop_lat)) %>%
    mutate(lon = as.numeric(stop_lon)) %>%
    glimpse()
  df1 <- stops1.df %>%
    filter(stop_name %in% stops2.df$stop_name) %>%
    glimpse()
}