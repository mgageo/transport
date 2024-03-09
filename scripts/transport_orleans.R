# <!-- coding: utf-8 -->
#
# le réseau de bus d'orleans
#
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
# source("geo/scripts/transport.R");orleans_jour()
orleans_jour <- function() {
  library(tidyverse)
  library(rio)
  carp()
  config_xls('pontivy')
}

# source("geo/scripts/transport.R");dsn <- orleans_shape()
#
orleans_shape <- function(shape_id = "POSB-POSD-alsac-LYH") {
  library(tidyverse)
  carp()
  tt <- tidytransit_lire("gtfs") %>%
    glimpse()
  trips.df <- tt$trips %>%
    filter(shape_id == !! shape_id) %>%
    glimpse()
  df1 <- trips.df %>%
    group_by(route_id, direction_id) %>%
    summarize(nb = n()) %>%
    glimpse()
  stop_times.df <- tt$stop_times %>%
    filter(trip_id %in% trips.df$trip_id) %>%
    filter(stop_sequence == 1) %>%
    group_by(stop_id) %>%
    summarize(nb = n()) %>%
    glimpse()
  df11 <- tidytransit_lire("gtfs_shapes_stops_coord") %>%
    st_drop_geometry() %>%
    filter(shape_id == !! shape_id) %>%
    glimpse()
  misc_print(df11)
  df11 <- tidytransit_lire("gtfs_shapes_stops") %>%
    filter(shape_id == !! shape_id) %>%
    glimpse()
  misc_print(df11)
}