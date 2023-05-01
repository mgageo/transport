# <!-- coding: utf-8 -->
#
# le réseau de bus
#
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
#
# comparaison gtfs osm
#
# source("geo/scripts/transport.R");gtfs2osm_jour()
gtfs2osm_jour <- function(reseau = "bibus") {
  library(tidyverse)
  library(rio)
  library(sf)
  carp()
  config_xls(reseau);
# mise au format interne des fichiers gtfs
  gtfs2mga_jour()
# mise au format interne des fichiers osm
  osm2mga_jour(force=FALSE)
# comparaison
  gtfs2osm_diff()
}
#
# source("geo/scripts/transport.R");gtfs2osm_diff()
gtfs2osm_diff <- function() {
  library(tidyverse)
  library(rio)
  library(sf)
  carp()
  gtfs2mga <- gtfs2mga_lire() %>%
    glimpse()
  osm2mga <- osm2mga_lire() %>%
    glimpse()
  gtfs.df <- gtfs2mga$shapes_stops %>%
    ungroup() %>%
    glimpse()
  osm.df <- osm2mga$relations_stops %>%
    dplyr::rename(ref=`ref:FR:STAR`) %>%
    glimpse()
  df <- full_join(gtfs.df, osm.df, by=c('shape_id'='ref')) %>%
    filter(arrets != stops) %>%
    glimpse()
}
#
# pour les stops
gtfs2osm_stops <- function(gtfs.sf, osm.sf) {
  carp()
  library(sf)
  library(tidyverse)
  library(knitr)
  library(kableExtra)
  gtfs.sf <- gtfs.sf %>%
    st_transform(2154)
  cc <- sf::st_coordinates(gtfs.sf)
  gtfs.sf <- cbind(gtfs.sf, cc) %>%
    glimpse()
  osm.sf <- osm.sf %>%
    st_transform(2154)
  Encoding(osm.sf$name) = "UTF-8"
  cc <- sf::st_coordinates(osm.sf)
  osm.sf <- cbind(osm.sf, cc) %>%
    glimpse()

  df <- dplyr::full_join(gtfs.sf %>%  st_drop_geometry(), osm.sf %>%  st_drop_geometry(), by=c('stop_id'='stop_id')) %>%
    arrange(stop_id) %>%
    glimpse()
  carp("les stops absents d'osm")
  df1 <- df %>%
    filter(! is.na(stop_id)) %>%
    filter(is.na(name))
  print(knitr::kable(df1[, c("stop_id", "stop_name")], format = "pipe"))
  carp("les stops absents du gtfs")
  df2 <- df %>%
    filter(! is.na(ref)) %>%
    filter(is.na(stop_name))
  print(knitr::kable(df2[, c("ref", "stop_id", "name", "osm_id")], format = "pipe"))
  df3 <- df %>%
    filter(! is.na(stop_name)) %>%
    filter(! is.na(name)) %>%
    glimpse()
  carp("différence de nom")
  df4 <- df3 %>%
    filter(stop_name != name) %>%
    glimpse()
  print(knitr::kable(df4[, c("stop_code", "stop_name", "name")], format = "pipe"))
  carp("différence de position")
  df4 <- df3 %>%
    mutate(distance = round(sqrt((X.x - X.y)^2 + (Y.x - Y.y)^2), 0)) %>%
    filter(distance > 50) %>%
    glimpse()
  print(knitr::kable(df4[, c("stop_code", "stop_name", "name", "distance")], format = "pipe"))
#  return()
#
# pour trouver d'éventuels stops osm pour les stops solitaires du gtfs
  carp("les stops gtfs solitaires")
  nc11 <- gtfs.sf %>%
    filter(stop_id %in% df1$stop_id) %>%
    glimpse()
  carp("les stops osm qui ne sont pas de ce réseau")
  nc12 <- osm.sf %>%
    filter(is.na(ref)) %>%
    glimpse()
  df11 <- nc11 %>%
    st_join(nc12, join = st_nearest_feature) %>%
    mutate(distance = round(sqrt((X.x - X.y)^2 + (Y.x - Y.y)^2), 0)) %>%
    mutate(k_ref = Config[1, "k_ref"]) %>%
    mutate(v_ref = sprintf("%s:%s", Config[1, "v_ref"], stop_id.x)) %>%
    st_drop_geometry()
  df12 <- df11 %>%
    filter(distance < 50) %>%
    arrange(name, stop_id.x)
  print(knitr::kable(df12, format = "pipe"))
#
# le fichier de création des nodes osm
#
  df13 <- df11 %>%
    filter(distance >= 50)
  if (nrow(df13) > 0) {
# le template level0
    dsn <- sprintf("%s/transport_level0_node.txt", cfgDir)
    carp("dsn: %s", dsn)
    template <- readLines(dsn)
    osm <- misc_df2tpl(df13, template)
    dsn <- sprintf("%s/transport_level0_node.txt", transportDir)
    carp("dsn: %s", dsn)
    writeLines(osm, dsn)
  }
}
#
# pour les stops
gtfs2osm_stops_create <- function(gtfs.sf) {
  carp()
  library(sf)
  library(tidyverse)
  library(knitr)
  library(kableExtra)
  df1 <- gtfs.sf %>%
    st_drop_geometry() %>%
    mutate(k_ref = Config[1, "k_ref"]) %>%
    mutate(v_ref = sprintf("%s", stop_id))
# le template level0
  dsn <- sprintf("%s/transport_level0_node.txt", cfgDir)
  carp("dsn: %s", dsn)
  template <- readLines(dsn)
  osm <- misc_df2tpl(df1, template)
  dsn <- sprintf("%s/transport_level0_node.txt", transportDir)
  carp("dsn: %s", dsn)
  writeLines(osm, dsn)
}