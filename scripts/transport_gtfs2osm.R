# <!-- coding: utf-8 -->
#
# le réseau de bus
#
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
#
# préparation des données gtfs pour comparaison avec osm
#
# source("geo/scripts/transport.R");gtfs2osm_jour()
gtfs2osm_jour <- function(reseau = Reseau, force = TRUE) {
  library(tidyverse)
  library(rio)
  library(sf)
  carp()
  config_xls(reseau);
# mise au format interne des fichiers gtfs pour les routes
  gtfs2osm_routes_stops(force = force)
#  gtfs2osm_routes_wiki("gtfs2osm_routes_stops")
  if( Config[1, "shapes"] == "TRUE" ) {
    gtfs2osm_routes_shapes_stops(force = force)
    gtfs2osm_routes_wiki("gtfs2osm_routes_shapes_stops")
  }
}
gtfs2osm_jour_v1 <- function(reseau = "bibus") {
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
#
# la conversion
# source("geo/scripts/transport.R");gtfs2osm_routes_stops()
gtfs2osm_routes_stops <- function(rds = "gtfs2osm_routes_stops", force_osm = TRUE) {
  library(tidyverse)
  library(tidytransit)
  carp()
  df1 <- tidytransit_lire(rds = "tidytransit_routes_stops") %>%
    group_by(ref_network) %>%
    arrange(desc(nb)) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    glimpse()
  df2 <- gtfs2osm_routes_osm(df1, force_osm = force_osm) %>%
    select(order(colnames(.))) %>%
    glimpse()
  df2 <- df2 %>%
    dplyr::select(colour, description, from, name, network, operator,`public_transport:version`
      ,ref, ref_network, shape_id, route_desc, route, text_colour, to, type, stops, stops_code, stop_names = names
      ,stops_osm_id, stops_osm_name) %>%
    glimpse()
  tidytransit_sauve(df2, rds)
  dsn <- sprintf("%s/%s.csv", transportDir, rds)
  readr::write_tsv(df2, file = dsn)
  carp("dsn: %s", dsn)
}
#
# la conversion
# source("geo/scripts/transport.R");gtfs2osm_routes_shapes_stops()
gtfs2osm_routes_shapes_stops <- function(rds = "gtfs2osm_routes_shapes_stops", force_osm = TRUE) {
  library(tidyverse)
  library(tidytransit)
  carp()
  df1 <- tidytransit_lire(rds = "tidytransit_routes_stops") %>%
    group_by(ref_network, shape_id) %>%
    arrange(desc(nb), desc(nb_stops)) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    arrange(ref_network) %>%
    glimpse()
  df2 <- df1 %>%
    dplyr::select(ref_network, shape_id, route_desc, nb, nb_stops) %>%
    arrange(ref_network, desc(nb), desc(nb_stops))
  misc_print(df2)
  df3 <- df1 %>%
    group_by(ref_network) %>%
    arrange(desc(nb), desc(nb_stops)) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    arrange(ref_network)
  df4 <- df3 %>%
    dplyr::select(ref_network, shape_id, route_desc, nb, nb_stops) %>%
    filter(ref_network == "C4-A") %>%
    arrange(ref_network, desc(nb), desc(nb_stops))
  misc_print(df4)
#  stop("*****")
  df4 <- gtfs2osm_routes_osm(df1, force_osm = force_osm)
  df4 <- df4 %>%
    dplyr::select(colour, description, from, name, network, operator,`public_transport:version`
      ,ref, ref_network, shape_id, route_desc, route, text_colour, to, type, stops, stops_code, stop_names = names
      ,stops_osm_id, stops_osm_name
    ) %>%
    select(order(colnames(.))) %>%
    glimpse()
  tidytransit_sauve(df4, rds)
  dsn <- sprintf("%s/%s.csv", transportDir, rds)
  readr::write_tsv(df4, file = dsn)
  carp("dsn: %s", dsn)
}
gtfs2osm_routes_osm <- function(df1, force_osm) {
  library(tidyverse)
  library(tidytransit)
  library(stringr)
  carp()
  df2 <- df1 %>%
    mutate(Route_color = sprintf("#%s", toupper(route_color))) %>%
    mutate(Route_text_color = sprintf("#%s", toupper(route_text_color))) %>%
    mutate(Route_color = ifelse(grepl("^#[0-9A-F]{6}$", Route_color), Route_color, "#000000")) %>%
    mutate(Route_text_color = ifelse(grepl("^#[0-9A-F]{6}$", Route_text_color), Route_text_color, "#FFFFFF")) %>%
    mutate(from = str_glue('{first_city} ({first_name})')) %>%
    mutate(to = str_glue('{last_city} ({last_name})')) %>%
    mutate(from_city = ifelse(str_detect(first_name, first_city), first_name, str_glue('{first_city} ({first_name})'))) %>%
    mutate(to_city = ifelse(str_detect(last_name, last_city), last_name, str_glue('{last_city} ({last_name})'))) %>%
    mutate(name = str_glue('{Config_route_name} {route_short_name} : {first_name} -> {last_name}')) %>%
    mutate(description = str_glue('Ligne {route_short_name} : {first_city} ({first_name}) -> {last_city} ({last_name})')) %>%
    rename(colour = Route_color) %>%
    mutate(ref = route_short_name) %>%
    rename(text_colour = Route_text_color) %>%
    mutate(network = Config_network) %>%
    mutate(operator = Config_operator) %>%
    mutate(`public_transport:version` = "2") %>%
    mutate(route = "bus") %>%
    mutate(type = "route") %>%
    glimpse()

  if (Config_city == "Bordeaux") {
    carp("Config_city == Bordeaux")
    df2 <- df2 %>%
      mutate(from = ifelse(str_detect(first_name, first_city), first_name, str_glue('{first_city} ({first_name})'))) %>%
      mutate(to = ifelse(str_detect(last_name, last_city), last_name, str_glue('{last_city} ({last_name})'))) %>%
      mutate(name = str_glue('{Config_route_name} {route_long_name} : {from} -> {to}')) %>%
#      filter(ref == "87") %>%
      glimpse()
#    stop("***")
  }
  if (Config_city == "STAR") {
    carp("Config_city == STAR")
    df2 <- df2 %>%
      mutate(name = str_glue('{Config_route_name} {route_short_name} Direction {to_city}')) %>%
      glimpse()
  }
  if (grepl("breizhgo", Config_reseau)) {
    df2 <- df2 %>%
      mutate(name = str_glue('{Config_route_name} {route_long_name} : {first} -> {last}')) %>%
      glimpse()
  }
#
# pour avoir les id osm des stops
  osm_stops.df <- overpass_get(query = "bus_stop_kref", format = "csv", force = force_osm) %>%
    rename(k_ref = Config[1, 'k_ref']) %>%
    mutate(k_ref = sprintf("%s", k_ref)) %>%
    clean_names()
  stops_zone <- "stops"
  if ( Reseau == "bordeaux") {
    stops_zone <- "stops_code"
  }
  for (i2 in 1:nrow(df2)) {
    stops <- df2[[i2, stops_zone]]
    stops.df <- data.frame(stop = unlist(strsplit(stops, ";"))) %>%
      left_join(osm_stops.df, by = c("stop" = "k_ref"))
    df3 <- stops.df %>%
      filter(is.na(id))
    if( nrow(df3) > 0) {
      glimpse(df3)
      glimpse(df2[i2, ])
      confess("***** stop inconnu nb: %s", nrow(df3))
    }
    df2[[i2, "stops_osm_id"]] <- paste0(stops.df$id, collapse = ",")
    df2[[i2, "stops_osm_name"]] <- paste0(stops.df$name, collapse = ",")
  }
  glimpse(df2)
  return(invisible(df2))
}
#
# la production du fichier pour le wiki
# source("geo/scripts/transport.R");gtfs2osm_routes_wiki()
gtfs2osm_routes_wiki <- function(rds = "gtfs2osm_routes_stops") {
  library(tidyverse)
  carp()
  dsn <- sprintf("%s/gtfs.zip", gtfsDir)
  carp("dsn: %s", dsn)
  gtfs <- strftime(file.info(dsn)$mtime, format="%d.%m.%Y")
  df1 <- tidytransit_lire(rds = rds) %>%
    glimpse() %>%
    dplyr::select(ref_network, name, from, to, stop_names) %>%
    glimpse()
  wiki <- sprintf('==Les routes gtfs avec les shapes %s==
{|class="wikitable sortable"
|-', gtfs)
  for (col in colnames(df1)) {
    wiki <-  stringr::str_glue('{wiki}
!scope="col" class="sortable"| {col}')
  }
  for (i1 in 1:nrow(df1)) {
    wiki <-  stringr::str_glue('{wiki}
|-
')
    for (col in colnames(df1)) {
      val <- df1[[i1, col]]
      wiki <-  stringr::str_glue('{wiki}
| {val}
')
    }
  }
  wiki <- stringr::str_glue('{wiki}
|}')
#  print(wiki)
  page <- sprintf("User:Mga_geo/Transports_publics/%s/%s", Config["wiki"], rds) %>%
    glimpse()
  wiki_page_init(page = page, article = wiki, force = TRUE)
  return(invisible())
}