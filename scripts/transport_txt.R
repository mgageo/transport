# <!-- coding: utf-8 -->
#
# utilisation des données d'OpenStreetMap
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
# auteur : Marc Gauthier
#
#
# fonctions fichiers txt
#
# conversion des stops d'un gtfs
# source("geo/scripts/transport.R");config_xls(Reseau);txt_gtfs_stops_sf(force = TRUE)
txt_gtfs_stops_sf <- function(force = FALSE) {
  library(tidyverse)
  library(janitor)
  library(sf)
  library(sp)
  library(rgdal)
  library(rgeos)
  dsn <- sprintf("%s/%s.Rds", josmDir, "stops_sf")
  carp("dsn: %s", dsn)
  if (file.exists(dsn) && force == FALSE) {
    nc <- readRDS(dsn)
    return(invisible(nc))
  }
  dsn1 <- sprintf("%s/stops.txt", gtfsDir)
  carp("dsn1: %s", dsn1)
  df <- rio::import(dsn1, encoding = "UTF-8")
  stops.df <- df %>%
    mutate(lat = as.numeric(stop_lat)) %>%
    mutate(lon = as.numeric(stop_lon)) %>%
#    filter(location_type == 0) %>%
    glimpse()
  df1 <- df %>%
    filter(parent_station != "") %>%
    mutate(highway = "bus_stop") %>%
    dplyr::select(highway, name = stop_name, latitude = stop_lat, longitude = stop_lon, "ref:QUB" = stop_id) %>%
    glimpse()
  dsn <- sprintf("%s/%s.csv", josmDir, "stops")
  rio::export(df1, dsn)
  stop("****")
#  plot(stops.sf)
  stops.sf <- st_as_sf(stops.df, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  saveRDS(stops.sf, file=dsn)
  dsn <- sprintf("%s/%s.geojson", josmDir, "stops")
  carp("dsn: %s", dsn)
  st_write(stops.sf, dsn, delete_dsn = TRUE)
  dsn <- sprintf("%s/%s.gpx", josmDir, "stops")
  carp("dsn: %s", dsn)
  spdf <- as_Spatial(stops.sf)
  writeOGR(spdf, dsn, layer="stops", driver="GPX", dataset_options="GPX_USE_EXTENSIONS=yes", overwrite_layer=TRUE, delete_dsn = TRUE)
  return(invisible(stops.sf))
}
# conversion des stops d'un gtfs
# source("geo/scripts/transport.R");config_xls('qub');txt_gtfs_stops(force = TRUE)
txt_gtfs_stops <- function(force = FALSE) {
  library(tidyverse)
  library(janitor)
  dsn <- sprintf("%s/%s.Rds", josmDir, "stops")
  carp("dsn: %s", dsn)
  if (file.exists(dsn) && force == FALSE) {
    nc <- readRDS(dsn)
    return(invisible(nc))
  }
  dsn1 <- sprintf("%s/stops.txt", gtfsDir)
  carp("dsn1: %s", dsn1)
  df <- rio::import(dsn1, encoding = "UTF-8")
  stops.df <- df %>%
    glimpse()
  saveRDS(stops.df, file = dsn)
  carp("dsn: %s", dsn)
  df1 <- df %>%
    group_by(stop_id) %>%
    summarize(nb = n()) %>%
    filter(nb > 1) %>%
    glimpse()
  df2 <- stops.df %>%
    filter(stop_id %in% df1$stop_id)
  misc_print(df2)
  return(invisible(stops.df))
}
# conversion des stop_times d'un gtfs
# source("geo/scripts/transport.R");config_xls('concarneau');txt_gtfs_stop_times()
txt_gtfs_stop_times <- function(force = FALSE) {
  library(tidyverse)
  library(janitor)
  dsn <- sprintf("%s/%s.Rds", josmDir, "stop_times")
  carp("dsn: %s", dsn)
  if (file.exists(dsn) && force == FALSE) {
    nc <- readRDS(dsn)
    return(invisible(nc))
  }
  dsn1 <- sprintf("%s/stop_times.txt", gtfsDir)
  carp("dsn1: %s", dsn1)
  df <- rio::import(dsn1, encoding = "UTF-8")
  stops.df <- df %>%
    glimpse()
  saveRDS(stops.df, file = dsn)
  carp("dsn: %s", dsn)
  return(invisible(stops.df))
}
# conversion des shapes d'un gtfs
# source("geo/scripts/transport.R");config_xls('tub');txt_gtfs_shapes_sf()
# source("geo/scripts/transport.R");config_xls('bibus');txt_gtfs_shapes_sf()
# source("geo/scripts/transport.R");config_xls('breizhgo');txt_gtfs_shapes_sf()
# source("geo/scripts/transport.R");config_xls('star');txt_gtfs_shapes_sf()
txt_gtfs_shapes_sf <- function() {
  library(tidyverse)
  library(janitor)
  library(sf)
  library(sp)
  library(rgdal)
  library(rgeos)
  shapes.df <- txt_gtfs_fichier_lire("shapes")
  df <- shapes.df %>%
    mutate(lat = as.numeric(shape_pt_lat)) %>%
    mutate(lon = as.numeric(shape_pt_lon)) %>%
    mutate(shape_id = gsub("\\*", "_", shape_id)) %>%
    glimpse()
  shapes <- sort(unique(df$shape_id)) %>%
    glimpse()
  i <- 0
  for (shape in shapes) {
    i <- i + 1
    carp("i: %s shape: %s", i, shape)
    df1 <- df %>%
      filter(shape_id == shape)
    txt_gtfs_shape_sf(shape, df1)
  }
}
# conversion des shapes d'un gtfs
#
txt_gtfs_shape_sf <- function(shape, df) {
  m <- as.matrix(df[order(df$shape_pt_sequence), c("lon", "lat")])
  gpx <- gtfs_gpx(m, shape);
  dsn <- sprintf("%s/shape_%s.gpx", level0Dir, shape)
  carp("dsn: %s", dsn)
  write(gpx, dsn)
  sfc <- sf::st_linestring(m) %>%
    st_sfc(crs = 4326)
  nc <- st_sf(shape_id = shape, geometry = sfc) %>%
    glimpse()
  shape <- gsub("[$:]", "_", shape)
# la version geojson : josm préfère le gpx !!!
#  dsn <- sprintf("%s/shape_%s.geojson", josmDir, shape)
#  carp("dsn: %s", dsn)
#  st_write(nc, dsn, delete_dsn = TRUE)
  dsn <- sprintf("%s/shape_%s.gpx", josmDir, shape)
  carp("dsn: %s", dsn)
  spdf <- as_Spatial(nc)
#  writeOGR(spdf, dsn, layer="routes", driver="GPX", dataset_options="GPX_USE_EXTENSIONS=yes", overwrite_layer=TRUE, delete_dsn = TRUE)
#  writeOGR(spdf, dsn, layer="tracks", driver="GPX", dataset_options="GPX_USE_EXTENSIONS=yes", overwrite_layer=TRUE, delete_dsn = TRUE)
  writeOGR(spdf, dsn, layer="tracks", driver="GPX", dataset_options="GPX_USE_EXTENSIONS=yes", "FORCE_GPX_TRACK=true", overwrite_layer=TRUE, delete_dsn = TRUE)
}
txt_gtfs_shapes_wiki <- function() {
  library(tidyverse)
  library(janitor)
  library(sf)
  library(sp)
  library(rgdal)
  library(rgeos)
  routes.df <- txt_gtfs_fichier_lire("routes")
  trips.df <- txt_gtfs_fichier_lire("trips")
  shapes.df <- txt_gtfs_fichier_lire("shapes")
  df1 <- trips.df %>%
    left_join(routes.df, by = c("route_id")) %>%
    group_by(route_id, route_short_name, route_long_name, direction_id, shape_id) %>%
    summarize(nb_trips = n()) %>%
    arrange(route_short_name) %>%
    glimpse()
  Encoding(df1$route_long_name) <- 'UTF-8'
  lignes <- knitr::kable(df1, format = "pipe")
  txt <- paste(lignes,  collapse = "\n")
  ctime <- txt_gtfs_shapes_ctime()
  carp("txt: %s", txt)
  txt <- sprintf('==les shapes des routes %s==
<pre>
%s
</pre>', ctime, txt)
  wiki_connect()
  page <- sprintf("User:Mga_geo/Transports_publics/%s/gtfs_shapes", Config["wiki"])
  wiki_page_init(page, txt, force = TRUE)
}
txt_gtfs_fichier_lire <- function(fichier = "routes") {
  library(tidyverse)
  dsn <- sprintf("%s/%s.txt", gtfsDir, fichier )
  carp("dsn: %s", dsn)
  df <- rio::import(dsn) %>%
    glimpse()
  return(invisible(df))
}
# source("geo/scripts/transport.R");txt_gtfs_ctime()
txt_gtfs_shapes_ctime <- function() {
  library(tidyverse)
  library(rio)
  dsn <- sprintf("%s/shapes.txt", gtfsDir)
  ctime <- file.info(dsn)$ctime
  carp("dsn: %s %s", dsn, ctime)
  return(invisible(ctime))
}