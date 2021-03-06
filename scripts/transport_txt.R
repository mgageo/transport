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
# source("geo/scripts/transport.R");config_xls('surf');txt_gtfs_stops_sf()
txt_gtfs_stops_sf <- function(force = FALSE) {
  library(tidyverse)
  library(janitor)
  library(sf)
  library(sp)
  library(rgdal)
  library(rgeos)
  dsn <- sprintf("%s/%s.Rds", josmDir, "stops")
  carp("dsn: %s", dsn)
  if (file.exists(dsn) && force == FALSE) {
    nc <- readRDS(dsn)
    return(invisible(nc))
  }
  dsn1 <- sprintf("%s/%s/%s/stops.txt", cfgDir, toupper(config[1, "reseau"]), config[1, "gtfs_dir"])
  carp("dsn1: %s", dsn1)
  df <- rio::import(dsn1, encoding = "UTF-8")
  stops.df <- df %>%
    mutate(lat = as.numeric(stop_lat)) %>%
    mutate(lon = as.numeric(stop_lon)) %>%
    filter(location_type == 0) %>%
    glimpse()
#  plot(stops.sf)
  stops.sf <- st_as_sf(stops.df, coords = c("lon", "lat"), crs = 4326, remove=FALSE)
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

# conversion des shapes d'un gtfs
# source("geo/scripts/transport.R");config_xls('tub');txt_gtfs_shapes_sf()
txt_gtfs_shapes_sf <- function() {
  library(tidyverse)
  library(janitor)
  library(sf)
  library(sp)
  library(rgdal)
  library(rgeos)
  glimpse(config)
  dsn <- sprintf("%s/%s/%s/shapes.txt", cfgDir, toupper(config[1, "reseau"]), config[1, "gtfs_dir"])
  carp("dsn: %s", dsn)
  df <- rio::import(dsn) %>%
    glimpse()
  df <- df %>%
    mutate(lat = as.numeric(shape_pt_lat)) %>%
    mutate(lon = as.numeric(shape_pt_lon)) %>%
    glimpse()
  shapes <- sort(unique(df$shape_id))
  for (shape in shapes) {
    carp("shape: %s", shape)
    df1 <- df %>%
      filter(shape_id == shape)
    txt_gtfs_shape_sf(shape, df1)
  }
}
# conversion des shapes d'un gtfs
#
txt_gtfs_shape_sf <- function(shape, df) {
  m <- as.matrix(df[order(df$shape_pt_sequence), c("lon", "lat")])
  sfc <- sf::st_linestring(m) %>%
    st_sfc(crs = 4326)
  nc <- st_sf(shape_id = shape, geometry = sfc) %>%
    glimpse()
  shape <- gsub("[$:]", "_", shape)
  dsn <- sprintf("%s/shape_%s.geojson", josmDir, shape)
  carp("dsn: %s", dsn)
#  st_write(nc, dsn, delete_dsn = TRUE)
  dsn <- sprintf("%s/shape_%s.gpx", josmDir, shape)
  carp("dsn: %s", dsn)
  spdf <- as_Spatial(nc)
#  writeOGR(spdf, dsn, layer="routes", driver="GPX", dataset_options="GPX_USE_EXTENSIONS=yes", overwrite_layer=TRUE, delete_dsn = TRUE)
#  writeOGR(spdf, dsn, layer="tracks", driver="GPX", dataset_options="GPX_USE_EXTENSIONS=yes", overwrite_layer=TRUE, delete_dsn = TRUE)
  writeOGR(spdf, dsn, layer="tracks", driver="GPX", dataset_options="GPX_USE_EXTENSIONS=yes", "FORCE_GPX_TRACK=true", overwrite_layer=TRUE, delete_dsn = TRUE)

}