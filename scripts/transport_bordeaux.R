# <!-- coding: utf-8 -->
#
# le réseau de bus TBM de Bordeaux
#
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
# source("geo/scripts/transport.R");bordeaux_jour()
bordeaux_jour <- function(reseau = "bordeaux", force = TRUE) {
  library(tidyverse)
  library(rio)
  library(archive)
  carp()
  config_xls(reseau)
}
#
# téléchargement des données en opendata
#
# source("geo/scripts/transport.R");bordeaux_opendata ()
bordeaux_opendata <- function(reseau = "bordeaux", force = TRUE) {
  library(tidyverse)
  library(rio)
  library(archive)
  carp()
  config_xls(reseau)
# https://opendata.bordeaux-metropole.fr/explore/dataset/sv_arret_p/export/
  gtfs_source <- "https://opendata.bordeaux-metropole.fr/api/explore/v2.1/catalog/datasets/sv_arret_p/exports/geojson?lang=fr&timezone=Europe%2FBerlin"
  dsn_source <- sprintf("%s/%s", gtfsDir, "sv_arret_p.geojson")
  carp("dsn_source: %s", dsn_source)
  if (! file.exists(dsn_source)) {
    download.file(gtfs_source, dsn_source)
  }
  gtfs_source <- "https://opendata.bordeaux-metropole.fr/api/explore/v2.1/catalog/datasets/sv_arret_p/exports/csv?lang=fr&timezone=Europe%2FBerlin&use_labels=true&delimiter=%3B"
  dsn_source <- sprintf("%s/%s", gtfsDir, "sv_arret_p.csv")
  carp("dsn_source: %s", dsn_source)
  if (! file.exists(dsn_source)) {
    download.file(gtfs_source, dsn_source)
  }
}
#
# source("geo/scripts/transport.R");bordeaux_arrets_lire()
bordeaux_arrets_lire <- function(reseau = "bordeaux", force = TRUE) {
  library(tidyverse)
  library(sf)
  carp("le fichier geojson")
  config_xls(reseau)
  dsn_source <- sprintf("%s/%s", gtfsDir, "sv_arret_p.geojson")
  carp("dsn_source: %s", dsn_source)
  arrets.sf <- st_read(dsn_source) %>%
    glimpse() %>%
#    head(100) %>%
    dplyr::select(ident, libelle)
  return(invisible(arrets.sf))
}
#
# source("geo/scripts/transport.R");bordeaux_arrets()
bordeaux_arrets <- function(reseau = "bordeaux", force = TRUE) {
  library(tidyverse)
  library(sf)
  carp()
  config_xls(reseau)
  arrets.sf <- bordeaux_arrets_lire()
  arrets.df <- arrets.sf %>%
    st_drop_geometry()
#    dsn_source <- sprintf("%s/%s", gtfsDir, "sv_arret_p.csv")
#  carp("dsn_source: %s", dsn_source)
#  arrets.df <- rio::import(dsn_source) %>%
#    glimpse()
#  stop("****")
#  tidytransit_zip_lire()
  carp("le gtfs")
  tt <- tidytransit_lire()
  stops.df <- tt$stops %>%
    glimpse() %>%
    dplyr::select(stop_code, stop_name, stop_lon, stop_lat)
  df1 <- arrets.df %>%
    left_join(stops.df, by = c("libelle" = "stop_name"), relationship = "many-to-many") %>%
    arrange(libelle) %>%
    glimpse()
  misc_print(head(df1, 10))
  stops.sf <- st_as_sf(stops.df, coords = c("stop_lon", "stop_lat"), crs = 4326, remove = FALSE) %>%
#    head(100) %>%
    glimpse()
  carp("rapprochement plus proche")
#  nc1 <- st_nearest_points(arrets.sf, stops.sf) %>%
#    glimpse()
  nc3 <- bordeaux_proches(stops.sf, arrets.sf)
  dsn <- sprintf("%s/%s", transportDir, "bordeaux_arrets.Rds")
  carp("dsn: %s", dsn)
  saveRDS(nc3, file = dsn)
}
bordeaux_proches <- function(nc1, nc2) {
  n <- st_nn(nc1, nc2, k = 1, progress = TRUE, returnDist = TRUE)
  ids <-  sapply(n[[1]], "[", 1)
  dists <- sapply(n[[2]], "[", 1)
  df3 <- data.frame(nc1, st_drop_geometry(nc2)[ids, , drop = FALSE])
  nc3 <- st_sf(df3)
  nc3$dist <- dists
  return(invisible(nc3))
}
#
# source("geo/scripts/transport.R");bordeaux_arrets_diff()
bordeaux_arrets_diff <- function(reseau = "bordeaux", force = TRUE) {
  library(tidyverse)
  library(nngeo)
  library(sf)
  carp()
  config_xls(reseau)
  dsn <- sprintf("%s/%s", transportDir, "bordeaux_arrets.Rds")
  nc1 <- readRDS(dsn) %>%
    st_drop_geometry()
  nc2 <- nc1 %>%
    filter(dist < 2) %>%
    filter(libelle == stop_name) %>%
    glimpse()
  misc_print(head(nc2, 10))
  nc3 <- nc1 %>%
    filter(dist < 2) %>%
    filter(libelle != stop_name) %>%
    glimpse()
  misc_print(head(nc3, 10))
  nc4 <- nc1 %>%
    filter(dist >= 2) %>%
    filter(libelle == stop_name) %>%
    arrange(-dist) %>%
    glimpse()
  misc_print(head(nc4, 10))
}
#
# source("geo/scripts/transport.R");bordeaux_arrets_overpass_query()
bordeaux_arrets_overpass_query <- function(reseau = "bordeaux", force = TRUE) {
  library(tidyverse)
  library(sf)
  carp()
  config_xls(reseau)
  dsn <- "bordeaux_arrets_overpass"
  requete <- sprintf('
area[name="%s"]->.a;
(
node(area.a)[highway=bus_stop][public_transport=platform]["%s"];
);
out meta;', Config[1, "zone"], Config[1, "k_ref"] )
  dsn <- overpass_query(query = requete, fic = dsn, force = force)
}
#
# source("geo/scripts/transport.R");bordeaux_arrets_overpass_parse(reseau = 'bordeaux', force  = FALSE)
bordeaux_arrets_overpass_parse <-  function(reseau = "bordeaux", force = TRUE) {
  library(tidyverse)
  library(rio)
  library(sf)
  library(osmdata)
  carp()
  config_xls(reseau)
  dsn <- bordeaux_arrets_overpass_query(reseau = reseau, force = force)
  carp("dsn: %s", dsn)
  q <- opq(bbox = c(45, -6, 58, 0))
  od <- osmdata_sf(q, dsn) %>%
    glimpse()
  points.sf <- od$osm_points %>%
    dplyr::select(name, "ref:FR:TBM") %>%
    glimpse()
  Encoding(points.sf$name) <- "UTF-8"
  arrets.sf <- bordeaux_arrets_lire()
  nc3 <- bordeaux_proches(points.sf, arrets.sf) %>%
    glimpse()
  dsn <- sprintf("%s/%s", transportDir, "bordeaux_overpass.Rds")
  carp("dsn: %s", dsn)
  saveRDS(nc3, file = dsn)
}
#
# source("geo/scripts/transport.R");bordeaux_arrets_overpass_diff()
bordeaux_arrets_overpass_diff <- function(reseau = "bordeaux", force = TRUE) {
  library(tidyverse)
  library(nngeo)
  library(sf)
  carp()
  config_xls(reseau)
  dsn <- sprintf("%s/%s", transportDir, "bordeaux_overpass.Rds")
  nc1 <- readRDS(dsn) %>%
    st_drop_geometry()
  nc2 <- nc1 %>%
    filter(ref.FR.TBM != ident) %>%
    glimpse()
  misc_print(head(nc2, 50))
}