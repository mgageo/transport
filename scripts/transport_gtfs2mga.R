# <!-- coding: utf-8 -->
# le réseau de bus
# utilisation des données d'OpenStreetMap
# auteur : Marc Gauthier
#
# https://github.com/oscarperpinan/spacetime-vis/blob/master/osmar.R
#
#
# en xml2
# https://gist.github.com/nuest/3ed3b0057713eb4f4d75d11bb62f2d66
#
# source("geo/scripts/transport.R");gtfs2mga_jour()
gtfs2mga_jour <- function(reseau = "bibus") {
  library(tidyverse)
  library(rio)
  library(sf)
  carp()
# la conversion en format "mga"
  gtfs2mga_gtfs_lire(reseau = reseau)
}
#
# conversion du gtfs en format interne
#
# gtfs2mga est une liste des différents objets
#
# source("geo/scripts/transport.R");gtfs2mga_gtfs_lire()
gtfs2mga_gtfs_lire <- function(reseau = "star") {
  library(tidyverse)
  carp()
  config_xls(reseau)
  gtfs2mga <- gtfs2mga_lire()
  dsn <- sprintf("%s/gtfs.zip", gtfsDir)
  gtfs2mga$tt <- tidytransit_zip_lire(dsn)
  gtfs2mga$shapes_sf <- tidytransit_gtfs_shapes_sf()
#  glimpse(gtfs2mga); stop('*******')
  gtfs2mga$stops_sf <- tidytransit_gtfs_stops_sf()
  gtfs2mga$shapes_stops <- tidytransit_shapes_stops()
  gtfs2mga$shapes_stops_sf <- tidytransit_shapes_stops_sf(gtfs2mga$shapes_stops)
  gtfs2mga_ecrire(gtfs2mga)
}
#
# conversion de certains fichiers du gtfs en format geojson
# source("geo/scripts/transport.R");gtfs2mga_gtfs_geojson("bibus")
gtfs2mga_gtfs_geojson <- function(reseau='star') {
  library(tidyverse)
  carp()
  config_xls(reseau)
  gtfs2mga_gtfs_shapes_geojson()
  gtfs2mga_gtfs_stops_geojson()
}
gtfs2mga_gtfs_shapes_geojson <- function(dsn = FALSE) {
  library(tidyverse)
  library(sf)
  carp()
  shapes.sf <- tidytransit_shapes_sf_lire() %>%
    glimpse()
  if (dsn == FALSE) {
    dsn <- sprintf("%s/%s/shapes.geojson", webDir,  Config[1, 'reseau'])
  }
  st_write(shapes.sf, dsn, delete_dsn = TRUE)
  carp("dsn: %s", dsn)
}
gtfs2mga_gtfs_stops_geojson <- function() {
  library(tidyverse)
  library(sf)
  carp()
  nc <- tidytransit_shapes_stops_sf()
  dsn <- sprintf("%s/%s/stops.geojson", webDir,  Config[1, 'reseau'])
  st_write(nc, dsn, delete_dsn = TRUE)
  carp("dsn: %s", dsn)
}

# =====================================================================================
#
# fonctions utilitaires
#
gtfs2mga_ecrire <- function(gtfs2mga, rds = "gtfs2mga.Rds") {
  carp()
  dsn <- sprintf("%s/%s", transportDir, rds)
  carp("dsn: %s", dsn)
  saveRDS(gtfs2mga, file = dsn)
}
# source("geo/scripts/transport.R"); gtfs2mga <- gtfs2mga_lire()%>%glimpse()
gtfs2mga_lire <- function(rds='gtfs2mga.Rds') {
  dsn <- sprintf("%s/%s", transportDir, rds)
  carp("dsn: %s", dsn)
  gtfs2mga <- list()
  if(file.exists(dsn)) {
    gtfs2mga <- readRDS(file=dsn)
  }
  return(invisible(gtfs2mga))
}
# source("geo/scripts/transport.R"); gtfs2mga <- gtfs2mga_debug()
gtfs2mga_debug <- function(rds='gtfs2mga.Rds') {
  carp()
  gtfs2mga <- gtfs2mga_lire() %>%
    glimpse()
  tt <- gtfs2mga$tt
  trips.df <- tt$trips %>%
    filter(shape_id == '0009-B-1390-1372') %>%
    dplyr::select(trip_id, shape_id) %>%
    glimpse()
  df <- tt$stop_times %>%
    glimpse() %>%
    filter(trip_id %in% trips.df[1, 'trip_id']) %>%
    dplyr::select(trip_id, stop_id, stop_sequence) %>%
    arrange(stop_sequence) %>%
    group_by(trip_id) %>%
    summarize(stops=paste(stop_id, collapse = ",")) %>%
    View(.)
}