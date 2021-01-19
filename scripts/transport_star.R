# <!-- coding: utf-8 -->
# le réseau de bus de Rennes
# utilisation des données opendata
# auteur : Marc Gauthier
#
# source("geo/scripts/transport.R");star_jour()
star_jour <- function() {
  library(tidyverse)
  library(rio)
  library(sf)
  carp()
# la conversion en sf des shapes
  star_gtfs_jour()
# la conversion en sf des routes
  star_osm_jour()
}
# source("geo/scripts/transport.R");star_gtfs_jour()
star_gtfs_jour <- function() {
  library(tidyverse)
  carp()
  config_xls('star')
  dsn <- 'D:/web/geo/TRANSPORT/STAR/GTFS/gtfs.zip'
#  tt <- tidytransit_zip_lire(dsn)
#  tidytransit_gtfs_shapes_sf()
#  tidytransit_gtfs_stops_sf()
  transport_gtfs_shapes_geojson()
  transport_gtfs_stops_geojson()
}
#
# la comparaison des deux sources
#
# pour les routes
# source("geo/scripts/transport.R");star_osm_routes_gtfs_shapes()
star_osm_routes_gtfs_shapes <- function() {
  carp()
  transport_osm_routes_gtfs_shapes_df()
  transport_osm_routes_gtfs_shapes_diff()
}
transport_gtfs_shapes_geojson <- function(dsn=FALSE) {
  library(tidyverse)
  library(sf)
  carp()
  shapes.sf <- tidytransit_shapes_sf_lire() %>%
    glimpse()
  if(dsn==FALSE) {
    dsn <- sprintf("%s/%s/shapes.geojson", webDir,  config[1, 'reseau'])
  }
  st_write(shapes.sf, dsn, delete_dsn=TRUE)
}
transport_gtfs_stops_geojson <- function() {
  library(tidyverse)
  library(sf)
  carp()
  nc <- tidytransit_shapes_stops_sf()
  dsn <- sprintf("%s/%s/stops.geojson", webDir,  config[1, 'reseau'])
  st_write(nc, dsn, delete_dsn=TRUE)
}
transport_osm_routes_gtfs_shapes_df <- function() {
  library(tidyverse)
  library(sf)
  carp()
  shapes.sf <- tidytransit_shapes_sf_lire() %>%
    mutate(source='shapes') %>%
    glimpse()
  routes.sf <- osm_routes_read() %>%
    dplyr::select(shape_id=ref.FR.STAR) %>%
    filter(! is.na(shape_id)) %>%
    glimpse()
  names(routes.sf$geometry)=NULL
  names(shapes.sf$geometry)=NULL

# avec la géométrie
  carp('jointure')
  df <- dplyr::left_join(routes.sf %>% as.data.frame(), shapes.sf %>% as.data.frame(), by=c('shape_id'='shape_id')) %>%
    glimpse()
#
# le tracé gtfs est inclus dans le buffer route osm
  carp('passage en Lambert 93 et buffer')
  nc1 <- st_transform(routes.sf, 2154) %>%
    st_buffer(50)
  nc2 <- st_transform(shapes.sf, 2154)
  df <- dplyr::left_join(nc1 %>% as.data.frame(), nc2 %>% as.data.frame(), by=c('shape_id'='shape_id')) %>%
    filter(! is.na(source)) %>%
    glimpse()
  transport_save(df, 'osm_routes_gtfs_shapes')
}
# source("geo/scripts/transport.R");transport_osm_routes_gtfs_shapes_diff()
transport_osm_routes_gtfs_shapes_diff <- function() {
  carp('contains')
  library(sf)
  library(tidyverse)
  df <- transport_read('osm_routes_gtfs_shapes') %>%
    arrange(shape_id)
  nc <- st_as_sf(df, geom_routes=st_sfc(df$geometry.x))
  nc$geom_shapes <- st_sfc(df$geometry.y)
  for (i in 1:nrow(df)) {
    nc1 <- nc[i, ]
    plot(st_geometry(nc1$geom_routes))
    plot(st_geometry(nc1$geom_shapes), add=TRUE)
    inters <- st_intersection(nc1$geom_shapes, nc1$geom_routes)
    plot(st_geometry(inters), add=TRUE, col='blue', lwd=3)
#    carp('inters')
#    glimpse(inters)
#    carp('shapes')
#    glimpse(nc1$geom_shapes)
    diff.sfc <<- st_difference(nc1$geom_shapes, inters)
    if (length(diff.sfc) >= 1 ) {
      carp('shape_id: %s', nc[[i, 'shape_id']])
      plot(st_geometry(diff.sfc), add=TRUE, col='red', lwd=3)
 #     break
    }
  }

}
transport_save <- function(obj, fic='objets_stop') {
  dsn <- sprintf("%s/%s.Rds", transportDir, fic)
  saveRDS(obj, dsn)
  carp('dsn: %s', dsn)
  return(invisible(obj))
}
transport_read <- function(fic='objets_stop') {
  dsn <- sprintf("%s/%s.Rds", transportDir, fic)
  obj <- readRDS(dsn)
  carp('dsn: %s', dsn)
  return(invisible(obj))
}
#
# pour les stops
# source("geo/scripts/transport.R");star_osm_stops_gtfs_stops()
star_osm_stops_gtfs_stops <- function() {
  library(tidyverse)
  carp()
  gtfs_stops.sf <- tidytransit_stops_sf_lire() %>%
    mutate(gtfs='gtfs') %>%
    glimpse()
  osm_stops.sf <- osm_stops_read() %>%
    dplyr::select(stop_id=ref.FR.STAR, name, osm_id) %>%
    mutate(osm='osm') %>%
    glimpse()
  Encoding(osm_stops.sf$name) <- 'UTF-8'

# sans géométrie
  df <- dplyr::full_join(osm_stops.sf %>% st_drop_geometry(), gtfs_stops.sf %>% st_drop_geometry(), by=c('stop_id'='stop_id')) %>%
    glimpse()
  osm_inc.df <- df %>%
    filter(is.na(gtfs)) %>%
    glimpse()
  gtfs_inc.df <- df %>%
    mutate(stop_id=as.numeric(stop_id)) %>%
    filter(stop_id < 15000) %>%
    filter(is.na(osm)) %>%
    glimpse()
# avec la géométrie
  df <- dplyr::full_join(osm_stops.sf %>% as.data.frame(), gtfs_stops.sf %>% as.data.frame(), by=c('stop_id'='stop_id')) %>%
    glimpse()
  df$d <- as.numeric(st_distance(df$geometry.x, df$geometry.y, by_element = TRUE))
  df1 <- df %>%
    dplyr::select(name, stop_name, osm_id, d) %>%
    filter(d > 50)
  View(df1)
}



