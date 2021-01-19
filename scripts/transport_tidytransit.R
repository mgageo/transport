# <!-- coding: utf-8 -->
#
# utilisation des données d'OpenStreetMap
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
# auteur : Marc Gauthier
#
#
# fonctions de tidytransit

#shapes_as_sf(tt$shapes)
# lecture des routes, données GTFS
tidytransit_zip_lire <- function(dsn, rds='gtfs.Rds') {
  carp()
  library(tidytransit)
#  dsn <- sprintf("%s/20190805/mobibreizh-bd-gtfs.zip", transportDir)
  carp("dsn: %s", dsn)
  tt <- tidytransit::read_gtfs(dsn, quiet = FALSE)
  dsn <- sprintf("%s/%s", transportDir, rds)
  carp("dsn: %s", dsn)
  saveRDS(tt, file=dsn)
  return(invisible(tt))
}
tidytransit_unzip_lire <- function(dsn, rds='gtfs.Rds') {
  carp()
  library(tidytransit)
#  dsn <- sprintf("%s/20190805/mobibreizh-bd-gtfs.zip", transportDir)
  carp("dsn: %s", dsn)
  file_list_df <- zip::zip_list(dsn)
  path <- dirname(dsn)
  tt <- tidytransit::create_gtfs_object(path, file_list_df, quiet = FALSE)
  dsn <- sprintf("%s/%s", transportDir, rds)
  carp("dsn: %s", dsn)
  saveRDS(tt, file=dsn)
  return(invisible(tt))
}
# source("geo/scripts/keolis.R"); tt <- tidytransit_lire()
tidytransit_lire <- function(rds='gtfs.Rds') {
  dsn <- sprintf("%s/%s", transportDir, rds)
  carp("dsn: %s", dsn)
  tt <- readRDS(file=dsn)
  return(invisible(tt))
}
#
# conversion des shapes d'un gtfs
# source("geo/scripts/transport.R");star_gtfs_shapes_sf()
tidytransit_gtfs_shapes_sf <- function(rds='gtfs_shapes') {
  library(tidyverse)
  library(tidytransit)
  carp()
  tt <- tidytransit_lire() %>%
    glimpse()
  shapes.sf <- shapes_as_sf(tt$shapes) %>%
    glimpse()
#  plot(shapes.sf)
  dsn <- sprintf("%s/%s.Rds", transportDir, rds)
  carp("dsn: %s", dsn)
  saveRDS(shapes.sf, file=dsn)
  return(invisible(shapes.sf))
}
# source("geo/scripts/transport.R");tidytransit_shapes_sf_lire()
tidytransit_shapes_sf_lire <- function(rds='gtfs_shapes') {
  dsn <- sprintf("%s/%s.Rds", transportDir, rds)
  carp("dsn: %s", dsn)
  nc <- readRDS(file=dsn)
  return(invisible(nc))
}
# source("geo/scripts/keolis.R"); tt <- tidytransit_lire()
tidytransit_lire <- function(rds='gtfs.Rds') {
  dsn <- sprintf("%s/%s", transportDir, rds)
  carp("dsn: %s", dsn)
  tt <- readRDS(file=dsn)
  return(invisible(tt))
}
#
# conversion des stops d'un gtfs
# source("geo/scripts/transport.R");tidytransit_gtfs_stops_sf()
tidytransit_gtfs_stops_sf <- function(rds='gtfs_stops') {
  library(tidyverse)
  library(tidytransit)
  library(sf)
  carp()
  tt <- tidytransit_lire() %>%
    glimpse()
#  plot(tt)
#  routes.sf <- get_route_geometry(tt)
#  plot(routes.sf)
  stops.df <- tt$stops %>%
    mutate(lat = as.numeric(stop_lat)) %>%
    mutate(lon = as.numeric(stop_lon)) %>%
    glimpse()
#  plot(stops.sf)
  stops.sf <- st_as_sf(stops.df, coords = c("lon", "lat"), crs = 4326, remove=FALSE)
  dsn <- sprintf("%s/%s.Rds", transportDir, rds)
  carp("dsn: %s", dsn)
  saveRDS(stops.sf, file=dsn)
  return(invisible(stops.sf))
}
# source("geo/scripts/transport.R");tidytransit_stops_sf_lire()
tidytransit_stops_sf_lire <- function(rds='gtfs_stops') {
  dsn <- sprintf("%s/%s.Rds", transportDir, rds)
  carp("dsn: %s", dsn)
  nc <- readRDS(file=dsn)
  return(invisible(nc))
}
#
# détermination des stops d'un shape
# source("geo/scripts/transport.R");tidytransit_shapes_stops()
tidytransit_shapes_stops <- function() {
  library(tidyverse)
  library(tidytransit)
  library(sf)
  carp()
  tt <- tidytransit_lire() %>%
    glimpse()
  trips.df <- tt$trips %>%
    dplyr::select(trip_id, shape_id) %>%
    glimpse()

  stop_times.df <- tt$stop_times %>%
    glimpse()
# on regroupe par voyage
  df1 <- stop_times.df %>%
    arrange(stop_sequence) %>%
    group_by(trip_id) %>%
    summarise(nb=n(), depart=first(stop_id), arrivee=last(stop_id), arrets=paste(stop_id, collapse = ";"))
# on trouve le shape
  df2 <- left_join(df1, trips.df) %>%
    glimpse()
  carp('echec jointure trips')
  df2 %>%
    filter(is.na(shape_id)) %>%
    glimpse()
# on regroupe
  carp("les arrets d'un shape")
  df3 <- df2 %>%
    filter(! is.na(shape_id)) %>%
    group_by(shape_id, arrets) %>%
    summarise(nb=n()) %>%
    glimpse()
  return(invisible(df3))
}
#
# ajout des coordonnées géographiques des stops d'un shape
# source("geo/scripts/transport.R");tidytransit_shapes_stops()
tidytransit_shapes_stops_sf <- function(df3) {
  library(tidyverse)
  library(tidytransit)
  library(sf)
  carp()
  if(missing(df3)) {
    carp('missing')
    df3 <- tidytransit_shapes_stops()
  }
  tt <- tidytransit_lire()
  stops.df <- tt$stops %>%
    mutate(lat = as.numeric(stop_lat)) %>%
    mutate(lon = as.numeric(stop_lon)) %>%
    dplyr::select(stop_id, stop_name, lat, lon) %>%
    glimpse()
  carp("shape avec plusieurs voyages")
  df4 <- df3 %>%
    group_by(shape_id) %>%
    summarise(nb=n()) %>%
    filter(nb > 1) %>%
    glimpse()
# pour pouvoir faire la jointure avec les stops
  df5 <- df3 %>%
    separate_rows(arrets) %>%
    dplyr::select(-nb, stop_id=arrets) %>%
    glimpse()
# la jointure avec les stops
  df6 <- left_join(df5, stops.df) %>%
    glimpse()
  nc1 <- st_as_sf(df6, coords = c("lon", "lat"), crs = 4326, remove=TRUE) %>%
    glimpse()
  return(invisible(nc1))
}
#
#
#
# en direct de https://github.com/r-transit/tidytransit/blob/master/R/spatial.R
shapes_as_sfg <- function(df) {
  # as suggested by www.github.com/mdsumner
  l_dfs <- split(df, df$shape_id)

  l_linestrings <- lapply(l_dfs,
                          shape_as_sf_linestring)

  return(sf::st_multilinestring(l_linestrings))
}
shape_as_sf_linestring <- function(df) {
  # as suggested by www.github.com/mdsumner

  m <- as.matrix(df[order(df$shape_pt_sequence),
                    c("shape_pt_lon", "shape_pt_lat")])

  return(sf::st_linestring(m))
}

# en direct de https://www.natedayta.com/2018/06/02/extending-gtfs-capabilities-with-parsing-into-simple-features/
shapes_as_sf <- function(df) {
# extract lon/lat values as matrix to build linestrings for each "shape_id"
  sfc <- df %>% # long data frame
    arrange(shape_pt_sequence) %>% # essentiel !
    split(.$shape_id) %>% # list of shorted data framee, one per shape
    map(~ dplyr::select(., shape_pt_lon, shape_pt_lat) %>% # order maters, lon-1st lat-2nd
        as.matrix %>% # coherce for st_linestrings happiness
        st_linestring) %>%
    st_sfc(crs = 4326) # bundle all shapes into a collection
# add collection on and convert to sf
  nc <- unique(df$shape_id) %>%
    sort() %>% # sort to match with names(sfc); split()'s factor-cohercion alpha sorts
    st_sf(shape_id = ., geometry = sfc) %>%
    glimpse()
}