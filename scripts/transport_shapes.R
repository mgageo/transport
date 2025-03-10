# <!-- coding: utf-8 -->
#
# le réseau de bus
#
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
# validation des tracés du fichier shapes
#
# source("geo/scripts/transport.R");shapes_jour(force = FALSE)
shapes_jour <- function(reseau = "rennes_kor", force = TRUE) {
  library(tidyverse)
  config_xls(reseau)
#  shapes_validation()
  shapes_validation_bilan()
}
# source("geo/scripts/transport.R");dsn <- shapes_validation(force = TRUE)
shapes_validation <- function(force = FALSE) {
  library(tidyverse)
  library(units)
  carp()
  df1 <- shapes_lire()
  traces.df <- data.frame()
  for (i1 in 1:nrow(df1)) {
    carp("i1: %s/%s %s", i1, nrow(df1), df1[[i1, "shape_id"]])
    df2 <- shapes_validation_trace(df1[[i1, "shape_id"]])
    traces.df <- bind_rows(traces.df, df2)
#    break
  }
  glimpse(traces.df)
  sauve_rds(traces.df, suffixe = Config_reseau)
  return(invisible(traces.df))
}
shapes_lire <- function(force = FALSE) {
  library(tidyverse)
  library(units)
  carp()
  dsn <- sprintf("%s/gtfs.zip", gtfsDir)
  carp("dsn: %s", dsn)
  options(encoding = "UTF-8")
  tt <<- tidytransit::read_gtfs(dsn, quiet = FALSE)
  df1 <- tt$shapes %>%
    group_by(shape_id) %>%
    summarize(nb = n())
  return(invisible(df1))
}
# source("geo/scripts/transport.R");dsn <- shapes_validation_bilan(force = TRUE)
shapes_validation_bilan <- function(force = FALSE) {
  library(tidyverse)
  library(units)
  carp()
  traces.df <- lire_rds("shapes_validation", suffixe = Config_reseau)
  df1 <- traces.df %>%
    group_by(shape_id) %>%
    summarize(nb = n()) %>%
    rowid_to_column("no")
  misc_print(df1)
  return(invisible(traces.df))
}
shapes_validation_trace <- function(shape_id, force = FALSE) {
  df1 <- tt$shapes %>%
    filter(shape_id == !!shape_id)
  distance_max <- set_units(2000, "m")
  distance_max <- 2000
# https://github.com/r-spatial/sf/issues/799
  empty <- st_as_sfc("POINT(EMPTY)", crs = 4326)
  nc1 <- st_as_sf(df1, coords = c("shape_pt_lon", "shape_pt_lat"), crs = 4326, remove = FALSE) %>%
    mutate(distance = sf::st_distance(
      geometry,
      dplyr::lag(geometry, default = empty),
      by_element = TRUE)
    ) %>%
    mutate(distance = as.numeric(distance)) %>%
    mutate(shape_distance = shape_dist_traveled - dplyr::lag(shape_dist_traveled)) %>%
#    mutate(delta = abs(shape_distance - distance)) %>%
    filter(distance > distance_max)
  if (nrow(nc1) > 0) {
    glimpse(nc1)
#    stop("****")
  }
  df1 <- nc1 %>%
    st_drop_geometry()
  return(invisible(df1))
}
#
# les deux versions du gtfs de la Star
# source("geo/scripts/transport.R");dsn <- shapes_rennes(force = TRUE)
shapes_rennes <- function(force = FALSE) {
  library(tidyverse)
  library(units)
  carp()
  star.df <- lire_rds("shapes_validation", suffixe = "rennes") %>%
    glimpse()
  korrigo.df <- lire_rds("shapes_validation", suffixe = "rennes_kor") %>%
    glimpse()
#  stop("****")
  shape <- "0050\\-B\\-1563\\-3301"
  config_xls("rennes")
  shapes_lire()
  shapes.df <- tt$shapes %>%
    filter(grepl(shape, shape_id)) %>%
    glimpse()
  shapes_geojson(shapes.df)
  config_xls("rennes_kor")
  shapes_lire()
  shapes_kor.df <- tt$shapes %>%
    filter(grepl(shape, shape_id)) %>%
    glimpse()
  shapes_geojson(shapes_kor.df)
}
#
# conversion en geojson
shapes_geojson <- function(df1) {
  library(tidyverse)
  library(readr)
  carp()
  shapes.sf <- shapes_as_sf(df1)
  for (i in 1:nrow(shapes.sf)) {
    shape_id <- shapes.sf[[i, "shape_id"]]
    shape <- gsub("[$:/]", "_", shape_id)
    nc1 <- shapes.sf[i, ] %>%
      st_transform(4326)
    dsn <- sprintf("d:/tmp/%s.geojson", shape)
    carp("dsn: %s", dsn)
    st_write(nc1, dsn, delete_dsn = TRUE, driver = "GeoJSON")
    df2 <- df1 %>%
      filter(shape_id == !!shape_id)
    dsn <- sprintf("d:/tmp/%s.txt", shape)
    carp("dsn: %s", dsn)
    write_csv(df2, dsn)
  }
}
