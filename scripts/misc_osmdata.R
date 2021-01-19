# <!-- coding: utf-8 -->
#
# utilisation des données d'OpenStreetMap
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
# auteur : Marc Gauthier
#
#
# source("geo/scripts/transport.R"); res <- osmdata_get_objet_polyline(ref = "11920346")
osmdata_get_objet_polyline <- function(ref, type = "relation", force = FALSE) {
  library(readr)
  library(tidyverse)
  library(osmdata)
  dat1 <<- opq_osm_id (type = type, id = ref) %>%
    opq_string () %>%
    osmdata_sf ()
  points.sf <- dat1$osm_points
  stops.sf <- points.sf %>%
    filter(! is.na(public_transport))
  df <- as.data.frame(st_coordinates(stops.sf))
  colnames(df) <- c("lon", "lat")
  enc <- encode(df)
  return(invisible(enc))
}
#
osmdata_query <- function(query, type, force = FALSE) {
  library(readr)
  library(tidyverse)
  library(osmdata)
  dsn <- overpass_query(query, type, force)
  res <- osmdata_sf(, dsn)
  return(invisible(res))
}