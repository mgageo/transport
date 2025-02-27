# <!-- coding: utf-8 -->
#
# utilisation des données d'OpenStreetMap
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
# auteur : Marc Gauthier
#
# la requête overpass doit extraire tous les objets
# (._;>>;);
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
osmdata_query <- function(query, fic, force = FALSE) {
  library(readr)
  library(tidyverse)
  library(osmdata)
  dsn <- overpass_query(query, fic, force) %>%
    glimpse()
  res <- osmdata_sf(, dsn)
  return(invisible(res))
}
# source("geo/scripts/transport.R");osmdata_relation_route_plot()
osmdata_relation_route_plot <- function(id = 13436728, force = FALSE) {
  library(tidyverse)
  library(sf)
  library(osmdata)
  carp()
  dsn <- osmapi_object_full(id, type = "relation", force = force)
  carp("dsn: %s", dsn)
  q <- opq(bbox = c(45, -6, 58, 0))
  osm.sf <- osmdata_sf(q, dsn)
  stops.sf <- osm.sf$osm_points %>%
    filter(! is.na(public_transport))
  plot(st_geometry(stops.sf), add = FALSE)
  if ( ! is.null(osm.sf$osm_multilines)) {
    plot(st_geometry(osm.sf$osm_multilines), add = TRUE)
  }

}