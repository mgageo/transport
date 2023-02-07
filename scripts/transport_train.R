# <!-- coding: utf-8 -->
# le réseau de trains
# utilisation des données d'OpenStreetMap
# auteur : Marc Gauthier
#
#
## les gaps dans une relation
#
# source("geo/scripts/transport.R");train_relation_route_gap(id = 402637, force = FALSE, force_osm = FALSE)
train_relation_route_gap <- function(id = 402637, force = TRUE, force_osm = TRUE) {
  library(tidyverse)
  rc <- osm_relation_route_members(id = id, force = force, force_osm = force_osm)
  if ("note:mga_geo" %in% names(rc$relation)) {
    carp("note:mga_geo id: %s", id)
    return()
  }
  carp("les ways")
  ways.df <- rc$ways.sf %>%
    st_drop_geometry()
  df1 <- ways.df %>%
    mutate(no = 1:n()) %>%
    select(no, id, node1, node9, name)
  misc_print(df1)
 carp("fin id: %s", id)
}