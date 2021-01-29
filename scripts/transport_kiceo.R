# <!-- coding: utf-8 -->
# le réseau de bus de Vannes
# utilisation des données opendata
# auteur : Marc Gauthier
#
# source("geo/scripts/transport.R");kiceo_jour()
kiceo_jour <- function() {
  carp()
  config_xls('kiceo');
  mobibreizh_gtfs_reseau(config[1, "reseau"], config[1, "agency_id"])
}
# source("geo/scripts/transport.R");nc <- kiceo_nodes_busstop_orphelins()
kiceo_nodes_busstop_orphelins <- function(force = TRUE) {
  library(tidyverse)
  library(rio)
  carp()
  config_xls('kiceo');
  dsn <- 'network_nodes_busstop_orphelins'
  requete <- sprintf("node['public_transport'='platform']['%s'] ->.a;
.a < ->.b;
.b > ->.c;
(node.a; -node.c;);
out meta;", config[1, "k_ref"])
  dsn <- overpass_query(query = requete, fic = dsn, force = force)
  nc <- oapi_sf_lire(fic = dsn)
  return(invisible(nc))
}
#
# source("geo/scripts/transport.R");nc <- kiceo_nodes_platform()
kiceo_nodes_platform <- function(force = TRUE) {
  library(tidyverse)
  library(rio)
  carp()
  config_xls('kiceo');
  dsn <- 'network_nodes_platform'
  requete <- sprintf("relation[network='%s']->.a;
node(r.a)['public_transport'='platform'];
out meta;", config[1, "network"])
  nc <- osm_overpass_query(requete = requete, fic = dsn, force = force)
  return(invisible(nc))
}
#
# source("geo/scripts/transport.R");res <- kiceo_relations_route()
kiceo_relations_route <- function(force = FALSE) {
  library(tidyverse)
  library(rio)
  carp()
  config_xls('kiceo');
  res <- osm_relations_route_parcours()
  nc <<- res$osm_multilines %>%
    glimpse()
  return(invisible(res))
}
# source("geo/scripts/transport.R");nc <- kiceo_nodes_proches()
kiceo_nodes_proches <- function(force = FALSE) {
  library(tidyverse)
  library(rio)
  carp()
  config_xls('kiceo');
  nc <- kiceo_nodes_platform()
  k_ref <- config[1, "k_ref"]
  carp("k_ref: %s", k_ref)
  nc <- nc %>%
    dplyr::select(ref = `ref:KICEO`, name, osm_id) %>%
    st_transform(2154) %>%
    glimpse()
  nc1 <- nc %>%
    filter(is.na(ref))
  nc2 <- nc %>%
    filter(!is.na(ref))
  nc2$no <- 1:nrow(nc2)
  nc1$proche <- st_nearest_feature(nc1, nc2)
  df <- dplyr::left_join(nc1 %>% as.data.frame(), nc2 %>% as.data.frame(), by=c('proche'='no')) %>%
    glimpse()
#  stop('***')
  df$d <- st_distance(df$geometry.x, df$geometry.y, by_element = TRUE)
  units(df$d) <- 'm'
  df1 <- df %>%
    mutate(d = as.numeric(d)) %>%
    dplyr::select(-geometry.x, -geometry.y) %>%
    filter(d < 50) %>%
    mutate(josm = sprintf("n%s,n%s", osm_id.x, osm_id.y)) %>%
    dplyr::select(name.x, name.y, josm, ref.y, d) %>%
    glimpse()
   print(knitr::kable(df1, format = "pipe"))
}