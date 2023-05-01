# <!-- coding: utf-8 -->
#
# quelques fonctions pour les réseaux de transport
#
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
# source("geo/scripts/transport.R");auray_jour()
auray_jour <- function() {
  carp()
  config_xls('auraybus');
  mobibreizh_gtfs_reseau(config[1, "reseau"], config[1, "agency_id"])
  txt_gtfs_stops_sf()
}
# source("geo/scripts/transport.R");nc <- auray_nodes_busstop_orphelins()
auray_nodes_busstop_orphelins <- function(force = TRUE) {
  library(tidyverse)
  library(rio)
  carp()
  config_xls('auray');
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
