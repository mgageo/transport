# <!-- coding: utf-8 -->
#
# utilisation des données d'OpenStreetMap
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
# auteur : Marc Gauthier
#
#
# éditions des fichiers
#
#

# source("geo/scripts/transport.R");open_scite()
open_scite <- function(force = TRUE) {
  dsn <- sprintf("%s/%s", gtfsDir, "routes.txt")
  misc_scite(dsn)
}
# source("geo/scripts/transport.R");open_pdf()
open_pdf <- function(force = TRUE) {
  pdf <- sprintf("%s/%s_valhalla_shapes.pdf", texDir ,Reseau)
  misc_openFile(pdf)
  pdf <- sprintf("%s/%s_tidytransit_refs_shapes_stops_carto.pdf", texDir ,Reseau)
  misc_openFile(pdf)
  pdf <- sprintf("%s/%s_reseau_osm_routes_shapes.pdf", texDir ,Reseau)
  misc_openFile(pdf)
}