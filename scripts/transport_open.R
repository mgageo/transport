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
# source("geo/scripts/transport.R");open_scite()
open_scite <- function(force = TRUE) {
  dsn <- sprintf("%s/%s", gtfsDir, "routes.txt")
  misc_scite(dsn)
}
#
# ouverture des pdf
#
# source("geo/scripts/transport.R");open_pdf()
open_pdf <- function(force = TRUE) {
  pdf <- sprintf("%s/%s_valhalla_shapes.pdf", texDir ,Reseau)
  misc_openFile(pdf)
  pdf <- sprintf("%s/%s_tidytransit_refs_shapes_stops_carto.pdf", texDir ,Reseau)
  misc_openFile(pdf)
  pdf <- sprintf("%s/%s_reseau_osm_routes_shapes.pdf", texDir ,Reseau)
  misc_openFile(pdf)
}
#
# actualisation du gtfs
#
# https://stackoverflow.com/questions/67369906/open-file-explorer-at-specified-folder-using-r-or-specifically-r-studio-in-win
#
# source("geo/scripts/transport.R");open_gtfs()
open_gtfs <- function(force = TRUE) {
  url <- Config_gtfs_source
  aaaammjj <- format(Sys.time(),"%Y%m%d")
  dsn <- sprintf("%s/gtfs_%s.zip", gtfsDir, aaaammjj)
  if (! file.exists(dsn)) {
    download.file(url, dsn)
  }
  carp("dsn: %s", dsn)
  utils::browseURL(gtfsDir)
}