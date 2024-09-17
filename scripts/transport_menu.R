# <!-- coding: utf-8 -->
#
# un menu
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
#
# source("geo/scripts/transport.R");menu()
menu <- function() {
  library(tidyverse)
  les_choix <- tribble(
    ~choix, ~fonction,
    "gtfs", "tidytransit_jour",
    "diff gtfs osm stops", "diff_stops",
    "gtfs2osm", "gtfs2osm_jour",
    "reseau gtfs osm routes tags", "reseau_osm_routes_shapes_pdf",
    "reseau_osm_routes_shapes_pdf", "reseau_osm_routes_shapes_pdf",
  )
  choix <- utils::menu(les_choix$choix, title = "action")
  if (choix == 0 ) {
    stop("*****")
  }
  le_choix <- les_choix[[choix, "fonction"]]
  print(le_choix)
  do.call(le_choix, list())
}