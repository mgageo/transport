# <!-- coding: utf-8 -->
#
# un menu
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
#
# source("geo/scripts/transport.R");menu()
menu <- function() {
  les_choix <- tribble(
    ~choix, ~fonction,
    "gtfs", "tidytransit_jour",
    "diff gtfs osm stops", "diff_stops",
    "gtfs2osm", "gtfs2osm_jour",
    "diff gtfs osm routes tags", "diff_relations_route_bus",
  )
  choix <- utils::menu(les_choix$choix, title = "action")
  if (choix == 0 ) {
    stop("*****")
  }
  le_choix <- les_choix[[choix, "fonction"]]
  print(le_choix)
  do.call(le_choix, list())
}