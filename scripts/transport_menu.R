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
    "OsmChange toogle", "menu_OsmChange",
    "gtfs", "tidytransit_jour",
    "diff gtfs osm stops", "diff_stops",
    "gtfs2osm", "gtfs2osm_jour",
    "diff_relations_route_bus", "diff_relations_route_bus",
    "diff_relations_routemaster_bus", "diff_relations_routemaster_bus",
    "reseau_osm_routes_shapes_pdf", "reseau_osm_routes_shapes_pdf",
    "reseau valhalla", "valhalla_jour",
    "reseau départ arrivée", "reseau_da",
    "open scite", "open_scite",
    "open pdf", "open_pdf",
  )
  for (i in 1:10) {
    titre <- sprintf("OsmChange: %s", OsmChange)
    choix <- utils::menu(les_choix$choix, title = titre)
    if (choix == 0 ) {
      stop("*****")
    }
    le_choix <- les_choix[[choix, "fonction"]]
    print(le_choix)
    do.call(le_choix, list())
  }
}
menu_OsmChange <- function() {
  if (OsmChange == TRUE) {
    OsmChange <<- FALSE
  } else {
    OsmChange <<- TRUE
  }
}