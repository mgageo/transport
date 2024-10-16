# <!-- coding: utf-8 -->
#
# quelques fonctions pour OpenStreetMap
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
#
# l'enchainement des traitements
# source("geo/scripts/osm.R"); ptna_jour(force = FALSE)
ptna_jour  <- function(force = TRUE) {
  library(arrow)
  ptna_overpass_get(force = force)
}
ptna_overpass_get  <- function(force = TRUE, force_osm = TRUE) {
  library(xml2)
  zoom <- "left=%0.5f&right=%0.5f&top=%0.5f&bottom=%0.5f"
  df1 <- overpass_get(query = "ptna", format = "csv", force = force_osm) %>%
    glimpse() %>%
    rename_with(~str_remove(.x, "@")) %>%
    filter(bus %notin% c("yes", "designated")) %>%
    filter(busway %notin% c("yes", "designated")) %>%
    filter(psv %notin% c("yes", "designated")) %>%
    glimpse() %>%
    mutate(select = sprintf("%s%s", type, id)) %>%
    mutate(zoom = sprintf(zoom, lon - .001, lon + .001, lat + .0005, lat - .0005)) %>%
    mutate(josm = sprintf("<a href='http://127.0.0.1:8111/load_object?new_layer=false&objects=%s' target='hiddenIframe'>josm</a>", select)) %>%
    dplyr::select(id, josm) %>%
    mutate(level0 = sprintf("w%s", id)) %>%
    glimpse()
  level0 <- paste0(df1$level0, collapse = ",")
  writeLines(level0)
  html_df2fic(df1)
}