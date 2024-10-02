# <!-- coding: utf-8 -->
#
# quelques fonctions pour OpenStreetMap
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
#
# l'enchainement des traitements
# source("geo/scripts/osm.R"); eumapper_jour(force = FALSE)
eumapper_jour  <- function(force = TRUE) {
  library(arrow)
  eumapper_overpass_get(force = force)
}
#
# lecture des fichiers
# source("geo/scripts/osm.R"); eumapper_overpass_get(force = FALSE, force_osm = FALSE)
eumapper_overpass_get  <- function(force = TRUE, force_osm = FALSE) {
  library(xml2)
  doc <- overpass_get(query = "eumapper", format = "xml", force = force_osm) %>%
    glimpse()
  ways <- xml2::xml_find_all(doc, "//way") %>%
    glimpse()
  ways.df <- tibble(
    id =  xml2::xml_attr(ways, "id"),
    user =  xml2::xml_attr(ways, "user"),
    changeset =  xml2::xml_attr(ways, "changeset"),
    version =  xml2::xml_attr(ways, "version")
  ) %>%
    filter(version == "1") %>%
    arrange(changeset, id) %>%
    glimpse()
  misc_print(ways.df)
}
eumapper_overpass_get  <- function(force = TRUE, force_osm = FALSE) {
  library(xml2)
  zoom <- "left=%0.5f&right=%0.5f&top=%0.5f&bottom=%0.5f"
  df1 <- overpass_get(query = "eumapper", format = "csv", force = force_osm) %>%
    rename_with(~str_remove(.x, "@")) %>%
    filter(version == 1) %>%
    group_by(changeset) %>%
    filter(row_number()==1) %>%
    mutate(select = sprintf("%s%s", type, id)) %>%
    mutate(zoom = sprintf(zoom, lon - .001, lon + .001, lat + .0005, lat - .0005)) %>%
    mutate(josm = sprintf("<a href='http://127.0.0.1:8111/load_and_zoom?%s&new_layer=true&relation_members=true&objects=%s' target='hiddenIframe'>josm</a>", zoom, select)) %>%
    dplyr::select(name, josm) %>%
    glimpse()
  html_df2fic(df1)
}