# <!-- coding: utf-8 -->
#
# le réseau de bus de Lorient
#
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
#
# source("geo/scripts/transport.R");izilo_jour()
izilo_jour <- function() {
  carp()
  config_xls('lorient')
  izilo_shape()
}

izilo_shape <- function(force = TRUE) {
  library(tidyverse)
  library(janitor)
  carp()
  df1 <- lire_rds("diff_relations_route_bus") %>%
    clean_names() %>%
    glimpse() %>%
    separate_wider_regex(
      gtfs_shape_id,
#      c(".*_[01]_", shape = ".*", "_.*_.*_.*_\\d\\d"),
#      c(shape = ".*", "_.*_.*_.*_\\d+$"),
      c(shape = ".*", "_.*$"),
      names_repair = "check_unique",
      too_few = c("align_start"),
      cols_remove = FALSE
    ) %>%
    filter(gtfs_shape_id != shape) %>%
    glimpse()
#  stop("****")
  osm <- ""
  nb_changes <- 0
  for (i1 in 1:nrow(df1)) {
    id <- df1[[i1, "id"]]
    shape  <- df1[[i1, "shape"]]
    tags.df <- tribble(~name, ~value,
"gtfs:shape_id", shape
)
    type <- "relation"
    o <- osmchange_object_modify_tags(id = id, type = type, tags = tags.df, Change = FALSE)
    nb_changes <- nb_changes + 1
    writeLines(o)
    osm <- sprintf("%s\n%s", osm, o)
#    break
  }
  osm <- paste(osm, "\n", collapse = "")
#  writeLines(osm)
  OsmChange <<- TRUE
  changeset_id <- osmapi_put("modify", text = osm, comment = "maj des attributs gtfs")
}
