# <!-- coding: utf-8 -->
#
# utilisation des données d'OpenStreetMap
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
# auteur : Marc Gauthier
#
#
## fonctions de valhalla
#
# pour trouver les ways osm d'un tracé shapefile
#
# https://link.springer.com/article/10.1007/s42979-022-01340-5
#
valhalla_url <- 'http://localhost:8002/trace_attributes'
#
# source("geo/scripts/transport.R");valhalla_jour()
valhalla_jour <- function(force = TRUE) {
  valhalla_shapes(force = force)
}
#
# source("geo/scripts/transport.R");valhalla_shapes()
valhalla_shapes <- function(force = TRUE) {
  carp()
  df1 <- tidytransit_lire(rds = "tidytransit_routes_stops") %>%
    group_by(ref_network, shape_id) %>%
    arrange(desc(nb), desc(nb_stops)) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    arrange(ref_network) %>%
    mutate(shape_id = gsub("[$:]", "_", shape_id))
  for (i1 in 1:nrow(df1)) {
    valhalla_shape(shape = df1[[i1, "shape_id"]])
  }
}
#
# source("geo/scripts/transport.R");valhalla_shape()
valhalla_shape <- function(shape = "1236_3a0010_7", force = TRUE) {
  library(httr2)
  library(sf)
  library(jsonlite)
  dsn_shape <- sprintf("%s/shape_%s.gpx", josmDir, shape)
  carp("dsn_shape: %s", dsn_shape)
  if (! file.exists(dsn_shape)) {
    carp("*** dsn_shape: %s", dsn_shape)
    return(invisible(FALSE))
  }
  shape.sf <- st_read(dsn_shape, layer = "tracks", quiet = TRUE)
  coord.df <- st_coordinates(shape.sf) %>%
    as_tibble() %>%
#    slice((1:10)) %>%
    mutate(val = sprintf('{"lat":%s,"lon":%s}', Y, X))
  coord <- str_flatten(coord.df$val, ",")
#  writeLines(coord)
#  coord <- '{"lat":48.124908,"lon":-1.591601},{"lat":48.118285,"lon":-1.605576}'
  body_tpl <- r"({"shape": [%s],"costing":"auto","directions_options":{"units":"meters"}, "verbose": true})"
  body.str <- sprintf(body_tpl, coord)

  body_str <- '{"shape": [{"lat":48.124908,"lon":-1.591601},{"lat":48.118285,"lon":-1.605576}],"costing":"auto","directions_options":{"units":"meters"}, "verbose": true}'
  body_str <- body.str
  writeLines(body_str, "d:/valhalla_shape.txt")

#  confess(body_str)
  req <- httr2::request(valhalla_url)
  resp <- req |>
    httr2::req_body_raw(body_str, type = "application/json") |>
    httr2::req_perform()
  resp.list <- resp |>
    resp_body_json()
  edges.list <- resp.list$edges
  carp("edges.list: %s", length(edges.list))
  if (length(edges.list) == 0) {
    txt <- jsonlite::toJSON(resp.list, pretty = TRUE)
#    writeLines(txt)
    return()
    confess("pas d'edge")
  }
  edges.df <- tibble()
  for (edge.list in edges.list) {
    edges.df <- bind_rows(edges.df , c(id = edge.list$way_id))
  }
  ways.df <- edges.df %>%
    group_by(id) %>%
    filter(row_number()==1) %>%
    ungroup() %>%
    mutate(level0 = sprintf("  wy %s", id))
  level0 <- str_flatten(ways.df$level0, "\n")
#  writeLines(level0)
  dsn_level0 <- sprintf("%s/shape_%s.txt", josmDir, shape)
  writeLines(level0, dsn_level0)
  carp("dsn_level0: %s", dsn_level0)
}
