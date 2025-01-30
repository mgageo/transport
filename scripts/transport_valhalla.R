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
# le serveur valhalla tourne dans une instance docker
# lancement en powershell dans d:/web/vds
# cd d:/web/vds
# .\scripts\valhalla.ps1
# http://localhost:8111/load_object?objects=w46864177,w46864149,w397036408,w397036408,w397036408,w397036409,w397036409,w46861636,w46861699,w46861699,w46861700,w46861700,w46861700,w214231415,w191567945,w191567945,w191567945,w46862067,w72915917,w191567942,w191567942,w191567942,w607956607,w361860041,w361860041,w28031523,w46864414,w46864414,w46864412,w361860041,w361860041,w607956607,w191567942,w191567942,w191567942,w124988393,w548959525,w124988394,w124988394,w590612036,w191568895,w590612035,w124988392,w24939645,w24939645,w24939645,w24939645,w24939645,w24939645,w24939645,w24939645,w100661270,w100661270,w100661270,w100661270,w24939648,w24939648,w24939648,w925820896,w925820896,w136009043,w136009043,w925820897,w925820897,w925820897,w136009042,w46864161,w46864161,w46864161,w46864161,w46864161,w548959526,w85333085,w5184799,w5184799,w5184799,w5184799,w5184798,w5184798,w5184797,w5184746,w5184746,w186958834,w186958816,w1237485186,w1237485191,w1237485192,w5184728,w5184728,w128856878,w128856875,w128856875,w590612345,w128855116,w128855116,w128855116,w214734410,w214734412,w244307814,w14858569,w191469160,w59476421,w59476421,w169197122,w169197122,w137835671,w137835670,w191469160,w191469160,w191469160,w191469160,w1237485202,w1237485203,w590612380,w1237485199,w1237485200,w14858571,w14858571,w14858571,w14858571,w14858571,w14858571,w14858571,w820398130,w1238062369,w820398142,w5184727,w5184727,w5184727,w590612447,w1238062395,w1238062396,w5184724,w5184724,w5184724,w5184724,w1238062386,w1238062387,w1238062388,w5184720,w5184720,w5184720,w214738050,w214738050,w214738052,w214738052,w214738052,w5184715,w5184715,w5184715,w5184717,w5184717,w5184716,w304151658,w304151659,w452869815,w452869816,w304151660,w304151660,w452869813,w452869813,w452869813,w452869812,w452869812,w1236770128,w5066306,w5066306,w5066306,w5066306,w5066306,w5066306,w5066306,w5066306,w5066306,w5066306,w5066306,w141655945,w141655945,w141655945,w141655945,w141655945,w161597300,w161597300,w161597300,w161597300,w161597300,w1238062356,w1238062357,w1238062358,w22757791,w22757791,w22757791,w217399174,w217399174,w156810602,
#
valhalla_url <- 'https://valhalla1.openstreetmap.de/trace_attributes?api_key=a5e5eecb-60e9-475a-9567-862a85e8defa'
valhalla_url <- 'http://localhost:8002/trace_attributes'
#
# source("geo/scripts/transport.R");valhalla_jour()
valhalla_jour <- function(force = TRUE) {
  library(httr2)
#  tryCatch(
#    request(valhalla_url) |>
#      req_perform() |>
#      resp_body_json(),
#      httr2_http_404 = function(cnd) NULL
#  )
  valhalla_shapes(force = force)
  valhalla_shapes_tex(force = force)
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
    mutate(dsn_shape = gsub("[$:]", "_", shape_id))
  josm.df <- tibble(
    ref_network = character(),
    shape_id = character(),
    dsn_shape = character(),
    osm = character(),
    josm = character(),
    level0 = character()
  )
  for (i1 in 1:nrow(df1)) {
    carp("i1: %s/%s", i1, nrow(df1))
    rc <- valhalla_shape(shape = df1[[i1, "dsn_shape"]], force = force)
    if (is.logical(rc)) {
      carp("erreur valhalla")
      next
    }
    josm.df <- josm.df %>%
      add_row(
        ref_network = df1[[i1, "ref_network"]],
        shape_id = df1[[i1, "shape_id"]],
        dsn_shape = rc$dsn_shape,
        osm = rc$osm,
        josm = rc$josm,
        level0 = rc$level0
      )
#    break
  }
  glimpse(josm.df)
  tidytransit_sauve(josm.df, "valhalla_shapes")
}
#
# source("geo/scripts/transport.R");valhalla_shapes_tex()
valhalla_shapes_tex <- function(force = TRUE) {
  carp()
  reseau_tpl_tex()
  df1 <- tidytransit_lire(rds = "valhalla_shapes") %>%
    glimpse()
  texFic <- sprintf("%s/%s", imagesDir, "valhalla_shapes.tex")
  TEX <- file(texFic)
  tex <- sprintf("<!-- coding: utf-8 -->
%s ", format(Sys.time(), "%d/%m/%Y %H:%M:%S"))
#
# le template tex
  dsn <- sprintf("%s/valhalla_shapes_tpl.tex", tplDir)
  template <- readLines(dsn) %>%
    glimpse()
  for (i1 in 1:nrow(df1)) {
    tpl <- tex_df2tpl(df1, i1, template)
    tpl <- escapeLatexSpecials(tpl)
    tex <- append(tex, tpl)
  }
  write(tex, file = TEX, append = FALSE)
  close(TEX)
  carp(" texFic: %s", texFic)
  tex_pdflatex(sprintf("%s_valhalla_shapes.tex", Reseau))
}
#
# source("geo/scripts/transport.R");valhalla_shape()
valhalla_shape <- function(shape = "18573", force = TRUE) {
  library(httr2)
  library(sf)
  library(jsonlite)
  dsn_shape <- sprintf("%s/shape_%s.gpx", josmDir, shape)
  carp("dsn_shape: %s", dsn_shape)
  if (! file.exists(dsn_shape)) {
    carp("*** dsn_shape: %s", dsn_shape)
    return(invisible(FALSE))
  }
  dsn_level0 <- sprintf("%s/shape_%s.txt", josmDir, shape)
  if (file.exists(dsn_level0) & force == FALSE) {
    carp("dsn_level0: %s", dsn_level0)
    return(invisible(FALSE))
  }
  dsn_valhalla <- sprintf("%s/shape_%s.json", josmDir, shape)

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
# pour test
#  body_str <- '{"shape": [{"lat":48.124908,"lon":-1.591601},{"lat":48.118285,"lon":-1.605576}],"costing":"auto","directions_options":{"units":"meters"}, "verbose": true}'
  body_str <- body.str
  writeLines(body_str, "d:/valhalla_shape.txt")

#  confess(body_str)
  req <- httr2::request(valhalla_url)
  resp <<- req |>
    req_error(is_error = \(resp) FALSE) |>
    httr2::req_body_raw(body_str, type = "application/json") |>
    httr2::req_perform(path = dsn_valhalla)
  resp.list <- resp |>
    resp_body_json()
  edges.list <- resp.list$edges
  carp("edges.list: %s", length(edges.list))
  if (length(edges.list) == 0) {
    txt <- jsonlite::toJSON(resp.list, pretty = TRUE)
#    writeLines(txt)
    return(invisible(FALSE))
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
    mutate(osm = sprintf("  <member type=\"%s\" ref=\"%s\" role=\"\"/>", "way", id)) %>%
    mutate(josm = sprintf("w%s", id)) %>%
    mutate(level0 = sprintf("  wy %s", id))
  osm <- str_flatten(ways.df$osm, "\n")
  josm <- str_flatten(ways.df$josm, ",")
  level0 <- str_flatten(ways.df$level0, "\n")
#  writeLines(level0)
  writeLines(level0, dsn_level0)
  carp("dsn_level0: %s", dsn_level0)
  rc <- list(
    dsn_shape = dsn_shape,
    osm = osm,
    josm = josm,
    level0 = level0
  )
  return(invisible(rc))
}
