# <!-- coding: utf-8 -->
#
# le réseau de bus Star de Rennes
#
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
star_csv_url <- "https://data.explore.star.fr/api/explore/v2.1/catalog/datasets/tco-busmetro-horaires-gtfs-versions-td/exports/csv?lang=fr&timezone=Europe%2FBerlin&use_labels=true&delimiter=%3B"
star_json_url <- "https://data.explore.star.fr/api/explore/v2.1/catalog/datasets/tco-busmetro-horaires-gtfs-versions-td/exports/json?lang=fr&timezone=Europe%2FBerlin"
# source("geo/scripts/transport.R");star_jour()
star_jour <- function(reseau = "rennes", force = TRUE) {
  library(tidyverse)
  library(rio)
  library(archive)
  carp()
  config_xls(reseau)
  star_gtfs_dl(reseau = reseau)
  tidytransit_jour(force= TRUE)
}
#
# https://themockup.blog/posts/2020-12-13-extracting-json-from-websites-and-public-apis-with-r/
# source("geo/scripts/transport.R");star_gtfs_dl()
star_gtfs_dl <- function(reseau = "rennes", force = FALSE) {
  library(tidyverse)
  library(archive)
  library(jsonlite)
  carp()
  config_xls(reseau)
  dsn <- sprintf("%s/gtfs.zip", gtfsDir)
  json_data <- fromJSON(star_json_url)
  df1 <- dplyr::bind_rows(json_data, .id = "id")
  i1 <- nrow(df1)
  i1 <- 1
  gtfs_source <- df1[i1, "url"]
  fn_source <- gsub("^.*/", "", gtfs_source)
  dsn_source <- sprintf("%s/%s", gtfsDir, fn_source)
  carp("dsn_source: %s", dsn_source)
  if (!file.exists(dsn_source)) {
    download.file(gtfs_source, dsn_source)
  }
  dsn <- sprintf("%s/gtfs.zip", gtfsDir)
  file.copy(dsn_source, dsn, overwrite = TRUE)
  archive_extract(dsn, gtfsDir)
  carp("fin dsn: %s", dsn)
}
# source("geo/scripts/transport.R");star_osm_jour(force = TRUE)
star_osm_jour <- function(reseau = "star", force = FALSE) {
  library(tidyverse)
  library(data.table)
  library(janitor)
  carp()
  star_osm_jour_routes(reseau = reseau, force = force)
  #  star_osm_jour_stops()
}
# source("geo/scripts/transport.R");star_osm_jour_routes(force = TRUE)
star_osm_jour_routes <- function(reseau = "star", force = FALSE) {
  library(tidyverse)
  library(data.table)
  library(janitor)
  carp()
  osm_relations_route_bus_verif(reseau = reseau, force = force)
  tex_pdflatex("star.tex")
}
# source("geo/scripts/transport.R");star_routes_shapes(force = FALSE)
star_routes_shapes <- function(reseau = "star", force = FALSE) {
  library(tidyverse)
  library(data.table)
  library(sf)
  library(janitor)
  carp()
  config_xls(reseau)
  dsn <- osm_relations_route_bus_csv(force = force)
  df <- fread(dsn, encoding = "UTF-8") %>%
    clean_names()
  texFic <- sprintf("%s/%s", imagesDir, "routes_shapes.tex")
  TEX <- file(texFic)
  tex <- "% <!-- coding: utf-8 -->"
  #
  # le template tex
  dsn <- sprintf("%s/routes_shapes_tpl.tex", texDir)
  template <- readLines(dsn) %>%
    glimpse()
  df <- df %>%
    filter(grepl("^0", gtfs_shape_id)) %>%
    arrange(gtfs_shape_id) %>%
    glimpse()
  for (i in 1:nrow(df)) {
    carp("i: %s/%s", i, nrow(df))
    tpl <- tex_df2tpl(df, i, template)
    tpl <- escapeLatexSpecials(tpl)
    tex <- append(tex, tpl)
    #    next
    id <- df[i, "id"]
    #    glimpse(df[i, ]);stop("****")
    osmapi_get_members_platform(ref = id, force = force)
    dsn_rds <- osmapi_get_ways(ref = id, force = force)
    osm.sf <- readRDS(dsn_rds) %>%
      st_transform(2154)
    if (st_is_empty(osm.sf[1, ])) {
      next
    }
    shape <- osm.sf[[1, "gtfs.shape_id"]]
    titre <- sprintf("relation: %s shape: %s", id, shape)
    plot(st_geometry(osm.sf), col = "blue", lwd = 3)
    title(titre)
    dsn_shape <- sprintf("%s/shape_%s.gpx", josmDir, shape)
    if (!file.exists(dsn_shape)) {
      title(sub = "shape absent")
      dsn <- dev2pdf(suffixe = id, dossier = "images")
      next
    }
    shape.sf <- st_read(dsn_shape, layer = "tracks", quiet = TRUE) %>%
      st_transform(2154)
    plot(st_geometry(shape.sf), add = TRUE, col = "green", lwd = 3)
    inters <- st_intersection(st_buffer(shape.sf, 10), osm.sf)
    plot(st_geometry(inters), add = TRUE, col = "grey", lwd = 3)
    dsn <- dev2pdf(suffixe = id, dossier = "images")
    tex <- append(tex, sprintf("\\mongraphique{images/star_routes_shapes_%s.pdf}", id))
    #    break
    next
    diff.sfc <- st_difference(shape.sf, inters)
    if (length(diff.sfc) >= 1) {
      carp("shape_id: %s diff: %s", shape, length(diff.sfc))
      plot(st_geometry(diff.sfc), add = TRUE, col = "red", lwd = 3)
      #     break
    }
    stop("****")
  }
  write(tex, file = TEX, append = FALSE)
  close(TEX)
  carp(" texFic: %s", texFic)
  tex_pdflatex("star.tex")
}
# source("geo/scripts/transport.R");star_osm_jour_stops(force = TRUE)
star_osm_jour_stops <- function(reseau = "star", force = FALSE) {
  library(tidyverse)
  library(data.table)
  library(janitor)
  carp()
  config_xls(reseau)
  df <- osm_nodes_stop_read()
  carp("recherche des arrets sans public_transport")
  df1 <- df %>%
    filter(is.na(public_transport)) %>%
    glimpse()
  carp("recherche des plaforms sans ref")
  df1 <- df %>%
    filter(grepl("platform", public_transport)) %>%
    filter(is.na(ref.FR.STAR)) %>%
    glimpse()
}
#
# la comparaison des deux sources
#
# pour les routes
# source("geo/scripts/transport.R");star_osm_routes_gtfs_shapes()
star_osm_routes_gtfs_shapes <- function() {
  carp()
  transport_osm_routes_gtfs_shapes_df()
  transport_osm_routes_gtfs_shapes_diff()
}

transport_osm_routes_gtfs_shapes_df <- function() {
  library(tidyverse)
  library(sf)
  carp()
  shapes.sf <- tidytransit_shapes_sf_lire() %>%
    mutate(source = "shapes") %>%
    glimpse()
  routes.sf <- osm_routes_read() %>%
    dplyr::select(shape_id = ref.FR.STAR) %>%
    filter(!is.na(shape_id)) %>%
    glimpse()
  names(routes.sf$geometry) <- NULL
  names(shapes.sf$geometry) <- NULL

  # avec la géométrie
  carp("jointure")
  df <- dplyr::left_join(routes.sf %>% as.data.frame(), shapes.sf %>% as.data.frame(), by = c("shape_id" = "shape_id")) %>%
    glimpse()
  #
  # le tracé gtfs est inclus dans le buffer route osm
  carp("passage en Lambert 93 et buffer")
  nc1 <- st_transform(routes.sf, 2154) %>%
    st_buffer(50)
  nc2 <- st_transform(shapes.sf, 2154)
  df <- dplyr::left_join(nc1 %>% as.data.frame(), nc2 %>% as.data.frame(), by = c("shape_id" = "shape_id")) %>%
    filter(!is.na(source)) %>%
    glimpse()
  transport_save(df, "osm_routes_gtfs_shapes")
}
# source("geo/scripts/transport.R");transport_osm_routes_gtfs_shapes_diff()
transport_osm_routes_gtfs_shapes_diff <- function() {
  carp("contains")
  library(sf)
  library(tidyverse)
  df <- transport_read("osm_routes_gtfs_shapes") %>%
    arrange(shape_id)
  nc <- st_as_sf(df, geom_routes = st_sfc(df$geometry.x))
  nc$geom_shapes <- st_sfc(df$geometry.y)
  for (i in 1:nrow(df)) {
    nc1 <- nc[i, ]
    plot(st_geometry(nc1$geom_routes))
    plot(st_geometry(nc1$geom_shapes), add = TRUE)
    inters <- st_intersection(nc1$geom_shapes, nc1$geom_routes)
    plot(st_geometry(inters), add = TRUE, col = "blue", lwd = 3)
    #    carp('inters')
    #    glimpse(inters)
    #    carp('shapes')
    #    glimpse(nc1$geom_shapes)
    diff.sfc <<- st_difference(nc1$geom_shapes, inters)
    if (length(diff.sfc) >= 1) {
      carp("shape_id: %s", nc[[i, "shape_id"]])
      plot(st_geometry(diff.sfc), add = TRUE, col = "red", lwd = 3)
      #     break
    }
  }
}
transport_save <- function(obj, fic = "objets_stop") {
  dsn <- sprintf("%s/%s.Rds", transportDir, fic)
  saveRDS(obj, dsn)
  carp("dsn: %s", dsn)
  return(invisible(obj))
}
transport_read <- function(fic = "objets_stop") {
  dsn <- sprintf("%s/%s.Rds", transportDir, fic)
  obj <- readRDS(dsn)
  carp("dsn: %s", dsn)
  return(invisible(obj))
}
#
# pour les stops
# source("geo/scripts/transport.R");star_osm_stops_gtfs_stops()
star_osm_stops_gtfs_stops <- function(force_osm = TRUE) {
  library(tidyverse)
  carp()
  config_xls("star")
  #  tidytransit_gtfs_stops_sf()
  df1 <- reseau_osm_stops_gtfs_stops(force_osm = force_osm)
  carp("distance")
  misc_print(df1)
  rc <- osm_arrets_disused(force_osm = force_osm)
  carp("plus desservi par la star")
  misc_print(rc$autre)
  misc_print(rc$hs)
}
#
# pour le changement de référence des routes
# source("geo/scripts/transport.R");star_gtfs_routes()
star_gtfs_routes <- function(force_osm = TRUE) {
  library(tidyverse)
  carp()
  config_xls("rennes")
  osm.df <- overpass_get(query = "relations_bus_star", format = "csv", force = force_osm) %>%
    mutate(Ref = gsub("\\-[AB]$", "", `ref:network`)) %>%
    filter(ref == Ref) %>%
    mutate(Ref = gsub("^0*", "", `gtfs:shape_id`)) %>%
    mutate(Ref = gsub("\\-[AB].*", "", Ref)) %>%
    filter(ref != Ref) %>%
    glimpse()
}