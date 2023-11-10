# <!-- coding: utf-8 -->
#
# le réseau de bus
#
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
## script chapeau pour un réseau
#
# le réseau est configuré en variable globale dans transport.R
#
# source("geo/scripts/transport.R");reseau_jour()
reseau_jour <- function(force = TRUE) {
  library(tidyverse)
  library(rio)
  library(sf)
  carp()
  reseau_tpl_tex()
  if (grepl("MOBIBREIZH", Config[1, "gtfs_dir"])) {
    mobibreizh_gtfs_reseaux(reseau = Reseau)
  }
  reseau_osm_jour(force = force)
  if (is.na(Config[1, "gtfs_dir"])) {
    return()
  }
  reseau_gtfs_jour(force = force)
  reseau_diff_jour(force = force)
}
# source("geo/scripts/transport.R");reseau_toto()
reseau_toto <- function(force = TRUE) {
  library(tidyverse)
  library(rio)
  library(sf)
  carp()
  reseau_tpl_tex(reseau = Reseau)
  reseau_osm_routes_shapes(force = TRUE)
  tex_pdflatex(sprintf("%s_reseau_osm_routes_shapes.tex", Reseau))
}
#
# recopie des templates tex
# source("geo/scripts/transport.R");reseau_tpl_tex()
reseau_tpl_tex <- function(force = TRUE) {
  carp()
  files <- list.files(tplDir, pattern = "reseau.*_tpl.tex$", full.names = TRUE, ignore.case = TRUE, recursive = FALSE)
  for (file in files) {
    tex <- readLines(file)
    dsn <- str_replace(file, "reseau", Reseau)
    dsn <- str_replace(dsn, "_tpl.tex", ".tex")
    dsn <- str_replace(dsn, ".*/", sprintf("%s/", texDir))
    carp("dsn: %s", dsn)
    write(tex, file = dsn, append = FALSE)
  }
  from <- sprintf("%s/misc.tex", tplDir)
  to <- sprintf("%s/misc.tex", texDir)
  file.copy(from = from, to = to, overwrite = TRUE)
}
# source("geo/scripts/transport.R");reseau_diff_jour(force = TRUE)
reseau_diff_jour <- function(force = TRUE) {
  library(tidyverse)
  library(data.table)
  library(janitor)
  gtfs2osm_jour(force = force)
  if (Config[[1, "shapes"]] != "FALSE") {
#    reseau_osm_routes_tag_shape(force = force)
#    tex_pdflatex(sprintf("%s_diff.tex", Reseau))
    reseau_osm_routes_shapes(force = force)
    tex_pdflatex(sprintf("%s_reseau_osm_routes_shapes.tex", Reseau))
  }
}
# source("geo/scripts/transport.R");reseau_gtfs_jour()
reseau_gtfs_jour <- function(force = TRUE) {
  library(tidyverse)
  reseau_tpl_tex()
  tidytransit_jour(force = force)
}
# source("geo/scripts/transport.R");reseau_osm_jour(force = TRUE, force_members = TRUE)
reseau_osm_jour <- function(force = TRUE, force_members = TRUE, force_osm = TRUE) {
  library(tidyverse)
  library(data.table)
  library(janitor)
  osm_jour(force = force)
  tex_pdflatex(sprintf("%s_osm.tex", Reseau))
}




#
########################################################################################
#
# pour les routes
#
# à partir des relations route=bus d'osm
# avec la carto dont celle du gtfs si disponible
# source("geo/scripts/transport.R");reseau_osm_routes_shapes(force = FALSE);tex_pdflatex(sprintf("%s_reseau_osm_routes_shapes.tex", Reseau))
reseau_osm_routes_shapes <- function(force = TRUE) {
  library(tidyverse)
  library(data.table)
  library(sf)
  library(janitor)
  carp()
  dsn <- osm_relations_route_bus_csv(force = force)
  df <- fread(dsn, encoding = "UTF-8") %>%
    as.data.table() %>%
    clean_names()
  texFic <- sprintf("%s/%s", imagesDir, "reseau_osm_routes_shapes.tex")
  TEX <- file(texFic)
  tex <- sprintf("<!-- coding: utf-8 -->
%s ", format(Sys.time(), "%d/%m/%Y %H:%M:%S"))
#
# le template tex
  dsn <- sprintf("%s/routes_shapes_tpl.tex", tplDir)
  template <- readLines(dsn) %>%
    glimpse()
  if (Reseau == "star") {
    star.df <- star202210_supprime()
    df <- df %>%
#    filter(grepl("^0", gtfs_shape_id)) %>%
      filter(ref %notin% star.df$ligne) %>%
#      filter(grepl("^08", gtfs_shape_id)) %>%
      filter(grepl("^0[01]", gtfs_shape_id)) %>%
      arrange(gtfs_shape_id) %>%
      glimpse()
  }
#  stop("****")
  df1 <- df %>%
    dplyr::select(id, timestamp, user, ref_network, gtfs_shape_id) %>%
    arrange(gtfs_shape_id)
  tex_df2kable(df1, suffixe = "lst", longtable = TRUE)
  lg.df <- tribble(~id, ~ref, ~ref_network, ~shape, ~osm_lg, ~shape_lg, ~inters_lg)
  lg.df <- tribble()
  for (i in 1:nrow(df)) {
    carp("i: %s/%s", i, nrow(df))
    id <-  df[[i, "id"]]
    shape <- df[[i, "gtfs_shape_id"]]
    if (is.na(shape)) {
      carp("**** id: %s pas de shape", id)
      next
    }
    dsn <- sprintf("%s/reseau_osm_routes_shapes_%s.pdf", imagesDir, id)
    rc <- carto_route_shape_mapsf(id, shape)
    lg.df <- lg.df %>%
      bind_rows(as_tibble(rc))
    dsn <- dev2pdf(suffixe = id, dossier = "images")
# pour le latex
#    tex <- append(tex, sprintf("\\mongraphique{images/reseau_routes_shapes_%s.pdf}", id))
    df[i, "shape"] <- shape
    df[i, "dsn_shape"] <- shape
    tpl <- tex_df2tpl(df, i, template)
    tpl <- escapeLatexSpecials(tpl)
    tex <- append(tex, tpl)
#    break
  }
  tidytransit_sauve(lg.df, "reseau_osm_routes_shapes")
  write(tex, file = TEX, append = FALSE)
  close(TEX)
  carp(" texFic: %s", texFic)
}
#
# source("geo/scripts/transport.R");reseau_osm_routes_shapes_tex(force = FALSE)
reseau_osm_routes_shapes_tex <- function(force = TRUE) {
  library(tidyverse)
  library(data.table)
  library(sf)
  library(janitor)
  carp()
  lg.df <- tidytransit_lire("reseau_osm_routes_shapes") %>%
    mutate(across(5:9, as.integer)) %>%
    filter(point1_distance > 150 | point9_distance > 150) %>%
    glimpse()
}
#
########################################################################################
#
#
## pour les stops
#
# la partie osm
#
# source("geo/scripts/transport.R");reseau_osm_jour_stops(force = TRUE)
reseau_osm_jour_stops <- function(reseau = "star", force = TRUE) {
  library(tidyverse)
  library(data.table)
  library(janitor)
  carp()
  config_xls(reseau)
#  osm_jour_stops(force_osm = force)
  reseau_diff_stops(force_osm = force)
}
#
# rapprochement osm # gtfs
# source("geo/scripts/transport.R");reseau_valid_stops(force_osm = FALSE)
reseau_valid_stops <- function(force_osm = TRUE) {
  library(tidyverse)
  library(data.table)
  library(janitor)
  carp()
  titre <- "reseau_valid_stops"
  html <- misc_html_titre(titre)
  html <- misc_html_append(html, sprintf("<h1>%s OSM</h1>", Reseau))
  gtfs_stops.sf <- tidytransit_lire("tidytransit_stops_sf") %>%
    mutate(gtfs = "gtfs") %>%
    glimpse()
  if ( Reseau == "bordeaux") {
    gtfs_stops.sf$stop_id <- gtfs_stops.sf$stop_code
    gtfs_stops.sf %>%
      filter(stop_id == "CVIN") %>%
      glimpse()
    stop("****")
  }
  zoom <- "left=%0.5f&right=%0.5f&top=%0.5f&bottom=%0.5f"
  select <- "%s%s"
  osm_stops.df <- overpass_get(query = "bus_stop_area", format = "csv", force = force_osm) %>%
    rename(k_ref = Config[1, 'k_ref']) %>%
    filter(k_ref != "") %>%
    clean_names() %>%
    mutate(k_ref = sprintf("%s", k_ref)) %>%
    glimpse()
  osm_stops.sf <- st_as_sf(osm_stops.df, coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
    dplyr::select(stop_id = k_ref, name, osm_id = id, osm_type = type, osm_lon = lon, osm_lat = lat) %>%
    mutate(select = sprintf(select, `osm_type`, `osm_id`)) %>%
    mutate(zoom = sprintf(zoom, `osm_lon` - .002, `osm_lon` + .002, `osm_lat` + .001, `osm_lat` - .001)) %>%
    mutate(josm = sprintf("<a href='http://127.0.0.1:8111/load_and_zoom?%s&select=%s'>josm</a>", zoom, select)) %>%
    dplyr::select(-select, -zoom) %>%
    mutate(osm = "osm")
  Encoding(osm_stops.sf$name) <- "UTF-8"


# sans géométrie
  df <- dplyr::full_join(osm_stops.sf %>% st_drop_geometry(), gtfs_stops.sf %>% st_drop_geometry(), by = c("stop_id"="stop_id")) %>%
    glimpse()
  html <- misc_html_append(html, "<h2>les stops osm absents du gtfs</h2>")
  osm_inc.df <- df %>%
    filter(is.na(gtfs)) %>%
    dplyr::select(stop_id, osm_type, osm_id, name, osm_lon, osm_lat, josm) %>%
    arrange(stop_id) %>%
    glimpse()
  transport_save(osm_inc.df, "reseau_diff_stops_osm_inc")
  html <- misc_html_append(html, sprintf("<h3>nb: %s</h3>", nrow(osm_inc.df)))
  html <- misc_html_append_df(html, osm_inc.df)
#
  html <- misc_html_append(html, "<h2>les stops gtfs absents d'osm</h2>")
  gtfs_inc.df <- df %>%
#    mutate(stop_id=as.numeric(stop_id)) %>%
#    filter(stop_id < 15000) %>%
    filter(is.na(osm)) %>%
    dplyr::select(stop_id, stop_name, stop_code, stop_desc, lat, lon, location_type) %>%
    arrange(stop_id) %>%
    mutate(zoom = sprintf(zoom, `lon` - .002, `lon` + .002, `lat` + .001, `lat` - .001)) %>%
    mutate(josm = sprintf("<a href='http://127.0.0.1:8111/load_and_zoom?%s'>josm</a>", zoom)) %>%
    dplyr::select(-zoom) %>%
    glimpse()
  html <- misc_html_append(html, sprintf("<h3>nb: %s</h3>", nrow(gtfs_inc.df)))
  html <- misc_html_append_df(html, gtfs_inc.df)
#
# avec la géométrie
  html <- misc_html_append(html, "<h2>distance > 50 mètres</h2>")
  carp("distance > 50 mètres")
  df <- dplyr::full_join(osm_stops.sf %>% as.data.frame(), gtfs_stops.sf %>% as.data.frame(), by=c('stop_id'='stop_id'))
  df$d <- as.numeric(st_distance(df$geometry.x, df$geometry.y, by_element = TRUE))
  df1 <- df %>%
    dplyr::select(stop_id, name, stop_name, osm_id, d) %>%
    filter(d > 50) %>%
    arrange(stop_id)
  html <- misc_html_append(html, sprintf("<h3>nb: %s</h3>", nrow(df1)))
  html <- misc_html_append_df(html, df1)
#
  dsn <- sprintf("%s/%s.html", webDir, titre)
  write(html, dsn)
  carp("dsn: %s", dsn)
  url <- sprintf("http://localhost/transport/%s.html", titre)
  browseURL(
    url,
    browser = "C:/Program Files/Mozilla Firefox/firefox.exe"
  )
  return(invisible(df1))
}


reseau_toto_html <- function() {
# les stops à partir des relations route
  html <- misc_html_append(html, "<h2>Les stops des relations route du réseau</h2>")
  network.sf <- osm_bus_stop_network(force = force_osm) %>%
    glimpse()
  network.df <- network.sf %>%
    mutate(lon = st_coordinates(.)[, 'X']) %>%
    mutate(lat = st_coordinates(.)[, 'Y']) %>%
    st_drop_geometry() %>%
    rename(k_ref = !!k_ref) %>%
    clean_names()

#
  html <- misc_html_append(html, "<h3>platform way</h3>")
  df1 <- network.df %>%
    filter(type == "way") %>%
    glimpse()
  html <- misc_html_append_df(html, df1)

#
  html <- misc_html_append(html, "<h3>pb de tags platform stop</h3>")
  df1 <- network.df %>%
    filter(highway != "bus_stop" | public_transport != "platform") %>%
    glimpse()
  df1 <- df1 %>%
    filter(bus != "yes" | public_transport != "stop_position") %>%
    glimpse()
  html <- misc_html_append_df(html, df1)
#
  html <- misc_html_append(html, "<h2>absence de k_ref sur les platform</h2>")
  df1 <- network.df %>%
    filter(highway == "bus_stop" & public_transport == "platform") %>%
    filter(is.na(k_ref) | k_ref == "") %>%
    glimpse()
 #  josm <- "http://127.0.0.1:8111/load_and_zoom?left=8.19&right=8.20&top=48.605&bottom=48.590&select=node413602999
  zoom <- "left=%0.5f&right=%0.5f&top=%0.5f&bottom=%0.5f"
  select <- "node%s"
  df2 <- df1 %>%
    mutate(select = sprintf(select, osm_id)) %>%
    mutate(zoom = sprintf(zoom, lon - .001, lon + .001, lat + .001, lat - .001)) %>%
    mutate(josm = sprintf("<a href='http://127.0.0.1:8111/load_and_zoom?%s&select=%s'>josm</a>", zoom, select)) %>%
    dplyr::select(-select, -zoom)

  html <- misc_html_append_df(html, df2)
#
  html <- misc_html_append(html, "<h3>k_ref en double sur les platform</h3>")
  df1 <- network.df %>%
    filter(highway == "bus_stop" & public_transport == "platform") %>%
    group_by(k_ref) %>%
    summarize(nb = n()) %>%
    filter(nb > 1) %>%
    glimpse()
  df2 <- network.df %>%
    filter(k_ref %in% df1$k_ref) %>%
    arrange(k_ref, osm_id)
  html <- misc_html_append_df(html, df2)
}


#
## les relations route
#
# source("geo/scripts/transport.R"); df1 <- reseau_relations_route_valid(force = FALSE) %>% glimpse()
reseau_relations_route_valid <- function(force = TRUE) {
  doc <- overpass_get(query = "relations_route_bus_network", format = "xml", force = force) %>%
    glimpse()
  objects <- xml2::xml_find_all(doc, "//relation")
  osm.df <- osmapi_objects_tags(objects)
  osm.df <- osm.df %>%
    clean_names()
#  View(osm.df)
  df1 <- osm.df %>%
    group_by(ref_network) %>%
    summarize(nb = n()) %>%
    filter(nb > 1)
  if (nrow(df1) > 0) {
    misc_print(df1)
    df2 <- osm.df %>%
      filter(ref_network %in% df1$ref_network) %>%
      dplyr::select(id, ref_network, name)
    misc_print(df2)
    stop("****")
  }
  gtfs.df <- transport_read("tidytransit_routes_shapes_stops") %>%
    group_by(ref_network) %>%
    arrange(ref_network, desc(nb), desc(nb_stops)) %>%
    filter(row_number()==1) %>%
    glimpse()
  carp("les routes inconnues du gtfs")
  df11 <- osm.df %>%
    dplyr::select(ref, ref_network, name) %>%
    filter(ref_network %notin% gtfs.df$ref_network)
  misc_print(df11)
  carp("les routes inconnues d'osm")
  df12 <- gtfs.df %>%
    filter(ref_network %notin% osm.df$ref_network)
  df13 <- df12 %>%
    dplyr::select(ref_network, route_id, route_long_name) %>%
    misc_print(.)
  level0 <- ""
  for (i12 in 1:nrow(df12)) {
    dsn <- sprintf("%s/reseau_relations_route_level0_%s.txt", osmDir, df12[i12, "ref_network"])
    level0 <- c(level0, read_lines(file = dsn))
  }
  dsn <- sprintf("%s/reseau_relations_route_valid.txt", osmDir)
  write(level0, dsn)
  carp("dsn: %s", dsn)
  return(invisible())
}
#
## les relations route_master
#

# source("geo/scripts/transport.R"); df1 <- reseau_relations_routemaster_valid(force = FALSE) %>% glimpse()
reseau_relations_routemaster_valid <- function(force = TRUE) {
  doc <- overpass_get(query = "relations_routemaster_bus_network", format = "xml", force = force) %>%
    glimpse()
  objects <- xml2::xml_find_all(doc, "//relation")
  osm.df <- osmapi_objects_tags(objects) %>%
    glimpse()
#  View(osm.df)
  df1 <- osm.df %>%
    group_by(`ref:network`) %>%
    summarize(nb = n()) %>%
    filter(nb > 1)
  if (nrow(df1) > 0) {
    misc_print(df1)
    stop("****")
  }
  tt <- tidytransit_lire("gtfs")
  gtfs.df <- tt$routes %>%
    glimpse()
  osm.df <- osm.df %>%
    clean_names()
  df11 <- osm.df %>%
    dplyr::select(ref, ref_network, name) %>%
    filter(ref_network %notin% gtfs.df$route_id)
  misc_print(df11)
  df12 <- gtfs.df %>%
    dplyr::select(route_id, route_long_name) %>%
    filter(route_id %notin% osm.df$ref_network)
  misc_print(df12)
  level0 <- ""
  for (i12 in 1:nrow(df12)) {
    dsn <- sprintf("%s/reseau_relations_routemaster_level0_%s.txt", osmDir, df12[i12, "route_id"])
    level0 <- c(level0, read_lines(file = dsn))
  }
  dsn <- sprintf("%s/reseau_relations_routemaster_valid.txt", osmDir)
  write(level0, dsn)
  carp("dsn: %s", dsn)
  return(invisible())
}