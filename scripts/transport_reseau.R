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
# source("geo/scripts/transport.R");reseau_toto(force = FALSE)
reseau_toto <- function(force = TRUE) {
  library(tidyverse)
  library(rio)
  library(sf)
  carp()
  reseau_tpl_tex()
  reseau_osm_routes_shapes(force = force)
  tex_pdflatex(sprintf("%s_reseau_osm_routes_shapes.tex", Reseau))
}
#
# recopie des templates tex
# source("geo/scripts/transport.R");reseau_tpl_tex()
reseau_tpl_tex <- function(force = TRUE) {
  library(glue)
  carp()
  files <- list.files(tplDir, pattern = "reseau_.*_tpl.tex$", full.names = TRUE, ignore.case = TRUE, recursive = FALSE)
  for (file in files) {
    tex <- readLines(file)
    dsn <- str_replace(file, "reseau", Reseau)
    dsn <- str_replace(dsn, "_tpl.tex", ".tex")
    dsn <- str_replace(dsn, ".*/", sprintf("%s/", texDir))
    carp("dsn: %s", dsn)
    tex <- misc_glue(c("Reseau", "Config_network"), tex, .open = "{{", .close = "}}")
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
#  gtfs2osm_jour(force = force)
  Carp("Reseau: %s Config %s", Reseau, Config[[1, "shapes"]])
  if (Config_shapes != FALSE) {
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
}
#
########################################################################################
#
# pour les routes
#
# à partir des relations route=bus d'osm
# avec la carto dont celle du gtfs si disponible
# source("geo/scripts/transport.R");reseau_osm_routes_shapes_pdf()
reseau_osm_routes_shapes_pdf <- function(force = TRUE) {
  reseau_osm_routes_shapes(force = force);
  tex_pdflatex(sprintf("%s_reseau_osm_routes_shapes.tex", Reseau))
}
# source("geo/scripts/transport.R");r
reseau_osm_routes_shapes <- function(force = TRUE) {
  library(tidyverse)
  library(data.table)
  library(sf)
  library(janitor)
  carp()
  df <- osm_relations_route_bus_csv(force = force) %>%
    clean_names() %>%
    mutate(shape = transport_shape2fic(gtfs_shape_id)) %>%
    arrange(ref_network)
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
#    star.df <- star202210_supprime()
    stop("****")
    df <- df %>%
#    filter(grepl("^0", gtfs_shape_id)) %>%
      filter(ref %notin% star.df$ligne) %>%
#      filter(grepl("^08", gtfs_shape_id)) %>%
      filter(grepl("^0[01]", gtfs_shape_id)) %>%
      arrange(gtfs_shape_id) %>%
      glimpse()
  }
  if (Reseau == "rennes") {
#    stop("****")
    df <- df %>%
      mutate(ordre = gsub("\\D.*", "", gtfs_shape_id)) %>%
      arrange(ordre) %>%
      glimpse()
  }

#  stop("****")
  df1 <- df %>%
    dplyr::select(id, timestamp, user, ref_network, gtfs_shape_id)
  tex_df2kable(df1, suffixe = "lst", longtable = TRUE)
  lg.df <- tribble(~id, ~ref, ~ref_network, ~shape, ~osm_lg, ~shape_lg, ~inters_lg)
  lg.df <- tribble()
  for (i in 1:nrow(df)) {
    id <-  df[[i, "id"]]
    carp("i: %s/%s id: %s", i, nrow(df), id)
    shape <- df[[i, "shape"]]
    if (is.na(shape)) {
      carp("**** id: %s pas de shape", id)
      next
    }
    if (id != "2122348") {
#      next
    }
    dsn <- sprintf("%s/reseau_osm_routes_shapes_%s.pdf", imagesDir, id)
    rc <- carto_route_shape_stops_mapsf(id, shape)
    if (is.logical(rc)) {
      next
    }
    mga1 <<- rc
    lg.df <- lg.df %>%
      bind_rows(tibble(enframe(rc)))
    mga <<- lg.df
    if (rc$erreur != "") {
      next
    }
    dsn <- dev2pdf(suffixe = id, dossier = "images")
# pour le latex
#    tex <- append(tex, sprintf("\\mongraphique{images/reseau_routes_shapes_%s.pdf}", id))
    df[i, "shape"] <- shape
    df[i, "dsn_shape"] <- sprintf("%s/shape_stops_%s.gpx", josmDir, shape)
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
    mutate(across(5:11, as.integer)) %>%
    glimpse()
  carp("distance départ et arrivée")
  df1 <- lg.df  %>%
    filter(point1_distance > 1500 & point9_distance > 1500) %>%
    arrange(ref, ref_network) %>%
    glimpse()
  carp("longueur en commun")
  df1 <- lg.df  %>%
    mutate(taux = (inters_lg * 100) /shape_lg) %>%
    filter(taux < 80) %>%
    arrange(taux, ref, ref_network) %>%
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
    clean_names() %>%
    glimpse()
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
  if (nrow(df12) > 0) {
    level0 <- ""
    for (i12 in 1:nrow(df12)) {
      dsn <- sprintf("%s/reseau_relations_route_level0_%s.txt", osmDir, df12[i12, "ref_network"])
      level0 <- c(level0, read_lines(file = dsn))
    }
    dsn <- sprintf("%s/reseau_relations_route_valid.txt", osmDir)
    write(level0, dsn)
    carp("dsn: %s", dsn)
  }
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
#
# les départs/arrivées osm versus shape
# source("geo/scripts/transport.R");reseau_da()
reseau_da <- function(force = TRUE, force_osm = TRUE) {
  library(tidyverse)
  library(sf)
  df1 <- overpass_get(query = "relations_route_bus_network", format = "csv", force = force)
  for (i1 in 1:nrow(df1)) {
    id <- df1[[i1, "@id"]]
    shape <- df1[[i1, "gtfs:shape_id"]]
    rc <- reseau_route_shape_da(id, shape)
    df1[i1, "rc"] <- rc
  }
  df2 <- df1 %>%
    filter(rc == -2) %>%
    dplyr::select("ref", "@id", "ref:network", "gtfs:shape_id") %>%
    arrange(ref) %>%
    mutate(sens = str_sub(`ref:network`, -1)) %>%
    glimpse() %>%
    dplyr::select(-"ref:network") %>%
    pivot_wider(
      names_from = c("sens"),
      values_from = c("@id", "gtfs:shape_id")
    )
  misc_print(df2)
  sauve_rds(df2)
}
# source("geo/scripts/transport.R");reseau_da_change()
reseau_da_change <- function(OsmChange = FALSE) {
  carp()
  df2 <- lire_rds("reseau_da") %>%
    glimpse()
  OsmChange <<- OsmChange
  osm <- ""
  nb_changes <- 0
  for (i2 in 1:nrow(df2)) {
    id <- df2[[i2, "@id_A"]]
    shape  <- df2[[i2, "gtfs:shape_id_B"]]
    tags.df <- tribble(~name, ~value,
"gtfs:shape_id", shape
)
    type <- "relation"
    o <- osmchange_object_modify_tags(id = id, type = type, tags = tags.df, Change = FALSE)
    nb_changes <- nb_changes + 1
#    writeLines(o)
    osm <- sprintf("%s\n%s", osm, o)
    id <- df2[[i2, "@id_B"]]
    shape  <- df2[[i2, "gtfs:shape_id_A"]]
    tags.df <- tribble(~name, ~value,
"gtfs:shape_id", shape
)
    type <- "relation"
    o <- osmchange_object_modify_tags(id = id, type = type, tags = tags.df, Change = FALSE)
    nb_changes <- nb_changes + 1
#    writeLines(o)
    osm <- sprintf("%s\n%s", osm, o)
  }
  osm <- paste(osm, "\n", collapse = "")
#  writeLines(osm);stop("****")
  changeset_id <- osmapi_put("modify", text = osm, comment = "maj des attributs gtfs")
}
#
# les départs/arrivées osm versus shape
# l'id de la relation et l'identification du shape
# quimper
# source("geo/scripts/transport.R");reseau_route_shape_da(id = "4050450", shape = "32236")
reseau_route_shape_da <- function(id, shape, force = TRUE, force_osm = TRUE) {
  library(tidyverse)
  library(sf)
  carp("id: %s shape: %s", id, shape)
  rc <- osmapi_get_transport(ref = id, force = force, force_osm = force_osm)
  glimpse(rc)
  if (rc$cr != "") {
    carp("***id: %s cr: %s", id, rc$cr)
    return(invisible(rc))
  }
  if (st_is_empty(rc$ways.sf[1,]) ) {
    carp("***id: %s", id)
    return(invisible(rc))
  }
  osm.sf <- rc$ways.sf %>%
    st_transform(2154)
  osm_points.sf <<- st_sf(st_cast(st_geometry(osm.sf), "POINT"))
  dsn_shape <- sprintf("%s/shape_%s.gpx", josmDir, shape)
  if (! file.exists(dsn_shape)) {
    carp("shape absent: %s", dsn_shape)
    return(-1)
  }
  shape.sf <- st_read(dsn_shape, layer = "tracks", quiet = TRUE) %>%
    st_transform(2154)
  shape_points.sf <<- st_sf(st_cast(st_geometry(shape.sf), "POINT"))
  osm_d <- osm_points.sf[1, ]
  osm_a <- osm_points.sf %>% slice(n())
  shape_d <- shape_points.sf[1, ]
  shape_a <- shape_points.sf %>% slice(n())
  dd <- as.integer(st_distance(osm_d, shape_d))
  da <- as.integer(st_distance(osm_d, shape_a))
  ad <- as.integer(st_distance(osm_a, shape_d))
  aa <- as.integer(st_distance(osm_a, shape_a))
  if ((dd+aa) > (da+ad)) {
    carp("inversion tracé id: r%s", id)
    return(-2)
  }
  return(0)
}