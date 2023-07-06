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
reseau_jour <- function(reseau = Reseau, force = TRUE) {
  library(tidyverse)
  library(rio)
  library(sf)
  carp()
  config_xls(reseau)
  reseau_tpl_tex(reseau = reseau)
  if (grepl("MOBIBREIZH", Config[1, "gtfs_dir"])) {
    mobibreizh_gtfs_reseaux(reseau = reseau)
  }
  reseau_osm_jour(reseau = reseau, force = force)
  if (is.na(Config[1, "gtfs_dir"])) {
    return()
  }
  reseau_gtfs_jour(reseau = reseau, force = force)
  reseau_diff_jour(reseau = reseau, force = force)
}
#
# recopie des templates tex
# source("geo/scripts/transport.R");reseau_tpl_tex(reseau = Reseau)
reseau_tpl_tex <- function(reseau = "star", force = FALSE) {
  carp()
  config_xls(reseau)
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
reseau_diff_jour <- function(reseau = Reseau, force = FALSE) {
  library(tidyverse)
  library(data.table)
  library(janitor)
  carp("reseau: %s", reseau)
  config_xls(reseau)
  if (Config[[1, "shapes"]] != "FALSE") {
    reseau_tpl_tex(reseau = reseau)
    reseau_osm_routes_tag_shape(reseau = reseau, force = force)
    reseau_osm_routes_shapes(reseau = reseau, force = force)
    tex_pdflatex(sprintf("%s_diff.tex", reseau))
  }
}
# source("geo/scripts/transport.R");reseau_gtfs_jour(reseau = Reseau)
reseau_gtfs_jour <- function(reseau = Reseau, force = TRUE) {
  library(tidyverse)
  carp("reseau: %s", reseau)
  config_xls(reseau)
  reseau_tpl_tex(reseau = reseau)
  tidytransit_jour(reseau = reseau, force = force)
}
# source("geo/scripts/transport.R");reseau_osm_jour(force = TRUE, force_members = TRUE)
reseau_osm_jour <- function(reseau = Reseau, force = TRUE, force_members = TRUE, force_osm = TRUE) {
  library(tidyverse)
  library(data.table)
  library(janitor)
  reseau <- Reseau
  reseau_tpl_tex(reseau = reseau)
  osm_relations_route_bus_verif(reseau = reseau, force = force)
  osm_relations_route_members(reseau = reseau, force = force_members, force_osm = force_osm, osrm = FALSE)
  osm_relations_route_members_valid(force = force_members, force_osm = force_osm)
  tex_pdflatex(sprintf("%s_osm.tex", reseau))
}
# source("geo/scripts/transport.R");reseau_osm_routes_jour(force = TRUE)
reseau_osm_routes_jour <- function(reseau = Reseau, force = TRUE) {
  library(tidyverse)
  library(data.table)
  library(janitor)
  carp("reseau: %s", reseau)
  config_xls(reseau)
  reseau_tpl_tex(reseau = reseau)
  reseau_osm_routes_refs(reseau = Reseau, force = force)
  tex_pdflatex(sprintf("%s_routes.tex", reseau))
}

#
# comparaison à partir des routes gtfs
# source("geo/scripts/transport.R");reseau_diff_routes_tags(reseau = Reseau, force = FALSE)
reseau_diff_routes_tags <- function(reseau = "star", force = FALSE) {
  library(tidyverse)
  library(data.table)
  library(sf)
  library(janitor)
  carp()
  config_xls(reseau)
# osm
  osm.df <- overpass_get(query = "relations_route_bus_network", format = "csv", force = force) %>%
    filter(`gtfs:shape_id` != "")
#  stop("****")
# le gtfs
  dsn <- sprintf("%s/%s", texDir, "tidytransit_gtfs_routes_shapes_stops.csv")
  shapes.df <- fread(dsn, encoding = "UTF-8") %>%
    mutate(route_color = sprintf("#%s", toupper(route_color))) %>%
    mutate(route_text_color = sprintf("#%s", toupper(route_text_color))) %>%
    mutate(first_city = toupper(first_city)) %>%
    mutate(last_city = toupper(last_city)) %>%
    mutate(first_name = str_glue('{first_city} {first_name}')) %>%
    mutate(last_name = str_glue('{last_city} {last_name}')) %>%
    mutate(gtfs_name = str_glue('{Config_route_name} {route_short_name} : {first_name} -> {last_name}')) %>%
    mutate(d = dplyr::recode(direction_id,
      "0" = "A",
      "1" = "B"
      )) %>%
    mutate(ref_network = sprintf("%s-%s", route_short_name, d)) %>%
    glimpse()

  champs.df <- tribble(
    ~osm, ~gtfs,
    "gtfs:shape_id", "shape_id",
    "name", "gtfs_name",
    "ref:network", "ref_network",
    "from", "first_name",
    "to", "last_name",
    "colour", "route_color",
    "text_colour", "route_text_color",
  )
  df1 <- osm.df %>%
    left_join(shapes.df, by = c("gtfs:shape_id" = "shape_id")) %>%
    filter(! is.na(route_short_name)) %>%
    mutate(shape_id = `gtfs:shape_id`) %>%
    arrange(shape_id) %>%
    glimpse()
  for (i1 in 1:nrow(df1)) {
    carp("i1: %s", i1)
    tags.osm <- list()
    tags.gtfs <- list()
    tags.df <- champs.df
    tags.df$OSM <- NA
    tags.df$GTFS <- NA
    for (ic in 1:nrow(champs.df)) {
      tags.df[ic, "OSM"] <- df1[[i1, tags.df[[ic, "osm"]]]]
      tags.df[ic, "GTFS"] <- df1[[i1, tags.df[[ic, "gtfs"]]]]
    }
    if (identical(tags.df$OSM, tags.df$GTFS)) {
      next
    }
    misc_print(tags.df)
    tags.df <- tags.df %>%
      dplyr::select(name = osm, value = GTFS)
    id <- df1[[i1, "@id"]]
    type <- "relation"
    osmchange_object_modify_tags(id = id, type = type, tags = tags.df)
  }
}
#
# comparaison à partir des routes osm
# source("geo/scripts/transport.R");reseau_osm_routes_tag_shape(reseau = Reseau, force = TRUE)
reseau_osm_routes_tag_shape <- function(reseau = "bibus", force = FALSE) {
  library(tidyverse)
  library(data.table)
  library(sf)
  library(janitor)
  carp()
  config_xls(reseau)
  df <- overpass_get(query = "relations_route_bus_network", format = "csv", force = force) %>%
    clean_names() %>%
    glimpse()
  carp("les shapes en double")
  df1 <- df %>%
    group_by(gtfs_shape_id) %>%
    summarize(nb = n()) %>%
    filter(nb > 1) %>%
    glimpse()
  df2 <- df %>%
    filter(gtfs_shape_id %in% df1$gtfs_shape_id) %>%
    dplyr::select(ref_network, id, name, from, to, ref, gtfs_shape_id) %>%
    arrange(gtfs_shape_id) %>%
    glimpse()
  tex_df2kable(df2, suffixe = "doublon", longtable = TRUE)
  carp("les shapes inconnus")
  tt <- tidytransit_lire()
  shapes.df <- tt$shapes %>%
    glimpse() %>%
    distinct(shape_id) %>%
    mutate(source = "gtfs") %>%
    glimpse()
  df11 <- df %>%
    filter(gtfs_shape_id != "") %>%
    left_join(shapes.df, by = c("gtfs_shape_id" = "shape_id"), relationship = "many-to-many")
  df12 <- df11 %>%
    filter(is.na(source)) %>%
    dplyr::select(gtfs_shape_id, ref_network, id, name, from, to, ref) %>%
    arrange(gtfs_shape_id) %>%
    mutate(INDEX = row_number()) %>%
    glimpse()
  df121 <- df12 %>%
    mutate(id = as.character(id)) %>%
    dplyr::select(INDEX, gtfs_shape_id, ref_network, id, name)
  df122 <- df12 %>%
    mutate(name = sprintf("%s => %s", from, to)) %>%
    dplyr::select(INDEX, gtfs_shape_id, ref_network, id, name) %>%
    mutate(gtfs_shape_id = "", ref_network = "", id = "") %>%
    glimpse()
  df123 <- df121 %>%
    bind_rows(df122) %>%
    arrange(INDEX) %>%
    glimpse()
#  stop("****")
  tex_df2kable(df123, suffixe = "inconnu", longtable = TRUE)
#
# pour trouver le shape potentiel
  df13 <- df12 %>%
    dplyr::select(id, ref_network, gtfs_shape_id) %>%
#    filter(! grepl("^2\\d\\d", ref)) %>%
#    filter(! grepl("^08", gtfs_shape_id)) %>%
    glimpse()
  dsn <- sprintf("%s/%s", texDir, "tidytransit_gtfs_routes_shapes_stops.csv")
  shapes.df <- fread(dsn, encoding = "UTF-8") %>%
    glimpse()
  df14 <- df13 %>%
    left_join(shapes.df, by = c("ref_network" = "ref_network")) %>%
#    group_by(gtfs_shape_id, id, ref_network) %>%
#    summarize(shapes = paste0(shape_id, collapse = ",")) %>%
    arrange(gtfs_shape_id, desc(nb)) %>%
    dplyr::select(id, gtfs_shape_id, shape_id, nb) %>%
    glimpse()
  tex_df2kable(df14, suffixe = "potentiel", longtable = TRUE)
  df15 <- df14 %>%
    mutate(id = sprintf("<a href='http://level0.osmz.ru/?url=relation/%s' target='_blank'>%s</a>", id, id))
  titre <- "reseau_osm_routes_tag_shape"
  misc_html_df2fic(df15, titre)
#
# pour changer en automatique
  df21 <- df14 %>%
    filter(! is.na(shape_id)) %>%
    group_by(id) %>%
    filter(row_number() == 1)
  if (nrow(df21) == 0) {
    return(invisible(df14))
  }
  misc_print(df21)
  for (i21 in 1:nrow(df21)) {
    tags <- list(
      "gtfs:shape_id" = df21[[i21, "shape_id"]]
    )
    url <- osmchange_object_modify_tags(id = df21[[i21, "id"]], type= "relation", tags = tags)
    carp("url: %s", url)
#    next
#    browseURL(
#      url,
#      browser = "C:/Program Files/Mozilla Firefox/firefox.exe"
#    )
  }
  return(invisible(df14))
}

#
# source("geo/scripts/transport.R");reseau_osm_routes_shapes(reseau = Reseau, force = FALSE);tex_pdflatex(sprintf("%s_diff.tex", Reseau))
reseau_osm_routes_shapes <- function(reseau = "star", force = TRUE) {
  library(tidyverse)
  library(data.table)
  library(sf)
  library(janitor)
  carp()
  config_xls(reseau)
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
  if (reseau == "star") {
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
  for (i in 1:nrow(df)) {
    carp("i: %s/%s", i, nrow(df))
    id <-  df[i, "id"]
    shape <- df[[i, "gtfs_shape_id"]]
    dsn <- sprintf("%s/reseau_osm_routes_shapes_%s.pdf", imagesDir, id)
#    glimpse(df[i, ]);stop("****")
#    osmapi_get_members_platform(ref = id, force = force)
    osm.sf <- osmapi_get_ways(ref = id, force = force, force_osm = force) %>%
      st_transform(2154)
    add <- FALSE
    if (! st_is_empty(osm.sf[1,]) ) {
      plot(st_geometry(osm.sf), col = "blue", lwd = 3)
      add <- TRUE;
    }

#    stop("****")
    dsn_shape <- sprintf("%s/shape_%s.gpx", josmDir, shape)
    if (! file.exists(dsn_shape)) {
      title(sub = "shape absent")
#      confess("*** dsn_shape: %s", dsn_shape)
    } else {
      shape.sf <- st_read(dsn_shape, layer = "tracks", quiet = TRUE) %>%
        st_transform(2154)
      plot(st_geometry(shape.sf), add = add, col = "green", lwd = 3)
      inters <- st_intersection(st_buffer(shape.sf, 10), osm.sf)
      plot(st_geometry(inters), add = TRUE, col = "grey", lwd=3)
    }
    titre <- sprintf("relation: %s shape: %s", id, shape)
    title(titre)
    legend("topleft",
      , legend=c("osm", "gtfs", "inters")
      , col=c("blue", "green", "grey")
      , lwd = 3, lty = 1, cex = 0.8, box.lty = 0
    )
    dsn <- dev2pdf(suffixe = id, dossier = "images")
# pour le latex
#    tex <- append(tex, sprintf("\\mongraphique{images/reseau_routes_shapes_%s.pdf}", id))
    df[i, "shape"] <- shape
    df[i, "dsn_shape"] <- dsn_shape
    tpl <- tex_df2tpl(df, i, template)
    tpl <- escapeLatexSpecials(tpl)
    tex <- append(tex, tpl)
#    break
    next
    diff.sfc <- st_difference(shape.sf, inters)
    if (length(diff.sfc) >= 1 ) {
      carp('shape_id: %s diff: %s', shape, length(diff.sfc))
      plot(st_geometry(diff.sfc), add = TRUE, col = 'red', lwd = 3)
 #     break
    }
    stop("****")
  }
  write(tex, file = TEX, append = FALSE)
  close(TEX)
  carp(" texFic: %s", texFic)
}

#
# la comparaison des deux sources
#
# pour les routes
# source("geo/scripts/transport.R");reseau_osm_routes_gtfs_shapes()
reseau_osm_routes_gtfs_shapes <- function() {
  carp()
  transport_osm_routes_gtfs_shapes_df()
  transport_osm_routes_gtfs_shapes_diff()
}

transport_osm_routes_gtfs_shapes_df <- function() {
  library(tidyverse)
  library(sf)
  carp()
  shapes.sf <- tidytransit_shapes_sf_lire() %>%
    mutate(source='shapes') %>%
    glimpse()
  routes.sf <- osm_routes_read() %>%
    dplyr::select(shape_id=ref.FR.STAR) %>%
    filter(! is.na(shape_id)) %>%
    glimpse()
  names(routes.sf$geometry)=NULL
  names(shapes.sf$geometry)=NULL

# avec la géométrie
  carp('jointure')
  df <- dplyr::left_join(routes.sf %>% as.data.frame(), shapes.sf %>% as.data.frame(), by=c('shape_id'='shape_id')) %>%
    glimpse()
#
# le tracé gtfs est inclus dans le buffer route osm
  carp('passage en Lambert 93 et buffer')
  nc1 <- st_transform(routes.sf, 2154) %>%
    st_buffer(50)
  nc2 <- st_transform(shapes.sf, 2154)
  df <- dplyr::left_join(nc1 %>% as.data.frame(), nc2 %>% as.data.frame(), by=c('shape_id'='shape_id')) %>%
    filter(! is.na(source)) %>%
    glimpse()
  transport_save(df, 'osm_routes_gtfs_shapes')
}
# source("geo/scripts/transport.R");transport_osm_routes_gtfs_shapes_diff()
transport_osm_routes_gtfs_shapes_diff <- function() {
  carp('contains')
  library(sf)
  library(tidyverse)
  df <- transport_read('osm_routes_gtfs_shapes') %>%
    arrange(shape_id)
  nc <- st_as_sf(df, geom_routes=st_sfc(df$geometry.x))
  nc$geom_shapes <- st_sfc(df$geometry.y)
  for (i in 1:nrow(df)) {
    nc1 <- nc[i, ]
    plot(st_geometry(nc1$geom_routes))
    plot(st_geometry(nc1$geom_shapes), add=TRUE)
    inters <- st_intersection(nc1$geom_shapes, nc1$geom_routes)
    plot(st_geometry(inters), add=TRUE, col='blue', lwd=3)
#    carp('inters')
#    glimpse(inters)
#    carp('shapes')
#    glimpse(nc1$geom_shapes)
    diff.sfc <<- st_difference(nc1$geom_shapes, inters)
    if (length(diff.sfc) >= 1 ) {
      carp('shape_id: %s', nc[[i, 'shape_id']])
      plot(st_geometry(diff.sfc), add=TRUE, col='red', lwd=3)
 #     break
    }
  }
}
transport_save <- function(obj, fic='objets_stop') {
  dsn <- sprintf("%s/%s.Rds", transportDir, fic)
  saveRDS(obj, dsn)
  carp("dsn: %s", dsn)
  return(invisible(obj))
}
transport_read <- function(fic='objets_stop') {
  dsn <- sprintf("%s/%s.Rds", transportDir, fic)
  obj <- readRDS(dsn)
  carp("dsn: %s", dsn)
  return(invisible(obj))
}
#
## pour les stops
#
# la partie osm
#
# source("geo/scripts/transport.R");reseau_osm_jour_stops(force = TRUE)
reseau_osm_jour_stops <- function(reseau = "star", force = FALSE) {
  library(tidyverse)
  library(data.table)
  library(janitor)
  carp()
  config_xls(reseau)
#  osm_jour_stops(force_osm = force)
  reseau_diff_stops(force_osm = force)
}
#
# comparaison osm # gtfs
# source("geo/scripts/transport.R");reseau_diff_stops()
reseau_diff_stops <- function(force_osm = TRUE) {
  library(tidyverse)
  library(data.table)
  library(janitor)
  carp()
  titre <- "reseau_diff_stops"
  html <- misc_html_titre(titre)
  html <- misc_html_append(html, "<h1>OSM</h1>")
  gtfs_stops.sf <- tidytransit_stops_sf_lire() %>%
    mutate(gtfs = "gtfs") %>%
    filter(location_type == 0) %>%
    glimpse()
  if (Reseau == "star") {
    gtfs_stops.sf <- gtfs_stops.sf %>%
      filter(!grepl("^5\\d\\d\\d", stop_id))
  }
  zoom <- "left=%0.5f&right=%0.5f&top=%0.5f&bottom=%0.5f"
  select <- "%s%s"
  osm_stops.df <- overpass_get(query = "bus_stop_network", format = "csv", force = force_osm) %>%
    rename(k_ref = Config[1, 'k_ref']) %>%
    clean_names() %>%
    mutate(k_ref = sprintf("%s", k_ref)) %>%
    glimpse()
  osm_stops.sf <- st_as_sf(osm_stops.df, coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
    dplyr::select(stop_id = k_ref, name, osm_id = id, osm_type = type, osm_lon = lon, osm_lat = lat) %>%
    mutate(select = sprintf(select, `osm_type`, `osm_id`)) %>%
    mutate(zoom = sprintf(zoom, `osm_lon` - .001, `osm_lon` + .001, `osm_lat` + .001, `osm_lat` - .001)) %>%
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
  html <- misc_html_append_df(html,osm_inc.df)
#
  html <- misc_html_append(html, "<h2>les stops gtfs absents d'osm</h2>")
  gtfs_inc.df <- df %>%
#    mutate(stop_id=as.numeric(stop_id)) %>%
#    filter(stop_id < 15000) %>%
    filter(is.na(osm)) %>%
    dplyr::select(stop_id, stop_name, stop_desc, lat, lon, location_type) %>%
    arrange(stop_id) %>%
    glimpse()
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

reseau_toto <- function() {
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
# source("geo/scripts/transport.R");reseau_relations_route_level0()
reseau_relations_route_level0 <- function(force_osm = FALSE) {
  library(stringr); # pour str_glue
  df <- transport_read("tidytransit_routes_shapes_stops") %>%
    group_by(ref_network) %>%
    arrange(ref_network, desc(nb), desc(nb_stops)) %>%
    filter(row_number()==1) %>%
    glimpse()
#  stop("******")
  osm_stops.df <- overpass_get(query = "bus_stop_kref", format = "csv", force = force_osm) %>%
    rename(k_ref = Config[1, 'k_ref']) %>%
    filter(public_transport == "platform") %>%
    clean_names() %>%
    mutate(type = dplyr::recode(type,
      "way" = "wy",
      "node" = "nd"
    )) %>%
    mutate(member = sprintf("  %s %s %s", type, id, "platform"))
  cols <- colnames(df)
  for (i in 1:nrow(df)) {
    stops.df <- tibble(stop = unlist(str_split(df[i, "stops"], ";")))
    stops.df <- stops.df %>%
      left_join(osm_stops.df, by = c("stop" = "k_ref"))
    platforms <- paste(stops.df$member, collapse = "\n")
    for (j in 1:ncol(df)) {
      col <- sprintf("relation_%s", cols[j])
      assign(col, df[[i, j]])
    }
    tpl <- "
relation
  on_demand = yes
  contact:phone = +33 2 98 34 42 22
  opening_hours = Mo-Fr 07:30-19:00; Sa 09:00-17:00
"
    tpl <- "
relation
  colour = {relation_Route_color}
  from = {relation_first_name}
  gtfs:shape_id = {relation_shape_id}
  name = {Config_route_name} {relation_route_short_name} : {relation_first_name} - {relation_last_name}
  network = {Config_network}
  network:wikidata = {Config_wikidata}
  network:wikipedia = {Config_wikipedia}
  operator = {Config_operator}
  public_transport:version = 2
  ref = {relation_route_short_name}
  ref:network = {relation_ref_network}
  route = bus
  text_colour = {relation_Route_text_color}
  to = {relation_last_name}
  type = route
  website = {Config_website}
{platforms}
"
    level0 <- str_glue(tpl)
#    print(level0);stop("*****")
    dsn <- sprintf("%s/reseau_relations_route_level0_%s.txt", osmDir, relation_shape_id)
    dsn <- sprintf("%s/reseau_relations_route_level0_%s.txt", osmDir, relation_ref_network)
    write(level0, dsn)
    carp("dsn: %s", dsn)
  }
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
# source("geo/scripts/transport.R");reseau_relations_routemaster_level0()
reseau_relations_routemaster_level0 <- function(force_osm = TRUE) {
  library(stringr); # pour str_glue
  tt <- tidytransit_lire("gtfs") %>%
    glimpse()
  df <- tt$routes %>%
    glimpse()
#  stop("******")
  osm.df <- overpass_get(query = "relations_route_bus_network", format = "csv", force = force_osm) %>%
    clean_names() %>%
    mutate(id = sprintf("  rel %s", id)) %>%
    glimpse()
  cols <- colnames(df)
  for (i in 1:nrow(df)) {
    routes.df <- osm.df %>%
      filter(ref == df[[i, "route_id"]])
    routes <- paste(routes.df$id, collapse = "\n")
#    carp("routes: %s", routes); stop("****")
    for (j in 1:ncol(df)) {
      col <- sprintf("relation_%s", cols[j])
      assign(col, df[[i, j]])
    }
    tpl <- "
relation
  on_demand = yes
  contact:phone = +33 2 98 34 42 22
  opening_hours = Mo-Fr 07:30-19:00; Sa 09:00-17:00
"
    tpl <- "
relation
  colour = {relation_Route_color}
  name = {Config_route_name} {relation_route_short_name} : {relation_route_long_name}
  network = {Config_network}
  network:wikidata = {Config_wikidata}
  network:wikipedia = {Config_wikipedia}
  operator = {Config_operator}
  public_transport:version = 2
  ref = {relation_route_id}
  ref:network = {relation_route_id}
  route_master = bus
  text_colour = {relation_Route_text_color}
  type = route_master
  website = {Config_website}
{routes}
"
    level0 <- str_glue(tpl)
#    print(level0);stop("*****")
    dsn <- sprintf("%s/reseau_relations_routemaster_level0_%s.txt", osmDir, relation_route_id)
    write(level0, dsn)
    carp("dsn: %s", dsn)
  }
}
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