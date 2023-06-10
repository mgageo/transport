# <!-- coding: utf-8 -->
#
# utilisation des données d'OpenStreetMap
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
# auteur : Marc Gauthier
#
#
## fonctions de tidytransit
#
# !! le fichier gtfs doit être nommé gtfs.zip
#
# source("geo/scripts/transport.R");tidytransit_jour()
tidytransit_jour <- function(reseau = Reseau, force = TRUE) {
  library(tidytransit)
  library(archive)
  config_xls(reseau)
  dsn <- sprintf("%s/%s", gtfsDir, "gtfs.zip")
  carp("dsn: %s", dsn)
  if (is.na(Config[1, "gtfs_dir"])) {
    return()
  }
#  setwd(gtfsDir)
  archive::archive_extract(dsn, gtfsDir)
#  setwd(baseDir)
  tidytransit_zip_lire(dsn = dsn)
  tidytransit_stops_sf()
#  glimpse(tt); stop("****")
  if( Config[1, "shapes"] == "TRUE" ) {
    tidytransit_shapes_stops()
    tidytransit_shapes_stops_tex()
    tidytransit_gtfs_shapes_gpx()
    tidytransit_gtfs_routes_shapes_stops()
    tidytransit_shapes_stops_coord()
    tidytransit_routes_fiche()
  }
  tidytransit_routes_stops()
}
# lecture des routes, données GTFS
# https://eu.ftp.opendatasoft.com/star/gtfs/GTFS_2022.1.2.0_20221003_20221023.zip
# source("geo/scripts/transport.R");tidytransit_zip_lire()
tidytransit_zip_lire <- function(dsn = "D:/web.var/TRANSPORT/STAR/GTFS/gtfs.zip", rds = 'gtfs') {
  carp()
  library(tidytransit)
#  dsn <- sprintf("%s/20190805/mobibreizh-bd-gtfs.zip", transportDir)
  dsn <- sprintf("%s/gtfs.zip", gtfsDir)
  carp("dsn: %s", dsn)
  options(encoding = "UTF-8")
  tt <<- tidytransit::read_gtfs(dsn, quiet = FALSE)
  Encoding(tt$stops$stop_name) <- "UTF-8"
  Encoding(tt$stops$stop_desc) <- "UTF-8"
  Encoding(tt$routes$route_long_name) <- "UTF-8"
  Encoding(tt$routes$route_short_name) <- "UTF-8"
  Encoding(tt$routes$route_desc) <- "UTF-8"
  tidytransit_sauve(tt, rds)
  return(invisible(tt))
}
tidytransit_unzip_lire <- function(dsn, rds = 'gtfs.Rds') {
  carp()
  library(tidytransit)
#  dsn <- sprintf("%s/20190805/mobibreizh-bd-gtfs.zip", transportDir)
  carp("dsn: %s", dsn)
  file_list_df <- zip::zip_list(dsn)
  path <- dirname(dsn)
  tt <- tidytransit::create_gtfs_object(path, file_list_df, quiet = FALSE)
  dsn <- sprintf("%s/%s", transportDir, rds)
  carp("dsn: %s", dsn)
  saveRDS(tt, file=dsn)
  return(invisible(tt))
}
# setwd("d:/web");source("geo/scripts/transport.R");tidytransit_zip_txt()
tidytransit_zip_txt <- function() {
  carp()
  config_xls('pennarbed')
  sauve_wd <- getwd()
  setwd(gtfsDir)
  carp("wd: %s", gtfsDir)
  files <- list.files(".", pattern = ".txt$", full.names = FALSE) %>%
    glimpse()
  utils::zip(zipfile = 'gtfs.zip', files = files, extras = "-j") %>%
    glimpse()
  setwd(sauve_wd)
}
#
## fonctions utilitaires
#
# sauvegarde / restauration
tidytransit_lire <- function(rds = "gtfs") {
  dsn <- sprintf("%s/%s.Rds", transportDir, rds)
  carp("dsn: %s", dsn)
  tt <- readRDS(file = dsn)
  return(invisible(tt))
}
tidytransit_sauve <- function(tt, rds = "sauve") {
  dsn <- sprintf("%s/%s.Rds", transportDir, rds)
  carp("dsn: %s", dsn)
  saveRDS(tt, file = dsn)
  return(invisible(tt))
}
###############################################################
#
## les shapes des routes
# source("geo/scripts/transport.R");config_xls(Reseau);nc <- tidytransit_gtfs_routes_shapes_stops()
tidytransit_gtfs_routes_shapes_stops <- function(rds = 'gtfs_routes_shapes') {
  library(tidyverse)
  library(tidytransit)
  carp()
  tt <- tidytransit_lire("gtfs")
  df1 <- tidytransit_lire("gtfs_shapes_stops") %>%
    glimpse()
#  df1 %>%
#    filter(grepl("^0806", shape_id)) %>%
#    glimpse();stop("@@@@@@@")
  df <- tt$routes %>%
    glimpse() %>%
    left_join(df1) %>%
    dplyr::select(route_short_name, direction_id, route_id, shape_id, route_long_name, route_desc
      , route_color, route_text_color, nb, names, stops, descs, first_name, last_name, first_stop, last_stop, first_desc, last_desc)

  tex_df2kable(df, num = TRUE)
  texFic <- sprintf("%s/%s", imagesDir, "tidytransit_gtfs_routes_shapes.tex")
  TEX <- file(texFic)
  tex <- "% <!-- coding: utf-8 -->"
  dsn <- sprintf("%s/routes_shapes_stops_tpl.tex", tplDir)
  template <- readLines(dsn)
  df$network <- Config[[1, "network"]]
  df <- df %>%
    mutate(nb = sprintf("%s", nb)) %>%
    mutate(d = dplyr::recode(direction_id,
      "0" = "A",
      "1" = "B"
      )) %>%
    mutate(ref_network = sprintf("%s-%s", route_short_name, d)) %>%
    mutate(direction_id = sprintf("%s", direction_id)) %>%
    dplyr::select(-d) %>%
    glimpse()
  if (grepl("(star)", Reseau)) {
    df <- df %>%
      filter(!grepl("^(Transport scolaire|Métro)$", route_desc))
  }
#  df %>%
#    filter(grepl("^0806", shape_id)) %>%
#    glimpse();stop("@@@@@@@")
  for (i in 1:nrow(df)) {
    tpl <- tex_df2tpl(df, i, template)
    tpl <- escapeLatexSpecials(tpl)
    tex <- append(tex, tpl)
  }
  write(tex, file = TEX, append = FALSE)
  close(TEX)
  carp("texFic: %s", texFic)
  dsn <- sprintf("%s/%s", texDir, "tidytransit_gtfs_routes_shapes_stops.csv")
  readr::write_tsv(df, file = dsn)
  carp("dsn: %s", dsn)
  tidytransit_sauve(df, "tidytransit_gtfs_routes_shapes_stops")

  df1 <- df %>%
    dplyr::select(ref = ref_network, shape_id, first_name, last_name) %>%
    arrange(ref)
#  df %>%
#    filter(grepl("^0067", shape_id)) %>%
#    glimpse();stop("****")
  tex_df2kable(df1, num = TRUE, suffixe = "ref_shape")
#  df1 <- readr::read_tsv(file = dsn, col_types = cols(.default = "c")) %>%
#    glimpse()
#  carp("identical: %s", identical(df, df1))
}
#
## sans les shapes
#
# détermination des stops d'une route
# source("geo/scripts/transport.R");tidytransit_routes_stops()
tidytransit_routes_stops <- function() {
  library(tidyverse)
  library(tidytransit)
  carp()
  tt <- tidytransit_lire("gtfs")
  df0 <- tt$stops %>%
    filter(stop_lat == 0)
  if (nrow(df0) > 0) {
    misc_print(df0)
  }
  trips.df <- tt$trips %>%
    dplyr::select(trip_id, service_id, route_id, direction_id)
  stops.df <- tt$stops %>%
    dplyr::select(stop_id, stop_name, stop_desc, stop_code)
  stop_times.df <- tt$stop_times %>%
    left_join(stops.df, by = c("stop_id"))
  carp("on regroupe par voyage")
  df1 <- stop_times.df %>%
    arrange(stop_sequence) %>%
    group_by(trip_id) %>%
    summarise(
      first_stop = first(stop_id)
      , last_stop = last(stop_id)
      , stops = paste(stop_id, collapse = ";")
      , stops_code = paste(stop_code, collapse = ";")
      , first_name = first(stop_name), last_name = last(stop_name), names = paste(stop_name, collapse = ";")
      , first_desc = first(stop_desc), last_desc = last(stop_desc), descs = paste(stop_desc, collapse = ";")
      , nb_stops = n()
    )
# on trouve la route
  df2 <- left_join(df1, trips.df)
  df3 <- df2 %>%
    filter(is.na(route_id))
  if (nrow(df3) > 0) {
    glimpse(df3)
    confess('echec jointure trips')
  }
  routes.df <- tt$routes
  df3 <- df2 %>%
    dplyr::select(-trip_id) %>%
    dplyr::select(-service_id) %>%
    dplyr::select(-first_desc, -last_desc, -descs) %>%
    distinct() %>%
    left_join(routes.df, by = c("route_id")) %>%
    mutate(d = dplyr::recode(direction_id,
      "0" = "A",
      "1" = "B"
      )) %>%
    mutate(ref_network = sprintf("%s-%s", route_short_name, d)) %>%
    mutate(direction_id = sprintf("%s", direction_id)) %>%
    dplyr::select(-d) %>%
    glimpse() %>%
    arrange(ref_network, nb_stops) %>%
# que les lignes de bus
    filter(route_type == 3) %>%
    dplyr::select(-route_url, -route_type) %>%
    glimpse()
  tidytransit_sauve(df3, "tidytransit_routes_stops")
  texFic <- sprintf("%s/%s", imagesDir, "tidytransit_routes_stops.tex")
  TEX <- file(texFic)
  tex <- "% <!-- coding: utf-8 -->"
  dsn <- sprintf("%s/routes_stops_tpl.tex", tplDir)
  template <- readLines(dsn)
  df3$network <- Config[[1, "network"]]
  for (i in 1:nrow(df3)) {
    tpl <- tex_df2tpl(df3, i, template)
    tpl <- escapeLatexSpecials(tpl)
    tex <- append(tex, tpl)
  }

  write(tex, file = TEX, append = FALSE)
  close(TEX)
  carp("texFic: %s", texFic)
  return(invisible())
}
#
# détermination des stops d'une route
# source("geo/scripts/transport.R");df <- tidytransit_routes_stops()
tidytransit_routes_stops_v1 <- function(rds = "routes_stops") {
  library(tidyverse)
  library(tidytransit)
  library(sf)
  carp()
  tt <- tidytransit_lire("gtfs") %>%
    glimpse()
  trips.df <- tt$trips %>%
    glimpse() %>%
    dplyr::select(trip_id, route_id, shape_id, direction_id, wheelchair_accessible) %>%
    glimpse()

  stop_times.df <- tt$stop_times %>%
    glimpse()
  carp("on regroupe par voyage")
  df1 <- stop_times.df %>%
    arrange(stop_sequence) %>%
    group_by(trip_id) %>%
    summarise(nb=n(), depart=first(stop_id), arrivee=last(stop_id), arrets=paste(stop_id, collapse = ";")) %>%
    glimpse()
# on trouve la route
  df2 <- left_join(df1, trips.df, by = c("trip_id")) %>%
    glimpse()

# on regroupe
  carp("les arrets d'une route")
  df4 <- df2 %>%
    filter(! is.na(route_id)) %>%
    group_by(route_id, direction_id, shape_id, depart, arrivee, arrets) %>%
    summarise(nb=n()) %>%
    glimpse()
  routes.df <- tt$routes %>%
    glimpse()
  df5 <- left_join(df4, routes.df, by = c("route_id")) %>%
    glimpse()
  carp("que les lignes de bus")
  df6 <- df5 %>%
    filter(route_type == 3) %>%
    glimpse()
  tidytransit_sauve(df6, rds)
  return(invisible(df6))
}
#
#
## les shapes des routes avec la carto
#
# source("geo/scripts/transport.R");config_xls(Reseau);nc <- tidytransit_gtfs_routes_shapes_stops_carto()
tidytransit_gtfs_routes_shapes_stops_carto <- function(rds = "gtfs_routes_shapes_stops_carto", force = TRUE) {
  library(tidyverse)
  library(tidytransit)
  carp()
  tt <- tidytransit_lire("gtfs")
  df1 <- tidytransit_lire("gtfs_shapes_stops") %>%
    glimpse()
  nc1 <- tidytransit_lire("gtfs_shapes_stops_coord") %>%
    st_transform(2154) %>%
    glimpse()
  df <- tt$routes %>%
    left_join(df1) %>%
    dplyr::select(route_short_name, direction_id, route_id, shape_id, route_long_name, route_color, route_text_color, route_desc
      , nb, names, stops, first_name, last_name, first_desc, last_desc, first_stop, last_stop) %>%
    mutate(names = gsub(";", ", ", names)) %>%
    mutate(stops = gsub(";", ", ", stops))
#  misc_print(df)
  tex_df2kable(df, num = TRUE)
  texFic <- sprintf("%s/%s", imagesDir, "tidytransit_gtfs_routes_shapes_stops_carto.tex")
  TEX <- file(texFic)
  tex <- "% <!-- coding: utf-8 -->"
  dsn <- sprintf("%s/tidytransit_gtfs_routes_shapes_stops_carto_tpl.tex", tplDir)
  template <- readLines(dsn)
  df$network <- Config[[1, "network"]]
  df <- df %>%
    mutate(nb = sprintf("%s", nb)) %>%
    mutate(d = dplyr::recode(direction_id,
      "0" = "A",
      "1" = "B"
      )) %>%
    mutate(ref_network = sprintf("%s-%s", route_short_name, d)) %>%
    mutate(direction_id = sprintf("%s", direction_id)) %>%
    dplyr::select(-d) %>%
    mutate(shape = gsub("[$:]", "_", shape_id)) %>%
    filter(! is.na(shape_id))
  if (grepl("(concarneau)", Reseau)) {
    df <- df %>%
      arrange(ref_network)
  }
  if (grepl("(star)", Reseau)) {
    df <- df %>%
      filter(!grepl("(Transport scolaire|Métro)", route_desc)) %>%
#      filter(grepl("^08", shape_id)) %>%
      glimpse()
  }
  stops_loins.df <- data.frame()
  for (i in 1:nrow(df)) {
#    glimpse(df[i, ]); stop("***")
    shape_id <- df[[i, "shape_id"]]
    shape <- df[[i, "shape"]]
    dsn_shape <- sprintf("%s/shape_%s.gpx", josmDir, shape)
    if (! file.exists(dsn_shape)) {
      glimpse(df[i, ])
      confess("*** dsn_shape: %s", dsn_shape)
    }
    shape.sf <- st_read(dsn_shape, layer = "tracks", quiet = TRUE) %>%
      st_transform(2154)
    plot(st_geometry(shape.sf), add = FALSE, col = "green", lwd = 3)
    nc2 <- nc1 %>%
      filter(shape_id == !!shape_id)
    plot(st_geometry(nc2), col = "green", lwd = 3, pch = 19, add = TRUE)
    df2 <- st_distance(nc2, shape.sf) %>%
      glimpse()
    nc2 <- cbind(nc2, df2) %>%
      mutate(distance = as.integer(df2)) %>%
      dplyr::select(-df2) %>%
      glimpse()
    loins.sf <- nc2 %>%
      filter(distance > 30)
    if (nrow(loins.sf) > 0) {
      plot(st_geometry(loins.sf), col = "red", lwd = 3, pch = 19, add = TRUE)
      glimpse(loins.sf)
      stops_loins.df <- rbind(stops_loins.df, st_drop_geometry(loins.sf))
#      stop("****")
    }
    titre <- sprintf("%s", shape_id)
    title(titre)
    dsn <- dev2pdf(suffixe = shape, dossier = "images")
    tpl <- tex_df2tpl(df, i, template)
    tpl <- escapeLatexSpecials(tpl)
    tex <- append(tex, tpl)
  }
  write(tex, file = TEX, append = FALSE)
  close(TEX)
  carp("texFic: %s", texFic)
  misc_print(stops_loins.df)
  tex_df2kable(stops_loins.df, suffixe = "loin", longtable = TRUE)
  return(invisible())
}

#
# détermination des stops des différents shapes
# source("geo/scripts/transport.R");tidytransit_shapes_stops()
tidytransit_shapes_stops <- function() {
  library(tidyverse)
  library(tidytransit)
  carp()
  tt <- tidytransit_lire("gtfs") %>%
    glimpse()
  df0 <- tt$stops %>%
    filter(stop_lat == 0)
  if (nrow(df0) > 0) {
    misc_print(df0)
  }
  trips.df <- tt$trips %>%
    dplyr::select(trip_id, shape_id, route_id, direction_id, wheelchair_accessible)
  stops.df <- tt$stops %>%
    dplyr::select(stop_id, stop_name, stop_desc)
  stop_times.df <- tt$stop_times %>%
    left_join(stops.df, by = c("stop_id"))
  carp("on regroupe par voyage")
  df1 <- stop_times.df %>%
    arrange(stop_sequence) %>%
    group_by(trip_id) %>%
    summarise(nb_stops = n()
      , first_stop = first(stop_id), last_stop = last(stop_id), stops = paste(stop_id, collapse = ";")
      , first_name = first(stop_name), last_name = last(stop_name), names = paste(stop_name, collapse = ";")
      , first_desc = first(stop_desc), last_desc = last(stop_desc), descs = paste(stop_desc, collapse = ";")
    )
# on trouve le shape
  df2 <- left_join(df1, trips.df) %>%
    glimpse()
#
# la ligne peut ne pas avoir d'horaires
#  df2 %>%
#    filter(grepl("^0806", shape_id)) %>%
#    glimpse();stop("@@@@@@@")
  df20 <- df2 %>%
    filter(is.na(shape_id))
  if (nrow(df20) > 0 ) {
    carp('echec jointure trips')
    glimpse(df20)
  }
# on regroupe
  carp("les arrets d'un shape")
  df3 <- df2 %>%
    filter(shape_id != "") %>%
    group_by(shape_id, route_id, direction_id
      , nb_stops, stops, first_stop, last_stop
      , names , first_name, last_name
      , descs , first_desc, last_desc
    ) %>%
    summarise(nb = n()) %>%
    ungroup() %>%
    mutate(d = dplyr::recode(direction_id,
      "0" = "A",
      "1" = "B"
      )) %>%
    mutate(ref_network = sprintf("%s-%s", route_id, d)) %>%
    arrange(ref_network, desc(nb_stops))
#  rio::export(df3, "d:piero.xlsx")
  tidytransit_sauve(df3, "gtfs_shapes_stops")
  return(invisible(df3))
}

#
# production tex et wiki
# source("geo/scripts/transport.R");tidytransit_shapes_stops_tex()
tidytransit_shapes_stops_tex <- function() {
  library(tidyverse)
  library(tidytransit)
  carp()
  df3 <- tidytransit_lire("gtfs_shapes_stops")
#  misc_print(df3)
  df4 <- df3 %>%
    glimpse() %>%
    dplyr::select(ref_network, shape_id, nb_stops, nb, names)

  tex_df2kable(df4, num = TRUE)
  page <- sprintf("User:Mga_geo/Transports_publics/%s/%s", Config["wiki"], "tidytransit_shapes_stops") %>%
    glimpse()
  wiki <- wiki_df2table(df4)
  wiki_page_init(page = page, article = wiki, force = TRUE)
  return(invisible(df3))
}
#
# détermination des coordonnées des stops d'un shape
# source("geo/scripts/transport.R");tidytransit_shapes_stops_coord()
tidytransit_shapes_stops_coord <- function() {
  library(tidyverse)
  library(tidytransit)
  carp()
  tt <- tidytransit_lire("gtfs")
  df2 <- tt$stops %>%
    dplyr::select(stop_id, stop_name, stop_desc, stop_lat, stop_lon) %>%
    glimpse()
  df1 <- tidytransit_lire("gtfs_shapes_stops") %>%
    ungroup() %>%
    dplyr::select(shape_id, stops) %>%
    group_by(shape_id) %>%
    separate_rows(stops, sep = ";") %>%
    mutate(id = row_number()) %>%
    ungroup() %>%
    rename(stop_id = stops)
  df3 <- df1 %>%
    left_join(df2, by = c("stop_id")) %>%
    mutate(stop_lat = as.numeric(stop_lat)) %>%
    mutate(stop_lon = as.numeric(stop_lon)) %>%
    filter(! is.na(stop_lat)) %>%
    filter(! is.na(stop_lon))
  nc1 <- st_as_sf(df3, coords = c("stop_lon", "stop_lat"), crs = 4326, remove = TRUE) %>%
    st_transform(4326) %>%
    glimpse()
  tidytransit_sauve(nc1, "gtfs_shapes_stops_coord")
  nc2 <- tidytransit_lire("gtfs_shapes") %>%
    st_transform(4326) %>%
    dplyr::select(name = shape_id) %>%
    mutate(id = 1:n()) %>%
    glimpse()
  for (i in 1:nrow(nc2)) {
    shape_id <- nc2[[i, "name"]]
    nc3 <- nc1 %>%
      filter(shape_id == !!shape_id) %>%
      dplyr::select(name = stop_name, id = stop_id)
    nc3 <- nc3 %>%
      rbind(nc2[i, ])
    dsn <- sprintf("%s/shape_stops_%s.geojson", josmDir, shape_id)
    st_write(nc3, dsn, delete_dsn = TRUE)
    carp("dsn: %s", dsn)
  }
  return(invisible(nc1))
}
#
# validation du numéro de sequence
# source("geo/scripts/transport.R");tidytransit_shapes_stops_valid()
tidytransit_shapes_stops_valid <- function() {
  library(tidyverse)
  df1 <- tidytransit_lire("shapes_stops")
  df2 <- df1 %>%
    group_by(shape_id) %>%
    summarize()
  for (i2 in 1:nrow(df2)) {
    df3 <- df1 %>%
      filter(shape_id == df2[[i2, "shape_id"]]) %>%
      arrange(stop_sequence) %>%
      tibble::rowid_to_column("ID")
    df4 <- df3 %>%
      filter(ID != stop_sequence)
    if (nrow(df4) > 0) {
      df3 %>%
        glimpse()
      stop("****")
    }
  }
}
#
# conversion des shapes d'un gtfs
# source("geo/scripts/transport.R");nc <- tidytransit_gtfs_shapes_sf()
tidytransit_gtfs_shapes_sf <- function(rds = 'gtfs_shapes') {
  library(tidyverse)
  library(tidytransit)
  carp()
  tt <- tidytransit_lire("gtfs")
  shapes.sf <- shapes_as_sf(tt$shapes) %>%
    mutate(lg = as.integer(st_length(.))) %>%
    glimpse()
#  plot(shapes.sf)
  tidytransit_sauve(shapes.sf, rds)
  return(invisible(shapes.sf))
}
#
# la conversion en format gpx pour josm
# source("geo/scripts/transport.R");config_xls(Reseau);nc <- tidytransit_gtfs_shapes_gpx()
tidytransit_gtfs_shapes_gpx <- function(rds = 'gtfs_shapes') {
  library(tidyverse)
  library(tidytransit)
  library(rgdal)
  library(sf)

  carp()
  tt <- tidytransit_lire("gtfs")
  shapes.sf <- shapes_as_sf(tt$shapes) %>%
#    filter(shape_id == "0091-B-2327-2710") %>%
    glimpse()
  for (i in 1:nrow(shapes.sf)) {
    shape <- shapes.sf[[i, "shape_id"]]
    shape <- gsub("[$:]", "_", shape)
#    plot(st_geometry(shapes.sf[i, ]))
    dsn <- sprintf("%s/shape_%s.gpx", josmDir, shape)
    carp("dsn: %s", dsn)
    spdf <- as_Spatial(shapes.sf[i, ])
    writeOGR(spdf, dsn, layer="tracks", driver="GPX", dataset_options="GPX_USE_EXTENSIONS=yes", "FORCE_GPX_TRACK=true", overwrite_layer=TRUE, delete_dsn = TRUE)
  }
}
#
# conversion des stops d'un gtfs en format sf
# source("geo/scripts/transport.R");tidytransit_gtfs_stops_sf()
tidytransit_gtfs_stops_sf <- function(rds = "gtfs_stops") {
  library(tidyverse)
  library(tidytransit)
  library(sf)
  carp()
  tt <- tidytransit_lire("gtfs") %>%
    glimpse()
#  plot(tt)
#  routes.sf <- get_route_geometry(tt)
#  plot(routes.sf)
  stops.df <- tt$stops %>%
    mutate(lat = as.numeric(stop_lat)) %>%
    mutate(lon = as.numeric(stop_lon)) %>%
    glimpse()
#  plot(stops.sf)
  stops.sf <- st_as_sf(stops.df, coords = c("lon", "lat"), crs = 4326, remove=FALSE)
  tidytransit_sauve(stops.sf, rds)
  dsn <- sprintf("%s/stops.geojson", josmDir)
  carp("dsn: %s", dsn)
  nc1 <- stops.sf %>%
    mutate(name = sprintf("%s/%s", stop_name, stop_id))
  if (Reseau == "qub") {
    nc1 <- stops.sf %>%
    filter(!grepl("^ARCOM@", stop_id))
  }
  st_write(nc1, dsn, delete_dsn = TRUE)
  carp("dsn: %s", dsn)
  return(invisible(stops.sf))
}
#
# validation du numéro de sequence sur le fichier stop_times
# c'est buggué ???
# source("geo/scripts/transport.R");config_xls(Reseau);tidytransit_stop_times_valid()
tidytransit_stop_times_valid <- function() {
  library(tidyverse)
  carp()
  tt <- tidytransit_lire("gtfs")
  df1 <- tt$stop_times
  df2 <- df1 %>%
    group_by(trip_id) %>%
    summarize()
  df <- tibble()
  for (i2 in 1:nrow(df2)) {
    if ( i2 %% 100 == 0) {
      carp("i2: %s/%s", i2, nrow(df2))
    }
    df3 <- df1 %>%
      filter(trip_id == df2[[i2, "trip_id"]]) %>%
      arrange(stop_sequence) %>%
      tibble::rowid_to_column("ID")
    df4 <- df3 %>%
      filter(ID != stop_sequence)
    if (nrow(df4) > 0) {
      df5 <- df3 %>%
         dplyr::select(ID, trip_id, stop_id, stop_sequence)
      misc_print(df5)
      df3 <- df3 %>%
        mutate(stop_sequence = ID)
      next
    }
    df3 <- df3 %>%
      dplyr::select(-ID)
    df4 <- rbind(df4, df3)
  }
  tt$stop_times <- df4
  tidytransit_sauve(tt, "gtfs_valid")
}

#
# détermination des tronçons communs entre les différents voyages
#
# pb avec les voyages dont les numéros de séquence ne sont pas bon
# source("geo/scripts/transport.R");config_xls(Reseau);df <- tidytransit_shapes_troncons()
tidytransit_shapes_troncons <- function() {
  carp()
  df2 <- tibble()
#
# recherche à partir des arrêts
  df3 <- tidytransit_lire("shapes_stops")
  for (i in 1:nrow(df3)) {
    carp("stop_id: %s %s %s", df3[i, "stop_id"], df3[i, "stop_sequence"], df3[i, "shape_id"])
    shape_id <- df3[[i, "shape_id"]]
    stop_id <- df3[[i, "stop_id"]]
    stop_sequence <- df3[[i, "stop_sequence"]]
    carp("les shapes avec cet arrêt")
    df4 <- df3 %>%
      filter(stop_id == !!stop_id)
    if (nrow(df4) == 1) {
      carp("*** un seul shape")
      next;
    }
    df5 <- df3 %>%
      filter(shape_id == !!shape_id) %>%
      arrange(stop_sequence)
    carp("les arrêts de ce shape: %s", paste0(df5$stop_id, collapse = ";"))
# on boucle sur les arrêts de ce shape
    les_stops <- c(stop_id)
    les_shapes <- list()
    suivant <- 0
#    carp("stop_id: %s stop_sequence: %s nrow: %s", stop_id, stop_sequence, nrow(df5))
#   la dernière station d'une route ?
    if (stop_sequence == nrow(df5)) {
      next;
    }
    depuis <- stop_sequence + 1
    for (ss in depuis:nrow(df5)) {
#      carp("ss: %s stop_sequence: %s nrow: %s", ss, stop_sequence, nrow(df5))
      stop_id <- df5[[ss, "stop_id"]]
      suivant <- suivant + 1
# il faut maintenant déterminer les arrêts suivants de ces shapes
      df6 <- df4 %>%
        mutate(stop_suivant = stop_sequence + suivant)
      df7 <- df6 %>%
        left_join(df3, by = c("shape_id" = "shape_id", "stop_suivant" = "stop_sequence")) %>%
        filter(stop_id.y == !!stop_id)
      if (nrow(df7) <= 1) {
        carp("*** plus de commun")
        break;
      }
# on mémorise la séquence par shape
      les_stops <- c(les_stops, stop_id)
      for  (i7 in 1:nrow(df7)) {
        les_shapes[[df7[[i7, "shape_id"]]]] <- paste0(les_stops, collapse = ";")
      }
    }

    df11 <- tibble(stops = unlist(les_shapes, use.names = FALSE), shape2 = names(les_shapes))
#     glimpse(df11);    stop("jhjjkk")
    df11$shape1 <- shape_id
    df2 <- rbind(df2, df11)
#    break;

  }
  glimpse(df2)
  tidytransit_sauve(df2, "shapes_troncons")
  return(invisible(df2))
}
# source("geo/scripts/transport.R");config_xls(Reseau);df <- tidytransit_shapes_troncons_verif()
tidytransit_shapes_troncons_verif <- function() {
  carp()
  df1 <- tidytransit_lire("shapes_troncons") %>%
    filter(shape1 != shape2) %>%
    mutate(shape3 = ifelse(shape1 < shape2, shape1, shape2)) %>%
    mutate(shape4 = ifelse(shape1 > shape2, shape1, shape2)) %>%
    distinct(shape3, shape4, stops) %>%
    glimpse()
}
#
# ajout des coordonnées géographiques des stops d'un shape
# source("geo/scripts/transport.R");tidytransit_shapes_stops_sf()
tidytransit_shapes_stops_sf <- function(df3, rds = "shapes_stops_sf") {
  library(tidyverse)
  library(tidytransit)
  library(sf)
  carp()
  if(missing(df3)) {
    carp('missing')
    df3 <- tidytransit_shapes_stops_tex() %>%
      glimpse()
  }
  tt <- tidytransit_lire("gtfs")
  carp("les stops")
  stops.df <- tt$stops %>%
    mutate(lat = as.numeric(stop_lat)) %>%
    mutate(lon = as.numeric(stop_lon)) %>%
    dplyr::select(stop_id, stop_name, lat, lon) %>%
    glimpse()
  carp("shape avec plusieurs voyages")
  df4 <- df3 %>%
    group_by(shape_id) %>%
    summarise(nb=n()) %>%
    filter(nb > 1) %>%
    glimpse()
  carp("pour pouvoir faire la jointure avec les stops")
  df5 <- df3 %>%
    separate_rows(arrets, sep = ";") %>%
    dplyr::select(-nb, stop_id=arrets) %>%
    glimpse()
  carp("la jointure avec les stops")
  df6 <- left_join(df5, stops.df) %>%
    glimpse()
  df7 <- df6 %>%
    filter(is.na(stop_name))
  if (nrow(df7) > 0 ) {
    carp("df7 nrow: %s", nrow(df7))
    glimpse(df7)
  }
  df8 <- df6 %>%
    filter(!is.na(stop_name))
  nc1 <- st_as_sf(df8, coords = c("lon", "lat"), crs = 4326, remove = TRUE) %>%
    glimpse()
  tidytransit_sauve(nc1, rds)
  plot(st_geometry(nc1))
  return(invisible(nc1))
}

#
# la fiche pour une ligne
# source("geo/scripts/transport.R");  config_xls(Reseau);tidytransit_ligne_fiche()
tidytransit_ligne_fiche <- function(ligne = "0013") {
  library(tidyverse)
  library(tidytransit)
  library(sf)
  carp("ligne: %s", ligne)
  re <- sprintf("^%s", ligne)
  df <- tidytransit_lire("gtfs_shapes_stops") %>%
    filter(grepl(re, shape_id)) %>%
    arrange(shape_id) %>%
    glimpse()
  texFic <- sprintf("%s/tidytransit_ligne_fiche_%s.tex", imagesDir, ligne)
  TEX <- file(texFic)
  tex <- "% <!-- coding: utf-8 -->"
#
# le template tex
  dsn <- sprintf("%s/tidytransit_ligne_fiche_tpl.tex", tplDir)
  template <- readLines(dsn, encoding = "UTF-8")
  for (i in 1:nrow(df)) {
    carp("i: %s/%s", i, nrow(df))
    shape_id <- df[i, "shape_id"]
    tpl <- tex_df2tpl(df, i, template)
    tpl <- escapeLatexSpecials(tpl)
    tex <- append(tex, tpl)
#    stop("****")
    dsn_shape <- sprintf("%s/shape_%s.gpx", josmDir, shape_id)
    if (! file.exists(dsn_shape)) {
      confess;
    }
    shape.sf <- st_read(dsn_shape, layer = "tracks", quiet = TRUE) %>%
      st_transform(2154)
    plot(st_geometry(shape.sf), col = "green", lwd = 3)
    dsn <- dev2pdf(suffixe = shape_id, dossier = "images")
  }
  write(tex, file = TEX, append = FALSE)
  close(TEX)
  carp(" texFic: %s", texFic)
  texFic <- sprintf("%s/tidytransit_ligne_fiche_%s.tex", imagesDir, ligne)
  TEX <- file(texFic)
  tex <- "% <!-- coding: utf-8 -->"
#
# le template tex
  dsn <- sprintf("%s/tidytransit_ligne_tpl.tex", tplDir)
  template <- readLines(dsn, encoding = "UTF-8")
  df1 <- tribble(~ligne, ligne) %>%
    glimpse()
  tex <- tex_df2tpl(df1, 1, template)
  dsn <- sprintf("%s/tidytransit_ligne_%s.tex", texDir, ligne)
  carp("dsn: %s", dsn)
  write(tex, file = dsn, append = FALSE)
  tex_pdflatex(sprintf("tidytransit_ligne_%s.tex", ligne))
}
#
## les fiches des différentes lignes
#
# version en markdown
# source("geo/scripts/transport.R"); config_xls(Reseau);df <- tidytransit_routes_fiche_md()
tidytransit_routes_fiche_md <- function(rds = "routes_fiche") {
  library(tidyverse)
  tt <<- tidytransit_lire("gtfs")
  df <- tidytransit_lire("routes_stops")
  shapes.sf <- tidytransit_lire("gtfs_shapes")
  df <- df %>%
    left_join(shapes.sf , bt = c("shape_id"))
  route_id.df <- df %>%
    group_by(route_id, direction_id, route_long_name) %>%
    summarize(nb = n()) %>%
    filter(nb > 1)
  md_entete("les lignes du réseau ...")
  for (i in 1:nrow(route_id.df)) {
    df1 <- df %>%
      filter(route_id == route_id.df[i, "route_id"] & direction_id == route_id.df[i, "direction_id"])
    tidytransit_route_fiche(df1)
#    break
  }
  dsn <- sprintf("%s/routes_fiche.md", webDir)
  md_print(dsn)
}
tidytransit_route_fiche <- function(df) {
  library(tidyverse)
  glimpse(df)
  tpl <- '
# {{route_id}}-{{direction_id}} {{route_long_name}}
'
  md <- misc_dfi2tpl(df, 1, tpl)
  md_append(md)
  glimpse(df)
  tpl <- '
## départ: {{depart}} arrivée: {{arrivee}}

shape: lg {{lg}} id {{shape_id}}

arrêts: {{arrets}}
{{arrets_name}}  '
  stops.df <- tt$stops
  for (i in 1:nrow(df)) {
    df1 <- df[i,] %>%
      ungroup() %>%
      dplyr::select(arrets) %>%
      separate_rows(arrets, sep = ";") %>%
      left_join(stops.df, by = c("arrets" = "stop_id"))

    arrets_name <- paste(df1$stop_name, collapse = ";")
    df[i, "arrets_name"] <- arrets_name
    md <- misc_dfi2tpl(df, i, tpl)
    md_append(md)
  }
}
#
# V2 en tex pur et dur
# source("geo/scripts/transport.R");tidytransit_routes_fiche()
tidytransit_routes_fiche <- function() {
  library(tidyverse)
  tt <- tidytransit_lire("gtfs") %>%
    glimpse()
  df1 <- tt$trips %>%
    glimpse() %>%
    dplyr::select(trip_id, route_id, shape_id, direction_id, wheelchair_accessible) %>%
    group_by(route_id, direction_id, shape_id) %>%
    summarize(nb = n()) %>%
    left_join(tt$routes, by = c("route_id")) %>%
    dplyr::select(route_id, d = direction_id, shape_id, nb, ref = route_short_name, name = route_long_name) %>%
    filter(shape_id != "") %>%
    arrange(route_id, shape_id) %>%
    glimpse()
  tex_df2kable(df1, num = TRUE)
  return()
}
#
#
#
## en direct de https://github.com/r-transit/tidytransit/blob/master/R/spatial.R
#
shapes_as_sfg <- function(df) {
  library(sf)
  # as suggested by www.github.com/mdsumner
  l_dfs <- split(df, df$shape_id)

  l_linestrings <- lapply(l_dfs,
                          shape_as_sf_linestring)

  return(sf::st_multilinestring(l_linestrings))
}
shape_as_sf_linestring <- function(df) {
  # as suggested by www.github.com/mdsumner

  m <- as.matrix(df[order(df$shape_pt_sequence),
                    c("shape_pt_lon", "shape_pt_lat")])

  return(sf::st_linestring(m))
}

# en direct de https://www.natedayta.com/2018/06/02/extending-gtfs-capabilities-with-parsing-into-simple-features/
shapes_as_sf <- function(df) {
  library(sf)
# extract lon/lat values as matrix to build linestrings for each "shape_id"
  sfc <- df %>% # long data frame
    arrange(shape_pt_sequence) %>% # essentiel !
    split(.$shape_id) %>% # list of shorted data framee, one per shape
    map(~ dplyr::select(., shape_pt_lon, shape_pt_lat) %>% # order maters, lon-1st lat-2nd
        as.matrix %>% # coherce for st_linestrings happiness
        st_linestring) %>%
    st_sfc(crs = 4326) # bundle all shapes into a collection
# add collection on and convert to sf
  nc <- unique(df$shape_id) %>%
    sort() %>% # sort to match with names(sfc); split()'s factor-cohercion alpha sorts
    st_sf(shape_id = ., geometry = sfc) %>%
    glimpse()
}
#
# détermination pour les voyages (trips)  et des arrêts (stops)
tidytransit_routes_trips_stops <- function() {
  library(tidyverse)
  tt <- tidytransit_lire("gtfs")
  dsn <- sprintf("%s/stop_times.txt", gtfsDir)
  mtime <- file.info(dsn)$mtime
  carp("dsn: %s %s", dsn, mtime)
  df <- tt$stop_times
  carp("ajout nom des stops")
  stops.df <- tt$stops
  df1 <- df %>%
    left_join(stops.df, c("stop_id"="stop_id")) %>%
    glimpse()
  carp('determination depart, arrivee, et arrets')
  df2 <- df1 %>%
    group_by(trip_id) %>%
    arrange(stop_sequence) %>%
    summarise(
      nb = n(),
      depart = first(stop_name),
      arrivee = last(stop_name),
      arrets = paste(stop_name, collapse = ";"),
      stops_id = paste(stop_id, collapse = ";")
    ) %>%
    glimpse()
  carp('ajout voyage')
  trips.df <-tt$trips
  df3 <- df2 %>%
    left_join(trips.df, c("trip_id"="trip_id")) %>%
    glimpse()
  carp('ajout route')
  routes.df <- tt$routes
  df4 <- df3 %>%
    left_join(routes.df, c("route_id"="route_id")) %>%
#    filter(agency_id==agency) %>%
    glimpse()
  df10 <- df4 %>%
    filter(route_long_name == "Lianes 1" & direction_id == 1) %>%
    glimpse()
#  stop("*****")
  carp('parcours differents')
  df5 <- df4 %>%
    group_by(route_short_name, route_long_name, direction_id, depart, arrivee, arrets) %>%
    summarise(nb=n()) %>%
    arrange(route_short_name, route_long_name, direction_id, nb, depart, arrivee, arrets) %>%
    select(route_short_name, route_long_name, direction_id, nb, depart, arrivee, arrets) %>%
    glimpse() %>%
    print(n=20)
  dsn <- sprintf("%s/routes_trips_stops.csv", reseau_dir)
  write.csv(df5, file=dsn, row.names=FALSE, na="", quote = FALSE, fileEncoding = "UTF-8")
  carp("dsn: %s", dsn)
  page <- sprintf("User:Mga_geo/Transports_publics/%s/%s", Config["wiki"], "tidytransit_routes_trips_stops") %>%
    glimpse()
  wiki <- sprintf("==%s==", mtime)
  wiki <- sprintf("%s
%s", wiki, wiki_df2table(df5))
  wiki_page_init(page = page, article = wiki, force = TRUE)
}