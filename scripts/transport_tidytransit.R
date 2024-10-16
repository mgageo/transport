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
stop <- "Ploubezre"

# source("geo/scripts/transport.R");tidytransit_jour()
tidytransit_jour <- function(force = TRUE) {
  library(tidytransit)
  library(archive)
  if (is.na(Config[1, "gtfs_dir"])) {
    return()
  }
  reseau_tpl_tex()
#  setwd(gtfsDir)
  dsn <- sprintf("%s/%s", gtfsDir, "gtfs.zip")
  carp("dsn: %s", dsn)
#  archive::archive_extract(dsn, gtfsDir)
#  setwd(baseDir)
  tidytransit_zip_lire()
  tidytransit_stops_sf()
  gtfs_stops_geocode()
  tidytransit_trips_stops()
#  glimpse(tt); stop("****")
  tidytransit_routes_fiche()
  tidytransit_routes_stops()
  tidytransit_routes_stops_tex()
  tidytransit_routes_trips_stops()
  tex_pdflatex(sprintf("%s_gtfs.tex", Reseau))
  if( Config[1, "shapes"] == "TRUE" ) {
    tidytransit_jour_shapes()
#    valhalla_jour()
  }
}
#
# source("geo/scripts/transport.R");tidytransit_tutu()
tidytransit_tutu <- function(force = TRUE) {
  library(tidytransit)
  reseau_tpl_tex()
  tex_pdflatex(sprintf("%s_gtfs.tex", Reseau))
}
#
# source("geo/scripts/transport.R");tidytransit_jour_shapes()
tidytransit_jour_shapes <- function(force = TRUE) {
  carp("tidytransit_jour_shapes")
  tidytransit_shapes_sf()
  tidytransit_shapes_stops()
  tidytransit_shapes_stops_tex()
  tidytransit_shapes_gpx()
  tidytransit_routes_shapes_stops()
  tidytransit_routes_shapes_stops_tex()
  tidytransit_shapes_stops_coord()
  tidytransit_routes_shapes_fiche()
  tidytransit_routes_shapes_stops_carto(force = force)
  tidytransit_routes_shapes_stops_carto_verif(force = force)
  tex_pdflatex(sprintf("%s_tidytransit_routes_shapes_stops.tex", Reseau))
  tex_pdflatex(sprintf("%s_tidytransit_routes_shapes_stops_carto.tex", Reseau))
  tex_pdflatex(sprintf("%s_tidytransit_shapes.tex", Reseau))
  tidytransit_refs_shapes_stops_carto()
}

# lecture des routes, données GTFS
# https://eu.ftp.opendatasoft.com/star/gtfs/GTFS_2022.1.2.0_20221003_20221023.zip
# source("geo/scripts/transport.R");tidytransit_zip_lire()
tidytransit_zip_lire <- function(rds = 'gtfs') {
  carp()
  library(tidytransit)
#  dsn <- sprintf("%s/20190805/mobibreizh-bd-gtfs.zip", transportDir)
  dsn <- sprintf("%s/gtfs.zip", gtfsDir)
  carp("dsn: %s", dsn)
  options(encoding = "UTF-8")
  tt <<- tidytransit::read_gtfs(dsn, quiet = FALSE)
#  df0 <- tt$stops %>%
#    filter(grepl(stop, stop_name)) %>%
#    glimpse()
#  if (nrow(df0) == 0) {
#    glimpse(tt$stops)
#    confess("***** gtfs.zip %s", stop)
#  }
  if (nrow(tt$agency) > 1) {
    agency_id <- Config_agency_id
    tt$agency <- tt$agency %>%
    filter(agency_id == !!agency_id) %>%
    glimpse()
    if (nrow(tt$agency) != 1) {
      confess("**** agency_id: %s", agency_id)
    }
  } else {
    agency_id <- tt$agency[[1, "agency_id"]]
  }
# que les routes de cet agency
  tt$routes <- tt$routes %>%
    filter(agency_id == !!agency_id) %>%
    glimpse()
  if (nrow(tt$routes) == 0) {
    confess("**** routes agency_id: %s", agency_id)
  }
  if (grepl("nomad", Reseau)) {
    tt$routes <- tt$routes %>%
      filter(grepl(Config_route_id, route_short_name)) %>%
      glimpse()
    if (nrow(tt$routes) == 0) {
      confess("**** routes agency_id: %s", agency_id)
    }
  }
#  stop("*****")
# que les voyages de ces routes
  tt$trips <- tt$trips %>%
    filter(route_id %in% tt$routes$route_id) %>%
    glimpse()
  if (nrow(tt$trips) == 0) {
    confess("**** trips agency_id: %s", agency_id)
  }
  mga <<- tt
# que les shapes de ces voyages
  if (!is.null(tt$shapes)) {
    tt$shapes <- tt$shapes %>%
      filter(shape_id %in% tt$trips$shape_id) %>%
      glimpse()
    if (nrow(tt$shapes) == 0) {
      confess("**** shapes agency_id: %s", agency_id)
    }
  }
# que les horaires de ces voyages
  tt$stop_times <- tt$stop_times %>%
    filter(trip_id %in% tt$trips$trip_id) %>%
    glimpse()
  if (nrow(tt$stop_times) == 0) {
    confess("**** stop_times agency_id: %s", agency_id)
  }
  carp("les arrêts utilisés dans stop_times")
  df1 <- tt$stop_times %>%
    distinct(stop_id) %>%
    arrange(stop_id) %>%
    glimpse()
  carp("les arrêts utilisés")
  tt$stops <- tt$stops %>%
    filter(stop_id %in% df1$stop_id) %>%
    glimpse()
#  tt$stops <- tt$stops %>%
#    filter(grepl("^TILT\\:", stop_id)) %>%
#    glimpse()
#  if (nrow(tt$stops) == 0) {
#    confess("**** stops agency_id: %s", agency_id)
#  }
#  df0 <- tt$stops %>%
#    filter(grepl(stop, stop_name)) %>%
#    glimpse()
#  if (nrow(df0) == 0) {
#    confess("***** %s", stop)
#  }
  tt$stops <- tt$stops %>%
    glimpse() %>%
    (function(.df){
      cls <- c("stop_desc", "stop_code") # columns I need
      .df[cls[!(cls %in% colnames(.df))]] = NA
      return(.df)
    }) %>%
    glimpse()
  tt$routes <- tt$routes %>%
    (function(.df){
      cls <- c("route_desc", "route_url") # columns I need
      .df[cls[!(cls %in% colnames(.df))]] = NA
      return(.df)
    }) %>%
    glimpse()
  dplyr::mutate_if(tt$stops, is.character, .funs = function(x){return(`Encoding<-`(x, "UTF-8"))})
  dplyr::mutate_if(tt$routes, is.character, .funs = function(x){return(`Encoding<-`(x, "UTF-8"))})
#  Encoding(tt$trips$trip_headsign) <- "UTF-8"
  if (grepl("breizhgo", Reseau)) {
    tt$stops <- tt$stops %>%
      mutate(stop_lat = ifelse(stop_name == "Kerberennez", 47.944355, stop_lat)) %>%
      mutate(stop_lon = ifelse(stop_name == "Kerberennez", -4.127847, stop_lon))
#    glimpse(tt$stops); stop("****")
  }
#  tt <- tidytransit_donnees_utiles(tt)
  if (nrow(tt$stops) == 0) {
    confess("bug donnees_utiles tt$stops")
  }
  if (Reseau == "bordeaux") {
    tt$stops %>%
      filter(stop_code == "CVIN") %>%
      glimpse()
  }
  if (Reseau == "breizhgo56") {
    tt$routes <- tt$routes %>%
      mutate(route_short_name = gsub("^0", "", route_short_name))
  }
  if (Reseau == "quimper") {
    tt$stops <- quimper_stops(tt$stops)
  }
# https://stackoverflow.com/questions/45857787/adding-column-if-it-does-not-exist
  tt$trips <- tt$trips %>%
    (function(.df){
      cls <- c("shape_id", "wheelchair_accessible") # columns I need
    # adding cls columns with NAs if not present in the piped data.frame
      .df[cls[!(cls %in% colnames(.df))]] = NA
      return(.df)
    }) %>%
  glimpse()
  tt <- ign_tidytransit_geocode(tt)
  transport_sauve(tt, rds)
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
## les shapes sans les routes
#
#
# source("geo/scripts/transport.R");tidytransit_jour_shapes_stops()
tidytransit_jour_shapes_stops <- function(force = TRUE) {
  carp("tidytransit_jour_shapes_stops")
  tidytransit_routes_shapes_stops()
  tidytransit_routes_shapes_stops_tex()
  tex_pdflatex(sprintf("%s_tidytransit_routes_shapes_stops.tex", Reseau))
  tex_pdflatex(sprintf("%s_tidytransit_shapes.tex", Reseau))
}
# détermination des stops des différents shapes
# source("geo/scripts/transport.R");df3 <- tidytransit_shapes_stops() %>% glimpse()
tidytransit_shapes_stops <- function() {
  library(tidyverse)
  library(tidytransit)
  carp()
# pour avoir la longueur
  df1 <- tidytransit_lire("gtfs_shapes") %>%
    st_drop_geometry() %>%
    glimpse()
  df11 <- df1 %>%
      filter(shape_id == "")
  if (nrow(df11) > 0) {
    carp("***** shape sans shape_id")
    glimpse(df11)
  }
  df2 <- tidytransit_lire("tidytransit_trips_stops")
  df21 <- df2 %>%
      filter(shape_id == "")
  if (nrow(df21) > 0) {
    carp("***** trips_stops sans shape")
    glimpse(df21)
  }
  df2 <- df2 %>%
      filter(shape_id != "")
  df11 <- df2 %>%
    left_join(df1, by = c("shape_id"))
  df3 <- df11 %>%
    group_by(shape_id) %>%
    arrange(shape_id, desc(nb_stops)) %>%
    mutate(Ordre = row_number()) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    glimpse()
  tidytransit_sauve(df3, "tidytransit_shapes_stops")
#  return(invisible(df3))
#  stop("*****")
# pour avoir la variable tt
  tt <- tidytransit_lire()
  stops.df <- tt$stops %>%
    dplyr::select(stop_id, stop_name, stop_lat, stop_lon)
  df4 <- df3 %>%
    mutate(stop_id = stops) %>%
    separate_longer_delim(stop_id, delim = ";") %>%
    left_join(stops.df, by = c("stop_id")) %>%
    glimpse()
  tidytransit_sauve(df4, "tidytransiy_shapes_stops_id")
  return(invisible(df3))
}

#
# production tex et wiki
# source("geo/scripts/transport.R");tidytransit_shapes_stops_tex()
tidytransit_shapes_stops_tex <- function() {
  library(tidyverse)
  library(tidytransit)
  library(janitor)
  carp()
#  Wiki <<- TRUE
  dsn <- sprintf("%s/stop_times.txt", gtfsDir)
  mtime <- file.info(dsn)$mtime
  carp("dsn: %s %s", dsn, mtime)
  df1 <- tidytransit_lire("tidytransit_shapes_stops")
  df2 <- df1 %>%
    group_by(route_id, dir = direction_id) %>%
    summarize(nb = n()) %>%
    pivot_wider(names_from = c("dir"), values_from = c("nb")) %>%
    adorn_totals() %>%
    glimpse()
#  misc_print(df2)
#  stop("*****")
  df4 <- df1 %>%
    glimpse() %>%
    arrange(ref_network, desc(nb), nb_stops, shape_id, names) %>%
    mutate(de = sprintf("%s (%s)", first_name, first_city)) %>%
    mutate(à = sprintf("%s (%s)", last_name, last_city)) %>%
    dplyr::select(
      ref = ref_network,
      nb,
      stops = nb_stops,
      shape_id,
      shape_lg,
      de,
      à
    )
  tex_df2kable(df4, num = TRUE)
  if (Wiki != TRUE) {
    return(invisible(df2))
  }
#  tex_df2longtblr(df4)
#  return(invisible(df3))
  page <- sprintf("User:Mga_geo/Transports_publics/%s/gtfs/%s", Config["wiki"], "tidytransit_shapes_stops") %>%
    glimpse()
  wiki <- sprintf("==%s==", mtime)
  wiki <- sprintf("
%s
===par route_id===
%s
===par route_id shape_id===
%s", wiki, wiki_df2table(df2, num = TRUE), wiki_df2table(df4, num = TRUE))
  wiki_page_init(page = page, article = wiki, force = TRUE)
  return(invisible(df2))
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
  df1 <- tidytransit_lire("tidytransit_shapes_stops") %>%
    ungroup() %>%
    group_by(shape_id) %>%
    arrange(desc(nb)) %>%
    filter(row_number() == 1) %>%
    dplyr::select(shape_id, stops) %>%
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
  tidytransit_sauve(nc1, "tidytransit_shapes_stops_coord")
  loins.df <- tibble()
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
    if (nrow(nc3) == 0) {
      carp("*** pas de stop shape_id: %s", shape_id)
      next
    }
    distances <- as.integer(st_distance(nc3, nc2[i, ]))
    if (max(distances) > 150 ) {
      df3 <- nc3 %>%
        st_drop_geometry()
      df3$distance <- distances
      df3$shape_id <- shape_id
      loins.df <- rbind(loins.df, df3)
    }
    nc3 <- nc3 %>%
      rbind(nc2[i, ])
    shape_id <- gsub("[:$/]", "_", shape_id)
    dsn <- sprintf("%s/shape_stops_%s.geojson", josmDir, shape_id)
    st_write(nc3, dsn, delete_dsn = TRUE)
    carp("dsn: %s", dsn)
    dsn <- sprintf("%s/shape_stops_%s.gpx", josmDir, shape_id)
    st_sf2gpx(nc3, dsn, shape_id)
    carp("dsn: %s", dsn)
  }
  if (nrow(loins.df) > 0) {
    loins.df <- loins.df %>%
      filter(distance > 150)
    misc_print(loins.df)
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
###############################################################
#
## les shapes avec les routes
# source("geo/scripts/transport.R");nc <- tidytransit_routes_shapes_stops()
tidytransit_routes_shapes_stops_v0 <- function(rds = 'gtfs_routes_shapes') {
  library(tidyverse)
  library(tidytransit)
  carp()
  tt <- tidytransit_lire("gtfs")
  df1 <- tidytransit_lire("tidytransit_shapes_stops") %>%
    glimpse()
#  df1 %>%
#    filter(grepl("^0806", shape_id)) %>%
#    glimpse();stop("@@@@@@@")
  df <- tt$routes %>%
    glimpse() %>%
    left_join(df1) %>%
    dplyr::select(route_short_name, direction_id, route_id, shape_id, route_long_name, route_desc
      , route_color, route_text_color, route_type
      , nb, names, stops, descs, first_name, last_name, first_stop, last_stop, first_desc, last_desc
      , first_city, last_city
    )
# que les routes "bus"
  df <- df %>%
    filter(route_type == "3")
  if (grepl("(star)", Reseau)) {
    df <- df %>%
      filter(!grepl("^(Transport scolaire|Métro)$", route_desc))
  }

  tex_df2kable(df, num = TRUE)
  texFic <- sprintf("%s/%s", imagesDir, "tidytransit_routes_shapes.tex")
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
  dsn <- sprintf("%s/%s", texDir, "tidytransit_routes_shapes_stops.csv")
  readr::write_tsv(df, file = dsn)
  carp("dsn: %s", dsn)
  tidytransit_sauve(df, "tidytransit_routes_shapes_stops")

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
# source("geo/scripts/transport.R");config_xls(Reseau);nc <- tidytransit_routes_shapes_stops()
tidytransit_routes_shapes_stops <- function(rds = "tidytransit_routes_shapes_stops") {
  library(tidyverse)
  library(tidytransit)
  carp()
  tt <- tidytransit_lire("gtfs")
  df1 <- tidytransit_lire("tidytransit_shapes_stops") %>%
    glimpse()
#  stop("****")

#  df1 %>%
#    filter(grepl("^0806", shape_id)) %>%
#    glimpse();stop("@@@@@@@")
# 25/05/2024
#  df <- tt$routes %>%
#    left_join(df1, by = c("route_id")) %>%
  df <- df1 %>%
    filter(! is.na(shape_id)) %>%
    mutate(nb = sprintf("%s", nb)) %>%
    mutate(d = dplyr::recode(direction_id,
      "0" = "A",
      "1" = "B"
      )) %>%
    glimpse() %>%
    mutate(ref_network = sprintf("%s-%s", route_short_name, d)) %>%
    mutate(direction_id = sprintf("%s", direction_id)) %>%
    dplyr::select(-d) %>%
    mutate(network = Config_network) %>%
    glimpse()
  dsn <- sprintf("%s/%s.csv", texDir, rds)
  readr::write_tsv(df, file = dsn)
  carp("dsn: %s", dsn)
  tidytransit_sauve(df, rds)
  return(invisible(df))
}
# source("geo/scripts/transport.R"); tidytransit_toto()
tidytransit_toto <- function(force = TRUE) {
  library(tidytransit)
  library(archive)
  reseau_tpl_tex()
  tidytransit_routes_shapes_stops_tex()
  tex_pdflatex(sprintf("%s_tidytransit_routes_shapes_stops.tex", Reseau))
}
# source("geo/scripts/transport.R");tidytransit_routes_shapes_stops_tex()
tidytransit_routes_shapes_stops_tex <- function(rds = "tidytransit_routes_shapes_stops") {
  carp()
  df <- tidytransit_lire(rds) %>%
    mutate(Nb = as.numeric(nb)) %>%
    arrange(ref_network, desc(Nb)) %>%
    mutate(dsn_shape = gsub("[$:]", "_", shape_id)) %>%
    mutate(dsn_shape = sprintf("%s/shape_%s.gpx", josmDir, dsn_shape)) %>%
    glimpse()
#  stop("*****")
  texFic <- sprintf("%s/%s.tex", imagesDir, rds)
  TEX <- file(texFic)
  tex <- "% <!-- coding: utf-8 -->"
  dsn <- sprintf("%s/tidytransit_routes_shapes_stops_tpl.tex", tplDir)
  template <- readLines(dsn)

  for (i in 1:nrow(df)) {
    tpl <- tex_df2tpl(df, i, template)
    tpl <- escapeLatexSpecials(tpl)
    tex <- append(tex, tpl)
  }
  write(tex, file = TEX, append = FALSE)
  close(TEX)
  if (Wiki != TRUE) {
    return(invisible())
  }
  df1 <- df %>%
    mutate(from = sprintf("%s/%s", first_city, first_name)) %>%
    mutate(to = sprintf("%s/%s", last_city, last_name)) %>%
#    mutate(shape_id = sprintf("shape_id:%s", shape_id)) %>%
    dplyr::select(ref_network, shape_id, nb, nb_stops, from, to)
  dsn <- sprintf("%s/stop_times.txt", gtfsDir)
  mtime <- file.info(dsn)$mtime
  carp("dsn: %s %s", dsn, mtime)
  carp("texFic: %s", texFic)
  page <- sprintf("User:Mga_geo/Transports_publics/%s/gtfs/%s", Config["wiki"], "tidytransit_routes_shapes_stops") %>%
    glimpse()
  wiki <- sprintf("==%s==", mtime)
  wiki <- sprintf("%s
%s", wiki, wiki_df2table(df1))
  wiki_page_init(page = page, article = wiki, force = TRUE)
  return(invisible())
}
#
#
## les shapes des routes avec la carto
#

#
# source("geo/scripts/transport.R");tidytransit_refs_shapes_stops_carto()
tidytransit_refs_shapes_stops_carto <- function(force = TRUE) {
  library(tidyverse)
  library(tidytransit)
  carp()
  df1 <- tidytransit_lire(rds = "tidytransit_shapes_stops")
  df1 <- df1 %>%
    filter(! grepl("^S", ref_network)) %>%
    group_by(ref_network, shape_id) %>%
    arrange(desc(nb), desc(nb_stops)) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    arrange(ref_network)
  df <- df1 %>%
    group_by(ref_network, route_long_name) %>%
    summarize(nb = n())
  if (Reseau == "rennes") {
    df1 <- df1 %>%
      arrange(route_sort_order, ref_network, desc(nb), desc(nb_stops))
    df <- df1 %>%
      group_by(route_sort_order, ref_network, route_long_name) %>%
      summarize(nb = n())
  }
  glimpse(df1)
#  stop("****")
  df2 <- df1 %>%
    dplyr::select(ref_network, shape_id, route_desc, nb, nb_stops) %>%
    arrange(ref_network, desc(nb), desc(nb_stops)) %>%
    glimpse()

  texFic <- sprintf("%s/%s", imagesDir, "tidytransit_refs_shapes_stops_carto.tex")
  TEX <- file(texFic)
  tex <- "% <!-- coding: utf-8 -->"
  dsn <- sprintf("%s/tidytransit_refs_shapes_stops_tpl.tex", tplDir)
  template <- readLines(dsn)
  dsn <- sprintf("%s/tidytransit_refs_shapes_stops_carto_tpl.tex", tplDir)
  template_ref <- readLines(dsn)
  for (i in 1:nrow(df)) {
    tpl <- tex_df2tpl(df, i, template_ref)
#    tpl <- escapeLatexSpecials(tpl)
    tex <- append(tex, tpl)
    df2 <- df1 %>%
      filter(ref_network == df[[i, "ref_network"]])
    ref_network <- df[[i, "ref_network"]]
    dsn <- misc_dsn(suffixe = ref_network, dossier = "images", extension = "pdf")
    if (! file.exists(dsn) | force == TRUE) {
      carto_ref_shapes_stops_mapsf(df2)
      dsn <- dev2pdf(suffixe = ref_network, dossier = "images")
    }
    for (i2 in 1:nrow(df2)) {
      tpl <- tex_df2tpl(df2, i2, template)
      tpl <- escapeLatexSpecials(tpl)
      tex <- append(tex, tpl)

    }
  }
  write(tex, file = TEX, append = FALSE)
  close(TEX)
  carp("texFic: %s", texFic)
  tex_pdflatex(sprintf("%s_tidytransit_refs_shapes_stops_carto.tex", Reseau))
  return(invisible())
}
# source("geo/scripts/transport.R"); tidytransit_toto2()
tidytransit_toto2 <- function(force = TRUE) {
  library(tidytransit)
  library(archive)
  reseau_tpl_tex()
#  tidytransit_routes_shapes_stops_carto()
  tidytransit_routes_shapes_stops_carto_verif()
  tex_pdflatex(sprintf("%s_tidytransit_routes_shapes_stops_carto.tex", Reseau))
}
# source("geo/scripts/transport.R");tidytransit_routes_shapes_stops_carto()
tidytransit_routes_shapes_stops_carto <- function(rds = "gtfs_routes_shapes_stops_carto", force = TRUE) {
  library(tidyverse)
  library(tidytransit)
  carp()
  tt <- tidytransit_lire("gtfs")
  df1 <- tidytransit_lire("tidytransit_shapes_stops")
  nc1 <- tidytransit_lire("tidytransit_shapes_stops_coord") %>%
    st_transform(2154)
  df <- tt$routes %>%
    left_join(df1) %>%
    dplyr::select(route_short_name, direction_id, route_id, shape_id, route_long_name, route_color, route_text_color, route_desc
      , nb, names, stops, first_name, last_name, first_city, last_city, first_stop, last_stop) %>%
    mutate(names = gsub(";", ", ", names)) %>%
    mutate(stops = gsub(";", ", ", stops))
#  misc_print(df)

  tex_df2kable(df, num = TRUE, suffixe = "liste")
  texFic <- sprintf("%s/%s", imagesDir, "tidytransit_routes_shapes_stops_carto.tex")
  TEX <- file(texFic)
  tex <- "% <!-- coding: utf-8 -->"
  dsn <- sprintf("%s/tidytransit_routes_shapes_stops_carto_tpl.tex", tplDir)
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
    mutate(shape = gsub("[$:/]", "_", shape_id)) %>%
    filter(! is.na(shape_id)) %>%
    arrange(ref_network, desc(nb))
  if (grepl("(concarneau)", Reseau)) {
    df <- df %>%
      arrange(ref_network)
  }
  if (grepl("(rennes)", Reseau)) {
    df <- df %>%
      filter(!grepl("(Transport scolaire|Métro)", route_desc)) %>%
#      filter(grepl("^08", shape_id)) %>%
      glimpse()
  }
  df <- df %>%
#    filter(shape_id == "POSB-POSD-alsac-LYH") %>%
    glimpse()
#  stop("*****")
  rc.df <- data.frame()
  for (i in 1:nrow(df)) {
#    glimpse(df[i, ]); stop("***")
    shape <- df[[i, "shape"]]
    if ( shape != "0007-A-2281-2337") {
#      next
    }
    dsn <- misc_dsn(suffixe = shape, dossier = "images", extension = "pdf")
#    carp("1 dsn: %s", dsn)
    rc <- carto_route_shape_stops(df[i, ], nc1) %>%
      glimpse()
    rc.df <- rc.df %>%
      bind_rows(as_tibble(rc))
    dsn <- dev2pdf(suffixe = shape, dossier = "images")
    tpl <- tex_df2tpl(df, i, template)
    tpl <- escapeLatexSpecials(tpl)
    tex <- append(tex, tpl)
#    break
# stop("*****")
  }
  write(tex, file = TEX, append = FALSE)
  close(TEX)
  carp("texFic: %s", texFic)
  glimpse(rc.df)
  tidytransit_sauve(rc.df, "tidytransit_routes_shapes_stops_carto")
  return(invisible())
}
#
# source("geo/scripts/transport.R");tidytransit_routes_shapes_stops_carto_verif(force = FALSE)
tidytransit_routes_shapes_stops_carto_verif <- function(force = TRUE) {
  library(tidyverse)
  library(janitor)
  carp()
  df <- tidytransit_lire("tidytransit_routes_shapes_stops_carto") %>%
    glimpse()
  points_out.df <- df %>%
    filter(points_out > 0) %>%
    arrange(points_out) %>%
#    filter(shape_id == "0038-B-2128-2059") %>%
    glimpse()
  shapes_out.df <- points_out.df %>%
    group_by(shape_id, route_long_name) %>%
    summarize(nb = n()) %>%
    arrange(nb) %>%
    glimpse()
  misc_print(shapes_out.df)
#  stop("****")
  points.df <- data.frame()
# les points du mauvais côté
  points_cote.df <- data.frame()
  for (i in 1:nrow(df)) {
    points.df <- bind_rows(points.df, df[i, "points_distance.df"])
    points_cote.df <- bind_rows(points_cote.df, df[i, "points_cote.df"])
  }
  df1 <- points.df %>%
    glimpse() %>%
    filter(distance > 50) %>%
    arrange(stop_id) %>%
    dplyr::select(-id, -stop_name, -stop_desc) %>%
    glimpse()
  misc_print(df1)
  tex_df2kable(df1, num = TRUE, suffixe = "loin")
  df2 <- points_cote.df %>%
    glimpse() %>%
    arrange(shape_id, stop_id) %>%
    dplyr::select(-id, -stop_name, -within) %>%
    glimpse()
  misc_print(df2)
  tex_df2kable(df2, num = TRUE, suffixe = "cote")
  return(invisible())
}
#
## sans les shapes
#
# détermination des stops d'une route
# source("geo/scripts/transport.R");tidytransit_routes_stops()
tidytransit_routes_stops <- function(rds = "tidytransit_routes_stops", force = TRUE) {
  library(tidyverse)
  library(tidytransit)
  carp()
  df4 <- tidytransit_lire("tidytransit_trips_stops")
  df5 <- df4 %>%
    group_by(route_id, direction_id, nb_stops, stops_code) %>%
    arrange(route_id, direction_id, desc(nb_stops)) %>%
    mutate(Ordre = row_number()) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    glimpse()
#  stop("******")
  tidytransit_sauve(df5, rds)
  tex_df2kable(df5, num = TRUE)
  dsn <- sprintf("%s/%s.csv", texDir, rds)
  readr::write_tsv(df5, file = dsn)
  carp("dsn: %s", dsn)
  return(invisible())
}
# détermination des stops d'une route
# source("geo/scripts/transport.R");tidytransit_trips_stops()
tidytransit_trips_stops <- function(rds = "tidytransit_trips_stops", force = TRUE) {
  library(tidyverse)
  library(tidytransit)
  carp()
  tt <- tidytransit_lire("gtfs")
  df0 <- tt$stops %>%
    filter(stop_lat == 0)
  if (nrow(df0) > 0) {
    misc_print(df0)
  }
#  df0 <- tt$stops %>%
#    filter(grepl(stop, stop_name)) %>%
#    glimpse()
#  if (nrow(df0) < 1) {
#    confess("****** stops stop: %s", stop)
#  }
  trips.df <- tt$trips %>%
    dplyr::select(trip_id, service_id, route_id, direction_id, shape_id)
  stops.df <- tt$stops %>%
    dplyr::select(stop_id, stop_name, stop_desc, stop_code, city)
  stop_times.df <- tt$stop_times %>%
    left_join(stops.df, by = c("stop_id"))
  carp("on regroupe par trajet")
  df1 <- stop_times.df %>%
    arrange(stop_sequence) %>%
    group_by(trip_id) %>%
    summarise(
      first_stop = first(stop_id)
      , last_stop = last(stop_id)
      , first_stop_code = first(stop_code)
      , last_stop_code = last(stop_code)
      , stops = paste(stop_id, collapse = ";")
      , stops_code = paste(stop_code, collapse = ";")
      , stops_id = paste(stop_id, collapse = ";")
      , first_name = first(stop_name), last_name = last(stop_name), names = paste(stop_name, collapse = ";")
      , first_desc = first(stop_desc), last_desc = last(stop_desc), descs = paste(stop_desc, collapse = ";")
      , first_city = first(city), last_city = last(city)
      , nb_stops = n()
    )
# on trouve la route
  df2 <- left_join(df1, trips.df, by = c("trip_id"))
  df3 <- df2 %>%
    filter(is.na(route_id))
  if (nrow(df3) > 0) {
    glimpse(df3)
    confess('echec jointure trips')
  }
  df3 <- df2 %>%
#    dplyr::select(-trip_id) %>%
    dplyr::select(-service_id) %>%
    group_by(route_id
      , direction_id
      , trip_id, shape_id
      , nb_stops, stops, first_stop, last_stop
      , names , first_name, last_name
      , descs , first_desc, last_desc
      , stops_code, first_stop_code, last_stop_code
      , first_city, last_city
    ) %>%
    summarize(nb = n())
# ajout des infos route
  routes.df <- tt$routes
  df4 <- df3 %>%
    left_join(routes.df, by = c("route_id")) %>%
    mutate(direction_id = sprintf("%s", direction_id)) %>%
    mutate(d = dplyr::recode(direction_id,
      "0" = "A",
      "1" = "B"
      )) %>%
    mutate(ref_network = sprintf("%s%s-%s", Config_route_prefixe, route_short_name, d)) %>%
    dplyr::select(-d) %>%
    dplyr::select(-route_url) %>%
    arrange(ref_network, desc(nb), desc(nb_stops)) %>%
    glimpse()
  tidytransit_sauve(df4, rds)
#
# problème sur Tilt/Lannion
#  df14 <- df4 %>%
#    filter(grepl(stop, stops)) %>%
#    glimpse()
#  if (nrow(df14) < 1) {
#    confess("****** stop: %s", stop)
#  }
  return(invisible(df4))
}
# source("geo/scripts/transport.R");tidytransit_routes_stops_tex()
tidytransit_routes_stops_tex <- function(rds = "tidytransit_routes_stops") {
  library(tidyverse)
  library(tidytransit)
  carp()
  df3 <- tidytransit_lire(rds)
  texFic <- sprintf("%s/%s", imagesDir, "tidytransit_routes_stops.tex")
  TEX <- file(texFic)
  tex <- "% <!-- coding: utf-8 -->"
  dsn <- sprintf("%s/tidytransit_routes_stops_tpl.tex", tplDir)
  template <- readLines(dsn, encoding = "UTF-8")
  df3$network <- Config[[1, "network"]]
  df3 <- df3 %>%
    select(order(colnames(.))) %>%
#    filter(Ordre == 1) %>%
    glimpse()
  for (i in 1:nrow(df3)) {
    tpl <- tex_df2tpl(df3, i, template)
    tpl <- escapeLatexSpecials(tpl)
    tex <- append(tex, tpl)
  }
  write(tex, file = TEX, append = FALSE)
  close(TEX)
  carp("texFic: %s", texFic)
  tex_pdflatex(sprintf("%s_tidytransit_routes.tex", Reseau))
  return(invisible())
}
#
# conversion des shapes d'un gtfs
# source("geo/scripts/transport.R");nc <- tidytransit_shapes_sf()
tidytransit_shapes_sf <- function(rds = "gtfs_shapes") {
  library(tidyverse)
  library(tidytransit)
  carp()
  tt <- tidytransit_lire("gtfs")
  shapes.sf <- shapes_as_sf(tt$shapes) %>%
    mutate(shape_lg = as.integer(st_length(.))) %>%
    glimpse()
#  plot(shapes.sf)
  tidytransit_sauve(shapes.sf, rds)
  return(invisible(shapes.sf))
}
#
# la conversion en format gpx pour josm
# https://r-spatial.org/r/2023/05/15/evolution4.html
#
# source("geo/scripts/transport.R");config_xls(Reseau);nc <- tidytransit_shapes_gpx()
tidytransit_shapes_gpx <- function(rds = 'gtfs_shapes') {
  library(tidyverse)
  library(tidytransit)
  library(sf)
  carp()
  tt <- tidytransit_lire("gtfs")
  shapes.sf <- shapes_as_sf(tt$shapes) %>%
#    filter(shape_id == "ILLENOO2:3191$SP1692690592764326$270") %>%
    glimpse()
#  stop("fghjklm")
  for (i in 1:nrow(shapes.sf)) {
    shape <- shapes.sf[[i, "shape_id"]]
    shape <- gsub("[$:/]", "_", shape)
#    plot(st_geometry(shapes.sf[i, ]))
    dsn <- sprintf("%s/shape_%s.gpx", josmDir, shape)
    carp("dsn: %s", dsn)
    st_sf2gpx(shapes.sf[i, ], dsn, shape)
    nc1 <- shapes.sf[i, ] %>%
      st_transform(4326)
    dsn <- sprintf("%s/shape_%s.geojson", josmDir, shape)
    carp("dsn: %s", dsn)
    st_write(nc1, dsn, delete_dsn = TRUE, driver = "GeoJSON")
    nc2 <<- st_line_sample(st_transform(nc1, 2154), density = 1/100) %>%
      st_transform(4326)
    dsn <- sprintf("%s/shape_%s_points.geojson", josmDir, shape)
    carp("dsn: %s", dsn)
    st_write(nc2, dsn, delete_dsn = TRUE, driver = "GeoJSON")
    break
#    st_write(shapes.sf[i, ], dsn, delete_dsn = TRUE, overwrite_layer = TRUE, driver = 'GPX', GPX_USE_EXTENSIONS = "yes")
#    confess;
#    spdf <- as_Spatial(shapes.sf[i, ])
#    writeOGR(spdf, dsn, layer="tracks", driver="GPX", dataset_options="GPX_USE_EXTENSIONS=yes", "FORCE_GPX_TRACK=true", overwrite_layer=TRUE, delete_dsn = TRUE)
  }
}
#
# conversion des stops d'un gtfs en format JOSM/conflate
# source("geo/scripts/transport.R");tidytransit_stops_conflate()
tidytransit_stops_conflate <- function() {
  library(tidyverse)
  library(tidytransit)
  library(sf)
  carp()
  tt <- tidytransit_lire("gtfs")
  df1 <- tt$stops %>%
    mutate(lat = as.numeric(stop_lat)) %>%
    mutate(lon = as.numeric(stop_lon)) %>%
    mutate(latitude = sprintf("%0.6f", stop_lat)) %>%
    mutate(longitude = sprintf("%0.6f", stop_lon)) %>%
    mutate(highway = "bus_stop") %>%
    mutate(public_transport = "platform") %>%
    dplyr::select(name = stop_name, "ref:Aléop" = stop_id, latitude, longitude, highway, public_transport) %>%
    glimpse()
  dsn <- sprintf("%s/tidytransit_stops_conflate.csv", reseau_dir)
  write.csv(df1, file = dsn, row.names = FALSE, na="", quote = FALSE, fileEncoding = "UTF-8")
  carp("dsn: %s nrow: %s", dsn, nrow(df1))
  return(invisible())
}
#
# conversion des stops d'un gtfs en format sf
# source("geo/scripts/transport.R");tidytransit_stops_sf()
tidytransit_stops_sf <- function(rds = "tidytransit_stops_sf") {
  library(tidyverse)
  library(tidytransit)
  library(sf)
  carp()
  tt <- tidytransit_lire("gtfs")
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
  if (Reseau == "bordeaux") {
    nc1 <- stops.sf %>%
    filter(stop_code == "CVIN") %>%
    glimpse()
  }
  st_write(nc1, dsn, delete_dsn = TRUE)
  carp("dsn: %s", dsn)
  dsn <- sprintf("%s/tidytransit_stops.gpx", josmDir)
  nc2 <- nc1 %>%
    dplyr::select(name)
  st_sf2gpx(nc2, dsn, "stops")
  carp("dsn: %s", dsn)
  tidytransit_sauve(nc1, rds)
  if (Reseau == "aleop44") {
    nc1 %>%
      filter(stop_id == "44HAUTalleA") %>%
      glimpse()
#    stop("µµµµµµµµµµµµµµµµµµµµµµ")
  }
  tidytransit_sauve(stops.sf, rds)

  return(invisible(stops.sf))
}
# source("geo/scripts/transport.R");tidytransit_stops_sf_valid()
tidytransit_stops_sf_valid <- function(rds = "tidytransit_stops_sf") {
  library(tidyverse)
  library(tidytransit)
  library(sf)
  carp()
  nc1 <- tidytransit_lire(rds)
  df1 <- st_proches(nc1, nc1, k = 2) %>%
    arrange(dist) %>%
    filter(dist < 5) %>%
    glimpse()
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
# détermination des tronçons communs entre les différents trajets
#
# pb avec les trajets dont les numéros de séquence ne sont pas bon
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
  carp("shape avec plusieurs trajets")
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
  df <- tidytransit_lire("tidytransit_shapes_stops") %>%
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
# V2 en tex pur et dur, + wiki
# source("geo/scripts/transport.R");tidytransit_routes_fiche()
tidytransit_routes_fiche <- function() {
  library(tidyverse)
  dsn <- sprintf("%s/stop_times.txt", gtfsDir)
  mtime <- file.info(dsn)$mtime
  carp("dsn: %s %s", dsn, mtime)
  tt <- tidytransit_lire("gtfs") %>%
    glimpse()
  df1 <- tt$trips %>%
    glimpse() %>%
    dplyr::select(trip_id, route_id, direction_id) %>%
    group_by(route_id, direction_id) %>%
    summarize(nb = n()) %>%
    left_join(tt$routes, by = c("route_id")) %>%
    arrange(route_id, direction_id, nb) %>%
    dplyr::select(route_short_name, route_id, d = direction_id,  nb, route_long_name) %>%
    arrange(route_short_name, route_id) %>%
    glimpse()
  tex_df2kable(df1, num = TRUE)
  misc_print(df1)
  Wiki <<- TRUE
  page <- sprintf("User:Mga_geo/Transports_publics/%s/gtfs/%s", Config["wiki"], "tidytransit_routes_fiche") %>%
    glimpse()
  wiki <- sprintf("==Fiche des routes==
%s",
    wiki_df2table(df1, num = TRUE)
  )
  wiki_page_init(page = page, article = wiki, force = TRUE)
  return(invisible())
}
# source("geo/scripts/transport.R");tidytransit_routes_shapes_fiche()
tidytransit_routes_shapes_fiche <- function() {
  library(tidyverse)
  dsn <- sprintf("%s/stop_times.txt", gtfsDir)
  mtime <- file.info(dsn)$mtime
  carp("dsn: %s %s", dsn, mtime)
  tt <- tidytransit_lire("gtfs") %>%
    glimpse()
  df1 <- tt$trips %>%
    glimpse() %>%
    dplyr::select(trip_id, route_id, shape_id, direction_id, wheelchair_accessible) %>%
    group_by(route_id, direction_id, shape_id) %>%
    summarize(nb = n()) %>%
    left_join(tt$routes, by = c("route_id")) %>%
    arrange(route_id, direction_id, nb, shape_id) %>%
    dplyr::select(route_id, d = direction_id, ref = route_short_name, nb, name = route_long_name, shape_id) %>%
    filter(shape_id != "") %>%
    glimpse()
  tex_df2kable(df1, num = TRUE)
  return(invisible())
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
# détermination pour les trajets (trips) et des arrêts (stops)
# source("geo/scripts/transport.R");tidytransit_routes_trips_stops()
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
  carp('ajout trajet')
  trips.df <-tt$trips
  df3 <- df2 %>%
    left_join(trips.df, c("trip_id" = "trip_id")) %>%
    glimpse()
  carp('ajout route')
  routes.df <- tt$routes
  df4 <- df3 %>%
    left_join(routes.df, c("route_id" = "route_id")) %>%
#    filter(agency_id==agency) %>%
    glimpse()

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
  page <- sprintf("User:Mga_geo/Transports_publics/%s/gtfs/%s", Config["wiki"], "tidytransit_routes_trips_stops") %>%
    glimpse()
  wiki <- sprintf("==%s==", mtime)
  wiki <- sprintf("%s
%s", wiki, wiki_df2table(df5))
  wiki_page_init(page = page, article = wiki, force = TRUE)
}

#
## les données effectivement utilisées
#
# source("geo/scripts/transport.R");tidytransit_donnees_utiles()
tidytransit_donnees_utiles <- function(tt, force = TRUE) {
  library(tidyverse)
  carp("le gtfs")
  shapes.df <- tt$shapes

  carp("les routes bus")
  routes.df <- tt$routes %>%
    filter(route_type == 3) %>%
    glimpse()
  carp("les stops")
  stops.df <- tt$stops %>%
    glimpse()
# que les routes "bus"
  if (grepl("(rennes)", Reseau)) {
    routes.df <- routes.df %>%
      filter(!grepl("^(Transport scolaire|Métro)$", route_desc))
    routes.df <- routes.df %>%
      filter(!grepl("^5", route_id))
    stops.df <- stops.df %>%
      filter(!grepl("^5", stop_id))
  }
# Lorient, pas les bateaux
  if (grepl("lorient", Reseau)) {
    routes.df <- routes.df %>%
      filter(! grepl("^B", route_short_name))
  }
# aleop44, pas les bateaux
  if (Reseau == "_aleop44") {
    routes.df <- routes.df %>%
      filter(grepl("^3", route_short_name))
  }
# le téléphérique
  if (grepl("(brest)", Reseau)) {
    routes.df <- routes.df %>%
      filter(route_short_name != "C")
  }
  if (Reseau == "quimper_") {
    stops.df <- stops.df %>%
      mutate(stop_id = gsub("^ARCOM@", "", stop_id)) %>%
      mutate(stop_code = gsub("^ARCOM@", "", stop_code)) %>%
      glimpse()
    stop("****")
  }
  carp("les voyages")
  trips.df <- tt$trips %>%
    filter(route_id %in% routes.df$route_id) %>%
    glimpse()
  carp("les stop_times")
  stop_times.df <- tt$stop_times %>%
    filter(trip_id %in% trips.df$trip_id) %>%
    glimpse()
  carp("les arrêts utilisés")
  df1 <- stop_times.df %>%
    distinct(stop_id) %>%
    arrange(stop_id) %>%
    glimpse()
#
# pour tracer
  carp("les arrêts configurés")
  if (!grepl("(quimper|strasbourg)", Reseau)) {
    stops.df <- stops.df %>%
      filter(location_type == 0)
  }
  if (Reseau == "breizhgo") {
    stops.df <- stops.df %>%
      filter(!grepl("^SNCF", stop_id))
    shapes.df <- shapes.df %>%
      filter(grepl("^LRR", shape_id))
  }
  carp("les arrêts non utilisés")
  stops.df %>%
    filter(stop_id %notin% df1$stop_id) %>%
    glimpse()
  carp("les arrêts utilisés")
  stops.df <- stops.df %>%
    filter(stop_id %in% df1$stop_id) %>%
    glimpse()
  carp("les shapes configurés")
  tt$routes <- routes.df
  tt$stops <- stops.df
  if (Reseau == "aleop44") {
    tt$stops %>%
      filter(stop_id == "44HAUTalleA") %>%
      glimpse()
    stop("µµµµµµµµµµµµµµµµµµµµµµ")
  }

  tt$trips <- trips.df
  tt$stop_times <- stop_times.df
  tt$shapes <- shapes.df
  return(invisible(tt))
}