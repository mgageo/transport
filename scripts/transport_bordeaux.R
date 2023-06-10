# <!-- coding: utf-8 -->
#
# le réseau de bus TBM de Bordeaux
#
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
# source("geo/scripts/transport.R");bordeaux_jour()
bordeaux_jour <- function(reseau = "bordeaux", force = TRUE) {
  library(tidyverse)
  library(rio)
  library(archive)
  carp()
  config_xls(reseau)
  reseau_tpl_tex(reseau = reseau)
#  bordeaux_opendata()
#  tidytransit_jour()
#  tidytransit_routes_trips_stops()
  tex_pdflatex(sprintf("%s_osm.tex", reseau))
}
#
# téléchargement des données en opendata
#
# source("geo/scripts/transport.R");bordeaux_opendata()
bordeaux_opendata <- function(force = TRUE) {
  library(tidyverse)
  library(rio)
  library(archive)
  carp()
# https://opendata.bordeaux-metropole.fr/explore/dataset/sv_arret_p/export/
  gtfs_source <- "https://opendata.bordeaux-metropole.fr/api/explore/v2.1/catalog/datasets/sv_arret_p/exports/geojson?lang=fr&timezone=Europe%2FBerlin"
  dsn_source <- sprintf("%s/%s", gtfsDir, "sv_arret_p.geojson")
  carp("dsn_source: %s", dsn_source)
  if (force == TRUE | ! file.exists(dsn_source) ) {
    download.file(gtfs_source, dsn_source)
  }
  gtfs_source <- "https://opendata.bordeaux-metropole.fr/api/explore/v2.1/catalog/datasets/sv_chem_l/exports/geojson?lang=fr&timezone=Europe%2FBerlin"
  dsn_source <- sprintf("%s/%s", gtfsDir, "sv_chem_l.geojson")
  carp("dsn_source: %s", dsn_source)
  if (force == TRUE | ! file.exists(dsn_source)) {
    download.file(gtfs_source, dsn_source)
  }
  gtfs_source <- "https://opendata.bordeaux-metropole.fr/api/explore/v2.1/catalog/datasets/sv_ligne_a/exports/csv?lang=fr&timezone=Europe%2FBerlin&use_labels=true&delimiter=%3B"
  dsn_source <- sprintf("%s/%s", gtfsDir, "sv_ligne_a.csv")
  carp("dsn_source: %s", dsn_source)
  if (force == TRUE | ! file.exists(dsn_source)) {
    download.file(gtfs_source, dsn_source)
  }
  gtfs_source <- "https://opendata.bordeaux-metropole.fr/api/explore/v2.1/catalog/datasets/sv_arret_p/exports/csv?lang=fr&timezone=Europe%2FBerlin&use_labels=true&delimiter=%3B"
  dsn_source <- sprintf("%s/%s", gtfsDir, "sv_arret_p.csv")
  carp("dsn_source: %s", dsn_source)
  if (force == TRUE | ! file.exists(dsn_source)) {
    download.file(gtfs_source, dsn_source)
  }

  return()
  gtfs_source <- "https://opendata.bordeaux-metropole.fr/api/datasets/1.0/offres-de-services-bus-tramway-gtfs/attachments/keolis_buszip/"
  dsn_source <- sprintf("%s/gtfs.zip", gtfsDir)
  if (force == TRUE | ! file.exists(dsn_source) ) {
    download.file(gtfs_source, dsn_source)
    setwd(gtfsDir)
    archive::archive_extract("gtfs.zip", dir = ".")
    setwd(baseDir)
  }
}
#
# source("geo/scripts/transport.R"); nc1 <- bordeaux_arrets_lire() %>% glimpse()
bordeaux_arrets_lire <- function(force = TRUE) {
  library(tidyverse)
  library(sf)
  dsn_source <- sprintf("%s/%s", gtfsDir, "sv_arret_p.geojson")
  carp("dsn_source: %s", dsn_source)
  arrets.sf <- st_read(dsn_source)
  return(invisible(arrets.sf))
}
#
# source("geo/scripts/transport.R");bordeaux_arrets()
bordeaux_arrets <- function(force = TRUE) {
  library(tidyverse)
  library(sf)
  carp()
  arrets.sf <- bordeaux_arrets_arret_lire()
  arrets.df <- arrets.sf %>%
    st_drop_geometry() %>%
    mutate(numero = sprintf("%s", numero)) %>%
    filter(vehicule == "BUS") %>%
    filter(actif != 1) %>%
    glimpse()
#    dsn_source <- sprintf("%s/%s", gtfsDir, "sv_arret_p.csv")
#  carp("dsn_source: %s", dsn_source)
#  arrets.df <- rio::import(dsn_source) %>%
#    glimpse()
#  stop("****")
#  tidytransit_zip_lire()
  carp("le gtfs")
  tt <- tidytransit_lire()
  stops.df <- tt$stops %>%
    glimpse()
  df1 <- arrets.df %>%
    left_join(stops.df, by = c("numero" = "stop_id")) %>%
    arrange(numero)
  df2 <- df1 %>%
    filter(is.na(stop_code)) %>%
    glimpse()
  df3 <- df1 %>%
    filter(is.na(gml_id)) %>%
    glimpse()
  df4 <- df1 %>%
    filter(! is.na(stop_code)) %>%
    filter(libelle != stop_name) %>%
    glimpse()
  df5 <- df4 %>%
    dplyr::select(ident, libelle, stop_name)
  misc_print(df5)
  return()
  df1 <- arrets.df %>%
    left_join(stops.df, by = c("libelle" = "stop_name"), relationship = "many-to-many") %>%
    arrange(libelle) %>%
    glimpse()
  misc_print(head(df1, 10))
  stops.sf <- st_as_sf(stops.df, coords = c("stop_lon", "stop_lat"), crs = 4326, remove = FALSE) %>%
#    head(100) %>%
    glimpse()
  carp("rapprochement plus proche")
#  nc1 <- st_nearest_points(arrets.sf, stops.sf) %>%
#    glimpse()
  nc3 <- bordeaux_proches(stops.sf, arrets.sf)
  dsn <- sprintf("%s/%s", transportDir, "bordeaux_arrets.Rds")
  carp("dsn: %s", dsn)
  saveRDS(nc3, file = dsn)
}
bordeaux_proches <- function(nc1, nc2, k = 1) {
  library(nngeo)
  n <- st_nn(nc1, nc2, k = k, progress = TRUE, returnDist = TRUE)
  mga <<- n
  ids <-  sapply(n[[1]], "[", k)
  dists <- sapply(n[[2]], "[", k)
  df3 <- data.frame(nc1, st_drop_geometry(nc2)[ids, , drop = FALSE])
  nc3 <- st_sf(df3)
  nc3$dist <- dists
  return(invisible(nc3))
}
#
# source("geo/scripts/transport.R");bordeaux_arrets_diff()
bordeaux_arrets_diff <- function(reseau = "bordeaux", force = TRUE) {
  library(tidyverse)
  library(nngeo)
  library(sf)
  carp()
  config_xls(reseau)
  dsn <- sprintf("%s/%s", transportDir, "bordeaux_arrets.Rds")
  nc1 <- readRDS(dsn) %>%
    st_drop_geometry()
  nc2 <- nc1 %>%
    filter(dist < 2) %>%
    filter(libelle == stop_name) %>%
    glimpse()
  misc_print(head(nc2, 10))
  nc3 <- nc1 %>%
    filter(dist < 2) %>%
    filter(libelle != stop_name) %>%
    glimpse()
  misc_print(head(nc3, 10))
  nc4 <- nc1 %>%
    filter(dist >= 2) %>%
    filter(libelle == stop_name) %>%
    arrange(-dist) %>%
    glimpse()
  misc_print(head(nc4, 10))
}
#
# source("geo/scripts/transport.R");bordeaux_arrets_overpass_query()
bordeaux_arrets_overpass_query <- function(force = TRUE) {
  library(tidyverse)
  library(sf)
  carp()
  dsn <- sprintf("bordeaux_arrets_overpass")
  requete <- sprintf('
area[name="%s"]->.a;
(
node(area.a)[highway=bus_stop][public_transport=platform];
);
out meta;', Config[1, "zone"] )
  dsn <- overpass_query(query = requete, fic = dsn, force = force)
  return(invisible(dsn))
}
#
# source("geo/scripts/transport.R");bordeaux_arrets_overpass_parse(force = FALSE)
bordeaux_arrets_overpass_parse <-  function(force = TRUE) {
  library(tidyverse)
  library(rio)
  library(sf)
  library(osmdata)
  carp()
  dsn <- bordeaux_arrets_overpass_query(force = force)
  carp("dsn: %s", dsn)
  q <- opq(bbox = c(45, -6, 58, 0))
  od <- osmdata_sf(q, dsn) %>%
    glimpse()
  points.sf <- od$osm_points %>%
    dplyr::select(osm_id, name, "ref:FR:TBM", "ref:TBM") %>%
    glimpse()
  Encoding(points.sf$name) <- "UTF-8"
  dsn <- sprintf("%s/bordeaux_arrets_overpass.Rds", transportDir)
  carp("dsn: %s", dsn)
  saveRDS(points.sf, file = dsn)
}
#
# source("geo/scripts/transport.R");bordeaux_arrets_arret_diff(reseau = "bordeaux", force  = FALSE)
bordeaux_arrets_arret_diff <-  function(name = "Talence", reseau = "bordeaux", force = TRUE) {
  library(tidyverse)
  library(rio)
  library(sf)
  library(janitor)
  carp("l'opendata")
  arrets.sf <- bordeaux_arrets_arret_lire() %>%
    filter(insee == "33522") %>%
#    filter(source == "SAEIV_BUS") %>%
    dplyr::select(ident, voirie, groupe, source, voirie) %>%
    glimpse()
  dsn <- sprintf("%s/bordeaux_overpass_%s.Rds", transportDir, name)
  carp("osm dsn: %s", dsn)
  stops.sf <- readRDS(file = dsn) %>%
    clean_names() %>%
    glimpse()
  df1 <- stops.sf %>%
    st_drop_geometry() %>%
    group_by(ref_fr_tbm) %>%
    summarize(nb = n()) %>%
    filter(nb > 1)
  if (nrow(df1) > 0) {
    confess("ref en double: %s", nrow(df1))
  }
  carp("les arrêts absents d'osm")
  df1 <- arrets.sf %>%
    st_drop_geometry() %>%
    filter(ident %notin% stops.sf$ref_fr_tbm) %>%
    filter(source == "SAEIV_BUS") %>%
    glimpse()
  misc_print(df1)
  arrets.sf <- arrets.sf %>%
    filter(ident %in% stops.sf$ref_fr_tbm) %>%
    glimpse()
  df1 <- dplyr::full_join(
      arrets.sf %>% as.data.frame(),
      stops.sf %>% as.data.frame(),
      by = c("ident" = "ref_fr_tbm")
    ) %>%
    glimpse()
  df1$d <- as.numeric(st_distance(df1$geometry.x, df1$geometry.y, by_element = TRUE))
  df2 <- df1 %>%
    filter(is.na(osm_id) | is.na(source) | d > 10) %>%
    dplyr::select(-geometry.x, -geometry.y) %>%
    glimpse()
  misc_print(df2)
}
#
# source("geo/scripts/transport.R");bordeaux_arrets_overpass_diff(force = FALSE)
bordeaux_arrets_overpass_diff <- function(force = TRUE) {
  library(tidyverse)
  library(nngeo)
  library(sf)
  carp()

  dsn2 <- sprintf("%s/bordeaux_arrets_overpass_diff.Rds", transportDir)
  if ( ! file.exists(dsn2) | force == TRUE) {
    dsn1 <- sprintf("%s/bordeaux_arrets_overpass.Rds", transportDir)
    nc1 <- readRDS(dsn1) %>%
#    slice_head(n = 10) %>%
      glimpse()
    nc2 <- bordeaux_proches(nc1, nc1, k = 2)
    carp("dsn2: %s", dsn2)
    saveRDS(nc2, file = dsn2)
  } else {
    nc2 <- readRDS(dsn2)
  }
#  josm <- "http://127.0.0.1:8111/load_and_zoom?left=8.19&right=8.20&top=48.605&bottom=48.590&select=node413602999
  zoom <- "left=%0.5f&right=%0.5f&top=%0.5f&bottom=%0.5f"
  select <- "node%s,node%s"
  df2 <- nc2 %>%
    mutate(X=st_coordinates(.)[,'X']) %>%
    mutate(Y=st_coordinates(.)[,'Y']) %>%
    st_drop_geometry() %>%
    filter(dist < 15) %>%
    filter(osm_id < osm_id.1) %>%
    mutate(area = gsub("[AR]$", "", ref.TBM)) %>%
    mutate(area1 = gsub("[AR]$", "", ref.TBM.1)) %>%
    filter(area != area1) %>%
    mutate(select = sprintf(select, osm_id, osm_id.1)) %>%
    mutate(zoom = sprintf(zoom, X - .001, X + .001, Y + .001, Y - .001)) %>%
    mutate(josm = sprintf("<a href='http://127.0.0.1:8111/load_and_zoom?%s&select=%s'>josm</a>", zoom, select)) %>%
    dplyr::select(-select, -zoom, -X, -Y) %>%
    glimpse()
  html <- misc_html_titre("bordeaux_arrets")
  html <- misc_html_append_df(html, df2)
  dsn <- sprintf("%s/bordeaux_arrets.html", webDir)
  write(html, dsn)
  carp("dsn: %s", dsn)
  url <- sprintf("http://localhost/transport/bordeaux_arrets.html")
  browseURL(
    url,
    browser = "C:/Program Files/Mozilla Firefox/firefox.exe"
  )
  return(invisible())
}
#
# source("geo/scripts/transport.R"); nc1 <- bordeaux_chemins_lire() %>% glimpse()
bordeaux_chemins_lire <- function(force = TRUE) {
  library(tidyverse)
  library(sf)
  dsn_source <- sprintf("%s/%s", gtfsDir, "sv_chem_l.geojson")
  carp("dsn_source: %s", dsn_source)
  chemins.sf <- st_read(dsn_source) %>%
    filter(vehicule %notin% c("TRAM"))
  return(invisible(chemins.sf))
}
#
# ajout départ et arrivée
# source("geo/scripts/transport.R"); nc1 <- bordeaux_chemins_da() %>% glimpse()
bordeaux_chemins_da <- function(force = TRUE) {
  library(tidyverse)
  library(sf)
  Reseau <<- "bordeaux"; # Bordeaux Métropole
  config_xls(Reseau)
  chemins.sf <- bordeaux_chemins_lire() %>%
    mutate(rg_sv_arret_p_na = sprintf("%s", rg_sv_arret_p_na))
  arrets.df <- bordeaux_arrets_lire() %>%
    st_drop_geometry() %>%
    mutate(gid = sprintf("%s", gid)) %>%
    dplyr::select(gid, libelle)
  lignes.df <- bordeaux_lignes_lire() %>%
    mutate(gid = sprintf("%s", gid)) %>%
    dplyr::select(gid, libelle, ident)
  nc1 <- chemins.sf %>%
    left_join(arrets.df, by = c("rg_sv_arret_p_nd" = "gid"), suffix = c("", "_nd")) %>%
    left_join(arrets.df, by = c("rg_sv_arret_p_na" = "gid"), suffix = c("", "_na")) %>%
    left_join(lignes.df, by = c("rs_sv_ligne_a" = "gid"), suffix = c("", "_li")) %>%
#    filter(libelle_nd == "Fontaine d'Arlac") %>%
#    filter(libelle_nd == "Les Renardeaux") %>%
    filter(libelle_li == "Flexo 51") %>%
#   filter(sens == "ALLER") %>%
    filter(sens == "RETOUR") %>%
    arrange(ident, sens) %>%
    filter(vehicule == "BUS") %>%
    glimpse()
  dsn <- sprintf("%s/%s", transportDir, "bordeaux_chemins_da.geojson")
  carp("dsn: %s", dsn)
  st_write(st_transform(nc1, 4326), dsn, delete_dsn = TRUE, driver = "GeoJSON")
}
#
# source("geo/scripts/transport.R"); df <- bordeaux_lignes_lire() %>% glimpse()
bordeaux_lignes_lire <- function(force = TRUE) {
  library(tidyverse)
  library(readr)
  library(janitor)
  carp()
  dsn <- sprintf("%s/%s", gtfsDir, "sv_ligne_a.csv")
  carp("dsn: %s", dsn)
  df <- read_delim(dsn, delim = ";") %>%
    clean_names()
  return(invisible(df))
}
# Bordeaux Métropole : 243300316 SIREN_EPCI
#
# source("geo/scripts/transport.R");bordeaux_metropole_ign()
bordeaux_metropole_ign <- function(reseau = "bordeaux", force = TRUE) {
  library(tidyverse)
  library(sf)
  carp()
  config_xls(reseau)
  nc1 <- ign_adminexpress_lire_sf(layer = "COMMUNE", force = FALSE) %>%
    filter(SIREN_EPCI == "243300316") %>%
    glimpse()
  dsn <- sprintf("%s/%s", transportDir, "metropole_ign.Rds")
  carp("dsn: %s", dsn)
  saveRDS(nc1, file = dsn)
}
#
# source("geo/scripts/transport.R");bordeaux_commune(commune = "Talence")
bordeaux_commune <- function(commune = "Talence", reseau = "bordeaux", force = TRUE) {
  library(tidyverse)
  library(sf)
  carp()
  config_xls(reseau)
  nc1 <- ign_adminexpress_lire_sf(layer = "COMMUNE", force = FALSE) %>%
    filter(SIREN_EPCI == "243300316") %>%
    glimpse()
  dsn <- sprintf("%s/%s", transportDir, "metropole_ign.Rds")
  carp("dsn: %s", dsn)
  nc1 <- readRDS(dsn) %>%
    filter(NOM == commune) %>%
    glimpse()
  return(invisible())
}
#
# source("geo/scripts/transport.R");bordeaux_infotbm()
bordeaux_infotbm <- function(reseau = "bordeaux", force = TRUE) {
  library(tidyverse)
  library(rvest)
  library(rio)
  carp()
  config_xls(reseau)
  url <- "https://www.infotbm.com/fr/lignes"
  session <- rvest::session(url)
#  session <- read_html(url) %>%
#    glimpse()
  liens <- session %>%
    html_node("div.m-md-b") %>%
    html_node("div.lines-wrapper") %>%
    html_node("div") %>%
    html_nodes("a.l-tbm") %>%
    html_attr("href") %>%
    glimpse()
  carp("liens: %s", length(liens))
  df <- tibble(
    lien = character(),
    ref = character(),
    no = character(),
    from = character(),
    to = character())
  for (lien in liens) {
    ligne <- session %>%
      session_jump_to(lien)
    lis <- session %>%
      html_node("nav.breadcrumb") %>%
      html_nodes("li")
    ref <- lis[[2]] %>%
      html_text2()
    no <- ""
    if (length(lis) > 2) {
      no <- lis[[3]] %>%
        html_text2()
    }
    lines <- ligne %>%
      html_node("div.lines") %>%
      html_nodes("h2")
    from <- ""
    if (length(lines) > 1) {
      from <- lines[[1]] %>%
        html_text2()
    }
    to <- ""
    if (length(lines) > 1) {
      to <- lines[[2]] %>%
        html_text2()
    }
    df <- df %>%
      add_row(lien = lien, ref = ref, no = no, from = from, to = to)
  }
  glimpse(df)
  dsn <- sprintf("%s/%s", transportDir, "bordeaux_infotbm.csv")
  carp("dsn: %s", dsn)
  rio::export(df, file = dsn)
  return(invisible())
}
#
# source("geo/scripts/transport.R");bordeaux_masters()
bordeaux_masters <- function(reseau = "bordeaux", force = TRUE) {
  library(tidyverse)
  library(rvest)
  library(rio)
  carp()
  config_xls(reseau)
  dsn <- sprintf("%s/%s", transportDir, "bordeaux_infotbm.csv")
  carp("dsn: %s", dsn)
  df1 <- rio::import(file = dsn, encoding = "UTF-8") %>%
    mutate(route_id = gsub(".*/", "", lien)) %>%
    mutate(route_id = as.integer(route_id)) %>%
    glimpse()
  dsn <- sprintf("%s/%s", gtfsDir, "routes.txt")
  carp("dsn: %s", dsn)
  df2 <- rio::import(file = dsn) %>%
    glimpse()
  df3 <- df2 %>%
    left_join(df1, by = c("route_id")) %>%
    mutate(id = -1) %>%
    mutate(description = sprintf("%s <> %s", from, to)) %>%
    mutate(route_color = sprintf("#%s", route_color)) %>%
    mutate(route_text_color = sprintf("#%s", route_text_color)) %>%
    mutate(url = sprintf("%s%s", "https://www.infotbm.com", lien)) %>%
    dplyr::select(
      id,
      ref = route_id,
      name = route_long_name,
      description,
      colour = route_color,
      text_colour = route_text_color,
      url
    ) %>%
    glimpse()
  df3 <- df3 %>%
    mutate(description = gsub("AMBES", "AMBÈS", description))%>%
    mutate(description = gsub("AMBARES", "AMBARÈS", description))%>%
    mutate(description = gsub("BEGLES", "BÈGLES", description))%>%
    mutate(description = gsub("SAINT-MEDARD", "SAINT-MÉDARD", description))%>%
    mutate(description = gsub(" MEDARD", " MÉDARD", description))%>%
    mutate(description = gsub("MERIGNAC", "MÉRIGNAC", description))
  dsn <- sprintf("%s/%s", cfg_dir, "masters.txt")
  carp("dsn: %s", dsn)
  rio::export(df3, file = dsn, sep = ";")
  return(invisible())
}
#
# source("geo/scripts/transport.R");bordeaux_arrets_network_overpass_query()
bordeaux_arrets_network_overpass_query <- function(reseau = "bordeaux", force = TRUE) {
  library(tidyverse)
  library(sf)
  carp()
  config_xls(reseau)
  dsn <- sprintf("bordeaux_arrets_network_overpass_query")
  requete <- sprintf('
relation[network="%s"][route=bus]->.a;
node(r.a)[public_transport~platform];
out meta;', Config[1, "network"] )
  dsn <- overpass_query(query = requete, fic = dsn, force = force)
}
#
# source("geo/scripts/transport.R");bordeaux_arrets_network_overpass_parse(reseau = "bordeaux", force  = FALSE)
bordeaux_arrets_network_overpass_parse <-  function(reseau = "bordeaux", force = TRUE) {
  library(tidyverse)
  library(rio)
  library(sf)
  library(osmdata)
  carp()
  config_xls(reseau)
  dsn <- bordeaux_arrets_network_overpass_query(reseau = reseau, force = force)
  carp("dsn: %s", dsn)
  q <- opq(bbox = c(45, -6, 58, 0))
  od <- osmdata_sf(q, dsn) %>%
    glimpse()
  points.sf <- od$osm_points %>%
    dplyr::select(osm_id, name, "ref:TBM", "ref:FR:TBM") %>%
    glimpse()
  Encoding(points.sf$name) <- "UTF-8"
  dsn <- sprintf("%s/bordeaux_arrets_network_overpass_parse.Rds", transportDir)
  carp("dsn: %s", dsn)
  saveRDS(points.sf, file = dsn)
}
#
# source("geo/scripts/transport.R");bordeaux_arrets_kref()
bordeaux_arrets_kref <- function(reseau = "bordeaux", force = TRUE) {
  library(tidyverse)
  library(rio)
  carp()
  config_xls(reseau)
  dsn <- sprintf("%s/bordeaux_arrets_network_overpass_parse.Rds", transportDir)
  carp("dsn: %s", dsn)
  df <- readRDS(dsn) %>%
    st_drop_geometry() %>%
    mutate(ref = gsub("^B_", "", `ref:FR:TBM`)) %>%
    mutate(ref = gsub("_", "", ref))
  df1 <- df %>%
    filter(!is.na(ref)) %>%
    filter(ref != `ref:TBM`) %>%
    glimpse()
  misc_print(df1)
  level0 <- paste(df1$osm_id, collapse = ',n')
  print(level0)
  df2 <- df %>%
    filter(is.na(`ref:TBM`)) %>%
    filter(!is.na(ref)) %>%
    glimpse()
#  misc_print(df2)
  carp("le gtfs")
  tt <- tidytransit_lire()
  stops.df <- tt$stops %>%
    filter(location_type == 0) %>%
    dplyr::select(stop_code, stop_name, stop_lon, stop_lat)
  carp("les ref inconnues du gtfs")
  df3 <- df2 %>%
    filter(ref %notin% stops.df$stop_code) %>%
    glimpse()
  carp("mais connue de arret")
  arrets.sf <- bordeaux_arrets_lire()
  arrets.df <- arrets.sf %>%
    st_drop_geometry()
  df3 <- df2 %>%
    filter(`ref:FR:TBM` %notin% arrets.df$ident) %>%
    glimpse()
  return(invisible())
}
#
# les arrêts effectivement utilisés
#
# source("geo/scripts/transport.R");bordeaux_arrets_utils()
bordeaux_arrets_utils <- function(force = TRUE) {
  library(tidyverse)
  library(sf)
  carp("le gtfs")
  tt <- tidytransit_lire()
  routes.df <- tt$routes %>%
    filter(route_type == 3)
  trips.df <- tt$trips %>%
    filter(route_id %in% routes.df$route_id) %>%
    glimpse()
  stop_times.df <- tt$stop_times %>%
    filter(trip_id %in% trips.df$trip_id) %>%
    distinct(stop_id) %>%
    glimpse()
  stops.df <- tt$stops %>%
    filter(location_type == 0) %>%
    glimpse() %>%
    filter(stop_id %in% stop_times.df$stop_id) %>%
    glimpse()

  return(invisible(stops.df))
}
#
## les cas problématiques
#
# source("geo/scripts/transport.R");bordeaux_pb_arret()
# sur ligne 1, Saget
bordeaux_pb_arret <- function(force = TRUE) {
  library(tidyverse)
  carp("le gtfs")
  tt <- tidytransit_lire() %>%
    glimpse()
  carp("le code des arrêts")
  stops.df <- tt$stops %>%
    filter(stop_name == "Saget") %>%
    filter(parent_station != "") %>%
    glimpse()
  carp("les stop_times")
  stop_times.df <- tt$stop_times %>%
    filter(stop_id %in% stops.df$stop_id) %>%
    glimpse()
  carp("les parcours")
  df1 <- stop_times.df %>%
    left_join(tt$trips, by = c("trip_id" = "trip_id")) %>%
    group_by(route_id, direction_id) %>%
    summarize(nb = n()) %>%
    glimpse()
  trips.df <- tt$trips %>%
    filter(trip_id %in% stop_times.df$trip_id) %>%
    filter(route_id == "01") %>%
    filter(direction_id == 0) %>%
    glimpse()
}