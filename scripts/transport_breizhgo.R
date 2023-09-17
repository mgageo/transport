# <!-- coding: utf-8 -->
#
# le réseau de bus de la région Bretagne
#
# fusion du réseau régional et des réseaux des départements
# https://www.breizhgo.bzh/se-deplacer-en-bretagne/se-deplacer-en-car
# 22 : Tibus
# 29 : PENNARBED
# 35 : Illenoo
# 56 : TIM
#
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
# source("geo/scripts/transport.R");breizhgo_jour()
breizhgo_jour <- function(reseau= "breizhgo", force = TRUE) {
  carp()
  config_xls(reseau);
#  mobibreizh_gtfs_reseau(Config[1, "reseau"], Config[1, "agency_id"])
  tidytransit_jour()
#  reseau_osm_jour(reseau = reseau, force = force)
}
# source("geo/scripts/transport.R");breizhgo_dpts()
breizhgo_dpts <- function(force = FALSE) {
  carp()
  library(rvest)
  dsn <- sprintf("%s/breizhgo_dpts.Rds", cfgDir)
  if (file.exists(dsn) & force == FALSE) {
    df1 <-  readRDS(dsn)
    return(invisible(df1))
  }
  dpts <- c("cotes-d-armor", "finistere", "ille-et-vilaine", "morbihan")
  df1 <- data.frame()
  for (dpt in dpts) {
    df <- breizhgo_dpt(dpt)
    df1 <- rbind(df1, df)
  }
  saveRDS(df1, file = dsn)
  carp("dsn: %s", dsn)
  return(invisible(df1))
}
breizhgo_dpt <- function(dpt) {
  carp()
  library(rvest)
  url <- sprintf("https://www.breizhgo.bzh/se-deplacer-en-bretagne/se-deplacer-en-car/%s", dpt)
  html <- rvest::read_html(url)
# div.col-sm-6:nth-child(1)
# div.col-sm-6:nth-child(1) > h4:nth-child(4)
  nodes <- html_node(html,"div.col-sm-6:nth-child(1)") %>%
    html_nodes("h4") %>%
    html_text()
  lignes <- grep("^Ligne", nodes, value = TRUE)
  df <- data.frame(ligne = lignes) %>%
    mutate(dpt = !!dpt)
  return(invisible(df))
}
#
# source("geo/scripts/transport.R");breizhgo_masters()
breizhgo_dpts_masters <- function(force = FALSE) {
  carp()
  library(tidyverse)
  library(stringr)
  library(rio)
  df <- breizhgo_dpts(force) %>%
    glimpse()
  vec <- c(
    "cotes-d-armor" = 22,
    "finistere" = 29,
    "ille-et-vilaine" = 35,
    "morbihan" = 56
  )
  df1 <- df %>%
    mutate(dpt_no = vec[as.character(dpt)]) %>%
    extract(ligne, c("no"), "Ligne (\\S+)", remove = FALSE) %>%
    mutate(ref_network = sprintf("%s-%s", dpt_no, no)) %>%
    mutate(ligne = str_trim(ligne)) %>%
    dplyr::select(ref_network, ligne) %>%
    glimpse()
  dsn <- sprintf("%s/breizhgo_dpts.csv", cfgDir)
  rio::export(df1, dsn)
  carp("dsn: %s", dsn)
#  misc_print(df1)
}
#
# source("geo/scripts/transport.R");breizhgo_dpts_masters()
breizhgo_dpts_masters <- function(force = FALSE) {
  carp()
  library(tidyverse)
  library(stringr)
  library(rio)
  library(sf)
  lignes.df <- mobibreizh_lignes_lire() %>%
    st_drop_geometry() %>%
    dplyr::select(id, nom) %>%
    glimpse()
  dsn <- sprintf("%s/breizhgo_dpts_mga.csv", cfgDir)
  dpts.df <- rio::import(dsn, encoding = "UTF-8")
  df1 <- lignes.df %>%
    full_join(dpts.df, by = c("id" = "ref_network")) %>%
    arrange(id) %>%
    glimpse()
  misc_print(df1)
}
#
# source("geo/scripts/transport.R");breizhgo_dpts_masters()
breizhgo_dpts_masters <- function(force = FALSE) {
  carp()
  library(tidyverse)
  library(stringr)
  library(rio)
  dsn <- sprintf("%s/breizhgo_dpts_mga.csv", cfgDir)
  dpts.df <- rio::import(dsn, encoding = "UTF-8") %>%
    extract(ligne, c("dpts"), ":\\s(.*)$", remove = FALSE) %>%
    glimpse()
  dsn <- sprintf("%s/BREIZHGO/routemaster.txt", cfgDir)
  masters.df <- rio::import(dsn, encoding = "UTF-8") %>%
    dplyr::select(ref_network, ref, name) %>%
    extract(name, c("masters"), ":\\s(.*)$", remove = FALSE) %>%
    glimpse()
  df <- masters.df %>%
    filter(is.na(masters))
  if (nrow(df) > 0) {
    View(df)
  }
  df1 <- dpts.df %>%
    full_join(masters.df, by = c("ref_network" = "ref_network")) %>%
    arrange(ref_network) %>%
    filter(! is.na(ref)) %>%
    filter(dpts == masters) %>%
    glimpse()
  misc_print(df1)
  View(df1)
}
#
# source("geo/scripts/transport.R");breizhgo_masters_lire()
breizhgo_masters_lire <- function(force = FALSE) {
  carp()
  library(tidyverse)
  library(stringr)
  library(rio)
  dsn <- sprintf("%s/BREIZHGO/routemaster.txt", cfgDir)
  masters.df <- rio::import(dsn, encoding = "UTF-8") %>%
    extract(name, c("masters"), ":\\s(.*)$", remove = FALSE) %>%
    glimpse()
  df <- masters.df %>%
    filter(is.na(masters) | is.na(colour) | is.na(text_colour))
  if (nrow(df) > 0) {
    View(df)
  }
  masters.df <- masters.df %>%
    mutate(description = masters) %>%
    dplyr::select(-masters)
  dsn <- sprintf("%s/BREIZHGO/master.txt", cfgDir)
  rio::export(masters.df, dsn, sep = ";")
}
#
# source("geo/scripts/transport.R");breizhgo_gtfs_routes()
breizhgo_gtfs_routes <- function(force = FALSE) {
  carp()
  library(tidyverse)
  library(stringr)
  library(rio)
  config_xls('breizhgo');
  vec <- c(
    "TIBUS" = "22",
    "PENNARBED" = "29",
    "ILLENOO2" = "35",
    "TIM" = "56"
  )
  dsn <- sprintf("%s/routes.txt", gtfsDir)
  routes.df <- rio::import(dsn, encoding = "UTF-8") %>%
    mutate(dpt_no = vec[agency_id]) %>%
    filter(! grepl("(TAD|NDP|PLG|VELO|ETE)", route_short_name)) %>%
    filter(! is.na(dpt_no)) %>%
    mutate(ref_network = sprintf("%s-%s", dpt_no, str_replace(route_short_name, "^0", ""))) %>%
    dplyr::select(-agency_id, -dpt_no, -route_type, -route_desc) %>%
    arrange(ref_network) %>%
    glimpse()
  misc_print(routes.df)
  dsn <- sprintf("%s/breizhgo_gtfs.csv", cfgDir)
  rio::export(routes.df, dsn)
  carp("dsn: %s", dsn)
}
# source("geo/scripts/transport.R");breizhgo_relations_routemaster_bus()
breizhgo_relations_routemaster_bus <- function(fic='relations_routemaster_bus') {
  carp()
  config_xls('breizhgo');
  osm_relations_routemaster_bus_csv(fic, force = TRUE)
}
# source("geo/scripts/transport.R"); df <- breizhgo_osm_gtfs()
breizhgo_osm_gtfs <- function() {
  carp()
  config_xls('breizhgo');
  dsn <- sprintf("%s/OSM/relations_routemaster_bus.csv", transportDir)
  carp("dsn: %s", dsn)
  osm.df <- rio::import(dsn, encoding = "UTF-8") %>%
    dplyr::select(ref_network = `ref:network`, name) %>%
    glimpse()
#  osm.df <- misc_df_utf8(osm.df)
  dsn <- sprintf("%s/breizhgo_gtfs.csv", cfgDir)
  carp("dsn: %s", dsn)
  gtfs.df <- rio::import(dsn) %>%
    dplyr::select(ref_network, route_long_name) %>%
    glimpse()
  df1 <- osm.df %>%
    full_join(gtfs.df, by = c("ref_network" = "ref_network")) %>%
    arrange(ref_network) %>%
#    filter(name == route_long_name) %>%
    glimpse()
  misc_print(df1)
  View(df1)
  return(invisible(df1))
}