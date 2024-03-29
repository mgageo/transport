# <!-- coding: utf-8 -->
#
# le réseau de bus
#
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
#
#
#
## lecture du fichier excel
# source("geo/scripts/transport.R");config_jour()
config_jour <- function() {
  df <- config_agency_lire() %>%
    filter(!is.na(reseau)) %>%
    filter(!is.na(network)) %>%
    filter(!is.na(gtfs_dir)) %>%
    filter(!is.na(route_ref)) %>%
    filter(!is.na(territoire)) %>%
#    filter(!is.na(`network:wikidata`)) %>%
    dplyr::select(reseau, name, )
  misc_print(df)
#  stop("****")
  for (i in 1:nrow(df)) {
    carp("i: %s reseau: %s", i, df[i, "reseau"])
    gc()
    config_xls(df[i, "reseau"])
#  tidytransit_jour()
#  gtfs2osm_routemasters()
  diff_relations_bus_tags(force = TRUE, OsmChange = FALSE)
#    tidytransit_jour()
#    dsn <- sprintf("%s/shapes.txt", gtfsDir)
#    if (file.exists(dsn)) {
#      carp("reseau: %s shapes.txt Config_shapes: %s", Reseau, Config_shapes)
#    }
  }
}
#
## lecture du fichier excel
# source("geo/scripts/transport.R");config_agency_lire()
config_agency_lire <- function() {
  library(rio)
  library(tidyverse)
  dsn <- sprintf("%s/%s.xls", cfgDir, "agency")
  df <- rio::import(dsn) %>%
    replace_na(list(shapes = "FALSE"))
  return(invisible(df))
}
## lecture du fichier excel et filtrage
# source("geo/scripts/transport.R");config_xls('star')
config_xls <- function(res = "star", xls = "agency") {
  library(rio)
  library(tidyverse)
  dsn <- sprintf("%s/%s.xls", cfgDir, xls)
  Reseau <<- res
  if(! file.exists(dsn)) {
    Config <<- config_xls_init()
  } else {
    carp("import dsn: %s reseau: %s", dsn, res)
    Config.df <<- rio::import(dsn) %>%
      replace_na(list(shapes = "FALSE"))
    Config <<- Config.df %>%
      filter(reseau == res) %>%
      mutate(route_prefixe = replace_na(route_prefixe, ""))
  }
  if (nrow(Config) != 1) {
    misc_print(Config)
    confess("****** res: %s", res)
  }
  carp("network: %s", Config[1, "network"])
  cols <- colnames(Config)
  for (i in 1:ncol(Config)) {
    col <- sprintf("Config_%s", cols[i])
    assign(col, Config[[1, i]], envir = .GlobalEnv)
  }
  cfg_dir <<- sprintf("%s/%s", cfgDir, toupper(Config[1, "reseau"]))
  transportDir <<- sprintf("%s/%s", varDir, toupper(Config[1, "reseau"]))
  texDir <<- sprintf("%s/%s", tplDir, toupper(Config[1, "reseau"]))
  imagesDir <<- sprintf("%s/%s/images", tplDir, toupper(Config[1, "reseau"]))
  reseau_dir <<- sprintf("%s/%s", varDir, toupper(Config[1, "reseau"]))
  gtfsDir <<- sprintf("%s/%s", varDir, Config[1, "gtfs_dir"])
  josmDir <<- sprintf("%s/%s", transportDir, "JOSM")
  osmDir <<- sprintf("%s/%s", transportDir, "OSM")
  osrmDir <<- sprintf("%s/%s", transportDir, "OSRM")
  level0Dir <<- sprintf("%s/%s", transportDir, "LEVEL0")
  level0Dir <<- sprintf("%s/%s/level0", webDir, Config[1, "reseau"])
#  carp("gtfsDir: %s", gtfsDir);stop("****")
  dir.create(gtfsDir, showWarnings = FALSE, recursive = TRUE)
  dir.create(osmDir, showWarnings = FALSE, recursive = TRUE)
  dir.create(osrmDir, showWarnings = FALSE, recursive = TRUE)
  dir.create(josmDir, showWarnings = FALSE, recursive = TRUE)
  dir.create(level0Dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(imagesDir, showWarnings = FALSE, recursive = TRUE)
  return(invisible(Config))
}
config_xls_init <- function() {
  config <- list(
    reseau = "star",
    gtfs_dir = "STAR/GTFS",
    network = "FR:STAR"
  )
  df <- as_tibble_row(config)
  return(invisible(df))
}
config_xls_init <- function() {
  config <- list(
    reseau = "dinan",
    gtfs_dir = "DINAN/GTFS",
    network = "Dinamo"
  )
  df <- as_tibble_row(config)
  return(invisible(df))
}
#
## mes fichiers "perso" par réseau
#
# source("geo/scripts/transport.R");config_masters(reseau = Reseau)
config_masters <- function(reseau = "star") {
  library(rio)
  library(tidyverse)
  config_xls(reseau)
  carp("texDir: %s", texDir)
  dsn <- sprintf("%s/%s", texDir, "masters_mga.txt")
  masters.df <- rio::import(dsn, encoding = "UTF-8") %>%
    glimpse()

  routes.df <- txt_gtfs_fichier_lire(fichier = "routes") %>%
    dplyr::select(short = route_short_name, long = route_long_name, colour = route_color, text_coulor = route_text_color)
  df1 <- masters.df %>%
    full_join(routes.df, by = c("gtfs" = "short"))
#  misc_print(df1)
  df2 <- df1 %>%
    mutate(colour = sprintf("#%s", colour)) %>%
    mutate(text_coulor = sprintf("#%s", text_coulor)) %>%
    dplyr::select(-gtfs, -long) %>%
    glimpse()
  dsn <- sprintf("%s/%s", texDir, "masters.txt")
  rio::export(df2, dsn, sep = ";")
}