# <!-- coding: utf-8 -->
#
# les réseaux de bus de la région Bretagne
#
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
#
## validation des fichiers gtfs de la région
#
# source("geo/scripts/transport.R");destineo_jour()
destineo_jour <- function(reseau = "destineo", force = TRUE) {
  library(tidyverse)
  library(rio)
  library(archive)
  carp()
  config_xls(reseau)
  destineo_dl()
  destineo_reseaux()
}
#
## la version avec un fichier gtfs unique
# https://donnees.paysdelaloire.fr/data/pdl.zip
#
# source("geo/scripts/transport.R");destineo_dl()
destineo_dl <- function(reseau = "destineo", force = TRUE) {
  library(tidyverse)
  library(rio)
  library(archive)
  carp()
  config_xls(reseau)
  dir.create(gtfsDir, showWarnings = FALSE, recursive = TRUE)
  gtfs_source <- Config[1, "gtfs_source"]
  fn_source <- gsub("^.*/", "", gtfs_source)
  fn_source <- gsub(".zip$", "", fn_source)
  today <- sprintf("%s", Sys.Date())
  dsn_source <- sprintf("%s/%s_%s.zip", gtfsDir, fn_source, today)
  carp("dsn_source: %s", dsn_source)
  if (! file.exists(dsn_source)) {
    download.file(gtfs_source, dsn_source)
  }
  dsn <- sprintf("%s/gtfs.zip", gtfsDir)
  file.copy(dsn_source, dsn, overwrite = TRUE)
  carp("dsn: %s", dsn)
  archive_extract(dsn, gtfsDir)
}
#
# extraction par réseau et production du gtfs.zip
#
# source("geo/scripts/transport.R");destineo_reseaux()
destineo_reseaux <- function() {
  library(tidyverse)
  library(rio)
  Tex <<- FALSE
  Wiki <<- FALSE
  df <- destineo_agency_lire() %>%
    glimpse()
#  return()
  for (i in 1:nrow(df)) {
    Reseau <- df[i, "reseau"]
    agency_id <- df[i, "agency_id"]
    gtfs_dir <- gtfsDir
    destineo_gtfs_reseau(Reseau, agency_id, gtfs_dir)
    next
    config_xls(Reseau)
    wiki_pages_init()
    shapes_dsn <- sprintf("%s/%s/shapes.txt", varDir, gtfs_dir)
    if (file.exists(shapes_dsn)) {
      size <- file.info(shapes_dsn)$size
      shapes <- df[i, "shapes"]
      if (size > 50000 & shapes != TRUE) {
        carp("reseau: %s shapes_dsn: %s size: %s", Reseau, shapes_dsn, format(size, big.mark = " "))
        carp("reseau: %s shapes: %s", Reseau, shapes)
      }
    }
    config_xls(Reseau);tidytransit_jour()
  }
}
#
# source("geo/scripts/transport.R");destineo_gtfs_reseau("auray", "AURAYBUS")
destineo_gtfs_reseau <- function(reseau, agency_id, gtfs_dir) {
  library(tidyverse)
  library(rio)
  library(stringr)
  library(archive)
  carp("reseau: %s agency_id: %s gtfs_dir: %s", reseau, agency_id, gtfs_dir)
  reseau_dir <- sprintf("%s/%s", gtfs_dir, agency_id)
  dir.create(reseau_dir, showWarnings = FALSE, recursive = TRUE)
#  regex <- sprintf("^%s:", agency_id)
  regex <- sprintf("^(%s):", agency_id)
  agencies <<- str_split(agency_id, "\\|")[[1]]

  fic <- "agency"
  dsn <- sprintf("%s/%s.txt", gtfs_dir, fic)
  carp("dsn: %s", dsn)
  df <- rio::import(dsn, encoding = "UTF-8") %>%
    filter(agency_id %in% agencies) %>%
    glimpse()
  dsn <- sprintf("%s/%s.txt", reseau_dir, fic)
  rio::export(df, dsn, format = "csv")
  fic <- "routes"
  dsn <- sprintf("%s/%s.txt", gtfs_dir, fic)
#  carp("dsn: %s", dsn)
  df <- rio::import(dsn, encoding = "UTF-8") %>%
    filter(agency_id %in% agencies)
  dsn <- sprintf("%s/%s.txt", reseau_dir, fic)
  rio::export(df, dsn, format = "csv")
  fic <- "trips"
  dsn <- sprintf("%s/%s.txt", gtfs_dir, fic)
#  carp("dsn: %s", dsn)
  df <- rio::import(dsn, encoding = "UTF-8") %>%
    filter(grepl(regex, route_id))
  dsn <- sprintf("%s/%s.txt", reseau_dir, fic)
  rio::export(df, dsn, format = "csv")
  fic <- "shapes"
  dsn <- sprintf("%s/%s.txt", gtfs_dir, fic)
#  carp("dsn: %s", dsn)
  df <- rio::import(dsn, encoding = "UTF-8") %>%
    filter(grepl(regex, shape_id))
  dsn <- sprintf("%s/%s.txt", reseau_dir, fic)
  rio::export(df, dsn, format = "csv")
  fic <- "stops"
  dsn <- sprintf("%s/%s.txt", gtfs_dir, fic)
#  carp("dsn: %s", dsn)
  df <- rio::import(dsn, encoding = "UTF-8") %>%
    filter(grepl(regex, stop_id))
  df <- df %>%
    mutate(stop_id = gsub("^(BIBUS|TIBUS)\\:", "", stop_id))
  dsn <- sprintf("%s/%s.txt", reseau_dir, fic)
  rio::export(df, dsn, format = "csv")
  fic <- "stop_times"
  dsn <- sprintf("%s/%s.txt", gtfs_dir, fic)
#  carp("dsn: %s", dsn)
  df <- rio::import(dsn, encoding = "UTF-8") %>%
    filter(grepl(regex, trip_id))
  df <- df %>%
    mutate(stop_id = gsub("^(BIBUS|TIBUS)\\:", "", stop_id))
  dsn <- sprintf("%s/%s.txt", reseau_dir, fic)
  rio::export(df, dsn, format = "csv")
  fic <- "calendar_dates"
  dsn <- sprintf("%s/%s.txt", gtfs_dir, fic)
  carp("dsn: %s", dsn)
  df <- rio::import(dsn, encoding = "UTF-8")
  dsn <- sprintf("%s/%s.txt", reseau_dir, fic)
  rio::export(df, dsn, format = "csv")
#  carp("dsn: %s", dsn)
  setwd(reseau_dir)
  carp("reseau_dir: %s", reseau_dir)
  files <- list.files('.', pattern = ".txt$", full.names = FALSE)
#  utils::zip(zipfile = 'gtfs_toto.zip', files = files)
  archive::archive_write_files("gtfs.zip", files)
  setwd(baseDir)
  carp("reseau_dir: %s", reseau_dir)
}

