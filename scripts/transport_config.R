# <!-- coding: utf-8 -->
# le réseau de bus
# utilisation des données d'OpenStreetMap
# auteur : Marc Gauthier
#
#
# lecture du fichier excel et filtrage
# source("geo/scripts/transport.R");config_xls('star')

config_xls <- function(res) {
  carp()
  library(rio)
  library(tidyverse)
  dsn <- sprintf('%s/agency.xls', cfgDir)
  config <<- import(dsn) %>%
    filter(reseau == res)
  transportDir <<- sprintf("%s/%s", cfgDir, toupper(config[1, "reseau"]))
  reseau_dir <<- sprintf("%s/%s", cfgDir, toupper(config[1, "reseau"]))
  gtfsDir <<- sprintf("%s/%s", transportDir, config[1, "gtfs_dir"])
  josmDir <<- sprintf("%s/%s", transportDir, "JOSM")
  osmDir <<- sprintf("%s/%s", transportDir, "OSM")
  dir.create(gtfsDir, showWarnings = FALSE, recursive = TRUE)
  dir.create(osmDir, showWarnings = FALSE, recursive = TRUE)
  dir.create(josmDir, showWarnings = FALSE, recursive = TRUE)
  return(invisible(config))
}
