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
  Config <<- import(dsn) %>%
    filter(reseau == res)
  if (nrow(Config) == 0 ) {
    confess("****** %s", res)
  }
  transportDir <<- sprintf("%s/%s", varDir, toupper(Config[1, "reseau"]))
  reseau_dir <<- sprintf("%s/%s", varDir, toupper(Config[1, "reseau"]))
  gtfsDir <<- sprintf("%s/%s", varDir, Config[1, "gtfs_dir"])
  josmDir <<- sprintf("%s/%s", transportDir, "JOSM")
  osmDir <<- sprintf("%s/%s", transportDir, "OSM")
  osrmDir <<- sprintf("%s/%s", transportDir, "OSRM")
  level0Dir <<- sprintf("%s/%s", transportDir, "LEVEL0")
#  carp("gtfsDir: %s", gtfsDir);stop("****")
  dir.create(gtfsDir, showWarnings = FALSE, recursive = TRUE)
  dir.create(osmDir, showWarnings = FALSE, recursive = TRUE)
  dir.create(osrmDir, showWarnings = FALSE, recursive = TRUE)
  dir.create(josmDir, showWarnings = FALSE, recursive = TRUE)
  dir.create(level0Dir, showWarnings = FALSE, recursive = TRUE)
  return(invisible(Config))
}
