# <!-- coding: utf-8 -->
#
# quelques fonctions pour la comptage oiseaux des jardins
# auteur: Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
# le fichier des actions sur les espèces xlsx
#
#
# source("geo/scripts/coj.R"); especes_lire()
especes_lire <- function(onglet='espece_clean') {
  carp()
  library(tidyverse)
  library(rio)
  dsn <- sprintf("%s/especes_mga.xlsx", cfgDir)
  df <- import(dsn, which=onglet) %>%
    glimpse()
  return(invisible(df))
}