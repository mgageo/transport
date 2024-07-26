# <!-- coding: utf-8 -->
#
# quelques fonctions pour la comptage oiseaux des jardins
# auteur: Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
# les traitements pour les fichiers LPO Marjorie Poitevin
#
# les données du site oiseauxdesjardins non intégrés dans faune-bretagne
#
## enchainement des traitements --------------------
#
# source("geo/scripts/coj.R");odj_jour()
odj_jour <- function(debug = TRUE) {
  carp()
  odj_lire(debug)
}
#
# récupération du fichier et controle minimum
#
odj_lire <- function(debug = FALSE, force = TRUE) {
  carp("début")
  library(janitor)
  library(rio)
  library(tidyverse)
  carp()
  if (exists("odj.df") && force == FALSE) {
    return(invisible(odj.df))
  }
  dsn <- sprintf("%s/ODJ/Données ODJ Bretagne 2013 2014 2015_comptage janvier.xlsx", varDir)
  carp("dsn: %s", dsn)
  df1 <- import(dsn, sheet = "2013")
  df1 <- df1[-1, ]
  df2 <- import(dsn, sheet = "2014")
  df2 <- df2[-1, ]
  df3 <- import(dsn, sheet = "2015")
  df3 <- df3[-1, ]
  odj.df <<- rbind(df1, df2, df3) %>%
    clean_names() %>%
    glimpse()
  return(invisible(odj.df))
}

