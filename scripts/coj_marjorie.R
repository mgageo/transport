# <!-- coding: utf-8 -->
#
# quelques fonctions pour la comptage oiseaux des jardins
# auteur: Marc Gauthier
# licence: Creative Commons Paternité - Pas d"Utilisation Commerciale - Partage des Conditions Initiales à l"Identique 2.0 France
# ===============================================================
#
# les données du site oiseauxdesjardins non intégrés dans faune-bretagne
#
marjorieDir <- sprintf('%s/MarjoriePoitevin/', varDir)
#
## enchainement des traitements --------------------
#
# source("geo/scripts/coj.R");marjorie_jour()
marjorie_jour <- function(debug = TRUE) {
  carp()
  marjorie_lire(debug)
}
#
# récupération des fichiers et controle minimum
#
marjorie_lire <- function(debug = TRUE) {
  carp()
  library("readxl")
  if ( exists("marjorie.df") && force == FALSE) {
    return(invisible(marjorie.df))
  }
  dsn <- sprintf("%s/Données ODJ Bretagne 2013 2014 2015_comptage janvier.xlsx", marjorieDir)
  df <- readxl::read_excel(dsn, col_names = TRUE) %>%
    glimpse()
  carp("dsn: %s nrow : %s", dsn, nrow(df))
}
