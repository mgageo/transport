# <!-- coding: utf-8 -->
#
# quelques fonctions pour la comptage oiseaux des jardins
# auteur: Marc Gauthier
# licence: Creative Commons Paternité - Pas d"Utilisation Commerciale - Partage des Conditions Initiales à l"Identique 2.0 France
# ===============================================================
#
# les traitements pour la LPO 44
#
## enchainement des traitements --------------------
#
# source("geo/scripts/coj.R");lpo44_jour()
lpo44_jour <- function(debug = TRUE) {
  carp()
  lpo44_lire(debug)
}
#
# récupération du fichier et controle minimum
#
lpo44_lire <- function(debug = FALSE, force = TRUE) {
  carp("début")
  library(janitor)
  library(rio)
  library(tidyverse)
  carp()
  if (exists("lpo44.df") & force == FALSE) {
    return(invisible(lpo44.df))
  }
  dsn <- sprintf("%s/LPO44/oiseaux_jardins_export_2013_2020.xlsx", varDir)
  carp("dsn: %s", dsn)
  df <- import(dsn)
  colnames(df) <- lpo44_correspondance()
  lpo44.df <<- df %>%
    clean_names() %>%
    glimpse()
  return(invisible(lpo44.df))
}
#
# la correspondance avec les noms anglais
#
# source("geo/scripts/coj.R");lpo44_correspondance()
lpo44_correspondance <- function() {
  carp("début")
  library(janitor)
  library(rio)
  library(tidyverse)
  carp()
  dsn <- sprintf("%s/LPO44/correspondance_noms.xlsx", varDir)
  carp("dsn: %s", dsn)
  df <- import(dsn) %>%
    glimpse()
  df1 <- df %>%
    dplyr::select(lpo44) %>%
    remove_empty(c("rows"))
  df2 <- df %>%
    dplyr::select(anglais, francais)
  df3 <- df1 %>%
    left_join(df2, by = c("lpo44" = "francais")) %>%
    mutate(anglais = ifelse(is.na(anglais), lpo44, anglais))
  return(invisible(df3$anglais))
}
