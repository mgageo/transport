# <!-- coding: utf-8 -->
#
# quelques fonctions pour la comptage oiseaux des jardins
# auteur: Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
# le traitement des données des bilans
#
# l'ensemble des traitements
# source("geo/scripts/coj.R");bilans_jour(TRUE)
bilans_jour <- function(force = TRUE) {
  carp()
  if (force == TRUE) {
	drive_jour_get()
  }
  bilans_get()
}
bilans_get <- function(force = TRUE) {
  carp()
  library(tidyverse)
  library(knitr)  
  bilans.df <- drive_fichier_lire("Bilans") %>%
    filter(! grepl("^ko", ok)) %>%
    filter(! is.na(fiches)) %>%
    mutate(nb = as.numeric(fiches)) %>%
    mutate(Departement = gsub(".0$", "", niveau)) %>%
    group_by(annee, Departement) %>%
    summarize(nb = max(nb)) %>%
    arrange(annee, Departement) %>%
    pivot_wider(names_from = Departement, values_from = nb, values_fill = list(nb = 0)) %>%
    dplyr::select(annee, `22`, `29`, `35`, `44`, `56`, bzh) %>%
    ungroup()
  df <- bilans.df %>%
    mutate(depts = reduce(select(., matches("\\d")), `+`))
  print(kable(df))
  return(invisible(bilans.df))
}
