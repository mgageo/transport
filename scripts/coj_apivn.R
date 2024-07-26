# <!-- coding: utf-8 -->
#
# la partie apivn
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
#
# la totale
# source("geo/scripts/coj.R");apivn_jour();
apivn_jour <- function() {
  carp()
  library(tidyverse)
  library(janitor)
  apivn_donnees_get();
}
#
# les données
# source("geo/scripts/coj.R");apivn_donnees_get();
apivn_donnees_get <- function(force = TRUE) {
  carp()
  library(tidyverse)
  library(janitor)
  library(rio)
  df <- apivn_extract_coj(user = "apivn133", force = force) %>%
    glimpse()
  df1 <- df %>%
    group_by(date_year, id_form_universal) %>%
    summarize(nb = n()) %>%
    group_by(date_year) %>%
    summarize(nbf = n(), nbo = sum(nb)) %>%
    glimpse()
  misc_print(df1)
#  return()
#
# filtrage sur la période du comptage
  for (annee in anneesFaune) {
    apivn_donnees_annee_get(df, annee, force = TRUE)
  }
}
#
apivn_donnees_annee_get <- function(df, annee, force) {
  library(tidyverse)
  library(janitor)
  library(rio)
  carp("annee: %s", annee)
  df1 <- annee_debut_fin(annee)
  Debut <- df1[1, "Debut"]
  Fin <- df1[1, "Fin"]
  df2 <- df %>%
    filter(date_year == !!annee) %>%
    filter(date >= !!Debut) %>%
    filter(date <= !! Fin)
  dsn <- sprintf("%s/apivn_%s.xlsx", varDir, annee)
  writexl::write_xlsx(df2, path = dsn)
  carp("dsn: %s nrow: %s", dsn, nrow(df2))
}
#
# comparaison avec l'export faune-bretagne
# source("geo/scripts/coj.R");apivn_faune_annee_diff();
apivn_faune_annee_diff <- function(annee = "2023", force = TRUE) {
  faune.df <- faune_donnees_jardin_annee_lire(annee = annee, force = TRUE) %>%
    glimpse()
  dsn <- sprintf("%s/apivn_%s.xlsx", varDir, annee)
  apivn.df <- readxl::read_excel(dsn, col_names = TRUE) %>%
    glimpse()
# comparaison sur les formulaires
  df1 <- faune.df %>%
    group_by(id_form) %>%
    summarize(nbf = n()) %>%
    mutate(id_form = as.numeric(id_form))
  df2 <- apivn.df %>%
    group_by(id_form) %>%
    summarize(nba = n())
  df3 <- df1 %>%
    full_join(df2, by = c("id_form"))
  carp("les formulaires absents d'aipvn")
  df4 <- df3 %>%
    filter(is.na(nba)) %>%
    glimpse()
  carp("les formulaires absents de l'export")
  df5 <- df3 %>%
    filter(is.na(nbf)) %>%
    glimpse()
}