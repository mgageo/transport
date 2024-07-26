# <!-- coding: utf-8 -->
#
# quelques fonctions pour la comptage oiseaux des jardins
# auteur: Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
# le traitement du fichier excel GEOCA
#
#
# source("geo/scripts/coj.R");geoca_jour()
geoca_jour <- function(force = TRUE) {
  carp()
  library(rio)
  geoca_lire(force = force)
  geoca_jardins(force = FALSE)
}
# source("geo/scripts/coj.R");geoca_lire()
geoca_lire <- function(force = FALSE) {
  carp()
  library(rio)
  if (! exists("geoca.df")) {
    dsn <- sprintf("%s/GEOCA/ODJ_GEOCA_2009-2017_in_ok.xlsx", varDir)
    carp("dsn: %s", dsn)
    geoca.df <<- import(dsn) %>%
      glimpse()
  }
  return(invisible(geoca.df))
}
# source("geo/scripts/coj.R");geoca_stat()
geoca_stat <- function() {
  carp()
  library(tidyverse)
  library(rio)
  df <- geoca_lire()
  df %>%
    group_by(ANNEE, DEPARTEMENT) %>%
    summarize(nb = n()) %>%
    print(n = 20)
  carp("sans lieu d'obs")
  df %>%
    filter(is.na(CODE_INSEE_LIEU_OBS)) %>%
    group_by(ANNEE, DEPARTEMENT) %>%
    summarize(nb = n()) %>%
    print(n = 20)
    glimpse()
  df1 <- df %>%
    group_by(ESPECE) %>%
    summarize(nb = n()) %>%
    print(n = 20)
  dsn <- sprintf("%s/GEOCA/especes_odj.xlsx", varDir)
  carp("dsn: %s", dsn)
  rio::export(df1, dsn)
  especes.df <- geoca_especes_lire()
  df2 <- df1 %>%
    left_join(especes.df, by = c("ESPECE" = "code")) %>%
    filter(is.na(nom)) %>%
    print(n = 20)

}
#
# détermination des jardins, oiseaux, espèces
# source("geo/scripts/coj.R");geoca_jardins()
geoca_jardins <- function(force = FALSE) {
  carp()
  library(tidyverse)
  library(rio)
  df <- geoca_lire(force = force)
# validité du champ effectif
  df1 <- df %>%
    filter(is.na(EFFECTIF)) %>%
    filter(! is.integer(EFFECTIF))
  if (nrow(df1) > 0) {
    carp("EFFECTIF nrow: %s", nrow(df1))
  }
  df <- df %>%
#    filter(! is.na(EFFECTIF)) %>%
#    filter(is.integer(EFFECTIF)) %>%
    mutate(EFFECTIF = as.integer(EFFECTIF)) %>%
    filter(! is.na(EFFECTIF))
# validité du champ espece
  df1 <- df %>%
    filter(is.na(ESPECE))
  if (nrow(df1) > 0) {
    carp("ESPECE nrow: %s", nrow(df1))
  }
  df <- df %>%
    filter(! is.na(ESPECE))

  df1 <- df %>%
    dplyr::select(-ESPECE, -EFFECTIF, -COMMENTAIRE) %>%
    distinct()
  df2 <- df1 %>%
    group_by(ANNEE, DEPARTEMENT) %>%
    summarize(nb_jardins = n())
  df3 <- df %>%
    group_by(ANNEE, ESPECE) %>%
    summarize(nb = n(), nb_oiseaux = sum(EFFECTIF)) %>%
    group_by(ANNEE) %>%
    summarize(nb_especes = n(), nb_oiseaux = sum(nb_oiseaux))
  df4 <- df2 %>%
    left_join(df3, by = c("ANNEE" = "ANNEE")) %>%
    mutate(DEPARTEMENT = sprintf("%s", DEPARTEMENT)) %>%
    print(n = 20)
  return(invisible(df4))
}
#
# détermination des codes insee utilisés
# source("geo/scripts/coj.R");geoca_insee()
geoca_insee <- function() {
  carp()
  library(tidyverse)
  library(janitor)
  df <- geoca_lire()
  df1 <- df %>%
    clean_names() %>%
    group_by(commune = commune_lieu_obs, insee= code_insee_lieu_obs, annee) %>%
    summarize(nb = n()) %>%
    pivot_wider(names_from = annee, values_from = nb, values_fill = list(nb = 0)) %>%
    glimpse()
  df2 <- df1[, order(colnames(df1), decreasing=TRUE)] %>%
    glimpse()
  export_df2xlsx(df2)
}
#
# différence sur les jardins, oiseaux, espèce
# source("geo/scripts/coj.R");geoca_jardins_diff()
geoca_jardins_diff <- function() {
  carp()
  library(tidyverse)
  library(janitor)
  df1 <- drive_fichier_lire("Bilans") %>%
    mutate(departement = gsub(".0$", "", niveau)) %>%
    mutate(jardins = gsub(".0$", "", fiches)) %>%
    filter(departement == "22") %>%
    filter(grepl("GEOCA", source)) %>%
    dplyr::select(annee, departement, jardins, especes, oiseaux) %>%
    glimpse()
  df2 <- geoca_jardins()
  df3 <- df1 %>%
    left_join(df2, by = c("annee" = "ANNEE", "departement" = "DEPARTEMENT")) %>%
    print(n = 20)
}
# source("geo/scripts/coj.R");geoca_especes_ecrire()
# https://cran.r-project.org/web/packages/htmltab/vignettes/htmltab.html
geoca_especes_ecrire <- function() {
  carp()
  library(tidyverse)
  library(rvest)
  library(rio)
  url <- "http://mousquey.piel.pagesperso-orange.fr/ornitho/liste.html"
  webpage <- read_html(url)
  tables <- html_nodes(webpage, "table")
  head(tables)
  df <- webpage %>%
    html_nodes("table") %>%
    html_table(fill = TRUE) %>%
    .[[1]]
  dsn <- sprintf("%s/GEOCA/especes.csv", varDir)
  export(df, dsn)
}
geoca_especes_lire <- function() {
  carp()
  library(rio)
  library(janitor)
  dsn <- sprintf("%s/GEOCA/especes_geoca.csv", varDir)
  carp("dsn: %s", dsn)
  df <- import(dsn, encoding = "UTF-8") %>%
    clean_names() %>%
    glimpse()
  return(invisible(df))
}
#
# différence geoca avec base sql
# source("geo/scripts/coj.R");geoca_diff()
geoca_diff <- function() {
  carp()
  library(rio)
  library(janitor)
  annees <- c("2013", "2014", "2015", "2016", "2017")
  geoca.df <- geoca_lire() %>%
    filter(ANNEE %in% annees) %>%
    glimpse()
  geoca_formulaire.df <- geoca_formulaire_geoca(geoca.df)
  sql.df <- sql_donnees_lire() %>%
    filter(saidep == "22") %>%
    filter(annee %in% annees) %>%
    glimpse()
  sql_formulaire.df <- geoca_formulaire_sql(sql.df)
  communes <- c("22278", "22113", "22168", "22187", "Total")
  df <- full_join(geoca_formulaire.df, sql_formulaire.df, by = c("insee"), suffix = c(".g", ".b")) %>%
    arrange(insee) %>%
    filter(insee %in% communes) %>%
    glimpse() %>%
    print(is.na(""))
}
geoca_formulaire_geoca <- function(df) {
  carp()
  df %>%
    group_by(ANNEE, DEPARTEMENT) %>%
    summarize(nb = n()) %>%
    spread(key = ANNEE, value = nb, fill = 0) %>%
    adorn_totals(where = c("row", "col"), name = "Total") %>%
    print()
  df1 <- df %>%
#    glimpse() %>%
    group_by(DATE, ANNEE, HEURE_DEBUT, TYPE_LIEU_OBS, DEPARTEMENT, LIEU_OBS, insee = CODE_INSEE_LIEU_OBS, COORDONNEES_OBSERVATEUR, MAIL_OBSERVATEUR) %>%
    summarize(nb = n())
  df1 <- df1 %>%
    group_by(ANNEE, insee) %>%
    summarize(nb = n()) %>%
    spread(key = ANNEE, value = nb, fill = 0) %>%
    adorn_totals(where = c("row", "col"), name = "Total") %>%
    glimpse()
  return(invisible(df1))
}
geoca_formulaire_sql <- function(df) {
  carp()
  library(janitor)
  df %>%
    group_by(annee, saidep) %>%
    summarize(nb = n()) %>%
    spread(key = annee, value = nb, fill = 0) %>%
    adorn_totals(where = c("row", "col"), name = "Total") %>%
    print()
  df1 <- df %>%
    group_by(annee, admid, saidep, credat, creheu, saityp, saicom) %>%
    summarize(nb = n()) %>%
    mutate(insee = gsub(" ", "", saicom))
  df1 <- df1 %>%
    group_by(annee, insee) %>%
    summarize(nb = n()) %>%
    spread(key = annee, value = nb, fill = 0) %>%
    adorn_totals(where = c("row", "col"), name = "Total") %>%
    glimpse()
  return(invisible(df1))
}
#
## différence avec les bilans publiés
#
#
# différence geoca avec base sql
# source("geo/scripts/coj.R");geoca_bilans_diff()
geoca_bilans_diff <- function() {
  carp()
  library(rio)
  library(janitor)
  bilans.df <- drive_fichier_lire("Bilans_22") %>%
    pivot_longer(c(-insee, -commune), names_to = "annee", values_to = "jardins") %>%
    filter(! is.na(jardins)) %>%
    filter(! is.na(commune)) %>%
    mutate(annee = gsub(".0$", "", annee)) %>%
    mutate(insee = sprintf("%s", insee)) %>%
    glimpse()
  carp("données geoca")
  geoca.df <- geoca_lire()
  geoca_formulaire.df <- geoca_formulaire_geoca(geoca.df) %>%
    pivot_longer(c(-insee), names_to = "annee", values_to = "jardins") %>%
    glimpse()
  carp("jointure")
  df1 <- bilans.df %>%
    left_join(geoca_formulaire.df, by = c("insee", "annee"), suffix = c(".bilan", ".excel")) %>%
    glimpse() %>%
    print(n = 50, is.na = "")
}
