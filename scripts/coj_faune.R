# <!-- coding: utf-8 -->
#
# quelques fonctions pour le comptage oiseaux des jardins
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d"Utilisation Commerciale - Partage des Conditions Initiales à l"Identique 2.0 France
#
#
# source("geo/scripts/coj.R");faune_jour()
faune_jour <- function() {
  carp("début")
  library(janitor)
  df <- faune_donnees_jardin_annee_lire("2020")
  coj_faune.df <<- df
  dsn <- sprintf("%s/faune_actions.xlsx", cfgDir)
  actions_init(dsn, prefixe = "faune_action_")
#  tex_pdflatex("faune_coj.tex")
#  return()
  df <- faune_donnees_jardin_validation(df)
  if (nrow(df) == 0) {
    stop("***")
  }
  faune_donnees_jardin_stat_participation(df)
  faune_donnees_jardin_stat_espece(df)
  tex_pdflatex("faune_coj.tex")
}
#
# interrogation de faune-bretagne, aucune donnée en 2013, 2014
#
# source("geo/scripts/coj.R");faune_region_annees_get()
faune_region_annees_get <- function() {
  carp("début")
  faune_region_get("2020"); return()
  for (annee in anneesFaune) {
    faune_region_get(annee, force = TRUE)
  }
}
# pour une année
# source("geo/scripts/coj.R");faune_coj_get("2024", TRUE)
faune_coj_get <- function(annee = "2020", force = TRUE) {
  carp("début")
  library(tidyverse)
  library(lubridate)
  df <- annee_debut_fin(annee)
  dsn <- sprintf("%s/coj_%s.xlsx", varDir, annee)
  if (file.exists(dsn) & force == FALSE) {
    return()
  }
  d <- df[1, "debut"]
  d <- dmy(d) - days(5)
  debut <- format(d, format = "%d.%m.%Y")
  d <- df[1, "fin"]
  d <- dmy(d) + days(15)
  fin <- format(d, format = "%d.%m.%Y")
#  carp("debut: %s fin: %s", debut, fin); return("***")
  biolo_export_coj(debut = debut, fin = fin, dsn = dsn)
  carp("fin dsn: %s", dsn)
}
# pour une année
# source("geo/scripts/coj.R");faune_region_get("2023", force = FALSE)
faune_region_get <- function(annee = "2019", force = TRUE) {
  carp("début")
  library(lubridate)
  df <- annee_debut_fin(annee)
  dsn <- sprintf("%s/region_%s.xlsx", varDir, annee)
  d <- df[1, "debut"]
  d <- dmy(d) - days(5)
  debut <- format(d, format = "%d.%m.%Y")
  d <- df[1, "fin"]
  d <- dmy(d) + days(15)
  fin <- format(d, format = "%d.%m.%Y")
#  stop("*****")
  carp("debut: %s fin: %s", debut, fin)
  biolo_export_region(debut = debut, fin = fin, dsn = dsn, force = force)
  carp("fin dsn: %s", dsn)
}
#
# source("geo/scripts/coj.R");faune_donnees_jardin_annees_lire()
faune_donnees_jardin_annees_lire <- function() {
  carp("début")
  library(janitor)
  if (exists("donnees_jardin.df")) {
    donnees_jardin.df <<- donnees_jardin.df %>%
      filter(total_count > 0)
    return(invisible(donnees_jardin.df))
  }
  if (exists("df2")) {
    rm("df2")
  }
  for (annee in c("2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021")) {
    carp("annee: %s", annee)
    df1 <- faune_donnees_jardin_annee_lire(annee)
    if (! exists("df2")) {
      df2 <- df1
    } else {
      df2 <- rbind(df2, df1)
    }
  }
  donnees_jardin.df <<- df2
  return(invisible(df2))
}
#
# les jardins saisis
# source("geo/scripts/coj.R");faune_donnees_jardin_annee_lire()
# https://cdnfiles1.biolovision.net/www.oiseauxdesjardins.fr/userfiles/Bilancomptagejanvier2019.pdf
faune_donnees_jardin_annee_lire <- function(annee = "2018", force = TRUE) {
  carp("début")
  library(janitor)
  library(rio)
  library(tidyverse)
  carp()
  if (exists("faune_jardin.df") & force == FALSE) {
    return(invisible(faune_jardin.df))
  }
  dsn <- sprintf("%s/coj_%s.xlsx", varDir, annee)
#  dsn <- sprintf("%s/region_%s.xlsx", varDir, annee)
  carp("dsn: %s", dsn)
  df <- import(dsn)
  df <- df[-1, ]
  df1 <- df %>%
    clean_names()
  df2 <- df1 %>%
    group_by(precision) %>%
    summarize(nb = n())
  misc_print(df2)
  faune_jardin.df <<- df1 %>%
#    remove_empty(c("rows")) %>%
    filter(precision == "Jardin")
  return(invisible(faune_jardin.df))
}
#
# la validation
faune_donnees_jardin_validation <- function(df) {
  carp("début")
  library(janitor)
  library(rio)
  library(tidyverse)
# total_count à 0
  df <- df %>%
    dplyr::select(date_year, county, id_sighting, id_form, id_place, name_species, total_count, insert_date, update_date) %>%
    mutate(total_count = as.numeric(total_count))
  df1 <- df %>%
    filter(total_count == 0) %>%
    glimpse()
  txt <- faune_formulaires_liste(df1)
  tex_texte(txt, dossier = "faune", suffixe = "total_count")
  df <- df %>%
    filter(total_count != 0)
# que les formulaires
  df <- df %>%
    mutate(id = as.numeric(id_form))
  df1 <- df %>%
    filter(is.na(id)) %>%
    glimpse()
  df1 <- df %>%
    filter(!is.na(id))
#  stop("***")
  return(invisible(df))
}
#
# la liste des formulaires
faune_formulaires_liste <- function(df1) {
  carp("début")
  library(janitor)
  library(rio)
  library(tidyverse)
  df <- df1 %>%
    group_by(id_form) %>%
    summarize(nb = n()) %>%
    mutate(id_form = as.numeric(id_form)) %>%
    arrange(id_form)
  tex <- ""
  for (i in 1:nrow(df)) {
    t <- faune_formulaire_liste(df[i, "id_form"])
    tex <- append(tex, t)
    tex <- append(tex, "")
  }
  return(invisible(tex))
}
#
# pour liste un formulaire
faune_formulaire_liste <- function(id) {
#  carp("début")
  library(tidyverse)
  library(lubridate)
  id <- as.character(id)
  df <- coj_faune.df %>%
    filter(id_form == get("id"))
  if (nrow(df) == 0) {
    carp("id: %s", id)
    stop("***")
  }
  df <- df %>%
    mutate(insert = excel_numeric_to_date(as.numeric(insert_date), include_time = TRUE)) %>%
    mutate(update = excel_numeric_to_date(as.numeric(update_date), include_time = TRUE))
  tpl <- "{{surname}} {{name}}, form: {{id_form}}, {{insert}}, {{update}}"
  txt <- tex_df2tpl(df, 1, tpl)
  txt <- append(txt, "")
  for (i in 1:nrow(df)) {
    tpl <- "{{name_species}}({{total_count}}),"
    t <- tex_df2tpl(df, i, tpl)
    txt <- append(txt, t)
  }
  return(invisible(txt))
}
#
# la stat participation
faune_donnees_jardin_stat_participation <- function(df) {
  carp("début")
  library(janitor)
  library(rio)
  library(tidyverse)
  df2 <- df %>%
    dplyr::select(date_year, county, id_form, id_place) %>%
    unique() %>%
    arrange(date_year, county) %>%
    group_by(date_year, county) %>%
    summarize(nb = n()) %>%
    spread(county, nb) %>%
    adorn_totals(where = c("row", "col"), name = "Total") %>%
    glimpse()
  return(invisible(df2))
}
#
# la stat espece
faune_donnees_jardin_stat_espece <- function(df) {
  carp("début")
  library(janitor)
  library(rio)
  library(tidyverse)
  df2 <- df %>%
    dplyr::select(espece = name_species, total_count) %>%
    mutate(nombre = as.numeric(total_count)) %>%
    group_by(espece) %>%
    summarize(
      nb = n(),
      min = min(nombre),
      moy = round(mean(nombre), 1),
      med = round(median(nombre), 1),
      max = max(nombre),
      var = round(var(nombre), 1),
      sd = round(sd(nombre), 1)
    ) %>%
    glimpse()
  tex_df2table(df2, dossier = "faune")
  export_df2xlsx(df2, onglet = "espece")
}
#
# =============================================================================================================
#
# les traitements du fichier xlsx actions
faune_action_renommer <- function(onglet) {
  carp()
  df4 <- faune_donnees_jardin_stat_participation(coj_faune.df)
  tex_df2table(df4, dossier = "faune", suffixe = "avant")
  df <- actions_onglet_lire(onglet)
  tex_df2table(df, dossier = "faune", suffixe = "espece")
  df1 <- coj_faune.df %>%
    left_join(df, by = c("name_species" = "ancien")) %>%
    mutate(name_species = ifelse(is.na(nouveau), name_species, nouveau))
  coj_faune.df <<- df1
}
faune_action_seuil <- function(onglet) {
  carp()
  df4 <- faune_donnees_jardin_stat_participation(coj_faune.df)
  tex_df2table(df4, dossier = "faune", suffixe = "avant")
  df <- actions_onglet_lire(onglet)
  tex_df2table(df, dossier = "faune", suffixe = "seuil")
  df1 <- coj_faune.df %>%
    group_by(id_form) %>%
    summarize(jardin_especes = n(), jardin_oiseaux = sum(as.integer(total_count))) %>%
    glimpse()
  for (i in 1:nrow(df)) {
    df2 <- faune_action_seuil_test(df1, df[i, "attribut"], df[i, "valeur"], df[i, "test"])
    if (exists("df3")) {
      df3 <- rbind(df3, df2)
    } else {
      df3 <- df2
    }
  }
  glimpse(df3)
  txt <- faune_formulaires_liste(df3)
  tex_texte(txt, dossier = "faune")
  ids <- unique(df3$id_form)
  df3 <- coj_faune.df %>%
    filter(!id_form %in% ids)
  df4 <- faune_donnees_jardin_stat_participation(df3)
  tex_df2table(df4, dossier = "faune", suffixe = "apres")
}
faune_action_seuil_test <- function(df1, attribut, valeur, test) {
  cause <- sprintf("%s %s %s", attribut, valeur, test)
  attribut <- "jardin_especes"
  if (test == "min") {
    df2 <- df1 %>%
      filter(!!rlang::sym(attribut) < get("valeur"))
  }
  if (test == "max") {
    df2 <- df1 %>%
      filter(!!rlang::sym(attribut) > get("valeur"))
  }
  if (nrow(df2) > 0) {
    df2$cause <- cause
  }
  return(invisible(df2))
}
#
# les espèces d'une liste
faune_action_espece <- function(onglet) {
  carp()
#  return()
  df4 <- faune_donnees_jardin_stat_participation(coj_faune.df)
  tex_df2table(df4, dossier = "faune", suffixe = "avant")
  df <- actions_onglet_lire(onglet)
  tex_df2table(df, dossier = "faune", suffixe = "espece")
  df1 <- coj_faune.df %>%
    left_join(df, by = c("name_species" = "espece")) %>%
    mutate(poids = replace_na(poids, 0))
  df3 <- df1 %>%
    group_by(id_form) %>%
    summarize(nb = n(), poids = sum(poids)) %>%
    filter(poids > 0) %>%
    glimpse()
  txt <- faune_formulaires_liste(df3)
  tex_texte(txt, dossier = "faune")
  ids <- unique(df3$id_form)
  df3 <- coj_faune.df %>%
    filter(!id_form %in% ids)
  df4 <- faune_donnees_jardin_stat_participation(df3)
  tex_df2table(df4, dossier = "faune", suffixe = "apres")
  df2 <- df3 %>%
    dplyr::select(espece = name_species, total_count) %>%
    mutate(nombre = as.numeric(total_count)) %>%
    group_by(espece) %>%
    summarize(nb = n()) %>%
    glimpse()
  tex_df2table(df2, dossier = "faune")
  df3 <- coj_faune.df %>%
    filter(id_form %in% ids)
  df2 <- df1 %>%
    filter(poids > 0) %>%
    dplyr::select(espece = name_species, total_count) %>%
    mutate(nombre = as.numeric(total_count)) %>%
    group_by(espece) %>%
    summarize(nb = n()) %>%
    glimpse()
  tex_df2table(df2, dossier = "faune", suffixe = "poids")
  df2 <- df1 %>%
    filter(poids == 0) %>%
    dplyr::select(espece = name_species, total_count) %>%
    mutate(nombre = as.numeric(total_count)) %>%
    group_by(espece) %>%
    summarize(nb = n()) %>%
    glimpse()
  tex_df2table(df2, dossier = "faune", suffixe = "reste")
}
#
## les données de la LPO fournies par Marjorie ---------------------------------------------
#
# source("geo/scripts/coj.R");faune_odj_lire()
faune_odj_lire <- function(annee = "2020", force = FALSE) {
  library(janitor)
  library(rio)
  library(tidyverse)
  carp()
  if (exists("faune_odj.df")) {
    return(invisible(faune_odj.df))
  }
  dsn <- sprintf("%s/LPO/Données ODJ janvier %s_bretagne.xlsx", varDir, annee)
  df <- import(dsn)
  faune_odj.df <<- df %>%
    clean_names() %>%
    glimpse()
  return(invisible(faune_odj.df))
}
#
## comparaison de deux versions du fichier ---------------------------------------------
#
# différence entre export faune et fichier Marjorie
# source("geo/scripts/coj.R");faune_donnees_diff()
faune_donnees_diff <- function() {
  library(tidyverse)
  library(compareDF)
  debut <- "25/01/2020"
  fin <- "26/01/2020"
  df1 <- faune_donnees_jardin_annee_lire(annee = "2020", force = FALSE) %>%
    filter(date_year == "2020") %>%
    filter(date_month == "1") %>%
    filter(date_day %in% c("25", "26")) %>%
    mutate(Ligne = 1 : nrow(.)) %>%
    mutate(date = excel_numeric_to_date(as.numeric(date))) %>%
    mutate(date = strftime(date, format="%d.%m.%Y")) %>%
    mutate(id_species = as.numeric(id_species)) %>%
    mutate(insee = as.numeric(insee)) %>%
    dplyr::select(email, name_species,  date, insee, municipality) %>%
    arrange(email, name_species,  date) %>%
    glimpse()
  df2 <- faune_odj_lire(force = FALSE) %>%
    mutate(Ligne = 1 : nrow(.)) %>%
    dplyr::select(email, name_species,  date, insee, municipality) %>%
    arrange(email, name_species,  date) %>%
    glimpse()
#  stop('***')
  champs <- c("email", "name_species",  "date")
  df3 <- compare_df(df1, df2, champs)
  print(summary(df3))
  dsn <- sprintf("%s/faunes_donnees_diff.xlsx", varDir)
  create_output_table(df3,
    output_type = "xlsx",
    file_name = dsn,
    limit = 20000,
    color_scheme = c(addition = "#52854C", removal = "#FC4E07", unchanged_cell = "#999999", unchanged_row = "#293352"),
    headers = NULL,
    change_col_name = "chng_type",
    group_col_name = "grp"
  )
}