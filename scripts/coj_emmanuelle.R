# <!-- coding: utf-8 -->
#
# quelques fonctions pour la comptage oiseaux des jardins
# auteur: Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
# le traitement des fichiers excel fournis par Emmanuelle Pfaff
#
# l'ensemble des traitements
# source("geo/scripts/coj.R");emmanuelle_jour(TRUE)
emmanuelle_jour <- function(force = TRUE) {
  carp()
  emmanuelle_lire(force)
  emmanuelle_bilans_diff()
}
# source("geo/scripts/coj.R");emmanuelle_lire(TRUE)
emmanuelle_lire <- function(force = FALSE) {
  carp()
  library(rio)
  if (! exists("emmanuelle.df") | force == TRUE) {
    dsn <- sprintf("%s/EmmanuellePfaff/odj_2019.xlsx", varDir)
    carp("dsn: %s", dsn)
    emmanuelle.df <<- import(dsn) %>%
      glimpse()
  }
  if (! exists("emmanuelle_details.df") | force == TRUE) {
    dsn <- sprintf("%s/EmmanuellePfaff/BDD_ODJ_details_2009_2019.xlsx", varDir)
    carp("dsn: %s", dsn)
    emmanuelle_details.df <<- import(dsn) %>%
      glimpse()
  }
  return(invisible(emmanuelle.df))
}
#
# production du fichier des espèces
#
# source("geo/scripts/coj.R");emmanuelle_details_valid()
emmanuelle_details_valid <- function() {
  carp()
  library(tidyverse)
  library(rio)
  library(lubridate)
  df <- emmanuelle_lire() %>%
    full_join(emmanuelle_details.df, by = c("cojid" = "cojdetailnumidentete")) %>%
    glimpse()
  df1 <- df %>%
    filter(is.na(cojdetailespece))
  carp("jardin sans detail: %s", nrow(df1))
  df2 <- df %>%
    filter(is.na(cojid))
  carp("detail sans jardin: %s", nrow(df2))
  df3 <- df %>%
    filter(! is.na(cojdetailespece)) %>%
    group_by(espece = cojdetailespece) %>%
    summarize(nb = n()) %>%
    print(n = 300)
  carp("les espèces: %s", nrow(df3))
  export_df2xlsx(df3, suffixe = "especes")
}
#
# source("geo/scripts/coj.R");emmanuelle_stat()
emmanuelle_stat <- function() {
  carp()
  library(tidyverse)
  library(rio)
  library(lubridate)
  df <- emmanuelle_lire()
  df1 <- df %>%
    glimpse() %>%
    dplyr::select(cojsaisied, insee = insee_final) %>%
    mutate(Date = as.Date(as.numeric(cojsaisied), origin="1899-12-30")) %>%
    mutate(annee = year(Date)) %>%
    mutate(Departement = substr(insee, 1, 2)) %>%
    glimpse() %>%
    group_by(annee, Departement) %>%
    filter(! is.na(Departement)) %>%
    arrange(annee, Departement) %>%
    summarize(nb = n()) %>%
    pivot_wider(names_from = Departement, values_from = nb, values_fill = list(nb = 0)) %>%
    ungroup() %>%
    mutate("bzh" = dplyr::select(., matches("\\d")) %>% rowSums(na.rm = TRUE))
  return(invisible(df1))
}
#
## différence avec les bilans publiés --------------------------------------------
#
# source("geo/scripts/coj.R");emmanuelle_bilans_diff()
emmanuelle_bilans_diff <- function() {
  carp()
  library(rio)
  library(janitor)
  library(knitr)
  library(tidyselect)
  bilans.df <- bilans_get()
  carp("données emmanuelle")
  stat.df <- emmanuelle_stat() %>%
    glimpse()
  df <- bilans.df %>%
    left_join(stat.df, by = c("annee"), suffix = c(".bil", ".xls")) %>%
    dplyr::select(sort(peek_vars())) %>%
    print(n = 20)
  kable(df)
}
#
# source("geo/scripts/coj.R");emmanuelle_especes_modif()
emmanuelle_especes_modif <- function() {
  carp()
  library(tidyverse)
  library(rio)
  dsn <- sprintf("%s/emmanuelle/especes_mga.xlsx", cfgDir)
  carp("dsn: %s", dsn)
  df <- rio::import(dsn) %>%
    glimpse()
  biolo.df <- biolo_espece_lire() %>%
    glimpse()
  df1 <- df %>%
    mutate(ligne = 1:nrow(.)) %>%
    filter(! Espece %in% biolo.df$lib_species) %>%
    print(n = 20, na.print = "")
}
#
## différence avec la base sql --------------------------------------------
#
# source("geo/scripts/coj.R");emmanuelle_sqlv2_details_diff()
emmanuelle_sqlv2_details_diff <- function() {
  carp()
  library(rio)
  library(lubridate)
  library(janitor)
  library(knitr)
  library(tidyselect)
  library(compareDF)
  emmanuelle_lire(FALSE)
  xls.df <- emmanuelle_details.df %>%
    filter(cojdetailcontroledate > dmy("01-01_2019")) %>%
    mutate(cojdetailnombre = sprintf("%s", cojdetailnombre)) %>%
    glimpse()
  annee <- "2019"
  details.df <<- sqlv2_table_lire(sprintf("bvcoj_v2_details_%s", annee)) %>%
    mutate(cojdetid = sprintf("%s", cojdetid)) %>%
    mutate(cojdetailnumidentete = sprintf("%s", cojdetailnumidentete)) %>%
    filter(cojdetailnombre != "0") %>%
    glimpse()
# normalisation du champ espèce
  dsn <- sprintf("%s/emmanuelle/especes_mga.xlsx", cfgDir)
  carp("dsn: %s", dsn)
  df <- rio::import(dsn) %>%
    glimpse()
  xls.df <- xls.df %>%
    left_join(df, by = c("cojdetailespece" = "espece")) %>%
    mutate(cojdetailespece = EspeceCoj) %>%
    glimpse()
#  return()
  champs <- c("cojdetid", "cojdetailnumidentete", "cojdetailnumligne", "cojdetailespece", "cojdetailnombre")
  df3 <- compare_df(details.df[, champs], xls.df[, champs], champs)
  dsn <- sprintf("%s/%s.xlsx", varDir, carp_call)
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
# source("geo/scripts/coj.R");emmanuelle_sqlv2_entete_diff()
emmanuelle_sqlv2_entete_diff <- function(annee= "2019") {
  carp()
  library(rio)
  library(lubridate)
  library(janitor)
  library(knitr)
  library(tidyselect)
  library(compareDF)
  emmanuelle_lire(FALSE)
  carp("source xls")
  xls.df <- emmanuelle.df %>%
#    glimpse() %>%
    mutate(Date = as.Date(as.numeric(cojsaisied), origin="1899-12-30")) %>%
    mutate(Annee = sprintf("%s", year(Date))) %>%
    filter(Annee == annee) %>%
    glimpse()
  carp("source sql")
  details.df <<- sqlv2_table_lire(sprintf("bvcoj_v2_entete_%s", annee)) %>%
    rename(cojcreatio = cojcreationstatut) %>%
#    mutate(cojdetailnumidentete = sprintf("%s", cojdetailnumidentete)) %>%
#    filter(cojdetailnombre != "0") %>%
    glimpse()
  champs <- c("cojid", "cojcreatio")
  df3 <<- compare_df(details.df[, champs], xls.df[, champs], champs)
  carp("création fichier excel")
  dsn <- sprintf("%s/%s.xlsx", varDir, carp_call)
  create_output_table(df3,
    output_type = "xlsx",
    file_name = dsn,
    limit = 20000,
    color_scheme = c(addition = "#52854C", removal = "#FC4E07", unchanged_cell = "#999999", unchanged_row = "#293352"),
    headers = NULL,
    change_col_name = "chng_type",
    group_col_name = "grp"
  )
  glimpse(df3$comparison_df)
  df4 <- details.df %>%
    filter(cojid %in% df3$comparison_df$cojid) %>%
    glimpse()
  export_df2xlsx(df4, suffixe = "plus")
}
# source("geo/scripts/coj.R");emmanuelle_sql_entete_diff()
emmanuelle_sql_entete_diff <- function(annee= "2015") {
  carp()
  library(rio)
  library(lubridate)
  library(janitor)
  library(knitr)
  library(tidyselect)
  library(compareDF)
  emmanuelle_lire(FALSE)
  carp("source xls")
  xls.df <- emmanuelle.df %>%
#    glimpse() %>%
    mutate(Date = as.Date(as.numeric(cojsaisied), origin="1899-12-30")) %>%
    mutate(Annee = sprintf("%s", year(Date))) %>%
    filter(Annee == annee) %>%
    mutate(cojid = gsub("_.*$", "", cojid)) %>%
    mutate(cojcreatio = gsub("__", "_", cojcreatio)) %>%
    glimpse()
  carp("source sql")
  sql.df <<- sql_export_lire() %>%
    glimpse() %>%
    filter(a == annee) %>%
    dplyr::select(1:27) %>%
    glimpse()
  carp("sql les jardins")
  entete.df <- sql.df %>%
    distinct() %>%
    rename(cojid = admid, cojcreatio = cresta) %>%
    glimpse()
  champs <- c("cojid", "cojcreatio")
  df3 <<- compare_df(entete.df[, champs], xls.df[, champs], champs)
  carp("nb groupes: %s", nrow(df3$comparison_df))
  carp("création fichier excel des absents")
  dsn <- sprintf("%s/%s.xlsx", varDir, carp_call)
  create_output_table(df3,
    output_type = "xlsx",
    file_name = dsn,
    limit = 20000,
    color_scheme = c(addition = "#52854C", removal = "#FC4E07", unchanged_cell = "#999999", unchanged_row = "#293352"),
    headers = NULL,
    change_col_name = "chng_type",
    group_col_name = "grp"
  )
  glimpse(df3$comparison_df)
  df4 <- details.df %>%
    filter(cojid %in% df3$comparison_df$cojid) %>%
    glimpse()
  export_df2xlsx(df4, suffixe = "plus")
}
