# <!-- coding: utf-8 -->
#
# quelques fonctions pour la comptage oiseaux des jardins
# auteur: Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
# le traitement des fichiers de Guillaume Gélinaud
#
# source("geo/scripts/coj.R");guillaume_lire()
guillaumeDir <- sprintf('%s/GuillaumeGelinaud/', varDir)
guillaume_lire <- function(force = FALSE) {
  library(rio)
  library(tidyverse)
  library(stringr)
  carp()
  guillaume_2011_lire()
#  guillaume_2012_lire()
#  guillaume_20xx_lire()
}
#
# pour les fichiers départementaux 2011
guillaume_2011_lire <- function(force = FALSE) {
  library(rio)
  library(tidyverse)
  library(stringr)
  carp()
  files <- list.files(guillaumeDir, pattern = "2011", full.names = TRUE, ignore.case = TRUE) %>%
    glimpse()
  for (i in 1:length(files)) {
    file <- files[i]
    carp("file: %s", file)
    df <- rio::import(file) %>%
      glimpse()
    guillaume_2011_stat(df)
  }
}
#
# pour les fichiers départementaux 2012
guillaume_2012_lire <- function(force = FALSE) {
  library(rio)
  library(tidyverse)
  library(stringr)
  carp()
  files <- list.files(guillaumeDir, pattern = "ODJ .._2012", full.names = TRUE, ignore.case = TRUE) %>%
    glimpse()
  stats.df <- data.frame()
  for (i in 1:length(files)) {
    file <- files[i]
    carp("file: %s", file)
    stat.list <- guillaume_2012_stat(file) %>%
      glimpse()
    stat.df <- data.frame(as.list(stat.list)) %>%
      glimpse()
    stats.df <- rbind(stats.df, stat.df)
#    stop("****")
  }
  stats.df %>%
    glimpse() %>%
    print()
}
#
# pour les fichiers globaux 20xx
guillaume_20xx_lire <- function(force = FALSE) {
  library(rio)
  library(tidyverse)
  library(stringr)
  carp()
  files <- list.files(guillaumeDir, pattern = "ODJ 20", full.names = TRUE, ignore.case = TRUE) %>%
    glimpse()
  stats.df <- data.frame()
  for (i in 1:length(files)) {
    file <- files[i]
    carp("file: %s", file)
    sheets <- paste0(excel_sheets(file))
    for(sheet in sheets) {
      stat.list <- guillaume_20xx_stat(file, sheet) %>%
        glimpse()
      stat.df <- data.frame(as.list(stat.list)) %>%
        glimpse()
      stats.df <- rbind(stats.df, stat.df)
    }
  }
  stats.df %>%
    glimpse() %>%
    print()
}
#
# les stat d'un fichier de l'année 2011
guillaume_2011_stat <- function(df) {
  library(janitor)
  library(tidyverse)
  library(stringr)
  carp()
  df1 <- df %>%
    clean_names() %>%
    glimpse()
# validité du champ effectif
  df2 <- df1 %>%
    filter(is.na(effectif))
  if (nrow(df2) > 0) {
    carp("effectif nrow: %s", nrow(df2))
  }
  df1 <- df1 %>%
    filter(! is.na(effectif))
# validité du champ espece
  df3 <- df1 %>%
    filter(is.na(espece))
  if (nrow(df3) > 0) {
    print(head(df3))
    carp("espece nrow: %s", nrow(df3))
  }
  df1 <- df1 %>%
    filter(! is.na(espece))
  carp("les jardins")
  df4 <- df1 %>%
    dplyr::select(-espece, -effectif) %>%
    distinct() %>%
    glimpse()
  carp("les especes")
  df4 <- df1 %>%
    dplyr::select(espece) %>%
    distinct() %>%
    glimpse()
}
#
# les stat d'un fichier de l'année 2012
guillaume_2012_stat <- function(dsn) {
  library(janitor)
  library(tidyverse)
  library(stringr)
  library(readxl)
  carp()
  fic <- gsub('.*/', '', dsn)
  fic <- gsub('\\..*$', '', fic)
  stat <- list(
    fic = fic
  )
  df <- read_excel(dsn)
  colnames(df) <- as.character(df[2,])
  df <- df[-1, ]
  df <- df[-1, ]
  df1 <- df %>%
    clean_names() %>%
    rename(effectif = nombre_total_eventuellement_fourchette) %>%
    rename(espece = nom_francais) %>%
    glimpse()
# validité du champ effectif
  df2 <- df1 %>%
    filter(is.na(effectif))
  stat[["effectif_na"]] <- nrow(df2)
  if (nrow(df2) > 0) {
    carp("effectif nrow: %s", nrow(df2))
  }
  df1 <- df1 %>%
    filter(! is.na(effectif))
# validité du champ espece
  df3 <- df1 %>%
    filter(is.na(espece))
  stat[["espece_na"]] <- nrow(df3)
  if (nrow(df3) > 0) {
    print(head(df3))
    carp("espece nrow: %s", nrow(df3))
  }
  df1 <- df1 %>%
    filter(! is.na(espece))
  carp("les jardins")
  df4 <- df1 %>%
    dplyr::select(-espece, -effectif) %>%
    distinct() %>%
    glimpse()
  stat[["jardins"]] <- nrow(df4)
  carp("les jardins par identifiant")
  df4 <- df1 %>%
    dplyr::select(identifiant_obs) %>%
    distinct() %>%
    glimpse()
  stat[["jardins_id"]] <- nrow(df4)
  carp("les especes")
  df4 <- df1 %>%
    dplyr::select(espece) %>%
    distinct() %>%
    glimpse()
  stat[["especes"]] <- nrow(df4)
  carp("les especes scientifique")
  df4 <- df1 %>%
    dplyr::select(nom_scientifique) %>%
    distinct() %>%
    glimpse()
  stat[["scientifique"]] <- nrow(df4)
  return(invisible(stat))
}
#
#
# les stat d'un fichier de l'année 2014 et suivantes
guillaume_20xx_stat <- function(dsn, sheet) {
  library(janitor)
  library(tidyverse)
  library(stringr)
  library(readxl)
  carp()
  fic <- gsub('.*/', '', dsn)
  fic <- gsub('\\..*$', '', fic)
  stat <- list(
    fic = fic
  )
  stat[["sheet"]] <- sheet
  df1 <- read_excel(dsn, sheet = sheet)
  stat[["jardins_id"]] <- nrow(df1)
#   validité du champ saidep
  df2 <- df1 %>%
    filter(is.na(saidep))
  stat[["saidep_na"]] <- nrow(df2)
  if (nrow(df2) > 0) {
    carp("saidep nrow: %s", nrow(df2))
  }
  df1 <- df1 %>%
    filter(! is.na(saidep))
  return(invisible(stat))
}