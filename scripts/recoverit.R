# <!-- coding: utf-8 -->
#
# quelques fonctions pour recoverit
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
#
# source("geo/scripts/recoverit.R");exif_stat()
source("geo/scripts/misc.R")
library(tidyverse)
library(exiftoolr)
library(arrow)
# le dossier avec les sauvegardes des traitements
rds_dir <- "c:/recoverit"
files_init <- function(files_version = 07) {
  files_version <<- files_version
# dossier avec les fichiers
  recover_dd <<- "F:"
  df <- read.table(text="version,dir,commentaire
01,E:/Recoverit 2023-12-27 at 10.09.25/Fichier sans nom/jpg,disque Pierre CB
02,E:/Recoverit 2023-12-29 at 06.02.25/Fichier sans nom/jpg,disque Pierre CB
03,F:/Recoverit 2023-12-31 at 18.54.05/Fichier sans nom/jpg,disque Pierre CB
04,F:/Recoverit 2024-01-03 at 16.45.01/Fichier sans nom/jpg,disque Pierre CB
05,E:,disque noir WD Marc
06,E:,disque bleu WD Marc
07,F:/Recoverit 2024-01-05 at 22.44.46/Fichier sans nom/jpg,disque Pierre CB
", header = TRUE, sep=",", blank.lines.skip = TRUE, stringsAsFactors = FALSE, quote = "") %>%
  filter(version == files_version)
  files_dir <<- df[[1, "dir"]]
  carp("files_dir: %s", files_dir)
# dossier avec l'arborescence
  arbo_dir <<- 'E:/Recoverit 2023-12-27 at 07.44.43/Fichier sans chemin/photo'
#
  copies_dsn <<- sprintf("%s/copies_%s.rds", rds_dir, files_version)
  sequences_dsn <<- sprintf("%s/sequences_%s.rds", rds_dir, files_version)
  exif_dsn <<- sprintf("%s/exif_%s.rds", rds_dir, files_version)
  infos_dsn <<- sprintf("%s/infos_%s.rds", rds_dir, files_version)
  google_dsn <<- sprintf("%s/google.rds", rds_dir)
}
#
# lecture de l'arborescence
# source("geo/scripts/recoverit.R");arbo_get()
arbo_get <- function() {
  files <- list.dirs(arbo_dir, full.names = FALSE, recursive = TRUE)
  nb_files <- length(files)
  print(sprintf("nb_files: %s", format(nb_files, big.mark = " ")))
  misc_print(files)
}

#
# lecture des données exif des fichiers d'un dossier
# source("geo/scripts/recoverit.R");exif_get()
exif_get <- function() {
  carp("liste des fichiers")
  files <- list.files(files_dir, pattern = "jpg$", full.names = TRUE, ignore.case = TRUE, recursive = FALSE)
  nb_files <- length(files)
  print(sprintf("nb_files: %s", format(nb_files, big.mark = " ")))
#  df <- exif_read(path = files) %>%
#    glimpse()
  tranche <- 1000
  debuts <- seq(1, nb_files, tranche)
  exif.df <- tibble()
# https://exiftool.org/TagNames/
  tags <- c("sourcefile", "filename", "FileCreateDate", "ModifyDate", "DateTimeOriginal", "filesize", "imagesize", "make", "model", "imagewidth", "imageheight", "megapixels", "warning")
  carp("lecture des exifs")
  for (debut in debuts) {
    print(sprintf("debut: %s/%s", format(debut, big.mark = " "), format(nb_files, big.mark = " ")))
    fin <- min((debut + tranche - 1), nb_files)
    f <- files[debut:fin] %>%
      glimpse()
    df <- exif_read(path = f, tags = tags) %>%
      glimpse()
    exif.df <- bind_rows(exif.df, df)
    saveRDS(exif.df, file = exif_dsn)
#    break
  }
  saveRDS(exif.df, file = exif_dsn)
  carp("fin nrow: %s", nrow(exif.df))
}
#
# quelques stats
# source("geo/scripts/recoverit.R");exif_stat()
exif_stat <- function() {
  library(janitor)
  exif.df <- readRDS(file = exif_dsn) %>%
    glimpse()
  stat.df <- exif.df %>%
    group_by(Warning) %>%
    summarize(nb = n()) %>%
    adorn_totals()
  misc_print(stat.df)
  minor.df <- exif.df %>%
    filter(grepl("minor", Warning))
#  misc_print(minor.df)
}

#
# controle de la date
# https://gist.github.com/micstr/69a64fbd0f5635094a53
#
# source("geo/scripts/recoverit.R");exif_date()
exif_date <- function(force = FALSE) {
  library(lubridate)
  library(magick)
  library(sqldf)
  exif.df <- readRDS(file = exif_dsn) %>%
    glimpse() %>%
    filter(is.na(Warning) | (Warning != "JPEG format error")) %>%
    glimpse()
  carp("is.na(ModifyDate)")
  df1 <- exif.df %>%
    filter(is.na(ModifyDate) | ! grepl("^[12]", ModifyDate)) %>%
    glimpse()

#  misc_print(minor.df)
  carp("ymd_hms")
  df2 <- exif.df %>%
    filter(! is.na(ModifyDate) & grepl("^[12]", ModifyDate)) %>%
    glimpse() %>%
    mutate(date = ymd_hms(ModifyDate)) %>%
    arrange(date) %>%
    glimpse()
  carp("les dates non valide")
  df2 %>%
    filter(is.na(date)) %>%
    glimpse()
  carp("date valide")
  df2 <- df2 %>%
    filter(! is.na(date)) %>%
    glimpse()
  carp("filesize < 100000")
  df2 <- df2 %>%
    filter(FileSize >= 100000) %>%
    glimpse()
#  exif_date_doublons(df2)
#  exif_date_google(df2)
#  exif_date_stat(df2)
  exif_date_sequence(df2)
}
exif_date_doublons <- function(df2, force = force) {
  df11 <- df2 %>%
    distinct(across(c(-SourceFile, -FileName, -FileCreateDate)), .keep_all = TRUE) %>%
    glimpse()
#  stop("*****")
  carp("les doublons date")
  df3 <- df11 %>%
    group_by(date) %>%
    summarize(nb = n()) %>%
    filter(nb > 1) %>%
    glimpse()
  if (nrow(df3) > 1000) {
    df4 <- df2 %>%
      filter(date %in% df3$date) %>%
      arrange(date) %>%
      glimpse()
    for (i3 in 1:nrow(df3)) {
      df5 <- df2 %>%
        filter(date == df3[[i3, "date"]]) %>%
        glimpse()
      imgs <- c()
      for (i5 in 1:nrow(df5)) {
        img <- image_read(df5[[i5, "SourceFile"]])
        print(image_info(img))
        imgs <- append(imgs, img)
      }
      img <- image_append(image_scale(imgs, "600x300"), stack = TRUE)
      print(image_info(img))
      print(img)
#      break
    }
  }
}
#
exif_date_stat <- function(df) {
  library(ggplot2)
  gg <- ggplot(
    df,
    aes(format(date, "%Y"))
    ) +
  geom_bar(stat = "count") +
  labs(x = "année", y = "nombre")
  print(gg)
}
#
exif_date_sequence <- function(df) {
  carp("début")
  copies.df <- tibble(file = character(), dossier = character())
  df1 <- df %>%
    mutate(Date = as.Date(date)) %>%
    group_by(Date) %>%
    summarize(nb = n()) %>%
#    filter(nb > 3) %>%
    arrange(Date) %>%
    mutate(delta = as.integer(Date - dplyr::lag(Date))) %>%
    filter((delta == 1) | (dplyr::lead(delta) == 1)) %>%
    glimpse()
  debut <- df1[[1, "Date"]]
  sequence <- 1
  df1[[1, "sequence"]] <- sequence
  for (i1 in 2:nrow(df1)) {
    if (df1[[i1, "delta"]] == 1) {
      df1[[i1, "sequence"]] <- sequence
      next
    }
    sequence <- sequence + 1
    df1[[i1, "sequence"]] <- sequence
    fin <- df1[[i1 - 1, "Date"]]
    debut <- df1[[i1, "Date"]]
  }
  misc_print(df1)
  df2 <- df1 %>%
    group_by(sequence) %>%
    summarize(
      debut = dplyr::first(Date),
      fin = dplyr::last(Date),
      jours = n(),
      nb = sum(nb)
    ) %>%
    mutate(dossier = sprintf("%s_%s", debut, fin)) %>%
    glimpse()
  misc_print(df2)
  saveRDS(df2, file = sequences_dsn)
  carp("sequences nb: %s", nrow(df2))
  return()
  carp("création des dossiers sequence")
  for (i2 in 1:nrow(df2)) {
    dossier <- sprintf("%s/photos/%s", recover_dd, df2[[i2, "dossier"]])
    dir.create(dossier, showWarnings = FALSE, recursive = TRUE)
    df3 <- df %>%
      filter(date >= df2[[i2, "debut"]]) %>%
      filter(date <= df2[[i2, "fin"]] + 1) %>%
      glimpse()
    if (nrow(df3) != df2[[i2, "nb"]]) {
      break
    }
    for (i3 in 1:nrow(df3)) {
      copies.df <- copies.df %>%
        add_row(file = df3[[i3, "SourceFile"]], dossier = dossier)
      exif_copy(df3[[i3, "SourceFile"]], dossier)
    }
  }
  saveRDS(copies.df, file = copies_dsn)
  carp("fin nb: %s", nrow(copies.df))
}
#
# copie en mode sans erreur
exif_copy <- function(file, dossier) {
#  try(fs::file_copy(file, dossier))
}
#
# la feuille Google Sheets
exif_date_google <- function(df2, force = FALSE) {
  df21 <- google_get(force = force)
  df22 <- sqldf::sqldf("SELECT a.*, b.*
      FROM df2 a
      LEFT JOIN df21 b
      on a.date >= b.debut AND a.date <= b.fin") %>%
    as_tibble() %>%
    group_by(date, FileSize) %>%
    arrange(jours) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    dplyr::select(FileName, date, dossier) %>%
    glimpse()
  misc_print(df22)
}
#
# la feuille Google Sheet
#
# source("geo/scripts/recoverit.R");google_get()
google_get <- function(force = FALSE) {
  library(googlesheets4)
  library(lubridate)
  if (force == FALSE & file.exists(google_dsn)) {
    df <- readRDS(file = google_dsn)
  } else {
    url <- "https://docs.google.com/spreadsheets/d/1MjDmz0tQ2FjNJJiB-OpzODFpB0tTrxv6bqjmTvC_l7U/edit?usp=sharing"
    df <- read_sheet(url) %>%
      glimpse()
    saveRDS(df, file = google_dsn)
    carp("fin nrow: %s", nrow(df))
  }
  df <- df %>%
    filter(! is.na(debut)) %>%
    filter(! is.na(fin)) %>%
    mutate(dossier = unlist(dossier)) %>%
    mutate(jours = as.integer(difftime(fin, debut, units = "days"))) %>%
    arrange(jours) %>%
    glimpse()
  return(invisible(df))
}
# source("geo/scripts/recoverit.R");google_write()
google_write <- function(force = FALSE) {
  library(googlesheets4)
  library(lubridate)
  url <- "https://docs.google.com/spreadsheets/d/1u4CBhv3OX9JujNWo3xcznY6bpASrQhorsIOIxCTl-Qc"
  df <- readRDS(file = sequences_dsn) %>%
    write_sheet(
      ss = gs4_get(url),
      sheet = sprintf("v%s", files_version)
    )
}
# source("geo/scripts/recoverit.R");google_upload()
google_upload <- function(force = FALSE) {
  library(googledrive)
  drive_auth("marc.gauthix@gmail.com")
  dest_dir <- "~/Informatique/DD Pierre Noël 2023/"
  file <- "./geo/scripts/recoverit.R"
  drive_upload(file, dest_dir, type = "document")
}
#
# lecture des infos des fichiers d'un dossier
# source("geo/scripts/recoverit.R");infos_get()
infos_get <- function() {
  carp("liste des fichiers")
  files <- list.files(files_dir, pattern = "jpg$", full.names = TRUE, ignore.case = TRUE, recursive = FALSE)
  nb_files <- length(files)
  print(sprintf("nb_files: %s", format(nb_files, big.mark = " ")))
  infos.df <- tibble()
  tranche <- 1000
  debuts <- seq(1, nb_files, tranche)
  carp("lecture des infos")
  for (debut in debuts) {
    print(sprintf("debut: %s/%s", format(debut, big.mark = " "), format(nb_files, big.mark = " ")))
    fin <- min((debut + tranche - 1), nb_files)
    f <- files[debut:fin] %>%
      glimpse()
    df <- file.info(f) %>%
      glimpse()
    infos.df <- bind_rows(infos.df, df)
    saveRDS(infos.df, file = infos_dsn)
#    break
  }
  saveRDS(infos.df, file = infos_dsn)
  carp("fin nrow: %s", nrow(infos.df))
}
infos_get1 <- function() {
  carp("liste des fichiers")
  files <- list.files(files_dir, pattern = "jpg$", full.names = TRUE, ignore.case = TRUE, recursive = FALSE)
  nb_files <- length(files)
  print(sprintf("nb_files: %s", format(nb_files, big.mark = " ")))
  infos.df <- tibble()
  carp("lecture des infos")
  infos.df <- file.info(files)
  saveRDS(infos.df, file = infos_dsn)
  carp("fin nrow: %s", nrow(infos.df))
}
# https://theautomatic.net/2018/07/11/manipulate-files-r/
# fileSnapshot
# source("geo/scripts/recoverit.R");infos_get2()
infos_get2 <- function() {
  carp("liste des fichiers")
  infos.df <- fileSnapshot(files_dir)
  saveRDS(infos.df, file = infos_dsn)
  carp("fin nrow: %s", nrow(infos.df))
}
#
## vous avez dit sale ?
files_init()
