# <!-- coding: utf-8 -->
#
# quelques fonctions pour la comptage oiseaux des jardins
# auteur: Marc Gauthier
# licence: Creative Commons Paternité - Pas d"Utilisation Commerciale - Partage des Conditions Initiales à l"Identique 2.0 France
# ===============================================================
#
# la partie Google Drive
# ===============================================================
#
# une fenêtre firefox doit s"ouvrir !
# source("geo/scripts/coj.R"); drive_jour_get()
drive_jour_get <- function(test = 1) {
  carp()
  library(googledrive)
  drive_auth("univasso35@gmail.com")
  drive_fichier_get("Bilans", test)
  drive_fichier_get("Bilans_22", test)
}
#
# source("geo/scripts/coj.R"); drive_fichier_get()
drive_fichier_get <- function(fichier = "Bilans", test = 1) {
  carp()
  library(tidyverse)
  library(googledrive)  
  dsn <- sprintf("%s/%s.xlsx", varDir, fichier)
  fic <- sprintf("~/Comptage Oiseaux des Jardins/bilans/%s", fichier)
  drive_download(
    fichier,
    path = dsn,
    overwrite = TRUE
  )
}
# source("geo/scripts/coj.R"); drive_fichier_lire()
drive_fichier_lire <- function(fichier = "Bilans") {
  carp()
  library(tidyverse)
  library(readxl)
  dsn <- sprintf("%s/%s.xlsx", varDir, fichier)
  df <- read_xlsx(dsn, col_names = TRUE)
  return(invisible(df))
}
# source("geo/scripts/coj.R"); drive_jour_upload()
drive_jour_upload <- function(test = 1) {
  carp()
  library(googledrive)
  drive_pdf_upload()
}
drive_pdf_upload <- function(test = 1) {
  carp()
  library(googledrive)
  drive_auth("univasso35@gmail.com")
  drive_fichier_upload("dept35_stat.pdf")
  drive_fichier_upload("dept35sql_stat.pdf")
}
#
drive_fichier_upload <- function(fichier, dossier, test = 1) {
  carp()
  library(tidyverse)
  dsn <- sprintf("%s/%s", texDir, fichier)
  path <- sprintf("~/Comptage Oiseaux des Jardins/%s/%s", dossier = "MarcGauthier", fichier)
  carp("path: %s", path)
  drive_upload(
    dsn,
    path = path,
    overwrite = TRUE
  )
}
