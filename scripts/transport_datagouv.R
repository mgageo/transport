# <!-- coding: utf-8 -->
#
# les gtfs de data.gouv
#
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
# source("geo/scripts/transport.R");datagouv_jour()
datagouv_jour <- function(reseau = "quimper", force = TRUE) {
  library(tidyverse)
  library(rio)
  library(archive)
  carp()
  config_xls(reseau)
  datagouv_gtfs_dl(reseau = reseau)
  tidytransit_jour(force = TRUE)
}
#
# source("geo/scripts/transport.R");datagouv_gtfs_dl()
datagouv_gtfs_dl <- function(reseau = "quimper", force = FALSE) {
  library(tidyverse)
  library(archive)
  library(jsonlite)
  library(httr2)
  library(lubridate)
  carp()
  config_xls(reseau)
  url <- Config_gtfs_source
  req <- request(url)
  resp <- req |>
    req_perform()
  resp |> resp_headers()
  modified <- resp %>%
    resp_header("last-modified")
  dg_date <- dmy_hms(modified) %>%
    glimpse()
  dg_fic <- format(dg_date, format = "%Y%m%d-%H%M%S") %>%
    glimpse()
  dg_dsn <- sprintf("%s/gtfs_%s.zip", gtfsDir, dg_fic)
  if (file.exists(dg_dsn)) {
    return("déjà présent")
  }
  resp <- req |>
    req_progress() %>%
    req_perform(path = dg_dsn)
  dsn <- sprintf("%s/gtfs.zip", gtfsDir)
  file.copy(dg_dsn, dsn, overwrite = TRUE)
  archive_extract(dsn, gtfsDir)
  carp("fin dsn: %s", dsn)
}
