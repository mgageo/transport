# <!-- coding: utf-8 -->
#
# utilisation des données d'OpenStreetMap
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
# auteur : Marc Gauthier
#
# https://wiki.openstreetmap.org/wiki/API_v0.6
#
# en direct de https://github.com/hrbrmstr/overpass/blob/master/R/overpass_query.r
#
# source("geo/scripts/transport.R"); res <- overpass_get_objet(ref = "11920346"); print(res)
overpass_base_url <- "http://overpass-api.de/api/interpreter"
overpass_status <- function(quiet=FALSE) {
  status <- httr::GET(overpass_base_url)
  status <- httr::content(status)
  status_now <- strsplit(status, "\n")[[1]][4]
  if (!quiet) {
    message(status_now)
    print(status_now)
  }
  if (grepl("after", status_now)) {
    available <- FALSE
    status_now <- gsub("Slot available after: ", "", status_now)
    status_now <- gsub(", in.*", "", status_now)
    slot_time <- lubridate::ymd_hms(status_now)
    current_time <- strsplit(status, "\n")[[1]][2]
    current_time <- lubridate::ymd_hms(gsub("Current time: ", "", current_time))
    waiting_time <- difftime(current_time, slot_time, units = "secs")
  } else {
    available <- TRUE
    waiting_time <- 0
  }
  return(invisible(list(available=available, waiting_time=waiting_time, msg=status)))
}
#
# source("geo/scripts/transport.R");res <- osrm_get_json()
overpass_query <- function(query, fic = "test", force = FALSE) {
  library(httr)
  library(tidyverse)
  dsn <- sprintf("%s/%s.osm", transportDir, fic)
  carp("dsn: %s", dsn)
  if (! file.exists(dsn) || force == TRUE) {
    query <- sprintf("data=[timeout:600][maxsize:1073741824];%s", query)
    carp("query: %s", query)
    res <- httr::POST(overpass_base_url, body=query, httr::write_disk(dsn, overwrite = TRUE))
    stop_for_status(res)
  }
  return(invisible(dsn))
}
overpass_query_v1 <- function(query, quiet=FALSE) {
  carp("query: %s", query)
  res <- httr::POST(overpass_base_url, body=query)
  httr::stop_for_status(res)
  if (!quiet) message("Query complete!")
  if (res$headers$`content-type` == "text/csv") {
    return(httr::content(res, as="text", encoding="UTF-8"))
  }
  doc <- xml2::read_xml(httr::content(res, as="text", encoding="UTF-8"))
  return(invisible(doc))
}
overpass_query_json <- function(query, fic = "test", force = FALSE) {
  library(httr)
  library(tidyverse)
  library(rjson)
  dsn <- sprintf("%s/%s.json", transportDir, fic)
  carp("dsn: %s", dsn)
  if (! file.exists(dsn) || force == TRUE) {
    query <- sprintf("data=[timeout:600][maxsize:1073741824][out:json];%s", query)
    res <- httr::POST(overpass_base_url, body=query, httr::write_disk(dsn, overwrite = TRUE))
    stop_for_status(res)
  }
  return(invisible(dsn))
}