# <!-- coding: utf-8 -->
#
# les réseaux de bus de la région Bretagne
#
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
#
## validation des fichiers gtfs de la région
#
# la version avec un fichier zip par agence, novembre 2023
# Depuis lundi 23 octobre, mobibreizh.bzh est remplacé par KorriGo.bzh
# https://www.korrigo.bzh/ftp/OPENDATA/MobiBreizh_OpenData.gtfs.zip

# source("geo/scripts/transport.R");mobibreizh_zip()
mobibreizh_zip <- function() {
  library(tidyverse)
  library(rio)
  library(httr)
  library(readr)
  library(archive)
  carp()
  url <- "https://www.korrigo.bzh/ftp/OPENDATA/MobiBreizh_OpenData.gtfs.zip"
  aaaammjj <- format(Sys.time(),"%Y%m%d")
  dsn <- sprintf("%s/gtfs_%s.zip", odDir, aaaammjj)
  if (! file.exists(dsn)) {
    download.file(url, dsn)
  }
  carp("dsn: %s", dsn)
  df <- unzip(zipfile = dsn, list = TRUE) %>%
    mutate(reseau = gsub(".zip$", "", Name)) %>%
    glimpse()
  for (i in 1:nrow(df)) {
    dest_dir <- sprintf("%s/%s", odDir, df[[i, "reseau"]])
    dest_file <- sprintf("%s/%s", dest_dir, df[[i, "Name"]])
    gtfs_file <- sprintf("%s/%s", dest_dir, "gtfs.zip")
    dir.create(dest_dir, showWarnings = FALSE)
    unzip(zipfile = dsn, files = c(df[[i, "Name"]]), list = FALSE, exdir = dest_dir, overwrite = TRUE, setTimes = TRUE)
    unzip(zipfile = dest_file, list = FALSE, exdir = dest_dir, overwrite = TRUE, setTimes = TRUE)
    file.copy(dest_file, gtfs_file, copy.date = TRUE, overwrite = TRUE)
  }
#  setwd(odDir)
#  archive::archive_extract("gtfs.zip", dir = ".")
  setwd(baseDir)
}
## la version avec un fichier gtfs unique
#
# https://transport.data.gouv.fr/datasets/base-de-donnees-multimodale-transports-publics-en-bretagne-mobibreizh/
# https://breizh.opendatasoft.com/explore/dataset/base-de-donnees-multimodale-transports-publics-en-bretagne-mobibreizh/table/
# https://exs.breizgo.cityway.fr/ftp/GTFS/MOBIBREIZHBRET.gtfs.zip
# https://breizh.opendatasoft.com/explore/dataset/base-de-donnees-multimodale-transports-publics-en-bretagne-mobibreizh/mobibreizh/ la carte
#
# le fichier doit avoir été téléchargé dans D:\web.var\TRANSPORT\MOBIBREIZH\
# puis éclaté dans ce dossier
#
# source("geo/scripts/transport.R");mobibreizh_v1()
mobibreizh_v1 <- function() {
  library(tidyverse)
  library(rio)
  library(httr)
  library(readr)
  library(archive)
  carp()
  url <- "https://data.bretagne.bzh/api/explore/v2.1/catalog/datasets/base-de-donnees-multimodale-transports-publics-en-bretagne-mobibreizh/exports/csv?lang=fr&timezone=Europe%2FBerlin&use_labels=true&delimiter=%3B"
  res <- httr::GET(url)
  data <- httr::content(res, "text") %>%
    glimpse()
  url <- "https://exs.breizgo.cityway.fr/ftp/GTFS/MOBIBREIZHBRET.gtfs.zip"
  dsn <- sprintf("%s/gtfs.zip", odDir)
  download.file(url, dsn)
  setwd(odDir)
  archive::archive_extract("gtfs.zip", dir = ".")
  setwd(baseDir)
}
#
# extraction par réseau et production du gtfs.zip
#
# source("geo/scripts/transport.R");mobibreizh_reseaux()
mobibreizh_reseaux <- function() {
  library(tidyverse)
  library(rio)
  df <- mobibreizh_agency_lire() %>%
    filter(agency_id != "") %>%
    filter(grepl("MOBIBREIZH", gtfs_dir)) %>%
    glimpse()
#  return()
  for (i in 1:nrow(df)) {
    reseau <- df[i, "reseau"]
    agency_id <- df[i, "agency_id"]
    gtfs_dir <- df[i, "gtfs_dir"]
    mobibreizh_gtfs_reseau(reseau, agency_id, gtfs_dir)
  }
}
# source("geo/scripts/transport.R");mobibreizh_gtfs_reseau_shapes2ogr()
mobibreizh_gtfs_reseau_shapes2ogr <- function(reseau = "rmat") {
  library(tidyverse)
  library(rio)
  reseau_dir <<- sprintf("%s/%s", odDir, reseau)
  fic <- "shapes"
  dsn <- sprintf("%s/%s.txt", reseau_dir, fic)
  carp("dsn: %s", dsn)
  df <- rio::import(dsn, encoding = "UTF-8")
  gtfs_shapes2ogr(df, reseau_dir)
}
#
# extraction des données pour les réseaux gérés
# source("geo/scripts/transport.R");mobibreizh_gtfs_reseaux(agency = "TIBUS")
# source("geo/scripts/transport.R");mobibreizh_gtfs_reseaux(agency = "AXEOBUS")
# source("geo/scripts/transport.R");mobibreizh_gtfs_reseaux(agency = "QUB")
# source("geo/scripts/transport.R");mobibreizh_gtfs_reseaux(reseau = "Kiceo")
# setwd("d:/web");source("geo/scripts/transport.R");mobibreizh_gtfs_reseaux(reseau = "breizhgo56")
mobibreizh_gtfs_reseaux <- function(reseau = "Kiceo") {
  library(tidyverse)
  library(rio)
  carp("reseau: %s", reseau)
  df <- config_xls(reseau)
  agency <- Config[[1, "agency_id"]]
  df <- mobibreizh_agency_lire() %>%
    filter(reseau == !!reseau) %>%
#    filter(reseau == "breizhgo") %>%
    filter(agency_id != "") %>%
    filter(agency_id == !!agency) %>%
    filter(gtfs_dir != "") %>%
#    filter(grepl("Conseil", gestionnaire)) %>%
    glimpse()
#  return()
  for (i in 1:nrow(df)) {
    reseau <- df[i, "reseau"]
    agency_id <- df[i, "agency_id"]
    gtfs_dir <- df[i, "gtfs_dir"]
    mobibreizh_gtfs_reseau(reseau, agency_id, gtfs_dir)
  }
}

# source("geo/scripts/transport.R");mobibreizh_gtfs_reseau("auray", "AURAYBUS")
mobibreizh_gtfs_reseau <- function(reseau, agency_id, gtfs_dir) {
  library(tidyverse)
  library(rio)
  library(stringr)
  library(archive)
  carp("reseau: %s agency_id: %s gtfs_dir: %s", reseau, agency_id, gtfs_dir)
  if ( reseau == "star") {
    return()
  }
  reseau_dir <- sprintf("%s/%s", varDir, gtfs_dir)
  dir.create(reseau_dir, showWarnings = FALSE, recursive = TRUE)
#  regex <- sprintf("^%s:", agency_id)
  regex <- sprintf("^(%s):", agency_id)
  agencies <<- str_split(agency_id, "\\|")[[1]]

  fic <- "agency"
  dsn <- sprintf("%s/%s.txt", odDir, fic)
  carp("dsn: %s", dsn)
  df <- rio::import(dsn, encoding = "UTF-8") %>%
    filter(agency_id %in% agencies) %>%
    glimpse()
  dsn <- sprintf("%s/%s.txt", reseau_dir, fic)
  rio::export(df, dsn, format = "csv")
  fic <- "routes"
  dsn <- sprintf("%s/%s.txt", odDir, fic)
#  carp("dsn: %s", dsn)
  df <- rio::import(dsn, encoding = "UTF-8") %>%
    filter(agency_id %in% agencies)
  dsn <- sprintf("%s/%s.txt", reseau_dir, fic)
  rio::export(df, dsn, format = "csv")
  fic <- "trips"
  dsn <- sprintf("%s/%s.txt", odDir, fic)
#  carp("dsn: %s", dsn)
  df <- rio::import(dsn, encoding = "UTF-8") %>%
    filter(grepl(regex, route_id))
  dsn <- sprintf("%s/%s.txt", reseau_dir, fic)
  rio::export(df, dsn, format = "csv")
  fic <- "shapes"
  dsn <- sprintf("%s/%s.txt", odDir, fic)
#  carp("dsn: %s", dsn)
  df <- rio::import(dsn, encoding = "UTF-8") %>%
    filter(grepl(regex, shape_id))
  dsn <- sprintf("%s/%s.txt", reseau_dir, fic)
  rio::export(df, dsn, format = "csv")
  fic <- "stops"
  dsn <- sprintf("%s/%s.txt", odDir, fic)
#  carp("dsn: %s", dsn)
  df <- rio::import(dsn, encoding = "UTF-8") %>%
    filter(grepl(regex, stop_id))
  df <- df %>%
    mutate(stop_id = gsub("^(BIBUS|TIBUS)\\:", "", stop_id))
  dsn <- sprintf("%s/%s.txt", reseau_dir, fic)
  rio::export(df, dsn, format = "csv")
  fic <- "stop_times"
  dsn <- sprintf("%s/%s.txt", odDir, fic)
#  carp("dsn: %s", dsn)
  df <- rio::import(dsn, encoding = "UTF-8") %>%
    filter(grepl(regex, trip_id))
  df <- df %>%
    mutate(stop_id = gsub("^(BIBUS|TIBUS)\\:", "", stop_id))
  dsn <- sprintf("%s/%s.txt", reseau_dir, fic)
  rio::export(df, dsn, format = "csv")
  fic <- "calendar_dates"
  dsn <- sprintf("%s/%s.txt", odDir, fic)
  carp("dsn: %s", dsn)
  df <- rio::import(dsn, encoding = "UTF-8")
  dsn <- sprintf("%s/%s.txt", reseau_dir, fic)
  rio::export(df, dsn, format = "csv")
#  carp("dsn: %s", dsn)
  setwd(reseau_dir)
  carp("reseau_dir: %s", reseau_dir)
  files <- list.files('.', pattern = ".txt$", full.names = FALSE)
#  utils::zip(zipfile = 'gtfs_toto.zip', files = files)
  archive::archive_write_files("gtfs.zip", files)
  setwd(baseDir)
  carp("reseau_dir: %s", reseau_dir)
}
# source("geo/scripts/transport.R");mobibreizh_jour()
mobibreizh_jour_v0 <- function() {
  mobibreizh_routes()
#  stop('***')
  mobibreizh_shapes()
  mobibreizh_star()
}
# source("geo/scripts/transport.R");mobibreizh_gtfs()
mobibreizh_gtfs <- function() {
  library(tidyverse)
  library(rio)
  carp()
  dsn <- 'D:/web.var/TRANSPORT/MOBIBREIZH/20191007/mobibreizh-bd-gtfs.zip'
  tt <- gtfs_zip_lire(dsn) %>%
    glimpse()
}
# source("geo/scripts/transport.R");mobibreizh_gtfs_ctime()
mobibreizh_gtfs_ctime <- function() {
  library(tidyverse)
  library(rio)
  dsn <- sprintf("%s/agency.txt", odDir)
  ctime <- file.info(dsn)$ctime
  carp("dsn: %s %s", dsn, ctime)
  return(invisible(ctime))
}
# source("geo/scripts/transport.R");mobibreizh_gtfs_trips()
mobibreizh_gtfs_trips <- function() {
  library(tidyverse)
  library(rio)

  tt <- gtfs_lire() %>%
    glimpse()
#  mobibreizh_gtfs_routes(tt)
#  mobibreizh_gtfs_shapes(tt)
  gtfs_trips(tt)
}
# source("geo/scripts/transport.R");mobibreizh_agency_lire()
mobibreizh_agency_lire <- function() {
  library(tidyverse)
  library(rio)
  dsn <- sprintf("%s/agency.xls", cfgDir)
  carp("dsn: %s", dsn)
  df <- rio::import(dsn, col_names = TRUE, na = "") %>%
    replace(is.na(.), '')
  return(invisible(df))
}
#
# la validation des routes
# source("geo/scripts/transport.R");mobibreizh_agency()
mobibreizh_agency <- function() {
  library(tidyverse)
  library(readxl)
  carp()
  df <- mobibreizh_agency_lire()
#
# le template wiki
#  template <- readLines(dsn)
  wiki <- "<!-- coding: utf-8 -->
==Par territoire==
{|class='wikitable' width='100%'
|-class='sorttop'
!scope='col'| Territoire
!scope='col'| Collectivité gestionnaire
!scope='col'| Nom du réseau
!scope='col'| {{Tag|network}}
!scope='col'| {{Tag|operator}}
!scope='col'| Site web d'informations
!scope='col'| agency
!scope='col'| Page wiki de suivi
"
  template <- "|-
!scope='row' style='text-align:left'| @$territoire@ || [[@$gestionnaire@]]
| @$reseau@ || {{TagValue|network||@$network@}} || {{TagValue|operator||@$operator@}}||@$site@
| @$agency@ ||
"
  for ( i in 1:nrow(df) ) {
    tpl <- template
    tpl <- template(tpl, df[i,])
    wiki <- sprintf("%s%s", wiki, tpl)
  }
  wiki <- sprintf("%s%s", wiki, '|}')
  dsn <- sprintf("%s/agency_wiki.txt", odDir)
  write(wiki, file = dsn, append = FALSE)
  carp("dsn: %s", dsn);
}
template <- function(tpl, df) {
  attributs <- colnames(df)
  glimpse(attributs)
  for (attribut in attributs) {
    pattern <- sprintf('@\\$%s@', attribut)
    v <- df[1, attribut]
#    carp("pattern: %s v: %s", pattern, v)
    tpl <- gsub(pattern, v, tpl)
  }
#  carp("tpl :%s", tpl)
  return(tpl)
}
#
# la validation des routes
# source("geo/scripts/transport.R");mobibreizh_routes()
mobibreizh_routes <- function() {
  library(tidyverse)
  carp()
  df <- gtfs_routes()
  df1 <- df %>%
    group_by(agency_id) %>%
    summarize(nb=n()) %>%
    glimpse() %>%
    print(n=100)
}
#
# la validation des shapefiles
mobibreizh_shapes <- function() {
  carp()
  df <- gtfs_shapes_verif()
}
#
# la validation des voyages/arrets
# source("geo/scripts/transport.R");mobibreizh_gtfs_trips_stops()
mobibreizh_gtfs_trips_stops <- function() {
  carp()
  odDir <<- sprintf("%s/web/geo/TRANSPORT/MOBIBREIZH/20191007", Drive)
  df <- gtfs_trips_stops()
}
#
# la comparaison sur le réseau STAR
# source("geo/scripts/transport.R");mobibreizh_star_routes()
mobibreizh_star_routes <- function() {
  library(tidyverse)
  library(stringr)
  odDir <<- sprintf("%s/web/geo/TRANSPORT/MOBIBREIZH", Drive)
  mobibreizh_routes.df <- gtfs_routes()
  odDir <<- sprintf("%s/web/geo/TRANSPORT/STAR/GTFS", Drive)
  star_routes.df <- gtfs_routes()
  df <- mobibreizh_routes.df %>%
    filter(agency_id == 'STAR') %>%
    glimpse()
  star_routes.df %>%
    glimpse()
  df %>%
    anti_join(star_routes.df,by=(c("route_short_name"="route_short_name"))) %>%
    glimpse() %>%
    print(10)
  star_routes.df %>%
    anti_join(df,by=(c("route_short_name"="route_short_name"))) %>%
    glimpse() %>%
    print(10)
}
#
# la comparaison sur le réseau STAR
mobibreizh_star_stops <- function() {
  library(tidyverse)
  library(stringr)
  odDir <<- sprintf("%s/web/geo/TRANSPORT/MOBIBREIZH", Drive)
  mobibreizh_stops.df <- gtfs_stops()
  odDir <<- sprintf("%s/web/geo/TRANSPORT/STAR", Drive)
  star_stops.df <- gtfs_stops()
  star_stops.df$stop_id <- sprintf('%04d', star_stops.df$stop_id)
  df <- mobibreizh_stops.df %>%
    filter(grepl(':STA', stop_id)) %>%
    mutate(timeo=str_extract(stop_id, "\\d+")) %>%
    glimpse()
  df1 <- df %>%
    group_by(timeo) %>%
    summarise(nb=n()) %>%
    glimpse()
  df2 <- df1 %>%
    left_join(df, by=c("timeo"="timeo")) %>%
    distinct(timeo, .keep_all = TRUE) %>%
    glimpse()
  df3 <- df2 %>%
    left_join(star_stops.df, by=c("timeo"="stop_id")) %>%
    glimpse()
  df3 %>%
    filter(stop_name.x != stop_name.y) %>%
    glimpse()
  df3 %>%
    filter(stop_lon.x != stop_lon.y) %>%
    glimpse()
  df3 %>%
    filter(is.na(stop_name.y)) %>%
    glimpse() %>%
    head(20)
  carp("star versus mobibreiz")
  df3 <- star_stops.df %>%
    left_join(df2, by=c("stop_id"="timeo")) %>%
    glimpse()

  df3 %>%
    filter(is.na(stop_name.y)) %>%
    glimpse() %>%
    head(20)
}
#
# les stops : ajout de la commune
# utilisation de la version IGN pour avoir des informations en plus de la géométrie
#
# source("geo/scripts/transport.R");mobibreizh_stops()
mobibreizh_stops <- function() {
  library(tidyverse)
  library(stringr)
  stops.df <- gtfs_stops_verif()
  if ( ! exists("communes.sf") ) {
    communes.sf <<- ign_ade_lire_sf()
  }
  glimpse(communes.sf)
  stops.sf <- st_as_sf(stops.df, coords = c("lon", "lat"), crs = 4326)
  stops.sf <- st_transform(stops.sf, 2154)
  communes.sf <- st_transform(communes.sf, 2154)
  carp("crs: %s", st_crs(stops.sf))
  carp("crs: %s", st_crs(communes.sf))
  nc <- st_join(stops.sf, communes.sf, join = st_intersects) %>%
    glimpse()
  filter(nc, NOM_REG != 'BRETAGNE') %>%
    group_by(NOM_DEP) %>%
    summarize(nb=n()) %>%
    glimpse()
  dsn <- sprintf("%s/mobibreizh_stops.Rds", odDir)
  saveRDS(nc,dsn)
}
#
# lecture du fichier
# source("geo/scripts/transport.R");mobibreizh_stops_lire()
mobibreizh_stops_lire <- function() {
  dsn <- sprintf("%s/mobibreizh_stops.Rds", odDir)
  carp("dsn: %s", dsn)
  nc <- readRDS(dsn)
  glimpse(nc)
  return(invisible(nc))
}
#
# validation des stops opendata et des arrêts osm
# source("geo/scripts/transport.R");mobibreizh_stops_valid()
mobibreizh_stops_valid <- function() {
  nc <- mobibreizh_stops_lire()
# on enlève la SNCF
  nc <- filter(nc, ! grepl(':SNC', stop_id))
  stops.sf <- filter(nc, NOM_REG == 'BRETAGNE')
  stops.sf <- filter(nc, INSEE_DEP == 35)
#  stops.sf <- filter(nc, INSEE_COM == 35051)
  arrets.sf <- oapi_arrets_lire()
  arrets.sf <- st_transform(arrets.sf, 2154)
#  st_distance(stops.sf, arrets.sf) %>%
#    glimpse()
  carp("calcul des distances")
  arrets.sf$name <- as.character(arrets.sf$name)
  for ( i in 1:nrow(stops.sf) ) {
    if ( i%%100 == 0 ) {
      carp("%d/%d", i, nrow(stops.sf))
    }
    g <- st_distance(stops.sf[i,], arrets.sf, byid=TRUE)
    j <- which.min(g)
    d <- g[j]
    stops.sf$distance[i] <- as.integer(d)
    stops.sf$arret[i] <- arrets.sf$name[j]
  }
  glimpse(stops.sf)
# la liste avec une distance grande
  filter(stops.sf, distance > 100) %>%
    select(NOM_COM, stop_id, stop_name, arret, distance) %>%
    arrange(desc(distance)) %>%
    print(n=100)
#  return(invisible(nc))
}
#
# le fichier geojson des lignes
# source("geo/scripts/transport.R");mobibreizh_lignes()
mobibreizh_lignes <- function() {
  library(sf)
  nc <- mobibreizh_lignes_lire() %>%
#    dplyr::select(-geo_point_2d) %>%
    st_transform(4326) %>%
    glimpse()
  dsn <- sprintf("%s/lignes-routieres.shp", odDir)
  st_write(nc, dsn, append = FALSE)
  carp("dsn: %s", dsn)
  for (i in 1:nrow(nc)) {
    mobibreizh_ligne(nc[i, ])
  }
}
mobibreizh_ligne <- function(nc) {
  library(sp)
  library(rgdal)
  spdf <- as(nc, 'Spatial') %>%
    glimpse()
  id <- nc[[1, "id"]]
  dsn <- sprintf("%s/lignes-routieres/lignes-routieres-%s.gpx", odDir, id)
  writeOGR(spdf, dsn, layer="shape", driver="GPX", dataset_options="GPX_USE_EXTENSIONS=yes", overwrite_layer=TRUE, delete_dsn = TRUE)
  carp("dsn: %s", dsn)
}
mobibreizh_ligne <- function(nc) {
  library(sp)
  library(rgdal)
  dsn <- sprintf("%s/lignes-routieres/lignes-routieres-%s.shp", odDir, nc[[1, "id"]])
  glimpse(nc)
  st_write(nc, dsn, append = FALSE)
  carp("dsn: %s", dsn)
}
mobibreizh_lignes_lire <- function() {
  library(sf)
  dsn <- sprintf("%s/lignes-routieres-departementales-gerees-par-la-region-bretagne.geojson", odDir)
  nc <- st_read(dsn) %>%
    dplyr::select(-geo_point_2d) %>%
    glimpse()
  return(invisible(nc))
}
#
# Base de données multimodale transports publics en Bretagne - MobiBreizh
# https://data.bretagne.bzh/explore/dataset/base-de-donnees-multimodale-transports-publics-en-bretagne-mobibreizh/api/
# ce fichier ne contient que les stops
# source("geo/scripts/transport.R");mobibreizh_multi_lire()
mobibreizh_multi_lire <- function() {
  library(rio)
  dsn <- sprintf("%s/base-de-donnees-multimodale-transports-publics-en-bretagne-mobibreizh.csv", odDir)
  df <- rio::import(dsn, encoding = "UTF-8") %>%
    glimpse()
  return(invisible(df))
}
mobibreizh_gtfs_stops_lire <- function() {
  library(rio)
  fic <- "stops"
  dsn <- sprintf("%s/%s.txt", odDir, fic)
  carp("dsn: %s", dsn)
  df <- rio::import(dsn, encoding = "UTF-8") %>%
    glimpse()
  return(invisible(df))
}
# source("geo/scripts/transport.R");mobibreizh_stops_diff()
mobibreizh_stops_diff <- function() {
  library(rio)
  gtfs.df <- mobibreizh_gtfs_stops_lire()
  multi.df <- mobibreizh_multi_lire()
  df <- gtfs.df %>%
    full_join(multi.df, by = c("stop_id" = "ID")) %>%
    filter(stop_name != Name) %>%
    glimpse()
}