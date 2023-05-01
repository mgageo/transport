# <!-- coding: utf-8 -->
#
# le réseau de bus de Saint-Brieuc
#
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
# source("geo/scripts/transport.R");tub_shiny()
tub_shiny <- function() {
  shiny::runApp("geo/scripts/transport")
}
# source("geo/scripts/transport.R");tub_gtfs()
tub_gtfs <- function() {
  library(tidyverse)
  library(rio)
  carp()
  tub_gtfs_shapes()
  tub_gtfs_stops()
}
#
# pour les shapes : la conversion en gpx/geojson
# source("geo/scripts/transport.R");tub_gtfs_shapes()
tub_gtfs_shapes <- function(tt) {
  carp()
  config_xls('tub');txt_gtfs_shapes_sf()
}
#
# pour les stops
# source("geo/scripts/transport.R");tub_gtfs_stops()
tub_gtfs_stops <- function() {
  carp()
  config_xls('tub');txt_gtfs_stops_sf()
}
#
## pour le tracé des parcours
#
# source("geo/scripts/transport.R");df <- tub_osrm(overpass = FALSE, osrm = TRUE)
tub_osrm <- function(overpass = FALSE, osrm = TRUE) {
  carp()
  library(knitr)
  library(kableExtra)
  config_xls('tub');
  df <- osm_relations_routes(force = overpass) %>%
    mutate(ways = as.numeric(ways)) %>%
    extract(shape, c("shape"), "(\\d+)") %>%
    filter(ways == 0) %>%
    arrange(id) %>%
    glimpse()
  for(i in 1:nrow(df)) {
    id <- df[i, "id"]
    shape <- df[i, "shape"]
    shape_fic <- sprintf("%s/JOSM/shape_%s_level0.txt", transportDir, shape)
    if (! file.exists(shape_fic)) {
      carp("shape_%s_level0.txt absent", shape)
    } else {
      df[i, "shape_fic"] <- shape_fic
    }
    if (osrm == FALSE) {
      next
    }
    tryCatch(
      {
        osrm_jour(ref = id, shape = shape)
      },
      error = function(condition){
        carp('erreur %s', condition)
      }
    )
  }
  df1 <- df %>%
    mutate(ref = sprintf("r%s", id)) %>%
    dplyr::select(id, ref, shape_fic)
  print(knitr::kable(df1, format = "pipe"))
  return(invisible(df1))
}
# source("geo/scripts/transport.R");df <- tub_level0(ligne = 1, force = FALSE)
tub_level0 <- function(ligne = 1, force = FALSE) {
  library(clipr)
  library(readr)
  i <- ligne
  df <- tub_osrm(overpass = force) %>%
    glimpse()
  if (nrow(df) < i ) {
    confess("ligne inexistante: %s", i)
  }
  id <- df[i , "id"]
  shape_fic <- df[i, "shape_fic"]
  if (file.exists(shape_fic)) {
    level0 <- read_lines(shape_fic)
    write_clip(level0)
    url <- sprintf("http://level0.osmz.ru/?url=relation/%s", id)
    browseURL(
      url,
      browser = "C:/Program Files/Mozilla Firefox/firefox.exe"
    )
    Sys.sleep(20)
  }
# https://josm.openstreetmap.de/wiki/Help/Preferences/RemoteControl
  url <- sprintf("http://localhost:8111/import?url=https://api.openstreetmap.org/api/0.6/relation/%s/full", id)
  browseURL(
    url,
    browser = "C:/Program Files/Mozilla Firefox/firefox.exe"
  )
}



#
## la comparaison des parcours des relations route versus les shapes
#
# https://gis.stackexchange.com/questions/236906/split-a-line-when-it-crosses-a-polygon-in-r
# source("geo/scripts/transport.R"); config_xls("tub");osm_routes_shapes(force = TRUE)
osm_routes_shapes <- function(force = FALSE) {
  carp()
  config_xls('tub');
  library(sf)
  nc <- osm_relations_route_parcours() %>%
    mutate(dsn = sprintf("%s/JOSM/%s", transportDir, note.mga_geo)) %>%
    st_transform(2154) %>%
    glimpse()
  nc$lg <- 0
  for (i in 5:nrow(nc)) {
    dsn <- nc[[i, "dsn"]]
    nc1 <- st_read(dsn, layer = "tracks", quiet = TRUE) %>%
      st_transform(2154)
    nc0 <- nc[i, ]
    nc2 <- st_buffer(nc0, 50)
    nc3 <- st_difference(nc1, nc2)
    if (nrow(nc3) > 0 ) {
      lg <- as.integer(st_length(nc3))
      nc[i, "lg"] <- lg
      if (lg > 1000) {
        dev.new()
        plot(st_geometry(nc0), col = "blue", lwd = 3)
        plot(st_geometry(nc1), col = "green", lwd = 1, add = TRUE)
        plot(st_geometry(nc3), col = "red", lwd = 5, add = TRUE)
        titre <- sprintf("%s %s", nc[[i, "ref.network"]], nc[[i, "note.mga_geo"]])
        title(titre)
      }

    }
#    stop("****")
  }
  nc <- nc %>%
    dplyr::select(osm_id, ref.network, lg) %>%
    filter(lg > 200) %>%
    arrange(lg)
  View(nc)
}
#
## récupération sur le site https://tub.bzh/reseau/carte-interactive
# source("geo/scripts/transport.R");tub_reseau()
#
# https://rstudio-pubs-static.s3.amazonaws.com/202583_aa19399e91204242b547339535a51a3d.html
tub_reseau <- function(force = TRUE) {
  carp()
  config_xls('tub');
  library(httr)
  library(tidyverse)
  library(jsonlite)
  library(readr)
  library(htmltab)
  library(utf8)
  library(stringr)
  dsn <- sprintf("%s/poi_80.json", transportDir)
  carp("dsn: %s", dsn)
  if (! file.exists(dsn) || force == TRUE) {
    url <- "https://tub.bzh/api/poi/list-pth/80"
    carp("url: %s", url)
    res <- httr::GET(url = url, encoding = "UTF-8", type = "application/json", verbose(), httr::write_disk(dsn, overwrite = TRUE))
    txt <- httr::content(res, as = "text")
  } else {
    txt <- read_file(dsn)
    txt <- as_utf8(txt)
  }
  txt <- unicode_utf8(txt)
  txt <- str_replace_all(txt, 'href="[^"]+"', "")
#  print(txt)
  dsn <- sprintf("%s/poi_80.txt", transportDir)
  write_file(txt, dsn)
#  json_parse <- fromJSON(txt) %>%
#    glimpse()
  stream_in(file(dsn)) %>%
    glimpse()
}
#
## différence entre deux fichiers gtfs
#
# source("geo/scripts/transport.R");config_xls('tub');tub_gtfs_stops_diff()
tub_gtfs_stops_diff <- function(force = FALSE) {
  library(tidyverse)
  library(janitor)
  dsn <- sprintf("%s/%s/%s/stops.txt", cfgDir, toupper(config[1, "reseau"]), config[1, "gtfs_dir"])
  carp("dsn: %s", dsn)
  df1 <- rio::import(dsn, encoding = "UTF-8") %>%
    filter(location_type == 0) %>%
    glimpse()
  dsn <- sprintf("%s/%s/%s/stops.txt", cfgDir, toupper(config[1, "reseau"]), config[1, "gtfs_old"])
  carp("dsn: %s", dsn)
  df2 <- rio::import(dsn, encoding = "UTF-8") %>%
    filter(location_type == 0) %>%
    glimpse()
  df <- df1 %>%
    full_join(df2, by = c("stop_id"), suffix = c(".1", ".2")) %>%
    filter(is.na(stop_code.2)) %>%
    arrange(stop_id) %>%
    glimpse()
}
#
# source("geo/scripts/transport.R");config_xls('tub');tub_stops_diff()
tub_stops_diff <- function(force = FALSE) {
  library(tidyverse)
  library(janitor)
  gtfs.sf <- txt_gtfs_stops_sf(force = force) %>%
    filter(location_type == 0) %>%
    glimpse()
  gtfs.df <- gtfs.sf %>%
    st_drop_geometry()
  osm.sf <- osm_area_busstop_get(force = force) %>%
    dplyr::select(ref = `ref:FR:TUB`, name, osm_id) %>%
    mutate(stop_id = as.numeric(str_extract(ref, "\\d+")))
  gtfs2osm_stops(gtfs.sf, osm.sf)
  return()
  osm.df <- osm.sf %>%
    st_drop_geometry() %>%
    filter(! is.na(`ref:FR:TUB`)) %>%
    dplyr::select(name, `ref:FR:TUB`) %>%
    mutate(stop_id = as.numeric(str_extract(`ref:FR:TUB`, "\\d+"))) %>%
    glimpse()
  df1 <- gtfs.df %>%
    full_join(osm.df, by = c("stop_id"), suffix = c(".gtfs", ".osm")) %>%
    filter(is.na(name)) %>%
    arrange(stop_id) %>%
    glimpse()
  nc1 <- st_join(gtfs.sf, osm.sf, join = st_nearest_feature) %>%
    glimpse()
}
