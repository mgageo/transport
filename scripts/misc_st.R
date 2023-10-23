# <!-- coding: utf-8 -->
#
# la partie package sf
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ==========================================================================
#
#
# https://www.r-bloggers.com/2023/04/progress-on-r-spatial-evolution-apr-2023/
#
# https://github.com/r-spatial/sf/issues/371
st_drop_geometry <- function(x) {
  if(inherits(x,"sf")) {
    x <- st_set_geometry(x, NULL)
    class(x) <- 'data.frame'
  }
  return(x)
}
st_drop_geom <- function(x) {
  if(inherits(x,"sf"))
    ret <- x[,setdiff(names(x),attr(x,'sf_column')),drop=T]
  else
    ret <- x
  class(ret) <- 'data.frame'
  return(ret)
}
st_version <- function(x) {
  library(sf)
  print(sprintf('sf_extSoftVersion()["GDAL"]: %s', sf_extSoftVersion()["GDAL"]))
  dsn <- "D:/bvi35/CouchesCBNBrest/CGTV_35.shp"
  gdal_utils("info", dsn, options = c("-mm", "-proj4"))
  st_read(dsn)
}
st_proches <- function(nc1, nc2) {
  carp()
  library(tidyverse)
  library(sf)
  nc2$no_ <- 1:nrow(nc2)
  nc1$proche_ <- st_nearest_feature(nc1, nc2)
  df <- dplyr::left_join(nc1 %>% as.data.frame(), nc2 %>% as.data.frame(), by = c('proche_' = 'no_'))
  df$d <- st_distance(df$geometry.x, df$geometry.y, by_element = TRUE)
  df <- df %>%
    dplyr::select(-proche_) %>%
    mutate(d = as.integer(d))
  return(invisible(df))
}
st_proches <- function(nc1, nc2, k = 1) {
  library(nngeo)
  n <- st_nn(nc1, nc2, k = k, progress = TRUE, returnDist = TRUE)
  mga <<- n
  ids <-  sapply(n[[1]], "[", k)
  dists <- sapply(n[[2]], "[", k)
  df3 <- data.frame(nc1, st_drop_geometry(nc2)[ids, , drop = FALSE])
  nc3 <- st_sf(df3)
  nc3$dist <- dists
  return(invisible(nc3))
}
#
## analyse d'un fichier osm avec sf/gdal
#
#
st_query <- function(requete, fic, force = FALSE, layer = "points") {
  library(tidyverse)
  library(stringr)
  library(sf)
  dsn2 <- sprintf("%s/%s.Rds", transportDir, fic)
  if (file.exists(dsn2) && force == FALSE) {
    nc <- readRDS(dsn2)
    return(invisible(nc))
  }
  dsn1 <- overpass_query(requete, fic, force = force)
  layers <- sf::st_layers(dsn1) %>%
    glimpse()
# https://github.com/r-spatial/sf/issues/1157
  ini_new <- "#configuration osm import
attribute_name_laundering=no
[points]
osm_id=yes
osm_version=yes
osm_timestamp=yes
osm_uid=yes
osm_user=yes
osm_changeset=yes
all_tags=yes
[lines]
osm_id=yes
osm_version=yes
osm_timestamp=yes
osm_uid=yes
osm_user=yes
osm_changeset=yes
all_tags=yes
[multipolygons]
osm_id=yes
osm_version=yes
osm_timestamp=yes
osm_uid=yes
osm_user=yes
osm_changeset=yes
all_tags=yes
[multilinestrings]
osm_id=yes
osm_version=yes
osm_timestamp=yes
osm_uid=yes
osm_user=yes
osm_changeset=yes
all_tags=yes
[other_relations]
osm_id=yes
osm_version=yes
osm_timestamp=yes
osm_uid=yes
osm_user=yes
osm_changeset=yes
all_tags=yes
"
  writeLines(ini_new, "ini_new.ini")
  nc <- data.frame()
  for (layer in layers$name) {
    carp("layer: %s", layer)
    nc1 <- st_read(dsn1, layer = layer, options = "CONFIG_FILE=ini_new.ini") %>%
      mutate(LAYER = !!layer)
    if (nrow(nc) == 0) {
      nc <- nc1
    } else {
      nc <- bind_rows(nc, nc1)
    }
  }
  glimpse(nc)
  if(nrow(nc) > 0) {

# comment faire sale
    for (i in 1:nrow(nc)) {
      all_tags <- nc[[i, "all_tags"]]
      pattern <- '"([^"]+)"=>"([^"]+)"'
      kv.list <<- all_tags %>%
        str_match_all(pattern)
      kv.df <- kv.list %>%
        data.frame()
      for (j in 1:nrow(kv.df)) {
        k <- kv.df[j, "X2"]
        v <- kv.df[j, "X3"]
        nc[i, k] <- v
      }
    }
  }
  saveRDS(nc, file = dsn2)
  return(invisible(nc))
}
#
# en direct de https://github.com/r-spatial/sf/blob/main/R/read.R
st_set_utf8 = function(x) {
	n = names(x)
	Encoding(n) = "UTF-8"
	to_utf8 = function(x) {
		if (is.character(x))
			Encoding(x) = "UTF-8"
		x
	}
	structure(lapply(x, to_utf8), names = n)
}
#
# conversion en format gpx compatble osm
st_sf2gpx <- function(nc1, name) {
  library(lubridate)
  library(sf)
  carp("début name: %s", name)
  gpx <- '<?xml version="1.0"?>
<gpx version="1.1" creator="GDAL 3.4.1" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:ogr="http://osgeo.org/gdal" xmlns="http://www.topografix.com/GPX/1/1" xsi:schemaLocation="http://www.topografix.com/GPX/1/1 http://www.topografix.com/GPX/1/1/gpx.xsd">
'
  for (i in 1:nrow(nc1)) {
    points <- st_cast(nc1[i, "geometry"], "POINT")
    df <- as_tibble(st_coordinates(points))
    gpx <- append(gpx, '<trk>')
    name <- sprintf('<name>%s</name>', name)
    gpx <- append(gpx, name)

    gpx <- append(gpx, '<trkseg>')
    datetime <- ymd("2021/11/01")
    for(i in 1:nrow(df)) {
      x <- datetime + minutes(i)
      trkpt <- sprintf('  <trkpt lat="%s" lon="%s">
      <ele>115.976196</ele>
      <time>%s</time>
    </trkpt>', df[i, "Y"], df[i, "X"], x)
      gpx <- append(gpx, trkpt)
    }
    gpx <- append(gpx, '</trkseg>')
    gpx <- append(gpx, '</trk>')
  }
  gpx <- append(gpx, '</gpx>')
  return(invisible(gpx))
}
#
# conversion en format gpx compatble osm
st_sf2gpx <- function(nc, dsn, name = "sf2gpx") {
  library(sf)
  carp("début name: %s", name)
  gpx <- '<?xml version="1.0"?>
<gpx version="1.1" creator="sf2gpx" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:ogr="http://osgeo.org/gdal" xmlns="http://www.topografix.com/GPX/1/1" xsi:schemaLocation="http://www.topografix.com/GPX/1/1 http://www.topografix.com/GPX/1/1/gpx.xsd">
'
#
# pour les point
  nc1 <- nc %>%
    filter(st_geometry_type(.) == "POINT") %>%
    glimpse()
  if (nrow(nc1) > 0) {
    for (i in 1:nrow(nc1)) {
      points <- st_cast(nc1[i, "geometry"], "POINT")
      df <- as_tibble(st_coordinates(points))
      wkpt <- sprintf('<wpt lat="%s" lon="%s">', df[1, "Y"], df[1, "X"])
      gpx <- append(gpx, wkpt)
      nom <- sprintf('  <name>%s</name>', nc1[[i, "name"]])
      gpx <- append(gpx, nom)
      gpx <- append(gpx, '</wpt>')
    }
  }
#
# pour les lignes
  nc1 <- nc %>%
    filter(st_geometry_type(.) == "LINESTRING") %>%
    glimpse()
  for (i in 1:nrow(nc1)) {
    points <- st_cast(nc1[i, "geometry"], "POINT")
    df <- as_tibble(st_coordinates(points))
    gpx <- append(gpx, '<trk>')
    name <- sprintf('<name>%s</name>', name)
    gpx <- append(gpx, name)

    gpx <- append(gpx, '<trkseg>')
    for(i in 1:nrow(df)) {
      trkpt <- sprintf('  <trkpt lat="%s" lon="%s"></trkpt>', df[i, "Y"], df[i, "X"])
      gpx <- append(gpx, trkpt)
    }
    gpx <- append(gpx, '</trkseg>')
    gpx <- append(gpx, '</trk>')
  }
  gpx <- append(gpx, '</gpx>')
  writeLines(gpx, dsn)
  return(invisible(gpx))
}