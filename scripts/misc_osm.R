# <!-- coding: utf-8 -->
#
# utilisation des données d'OpenStreetMap
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
# auteur : Marc Gauthier
#
#
#243500139;communauté d’agglomération de Rennes Métropole
#243500675;communauté de communes du Pays d’Aubigné
#243500659;communauté de communes du Pays de Châteaugiron
#243500774;communauté de communes du Pays de Liffré
#243500667;communauté de communes du Val d’Ille
#
osm_install <- function() {
  install.packages("Rcurl")
  install.packages("R.utils")
  install.packages("OpenStreetMap")
}
#
# la partie fonds de carte osm and co
# https://www.r-bloggers.com/the-openstreetmap-package-opens-up/
# https://github.com/mtennekes/tmap/tree/master/demo/LondonCrimes
# http://maps.stamen.com/terrain-background/#14/48.0868/-1.6917#
#
# carte dans la projection d'une grille
# plusieurs étapes :
# - détermination de la projection
# - détermination de l'emprise
# - agrandissement de l'emprise
# - calcul du polygone englobant
# - transformation de ce polygone en projection lonlat
# - extraction de la carte
# - transformation en format raster
# - découpe à l'emprise
# - affichage
osm_test <- function() {
  require(OpenStreetMap)
  require(raster)
  zone.spdf <- fonds_grille_lire()
  zone.crs <- proj4string(zone.spdf)
  bb <- bbox(zone.spdf)
  bb[,1] <- bb[,1] - 500
  bb[,2] <- bb[,2] + 500
  e <- extent(c(bb[1,1], bb[1,2], bb[2,1], bb[2,2]))
  clip.sp <- as(e, "SpatialPolygons")
  proj4string(clip.sp) <- CRS(zone.crs)
  lonlat <- spTransform(clip.sp, CRS("+init=epsg:4326"))
  boundingBox <- bbox(lonlat)
  Log(sprintf("upper left %s %s lower right %s %s", boundingBox[2,2], boundingBox[1,1], boundingBox[2,1], boundingBox[1,2]))
  nm <- c(
    "esri",
    "osm", "maptoolkit-topo", "bing",
    "stamen-toner", "stamen-watercolor", "stamen-terrain"
  )
  par(mfrow=c(3,3))
  for(i in 1:length(nm)){
    map <- openproj(openmap(c(boundingBox[2,2], boundingBox[1,1]), c(boundingBox[2,1], boundingBox[1,2]), 14, type=nm[i], minNumTiles=40), projection = zone.crs)
    plot(map)
  }
  stop("***")
  r <- raster(map)
  r <- crop(r, e)
  plotRGB(r)
  plot(zone.spdf, add=TRUE)
  rf <- writeRaster(r, filename="test.tif", format="GTiff", overwrite=TRUE)
}
#
# http://maps.stamen.com/m2i/#terrain-background/2000:1000/15/48.1026/-1.7450
osm_stamen <- function() {
  print(sprintf("osm_stamen()"))
  require(OpenStreetMap)
  require(raster)
  zone.spdf <- fonds_grille_lire()
  zone.crs <- proj4string(zone.spdf)
  bb <- bbox(zone.spdf)
  bb[,1] <- bb[,1] - 500
  bb[,2] <- bb[,2] + 500
  e <- extent(c(bb[1,1], bb[1,2], bb[2,1], bb[2,2]))
  clip.sp <- as(e, "SpatialPolygons")
  proj4string(clip.sp) <- CRS(zone.crs)
  lonlat <- spTransform(clip.sp, CRS("+init=epsg:4326"))
  boundingBox <- bbox(lonlat)
  Log(sprintf("upper left %s %s lower right %s %s", boundingBox[2,2], boundingBox[1,1], boundingBox[2,1], boundingBox[1,2]))
  url <- "http://a.tile.stamen.com/terrain-background/{z}/{x}/{y}.png"
  map <- openproj(openmap(c(boundingBox[2,2], boundingBox[1,1]), c(boundingBox[2,1], boundingBox[1,2]), 14, type=url, minNumTiles=40), projection = zone.crs)
  plot(map)
#  stop("***")
  r <- raster(map)
  r <- crop(r, e)
  plotRGB(r)
  plot(zone.spdf, add=TRUE)
  dsn <- sprintf("%s/stamen_terrain-background.tif", varDir)
  rf <- writeRaster(r, filename=dsn, format="GTiff", overwrite=TRUE)
}
osm_stamen_lire <- function() {
  require(raster)
  dsn <- sprintf("%s/stamen_terrain-background.tif", varDir)
  img <- brick(dsn)
  plotRGB(img)
}
#
# la patie api openstreetmap
# ======================================
#
# création d'une requête osm3
# un type et une emprise (boundingbox)
osm3_requete <- function(zone, type) {
  requete <- osm3_query(type)
  s="48.09";n="48.15";w="-1.38";e="-1.60"
# Acigné Servon
  w="-1.62";n="48.20";e="-1.30";s="48.00"
  body <- requete
  body <- gsub("\\$s", s, body)
  body <- gsub("\\$n", n, body)
  body <- gsub("\\$e", e, body)
  body <- gsub("\\$w", w, body)
  return(body)
}
#
# les différents types de requête disponible
osm3_query <- function(type) {
  switch(type,
# tous dans une bbox
  'bbox' = {
    body <- '<osm-script timeout="180">
<union>
    <bbox-query s="$s" n="$n" w="$w" e="$e"/>
  <recurse type="up"/>
</union>
<print/>
</osm-script>
'
  },
# limite des communes
  'limite_commune_rel' = {
    body <- '<osm-script timeout="180">
  <query type="relation">
    <bbox-query s="$s" n="$n" w="$w" e="$e"/>
    <has-kv k="admin_level" v="8"/>
    <has-kv k="boundary" v="administrative"/>
    <has-kv k="ref:INSEE"/>
   </query>
<print order="quadtile"/>
</osm-script>
'
  },
  'limite_commune_bbox' = {
#     <area-query ref="7465"/>
  body3 <- '<osm-script timeout="180">
 <union>
  <query type="relation">
    <bbox-query s="$s" n="$n" w="$w" e="$e"/>
    <has-kv k="admin_level" v="8"/>
    <has-kv k="boundary" v="administrative"/>
    <has-kv k="ref:INSEE"/>
   </query>
  <recurse type="relation-way"/>
  <recurse type="way-node"/>
</union>
<print order="quadtile"/>
</osm-script>
'
  },
# la limite d'une commune
  'limite_commune' = {
  body <- '<osm-script timeout="180">
<union>
  <query type="relation">
    <has-kv k="admin_level" v="8"/>
    <has-kv k="boundary" v="administrative"/>
    <has-kv k="ref:INSEE" v="35001"/>
  </query>
  <recurse type="relation-node" into="nodes"/>
  <recurse type="relation-way"/>
  <recurse type="way-node"/>
</union>
<print order="quadtile"/>
</osm-script>
'
  },
# limite de plusieurs communes
  'limite_communes' = {
    body <- '<osm-script timeout="180">
<union>
'
  aCommunes <- c("35001","35039","35051","35068", "35099","35207","35327")
  for (commune in aCommunes) {
    body <-sprintf('%s
  <query type="relation">
    <has-kv k="admin_level" v="8"/>
    <has-kv k="boundary" v="administrative"/>
    <has-kv k="ref:INSEE" v="%s"/>
  </query>', body, commune)
}
    body <-sprintf('%s
  <recurse type="relation-node" into="nodes"/>
  <recurse type="relation-way"/>
  <recurse type="way-node"/>
</union>
<print order="quadtile"/>
</osm-script>
', body)
  },
# limite d'un département
  'limite_departement' = {
# la relation du département
  body <- '<osm-script timeout="180">
  <query type="relation">
    <has-kv k="admin_level" v="6"/>
    <has-kv k="boundary" v="administrative"/>
    <has-kv k="name" v="Ille-et-Vilaine"/>
   </query>
<print order="quadtile"/>
</osm-script>
'
  },
# http://wiki.openstreetmap.org/wiki/FR:France_roads_tagging
  'highway' = {
  body <- '<osm-script timeout="180">
<union>
  <query type="way">
    <bbox-query s="$s" n="$n" w="$w" e="$e"/>
    <has-kv k="highway" v="motorway"/>
  </query>
  <query type="way">
    <bbox-query s="$s" n="$n" w="$w" e="$e"/>
    <has-kv k="highway" v="primary"/>
  </query>
  <query type="way">
    <bbox-query s="$s" n="$n" w="$w" e="$e"/>
    <has-kv k="highway" v="secondary"/>
  </query>
  <recurse type="way-node"/>
</union>
<print order="quadtile"/>
</osm-script>
'
  }
  )
  return(body)
}
#  http://www.omegahat.org/RCurl/philosophy.html
#
# utilisation de l'overpass avec requete en post
osm_xapi <- function() {
  library("RCurl")
  print(sprintf("osm_xapi() début"))
  h = basicTextGatherer()
  url <- 'http://overpass-api.de/api/interpreter'
  url <- 'http://api.openstreetmap.fr/oapi/interpreter'
  if ( url.exists(url) ) {
    print(sprintf("xapi() url.exists : %s", url))
    return;
  }
# limite des communes
  type <- 'highway'
  zone <- 'AcigneServon'
  query <- sprintf("%s_%s", type, zone)
  body <- osm3_requete(zone, type)
  print(sprintf("xapi() body: %s", body))
  f_osm3 <- sprintf("%s/osm/OSM/%s.osm3", baseDir, query)
  write(body, f_osm3)
  print(sprintf("xapi() f_osm3: %s", f_osm3))
  h$reset()
  curlPerform(url = url,
    httpheader=c(
      Accept="text/xml",
      Accept="multipart/*",
      'Content-Type' = "text/xml; charset=utf-8"
    ),
    postfields=body,
    writefunction = h$update,
    verbose = TRUE
  )
  response = h$value()
  f_osm <- sprintf("%s/osm/OSM/%s.osm", baseDir, query)
  write(response, f_osm)
  print(sprintf("osm_xapi() f_osm: %s", f_osm))
}
#
# les différents types de requête disponible
oapi_requete <- function(zone, type) {
  switch(zone,
  'Cesson' = {
    Ville <- 'cesson';
    ullr <-"-1.636687 48.160419 -1.550375 48.086584";
    code_insee <- '35051';
    osm_rel <- 78150
  }
  );
  switch(type,
  'highway' = {
    body <- sprintf('(area["ref:INSEE"="35051"]->.RM;way(area.RM)[highway][highway!=lane][highway!=track][highway!=service][access!=private];>;);out meta qt;', code_insee)
  },
  'bus_stop' = {
    body <- sprintf('area["ref:INSEE"="%s"]->.RM;node(area.RM)[highway=bus_stop];out meta qt;', code_insee)
  }
  )
  return(body)
}
# requête en get
osm_oapi <- function(data, f_osm) {
  library(httr)
  library(rvest)
  library(tidyverse)
  library(RCurl)
  carp("début data:%s", data)
  url <- 'https://overpass-api.de/api/interpreter'
#  url <- 'http://api.openstreetmap.fr/oapi/interpreter'
#  url <- 'https://overpass-turbo.eu/'
  if ( url.exists(url) ) {
    print(sprintf("oapi() url.exists : %s", url))
    return;
  }
  res <- httr::RETRY ("POST", url, body = data)
  stop_for_status(res)
  doc <- httr::content (res, as = 'text', encoding="UTF-8", type = "application/xml") %>%
    glimpse()
  write(doc, f_osm)
  carp("f_osm: %s", f_osm)
}
# requête en get
osm_oapi_rcurl <- function(data, f_osm) {
  library("RCurl")
  carp("début data:%s", data)
  h = basicTextGatherer()
  url <- 'https://overpass-api.de/api/interpreter'
#  url <- 'http://api.openstreetmap.fr/oapi/interpreter'
#  url <- 'https://overpass-turbo.eu/'
  if ( url.exists(url) ) {
    print(sprintf("oapi() url.exists : %s", url))
    return;
  }
  url = sprintf('%s?data=[timeout:360][maxsize:1073741824];%s', url, URLencode(data));
  carp("url: %s", url)
  h = basicTextGatherer()
  h$reset()
  curlPerform(url = url,
    writefunction = h$update,
    verbose = FALSE
  )
  response = h$value()
  write(response, f_osm)
  carp("f_osm: %s", f_osm)
}
# requête en get
osm_oapi_json <- function(data, dsn) {
  library(httr)
  carp("début data:%s", data)
  url <- 'http://overpass-api.de/api/interpreter'
  data <- sprintf("[out:json][timeout:180];%s", data)
  url = sprintf('%s?data=%s', url, URLencode(data));
  carp("url: %s", url)
  res <- httr::GET(url)
  json <- httr::content(res, as="text", encoding="UTF-8")
  print(summary(json))
  write(json, dsn)
  print(sprintf("osm_oapi_json() dsn: %s",dsn))
}
# requête en get
osm_oapi_geojson <- function(data, dsn) {
  library(httr)
  carp("début data:%s", data)
  url <- 'http://overpass-api.de/api/interpreter'
  data <- sprintf("[out:GeoJson][timeout:180];%s", data)
  url = sprintf('%s?data=%s', url, URLencode(data));
  carp("url: %s", url)
  res <- httr::GET(url)
  json <- httr::content(res, as="text", encoding="UTF-8")
  print(summary(json))
  write(json, dsn)
  print(sprintf("osm_oapi_json() dsn: %s",dsn))
}
osm_oapi_bbox <- function(sf) {
  sf$cercle <- sf %>%
    st_buffer(500) %>%
    st_geometry()
  st_geometry(sf) <- "cercle"
  sf <- st_transform(sf, 4326)
  bb <- st_bbox(sf)
  opq0 <- opq(bbox = bb)
  opq1 <- add_osm_feature(opq = opq0, key = 'highway')
  print(opq_string(opq1))
  res1 <- osmdata_sf(opq1)
}
#
## les requêtes transport
# =============================================================================================
#
# lecture avec sf et extraction de l'ensemble des tags
#
#
# source("geo/scripts/transport.R");config_xls('tub');nc <-osm_area_busstop_get()
osm_area_busstop_get <- function(force = FALSE) {
  library(tidyverse)
  library(stringr)
  dsn <- sprintf("%s/nodes_busstop.Rds", transportDir)
  if (file.exists(dsn) && force == FALSE) {
    nc <- readRDS(dsn)
    return(invisible(nc))
  }
  requete <- sprintf('area[name="%s"]->.a;(node(area.a)[highway=bus_stop];node(area.a)[public_transport!=stop_position][bus];);out meta;', config[1, 'zone'])
  dsn1 <- overpass_query(requete, "nodes_busstop", force = force)
# https://github.com/r-spatial/sf/issues/1157
  ini_new <- "#configurartion osm import
attribute_name_laundering=no
[points]
osm_id=yes
osm_version=yes
osm_timestamp=yes
osm_uid=yes
osm_user=yes
osm_changeset=yes
all_tags=yes
"
  writeLines(ini_new, "ini_new.ini")
  nc <- st_read(dsn1, layer = "points", options = "CONFIG_FILE=ini_new.ini")
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
  saveRDS(nc, file = dsn)
  return(invisible(nc))
}
#
#
osm_overpass_query <- function(requete, fic, force = FALSE, layer = "points") {
  library(tidyverse)
  library(stringr)
  dsn2 <- sprintf("%s/%s.Rds", transportDir, fic)
  if (file.exists(dsn2) && force == FALSE) {
    nc <- readRDS(dsn2)
    return(invisible(nc))
  }
  dsn1 <- overpass_query(requete, fic, force = force)
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
  nc <- st_read(dsn1, layer = layer, options = "CONFIG_FILE=ini_new.ini")
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
# pour le parcours des relations route
# source("geo/scripts/transport.R"); config_xls("tub");nc <- osm_relations_route_parcours()
osm_relations_route_parcours <- function(force = FALSE) {
  carp()
  library(sf)
  dsn <- sprintf("%s/relations_route_parcours.Rds", transportDir)
  if (file.exists(dsn) && force == FALSE) {
    df <- readRDS(dsn)
    return(invisible(df))
  }
  requete <- sprintf("area[name='Bretagne'];(relation[network='%s'][type=route][route=bus](area);>>;);out meta;", config[1, 'network'])
  res <- osmdata_query(requete, "relations_route_parcours", force = force)
  nc <- res$osm_multilines
#  nc <- cbind(nc, st_coordinates(st_centroid(nc)))
#  plot(st_geometry(nc))
#  text(st_coordinates(st_centroid(nc)), labels = nc$ref.network, cex=1.2, col='black')
  return(invisible(res))
}
osm_relations_routes <- function(force = FALSE) {
  carp()
  config_xls('tub');
  library(jsonlite)
  dsn <- sprintf("%s/relations_route.Rds", transportDir)
  if (file.exists(dsn) && force == FALSE) {
    df <- readRDS(dsn)
    return(invisible(df))
  }
  requete <- sprintf("(relation[network='%s'][type=route][route=bus];);out meta;", config[1, 'network'])
  dsn1 <- overpass_query_json(requete, "relations_route", force = force)
  json1.list <- jsonlite::fromJSON(dsn1, simplifyVector = FALSE, simplifyDataFrame = FALSE)
  elements.list <- json1.list$elements
  routes.df <- data.frame(
    id = character(),
    user = character(),
    timestamp = character(),
    shape = character(),
    nodes = numeric(),
    ways = numeric()
  )
  for (i in 1:length(elements.list)) {
    element.list <- elements.list[[i]]
    tags <- element.list$tags
    shape <- ""
    if ( exists("note:mga_geo", where=tags)) {
      shape <- tags[["note:mga_geo"]]
    }
    nodes <- 0
    ways <- 0
    if ( exists("members", where=element.list)) {
      members.list <- element.list$members
      df1 <- do.call(rbind, lapply(members.list, data.frame))
      nodes <- nrow(filter(df1, type == "node"))
      ways <- nrow(filter(df1, type == "way"))    }
#    carp("shape: %s", shape)
#    glimpse(tags); stop("****")
    routes.df[i, ] = c(
      element.list$id,
      element.list$user,
      element.list$timestamp,
      shape,
      nodes,
      ways
    )
  }
  saveRDS(routes.df, file = dsn)
  return(invisible(routes.df))
}