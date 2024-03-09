# <!-- coding: utf-8 -->
#
# quelques fonctions génériques pour utilisation des données d'OpenStreetMap pour le transport
#
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
#
transport_save <- function(obj, fic = 'objets_stop') {
  dsn <- sprintf("%s/%s.Rds", transportDir, fic)
  saveRDS(obj, dsn)
  carp("dsn: %s", dsn)
  return(invisible(obj))
}
transport_sauve <- function(obj, fic = 'objets_stop') {
  dsn <- sprintf("%s/%s.Rds", transportDir, fic)
  saveRDS(obj, dsn)
  carp("dsn: %s", dsn)
  return(invisible(obj))
}
transport_ecrire <- function(obj, fic = 'objets_stop') {
  dsn <- sprintf("%s/%s.Rds", transportDir, fic)
  saveRDS(obj, dsn)
  carp("dsn: %s", dsn)
  return(invisible(obj))
}
transport_read <- function(fic = 'objets_stop') {
  dsn <- sprintf("%s/%s.Rds", transportDir, fic)
  obj <- readRDS(dsn)
  carp("dsn: %s", dsn)
  return(invisible(obj))
}
transport_lire <- function(rds = 'objets_stop') {
  dsn <- sprintf("%s/%s.Rds", transportDir, rds)
  obj <- readRDS(dsn)
  carp("dsn: %s", dsn)
  return(invisible(obj))
}
transport_html_browse <- function(html, titre, Exit = FALSE) {
#  if (html != "") {
    dsn <- sprintf("%s/%s.html", webDir, titre)
    write(html, dsn)
    carp("dsn: %s", dsn)
#  }
  if (HtmlBrowse != FALSE) {
    url <- sprintf("http://localhost/transport/%s.html", titre)
    browseURL(
      url,
      browser = "C:/Program Files/Mozilla Firefox/firefox.exe"
    )
  }
  if (Exit != FALSE) {
    stop("****transport_html_browse")
  }
}

#
# source("geo/scripts/transport.R");transport_modifiy_tags()
transport_modifiy_tags <- function(rds = "reseau_osm_routes_tag_shape") {
  library(tidyverse)
  carp()
  df <- transport_read(rds)
  for (i in 1:nrow(df)) {
    tags <- list(
#      "gtfs:shape_id" = df[[i, "shape_id"]]
      "opening_hours" = "Jul 10-Sep 03 2023 off"
    )
    url <- osmchange_object_modify_tags(id = df[[i, "id"]], type= "relation", tags = tags)
    carp("url: %s", url)
# pour vérifier le changeset
#    next
#    browseURL(
#      url,
#      browser = "C:/Program Files/Mozilla Firefox/firefox.exe"
#    )
#    stop("****")
  }
  return(invisible())
}
#
# ne sert plus
transport_osm_routes_gtfs_shapes_df <- function() {
  library(tidyverse)
  library(sf)
  carp()
  shapes.sf <- tidytransit_shapes_sf_lire() %>%
    mutate(source='shapes') %>%
    glimpse()
  routes.sf <- osm_routes_read() %>%
    dplyr::select(shape_id=ref.FR.STAR) %>%
    filter(! is.na(shape_id)) %>%
    glimpse()
  names(routes.sf$geometry)=NULL
  names(shapes.sf$geometry)=NULL

# avec la géométrie
  carp('jointure')
  df <- dplyr::left_join(routes.sf %>% as.data.frame(), shapes.sf %>% as.data.frame(), by=c('shape_id'='shape_id')) %>%
    glimpse()
#
# le tracé gtfs est inclus dans le buffer route osm
  carp('passage en Lambert 93 et buffer')
  nc1 <- st_transform(routes.sf, 2154) %>%
    st_buffer(50)
  nc2 <- st_transform(shapes.sf, 2154)
  df <- dplyr::left_join(nc1 %>% as.data.frame(), nc2 %>% as.data.frame(), by=c('shape_id'='shape_id')) %>%
    filter(! is.na(source)) %>%
    glimpse()
  transport_save(df, 'osm_routes_gtfs_shapes')
}
# source("geo/scripts/transport.R");transport_osm_routes_gtfs_shapes_diff()
transport_osm_routes_gtfs_shapes_diff <- function() {
  carp('contains')
  library(sf)
  library(tidyverse)
  df <- transport_read('osm_routes_gtfs_shapes') %>%
    arrange(shape_id)
  nc <- st_as_sf(df, geom_routes=st_sfc(df$geometry.x))
  nc$geom_shapes <- st_sfc(df$geometry.y)
  for (i in 1:nrow(df)) {
    nc1 <- nc[i, ]
    plot(st_geometry(nc1$geom_routes))
    plot(st_geometry(nc1$geom_shapes), add=TRUE)
    inters <- st_intersection(nc1$geom_shapes, nc1$geom_routes)
    plot(st_geometry(inters), add=TRUE, col='blue', lwd=3)
#    carp('inters')
#    glimpse(inters)
#    carp('shapes')
#    glimpse(nc1$geom_shapes)
    diff.sfc <<- st_difference(nc1$geom_shapes, inters)
    if (length(diff.sfc) >= 1 ) {
      carp('shape_id: %s', nc[[i, 'shape_id']])
      plot(st_geometry(diff.sfc), add=TRUE, col='red', lwd=3)
 #     break
    }
  }
}
# pour l'outil conflate de josm
transport_stops_conflate <- function(stops.df, fic = "toto") {
  df1 <- stops.df %>%
    mutate(lat = as.numeric(stop_lat)) %>%
    mutate(lon = as.numeric(stop_lon)) %>%
    mutate(latitude = sprintf("%0.6f", stop_lat)) %>%
    mutate(longitude = sprintf("%0.6f", stop_lon)) %>%
    mutate(highway = "bus_stop") %>%
    mutate(public_transport = "platform") %>%
    dplyr::select(name = stop_name, "ref:Aléop" = stop_id, latitude, longitude, highway, public_transport) %>%
    glimpse()
  dsn <- sprintf("%s/%s.csv", reseau_dir, fic)
  write.csv(df1, file = dsn, row.names = FALSE, na="", quote = FALSE, fileEncoding = "UTF-8")
  carp("dsn: %s nrow: %s", dsn, nrow(df1))
  return(invisible(df1))
}
# ???
# wkbMultiLineString wkbPolygon
ogr_lire <- function(dsn, geomType="wkbLineString", layer=FALSE, crs="+init=epsg:2154") {
  require(rgdal)
  require(rgeos)
  Log(sprintf("ogr_lire() dsn:%s", dsn))
  if ( layer == FALSE ) {
    layer <- ogrListLayers(dsn)
    print(sprintf("ogr_lire() %s %s", layer, dsn))
    spdf <- readOGR(dsn, layer=layer, stringsAsFactors=FALSE, use_iconv=TRUE, encoding="UTF-8", require_geomType=geomType)
  } else {
    spdf <- readOGR(dsn, layer=layer, stringsAsFactors=FALSE, use_iconv=TRUE, encoding="UTF-8")
  }
  if ( crs != FALSE ) {
    spdf <- spTransform(spdf, CRS("+init=epsg:2154"))
  }
  return(invisible(spdf))
}
transport_df2html <- function(df1, dsn = FALSE, titre = "Titre") {
  df2 <- df1 %>%
    glimpse() %>%
    rename_with(~str_remove(., "@")) %>%
    mutate(level0 = sprintf("<a href='http://level0.osmz.ru/?url=%s/%s' target='_blank'>%s</a>", type, id, id)) %>%
    mutate(josm = sprintf("<a href='http://localhost:8111/load_object?objects=%s%s,relation_members=true,referrers=true' target='josm'>josm</a>", str_sub(type, 1, 1), id)) %>%
    mutate(ptna = sprintf("<a href='https://www.openstreetmap.org/%s/%s' target='ptna'>ptna</a>", type, id)) %>%
    glimpse()
  html_df2fic(df2, dsn = dsn, titre = titre)
}