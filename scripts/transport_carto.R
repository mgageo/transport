# <!-- coding: utf-8 -->
#
# utilisation des données d'OpenStreetMap
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
# auteur : Marc Gauthier
#
#
#
## les différentes cartographies
#
#
carto_route_shapes_stops <- function(df, nc1) {
  library(tidyverse)
  library(sf)
  carp()
  shape_id <- df[[1, "shape_id"]]
  shape <- df[[1, "shape"]]
  dsn_shape <- sprintf("%s/shape_%s.gpx", josmDir, shape)
  if (! file.exists(dsn_shape)) {
    glimpse(df[1, ])
    confess("*** dsn_shape: %s", dsn_shape)
  }
  shape.sf <- st_read(dsn_shape, layer = "tracks", quiet = TRUE) %>%
    st_transform(2154)
  plot(st_geometry(shape.sf), add = FALSE, col = "green", lwd = 3)
  nc2 <- nc1 %>%
    filter(shape_id == !!shape_id)
  plot(st_geometry(nc2), col = "green", lwd = 3, pch = 19, add = TRUE)
  df2 <- st_distance(nc2, shape.sf) %>%
    glimpse()
  nc2 <- cbind(nc2, df2) %>%
    mutate(distance = as.integer(df2)) %>%
    dplyr::select(-df2) %>%
    glimpse()
  loins.sf <- nc2 %>%
    filter(distance > 30)
  if (nrow(loins.sf) > 0) {
    plot(st_geometry(loins.sf), col = "red", lwd = 3, pch = 19, add = TRUE)
    glimpse(loins.sf)
    stops_loins.df <- rbind(stops_loins.df, st_drop_geometry(loins.sf))
  }
  titre <- sprintf("%s", shape_id)
  title(titre)
}
#
# la carte de la ligne
# https://rgeomatic.hypotheses.org/2077
# https://riatelab.github.io/mapsf/
carto_route_shapes_stops_mapsf <- function(df, nc1) {
  library(tidyverse)
  library(sf)
  library(mapsf)
  carp()
  shape_id <- df[[1, "shape_id"]]
  shape <- df[[1, "shape"]]
  dsn_shape <- sprintf("%s/shape_%s.gpx", josmDir, shape)
  if (! file.exists(dsn_shape)) {
    glimpse(df[1, ])
    confess("*** dsn_shape: %s", dsn_shape)
  }
  shape.sf <- st_read(dsn_shape, layer = "tracks", quiet = TRUE) %>%
    st_transform(2154)
  mf_init(shape.sf, expandBB = c(0.1, 0.1, 0.1, 0.1))
  mf_map(x = shape.sf, col = "green", lwd = 3, add = TRUE)
  nc2 <- nc1 %>%
    filter(shape_id == !!shape_id) %>%
    mutate(Name = sprintf("%s %s", id, stop_name))
  if (nrow(nc2) > 0) {
    mf_map(x = nc2, col = "green", lwd = 3, pch = 19, add = TRUE)
    mf_label(x = nc2,
      var = "Name",
      cex = 0.8,
      overlap = FALSE,
      lines = TRUE
    )
  }
  mf_title(shape)
  points.sf <- st_sf(st_cast(st_geometry(shape.sf), "POINT"))
  mf_annotation(points.sf[1, ], txt = "Départ")
}
#
# la carte de la ligne
# l'id de la relation et l'identification du shape
# source("geo/scripts/transport.R");carto_route_shape(id = "10554878", shape = "Din1R1AL10")
# source("geo/scripts/transport.R");carto_route_shape_mapsf(id = "7711144", shape = "0122-B-1065-1090")
carto_route_shape <- function(id, shape, force = TRUE) {
  library(tidyverse)
  library(sf)
  carp("id: %s shape: %s", id, shape)
  osm.sf <- osmapi_get_ways(ref = id, force = force, force_osm = force) %>%
    st_transform(2154)
  add <- FALSE
  if (! st_is_empty(osm.sf[1,]) ) {
    plot(st_geometry(osm.sf), col = "blue", lwd = 3)
    add <- TRUE;
  }

#    stop("****")
  dsn_shape <- sprintf("%s/shape_%s.gpx", josmDir, shape)
  if (! file.exists(dsn_shape)) {
    title(sub = "shape absent")
#      confess("*** dsn_shape: %s", dsn_shape)
  } else {
    shape.sf <- st_read(dsn_shape, layer = "tracks", quiet = TRUE) %>%
      st_transform(2154)
    plot(st_geometry(shape.sf), add = add, col = "green", lwd = 3)
    inters <- st_intersection(st_buffer(shape.sf, 10), osm.sf)
    plot(st_geometry(inters), add = TRUE, col = "grey", lwd=3)
  }
  titre <- sprintf("relation: %s shape: %s", id, shape)
  title(titre)
  legend("topleft",
    , legend=c("osm", "gtfs", "inters")
    , col=c("blue", "green", "grey")
    , lwd = 3, lty = 1, cex = 0.8, box.lty = 0
  )
}
#
# la carte des shapes d'une ligne
#
carto_ref_shapes_stops_mapsf <- function(df, force = TRUE) {
  library(tidyverse)
  library(sf)
  library(mapsf)
  carp()
  df <- df %>%
    filter(shape_id != "") %>%
    mutate(dsn_shape = sprintf("%s/shape_%s.gpx", josmDir, shape_id)) %>%
    filter(file.exists(dsn_shape)) %>%
    glimpse()
  if (nrow(df) < 1) {
    carp("pas de shape")
    return()
  }
  departs.df <- tibble()
  arrivees.df <- tibble()
  for (i in 1:nrow(df)) {
    shape.sf <- st_read(df[i, "dsn_shape"], layer = "tracks", quiet = TRUE)
    df[i,"geometry"] <- shape.sf[1, "geometry"]
    points.sf <- st_sf(st_cast(st_geometry(shape.sf), "POINT"))
    departs.df[i, "name"] <- df[i, "first_name"]
    departs.df[i, "geometry"] <- points.sf[1, 1]
    arrivees.df[i, "name"] <- df[i, "last_name"]
    j <- nrow(points.sf)
    arrivees.df[i, "geometry"] <- points.sf[j, 1]
  }
  shapes.sf <- df %>%
    st_as_sf(sf_column_name = "geometry") %>%
    st_transform(2154)
  bouts.sf <- bind_rows(departs.df, arrivees.df) %>%
    st_as_sf(sf_column_name = "geometry") %>%
    st_transform(2154) %>%
    glimpse()
  mf_init(shapes.sf, expandBB = c(0.1, 0.1, 0.1, 0.1))
  mf_map(x = shapes.sf, col = "blue", lwd = 3, add = TRUE)
  mf_label(x = bouts.sf,
    var = "name",
    cex = 0.8,
    overlap = TRUE,
    lines = TRUE
  )
  mf_title(sprintf("%s",df[1, "ref_network"]), inner = TRUE)
}
#
# la carte de la ligne
# l'id de la relation et l'identification du shape
# source("geo/scripts/transport.R");carto_route_shape_mapsf(id = "10554878", shape = "Din1R1AL10")
carto_route_shape_mapsf <- function(id, shape, force = TRUE, force_osm = TRUE) {
  library(tidyverse)
  library(sf)
  library(mapsf)
  carp("id: %s shape: %s", id, shape)
  osm.sf <- osmapi_get_ways(ref = id, force = force, force_osm = force_osm)

  if (nrow(osm.sf) < 1) {
    carp("***id: %s", id)
  }
  point1_distance <- -1
  osm.sf <- osm.sf %>%
    st_transform(2154)
  mf_init(osm.sf, expandBB = c(0.1, 0.1, 0.1, 0.1))
  mf_map(x = osm.sf, col = "blue", lwd = 3, add = TRUE)
  osm_points.sf <- st_sf(st_cast(st_geometry(osm.sf), "POINT"))
  mf_annotation(osm_points.sf[1, ], txt = "Départ", col_txt = "blue", col_arrow = "blue")
  dsn_shape <- sprintf("%s/shape_%s.gpx", josmDir, shape)
  if (! file.exists(dsn_shape)) {
    shape <- sprintf("%s absent", shape)
  } else {
    shape.sf <- st_read(dsn_shape, layer = "tracks", quiet = TRUE) %>%
      st_transform(2154)
    mf_map(x = shape.sf, col = "green", lwd = 3, add = TRUE)
    inters <- st_intersection(st_buffer(shape.sf, 10), osm.sf)
    mf_map( x = inters, add = TRUE, col = "grey", lwd = 3)
    shape_points.sf <- st_sf(st_cast(st_geometry(shape.sf), "POINT"))
    mf_annotation(shape_points.sf[1, ], txt = "Départ", col_txt = "green", col_arrow = "green")
    point1_distance <- st_distance(osm_points.sf[1, ], shape_points.sf[1, ], by_element = TRUE)
    point9_distance <- st_distance(last(osm_points.sf), last(shape_points.sf), by_element = TRUE)
  }
  mf_title(sprintf("r%s %s", id, shape), inner = TRUE)
  mf_legend_t(title = "", val = c("osm", "gtfs", "inter"), pal = c("blue", "green", "grey"))
  rc <- list(
    "id" = id,
    "shape" = shape,
    "point1_distance" = point1_distance,
    "point9_distance" = point9_distance,
    "osm_lg" = st_length(osm.sf),
    "shape_lg" = st_length(shape.sf),
    "inters_lg" = st_length(inters)
  )
  return(invisible(rc))
}
#
# la carte de la ligne avec les stops en provenance d'osm
# l'id de la relation et l'identification du shape
# source("geo/scripts/transport.R");carto_route_shape_stops_mapsf(id = "10554878", shape = "Din1R1AL10")
carto_route_shape_stops_mapsf <- function(id, shape, force = TRUE, force_osm = TRUE) {
  library(tidyverse)
  library(sf)
  library(mapsf)
  carp("id: %s shape: %s", id, shape)
  rc <- osmapi_get_transport(ref = id, force = force, force_osm = force_osm)

  if (st_is_empty(rc$ways.sf[1,]) ) {
    carp("***id: %s", id)
    return(invisible(rc))
  }
  osm.sf <- rc$ways.sf %>%
    st_transform(2154)
  mf_init(osm.sf, expandBB = c(0.1, 0.1, 0.1, 0.1))
  mf_map(x = osm.sf, col = "blue", lwd = 3, add = TRUE)
  points.sf <- st_sf(st_cast(st_geometry(osm.sf), "POINT"))
  mf_annotation(points.sf[1, ], txt = "Départ", col_txt = "blue", col_arrow = "blue")
  platforms.df <- rc$members.df %>%
    filter(grepl("platform", role)) %>%
    mutate(Id = row_number()) %>%
    mutate(Name = sprintf("%s-%s", Id, name)) %>%
    mutate(lat = as.numeric(lat)) %>%
    mutate(lon = as.numeric(lon))
  platforms.sf <- st_as_sf(platforms.df, coords = c("lon", "lat"), crs = 4326, remove = TRUE) %>%
    st_transform(2154) %>%
    glimpse()
  mf_label(x = platforms.sf,
    var = "Name",
    cex = 0.8,
    overlap = FALSE,
    lines = TRUE
  )
  dsn_shape <- sprintf("%s/shape_%s.gpx", josmDir, shape)
  if (! file.exists(dsn_shape)) {
    shape <- sprintf("%s absent", shape)
  } else {
    shape.sf <- st_read(dsn_shape, layer = "tracks", quiet = TRUE) %>%
      st_transform(2154)
    mf_map(x = shape.sf, col = "green", lwd = 3, add = TRUE)
    inters <- st_intersection(st_buffer(shape.sf, 10), osm.sf)
    mf_map( x = inters, add = TRUE, col = "grey", lwd = 3)
    points.sf <- st_sf(st_cast(st_geometry(shape.sf), "POINT"))
    mf_annotation(points.sf[1, ], txt = "Départ", col_txt = "green", col_arrow = "green")
  }
  mf_title(sprintf("r%s %s", id, shape))
}