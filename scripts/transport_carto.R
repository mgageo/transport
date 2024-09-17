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
# cartographie d'une route avec le tracé shape et les stops
# - calcul de la distance des stops au tracé
# - vérification de l'emplacement des stops : côté droit du tracé
carto_route_shape_stops <- function(df, nc1) {
  library(tidyverse)
  library(sf)
  library(mapsf)
  shape_id <- df[[1, "shape_id"]]
  carp("*****shape_id: %s", shape_id)
  rc <- as.list(df)
  if ( shape_id != "0007-A-2281-2337") {
#    return(invisible(rc))
  }
  shape <- df[[1, "shape"]]
  dsn_shape <- sprintf("%s/shape_%s.gpx", josmDir, shape)
  if (! file.exists(dsn_shape)) {
    glimpse(df[1, ])
    carp("*** dsn_shape: %s", dsn_shape)
    return(invisible(FALSE))
  }
  shape.sf <- st_read(dsn_shape, layer = "tracks", quiet = TRUE) %>%
    st_transform(2154)
  mf_init(shape.sf, expandBB = c(1500, 1500, 1500, 1500))
  plot(st_geometry(shape.sf), add = FALSE, col = "green", lwd = 3)
  shape_points.sf <- st_sf(st_cast(st_geometry(shape.sf), "POINT"))
  mf_annotation(shape_points.sf[1, ], txt = "Départ", col_txt = "green", col_arrow = "green")
  nc2 <- nc1 %>%
    filter(shape_id == !!shape_id) %>%
    mutate(Name = sprintf("%s %s", id, stop_name))
  plot(st_geometry(nc2), col = "green", lwd = 3, pch = 19, add = TRUE)
  mf_label(x = nc2,
    var = "Name",
    cex = 0.8,
    col = "black",
    overlap = FALSE,
    lines = TRUE
  )
  df2 <- st_distance(nc2, shape.sf)
  nc2 <- cbind(nc2, df2) %>%
    mutate(distance = as.integer(df2)) %>%
    dplyr::select(-df2)
  point_dernier <- nrow(nc2)
  loins.sf <- nc2 %>%
    filter(distance > 50)
  carp("les points trop loin: %s", nrow(loins.sf))
  if (nrow(loins.sf) > 0) {
    plot(st_geometry(loins.sf), col = "red", lwd = 3, pch = 19, add = TRUE)
    mf_label(x = loins.sf,
      var = "Name",
      cex = 0.8
    )
  }
  carp("les points sur le tracé")
#    stop("*****")
  dessus.sf <- nc2 %>%
    filter(distance == 50)
  if (nrow(dessus.sf) > 0) {
    plot(st_geometry(dessus.sf), col = "pink", lwd = 3, pch = 19, add = TRUE)
    mf_label(x = dessus.sf,
      var = "Name",
      cex = 0.8,
      overlap = FALSE,
      lines = TRUE
    )
  }
# bug sf
# si la ligne repasse au même endroit
  cote_droit.sf <- shape.sf %>%
    st_buffer(-50, singleSide = TRUE, bOnlyEdges = FALSE) %>%
    st_buffer(0)
  cote_gauche.sf <- shape.sf %>%
    st_buffer(50, singleSide = TRUE, bOnlyEdges = FALSE) %>%
    st_buffer(0)
  if ( shape_id == "0007-A-2281-2337") {
    dsn <- sprintf("%s/cote_droit_shape.geojson", josmDir)
    st_write(st_transform(shape.sf, 4326), dsn, delete_dsn = TRUE)
    carp("dsn: %s", dsn)
    dsn <- sprintf("%s/cote_droit_buffer.geojson", josmDir)
    st_write(st_transform(cote_droit.sf, 4326), dsn, delete_dsn = TRUE)
    carp("dsn: %s", dsn)
#    stop("mmmmmmmmmmmmmmm")
  }
  carp("les points proches")
  points.sf <<- nc2 %>%
    filter(distance < 50)
  carp("les points dans le buffer")
# pas dans le buffer côté droit
  points.sf$within <- st_within(points.sf, cote_droit.sf, sparse = F)
  points_out.sf <- points.sf %>%
    filter(within == FALSE) %>%
    filter(id != 1) %>%
    filter(id != point_dernier)
# dans le buffer côte gauche
  points.sf$within <- st_within(points.sf, cote_gauche.sf, sparse = F)
  points_out.sf <- points.sf %>%
    filter(within == TRUE) %>%
    filter(id != 1) %>%
    filter(id != point_dernier)
  plot(st_geometry(points_out.sf), col = "orange", lwd = 3, pch = 19, add = TRUE)
  if (nrow(points_out.sf) > 0) {
#    stop("*****")
  }
  titre <- sprintf("%s", shape_id)
  title(titre)
  rc[["points_distance.df"]] <- list(st_drop_geometry(nc2))
  rc[["points_cote.df"]] <- list(st_drop_geometry(points_out.sf))
  rc[["shape_lg"]] <- st_length(shape.sf)
  rc[["points_out"]] <- nrow(points_out.sf)
  return(invisible(rc))
}
#
# pour savoir si les stations sont du bon côté
# https://github.com/r-spatial/sf/issues/1001
#
# cartographie d'une route avec le tracé à partir des données osm (ways et platforms)
# - calcul de la distance des platforms au tracé
# - vérification de l'emplacement des platforms : côté droit du tracé
# source("geo/scripts/transport.R");carto_route_shape_stops_cote(id = 4259837)
carto_route_shape_stops_cote <- function(id, force = TRUE) {
  library(tidyverse)
  library(mapsf)
  rc <- tidytransit_lire(sprintf("osm_relation_route_gap_%s", id)) %>%
    glimpse()
  ways.sf <- rc$ways.sf %>%
    st_transform(2154)
  trace.sf <- rc$trace.sf %>%
    st_transform(2154)
  mf_init(ways.sf, expandBB = c(200, 200, 200, 200))
  plot(st_geometry(ways.sf), add = FALSE, col = "green", lwd = 3)
  ways_points.sf <- st_sf(st_cast(st_geometry(ways.sf), "POINT"))
  mf_annotation(ways_points.sf[1, ], txt = "Départ", col_txt = "green", col_arrow = "green")
  platforms.sf <- rc$members.sf %>%
    filter(grepl("platform", public_transport))
  df2 <- st_distance(platforms.sf, trace.sf) %>%
    glimpse()
  platforms.sf <- cbind(platforms.sf, df2) %>%
    mutate(distance = as.numeric(round(df2, 1))) %>%
    dplyr::select(-df2) %>%
    glimpse()
  carp("les points trop loin")
  loins.sf <- platforms.sf %>%
    filter(distance > 50)
  if (nrow(loins.sf) > 0) {
    plot(st_geometry(loins.sf), col = "red", lwd = 3, pch = 19, add = TRUE)
    mf_label(
      x = loins.sf,
      var = "Name",
      cex = 0.8,
      col = "blue",
      overlap = FALSE,
      lines = TRUE
    )
  }
  carp("les points trop proches")
  proches.sf <- platforms.sf %>%
    filter(distance == 0)
  if (nrow(proches.sf) > 0) {
    plot(st_geometry(proches.sf), col = "orange", lwd = 3, pch = 19, add = TRUE)
    mf_label(
      x = proches.sf,
      var = "Name",
      cex = 0.8,
      col = "orange",
      overlap = FALSE,
      lines = TRUE
    )
  }
# buffer de 50 mètres du côté droit
  cote_droit.sf <- trace.sf %>%
    st_buffer(-50, singleSide = TRUE, bOnlyEdges = FALSE) %>%
    st_buffer(0)
  platforms.sf$within <- as.logical(st_within(platforms.sf, cote_droit.sf, sparse = F))
  platforms_out.sf <- platforms.sf %>%
    filter(within == FALSE) %>%
    filter(id != 1)
  if (nrow(platforms_out.sf) > 0) {
    plot(st_geometry(platforms_out.sf), col = "red", lwd = 3, pch = 19, add = TRUE)
    mf_label(
      x = platforms_out.sf,
      var = "Name",
      cex = 0.8,
      col = "red",
      overlap = FALSE,
      lines = TRUE
    )
  }
  rc$loins.sf <- loins.sf
  rc$proches.sf <- proches.sf
  rc$platforms_out.sf <- platforms_out.sf
  titre <- sprintf("%s", id)
  title(titre)
  legend("topleft",
    , legend=c(">50m", "0m", "côté")
    , col=c("blue", "orange", "red")
    , lwd = 3, lty = 1, cex = 0.8, box.lty = 0
  )
  return(invisible(rc))
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
# source("geo/scripts/transport.R");carto_route_shape(id = "7711144", shape = "0122-B-1065-1090")
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
    st_transform(2154) %>%
    mutate(id = n() - row_number() + 2)
  bouts.sf <- bind_rows(departs.df, arrivees.df) %>%
    st_as_sf(sf_column_name = "geometry") %>%
    st_transform(2154) %>%
    glimpse()
  mf_init(shapes.sf, expandBB = c(0.1, 0.1, 0.1, 0.1))
  pal <- hcl.colors(n = nrow(shapes.sf), palette = "Cividis")
  mf_map(x = shapes.sf, col = pal, lwd = shapes.sf$id, add = TRUE)
  mf_legend(type = 'typo', title = "", val = shapes.sf$shape_id, pal = pal)
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
# source("geo/scripts/transport.R");reseau_osm_routes_shapes(force = FALSE)
# source("geo/scripts/transport.R");carto_route_shape_mapsf(id = "10554878", shape = "Din1R1AL10")
carto_route_shape_mapsf <- function(id, shape, force = TRUE, force_osm = TRUE) {
  library(tidyverse)
  library(sf)
  library(mapsf)
  carp("id: %s shape: %s", id, shape)
  osm.sf <- osmapi_get_ways(ref = id, force = force, force_osm = force_osm)
#  glimpse(osm.sf);confess();
  if (nrow(osm.sf) < 1) {
    carp("***pas de ways *** id: %s", id)
    return(invisible(FALSE))
  }
  point1_distance <- -1
  osm.sf <- osm.sf %>%
    st_transform(2154)
  mf_init(osm.sf, expandBB = c(0.1, 0.1, 0.1, 0.1))
  mf_map(x = osm.sf, col = "blue", lwd = 3, add = TRUE)
  osm_points.sf <- st_sf(st_cast(st_geometry(osm.sf), "POINT"))
  mf_annotation(osm_points.sf[1, ], txt = "Départ", col_txt = "blue", col_arrow = "blue")
  mf_title(sprintf("r%s %s", id, shape), inner = TRUE)
  mf_legend(type = "typo", title = "", val = c("osm", "gtfs", "inter"), pal = c("blue", "green", "grey"))

  dsn_shape <- sprintf("%s/shape_stops_%s.gpx", josmDir, transport_shape2fic(shape))
  if (! file.exists(dsn_shape)) {
#    confess("dsn_shape: %s", dsn_shape)
    shape <- sprintf("%s absent", shape)
    rc <- list(
      "id" = id,
      "ref" = osm.sf[[1, "ref"]],
      "ref_network" = osm.sf[[1, "ref_network"]],
      "shape" = shape,
      "osm_lg" = st_length(osm.sf)
    )
  } else {
    shape.sf <- st_read(dsn_shape, layer = "tracks", quiet = TRUE) %>%
      st_transform(2154)
    mf_map(x = shape.sf, col = "green", lwd = 3, add = TRUE)
    inters <- st_intersection(st_buffer(shape.sf, 10), osm.sf)
    mf_map( x = inters, add = TRUE, col = "grey", lwd = 3)
    shape_points.sf <- st_sf(st_cast(st_geometry(shape.sf), "POINT"))
    mf_annotation(shape_points.sf[1, ], txt = "Départ", col_txt = "green", col_arrow = "green")
    point1_distance <- st_distance(osm_points.sf[1, ], shape_points.sf[1, ], by_element = TRUE)
    point19_distance <- st_distance(osm_points.sf[1, ], last(shape_points.sf), by_element = TRUE)
    point9_distance <- st_distance(last(osm_points.sf), last(shape_points.sf), by_element = TRUE)
    point91_distance <- st_distance(last(osm_points.sf), shape_points.sf[1, ], by_element = TRUE)
    rc <- list(
      "id" = id,
      "ref" = osm.sf[[1, "ref"]],
      "ref_network" = osm.sf[[1, "ref_network"]],
      "shape" = shape,
      "point1_distance" = point1_distance,
      "point9_distance" = point9_distance,
      "point19_distance" = point19_distance,
      "point91_distance" = point91_distance,
      "osm_lg" = st_length(osm.sf),
      "shape_lg" = st_length(shape.sf),
      "inters_lg" = st_length(inters)
    )
  }
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
#
# la carte de la ligne
# https://rgeomatic.hypotheses.org/2077
# https://riatelab.github.io/mapsf/
carto_route_members_mapsf <- function(rc) {
  library(tidyverse)
  library(sf)
  library(mapsf)
  carp()
  mf_init(rc$ways.sf, expandBB = c(0.1, 0.1, 0.1, 0.1))
  mf_map(x = rc$ways.sf, col = "black", lwd = 3, add = TRUE)
#  mf_map(x = rc$platforms.sf, col = "blue", lwd = 3, pch = 19, add = TRUE)
  if (nrow(rc$members.sf) > 0) {
    mf_map(x = rc$members.sf, col = rc$members.sf$couleur, lwd = 3, pch = 19, add = TRUE)
    mf_label(x = rc$members.sf,
      var = "Id",
      cex = 1.5,
      overlap = FALSE,
      lines = TRUE,
      col = rc$members.sf$couleur
    )
  }
  if (! "ref:network" %in% names(rc$relation)) {
    rc$relation[1, "ref:network" ] <- NA
  }
  glimpse(rc)
  titre <- sprintf("relation: %s ref_network: %s", rc$relation[1, "@id"], rc$relation[1, "ref:network"])
  carp("titre: %s", titre)
  mf_title(titre)
  mf_annotation(rc$ways_points.sf[1, ], txt = "Départ")
}