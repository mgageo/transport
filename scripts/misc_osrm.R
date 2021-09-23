# <!-- coding: utf-8 -->
# le réseau de bus
# utilisation des données d'OpenStreetMap pour le routage
# auteur : Marc Gauthier
#
# https://blog.ippon.fr/2018/04/03/osrm-pour-tracer-la-route/
# https://github.com/ITSLeeds/osmextract
# https://gis.stackexchange.com/questions/244090/query-for-way-that-connects-two-intersections
# https://stateofther.github.io/finistR2019/d-mypkg.html
#
# https://usethis.r-lib.org/articles/articles/usethis-setup.html
# https://cran.r-project.org/web/packages/taxize/index.html
#
# sur ume machine virtuelle ubuntu 18.04, serveur osrm dans le compte marc
# @IP : 192.168.0.2
#
osrm_install <- function() {
  remotes::install_github("ITSLeeds/osmextract")
}

#
# source("geo/scripts/transport.R");config_xls('tub');ways <- osrm_jour(ref = "11920355", shape = "12629")
osrm_jour <- function(ref = "11920349", type = "relation", shape = "12844", force = TRUE) {
  carp()
  library(tidyverse)
  library(stringr)
  library(httr)
#
# le tracé du parcours, version shape du gtfs
  polyline1 <- osrm_gpx_polyline(shape)
  sfc1 <- osrm_polyline_sfc(polyline1)
  plot(sfc1, lwd = 4, col = "black")
#
# par interrogation de la relation avec l'api, détermination des platforms
  if ( 1 == 2 ) {
    polyline2 <- osrm_platforms_polyline(ref = ref, force = force)
    sfc2 <- osrm_polyline_sfc(polyline2)
    plot(sfc2, add = TRUE, col = "blue", lwd = 2)
  }
#
# par interrogation de la relation avec l'overpass, détermination des platforms
  if ( 1 == 2 ) {
    polyline3 <- osmdata_get_objet_polyline(ref = ref)
    sfc3 <- osrm_polyline_sfc(polyline3)
    plot(sfc3, add = TRUE, col = "green", lwd = 2)
  }
#  return()
#
# avec les données du shape
  ways.df <- osrm_polyline_ways(ref = shape, type = "shape", polyline = polyline1, force = force)
  glimpse(ways.df)
  res <- osrm_get_json(ref = shape, type = "shape", service = "route", polyline = polyline1, force = TRUE)
  polyline4 <- res$routes[[1]]$geometry
  sfc4 <- osrm_polyline_sfc(polyline4)
  plot(sfc4, add = TRUE, col = "red", lwd = 2)
}
#
# détermination des ways suite au routage
osrm_polyline_ways <- function(ref = "12627", type = "shape", polyline, force = FALSE) {
  carp()
  df <- osrm_get_legs_nodes_ways(ref = ref, type = type, polyline = polyline, force = force)
  if ( class(df) == "logical") {
    return(invisible(FALSE))
  }
  df <- df %>%
    mutate(ways = str_sub(ways, 2)) %>%
    filter(! is.na(ways))
  way_prec <- ""
  ways.df <- data.frame(way=character())
  k <- 0
  carp("pour n'avoir qu'une fois la way")
  for (i in 1:(nrow(df)-1)) {
    ways1 <- df[i, "ways"]
    ways2 <- df[i + 1, "ways"]
    carp("%s : %s # %s", i, ways1, ways2)
    ways1.list <<- str_split(ways1, ",")
    ways2.list <<- str_split(ways2, ",")
    way.list <<- intersect(ways1.list[[1]], ways2.list[[1]])
    if(length(way.list) == 0) {
      next;
    }
    way <<- way.list[[1]]
    if ( way == way_prec) {
      next
    }
#    carp("%s", way)
    way_prec <- way
    if (!way %in% ways.df$way) {
      k <- k + 1
      ways.df[k , ] <- c(way)
    }
  }
  ways.df <- ways.df %>%
    mutate(way = sprintf("  wy %s", way))
  dsn <- sprintf("%s/%s_%s_level0.txt", level0Dir, type, ref)
  carp(dsn)
  write_lines(ways.df$way, file = dsn, append = FALSE)
  res <- osrm_get_json(ref = ref, type = type, polyline = polyline, force = FALSE)
  return(invisible(res))
}
#
# transformation du gpx en polyline (codage Google)
# le gpx a été produit à partir du shape, scripts/transport_txt.R
# https://cran.r-project.org/web/packages/googlePolylines/vignettes/sfencode.html
osrm_gpx_polyline <- function(ref = "12627") {
  carp()
  library(sf)
  library(googlePolylines)
  dsn <- sprintf("%s/JOSM/shape_%s.gpx", transportDir, ref)
  carp(dsn)
  print(st_layers(dsn))
#  nc <- st_read(dsn, layer = "route_points") %>%
  nc <- st_read(dsn, layer = "tracks") %>%
    st_transform(2154) %>%
    st_simplify(dTolerance = 100) %>%
    st_transform(4326)
  carp("nrow: %s", nrow(nc))
  df <- as.data.frame(st_coordinates(nc))
  colnames(df) <- c("lon", "lat")
  enc <- encode(df)
#  print(str(enc))
#  stop("***")
  return(invisible(enc))
}
#
# transformation des platforms en polyline (codage Google)
#
# source("geo/scripts/transport.R");osrm_platforms_polyline()
osrm_platforms_polyline <- function(ref = "11920346", force = "TRUE") {
  carp()
  library(sf)
  library(googlePolylines)
  df <- osmapi_get_members_platform(ref = ref, force = force) %>%
    dplyr::select(lon, lat)
  enc <- encode(df)
  return(invisible(enc))
}
#
# https://stackoverflow.com/questions/57376373/how-to-create-spatial-line-dataframe-from-encoded-polylines
#
# source("geo/scripts/transport.R");osrm_polyline_sfc()
osrm_polyline_sfc <- function(
    polyline = "_gzfH||rPKWgC_HaBuEGc@?g@?ST@XBb@?|CIxAEhB[b@Ol@?|@HL??LPlCJj@B^",
    force = FALSE
  ) {
  carp()
  library(sf)
  library(googlePolylines)
  coords <- googlePolylines::decode(polyline)
  sfg <- lapply( coords, function(x) sf::st_linestring( x = as.matrix(x) ) )
  sfc <- sf::st_sfc( sfg )
#  sf <- sf::st_as_sf( cbind( Test_TSD, sfc ) )
  return(invisible(sfc))
}
#
# source("geo/scripts/transport.R");df <- osrm_get_legs_nodes_ways()
osrm_get_legs_nodes_ways <- function(ref = "12627", type = "shape", polyline = "_gzfH||rPKWgC_HaBuEGc@?g@?ST@XBb@?|CIxAEhB[b@Ol@?|@HL??LPlCJj@B^", force = FALSE) {
  carp()
  library(httr)
  library(tidyverse)
  library(rjson)
  dsn <- sprintf("%s/%s_%s.Rds", osrmDir, type, ref)
  if (file.exists(dsn) && force == FALSE) {
    df <- readRDS(dsn)
    return(invisible(FALSE))
  }
  res <- osrm_get_json(ref = ref, type = type, polyline = polyline, force = force)
  if ( class(res) == "logical") {
    return(invisible(FALSE))
  }
  carp("les nodes des legs")
  df0 <- osrm_get_legs_nodes(res) %>%
    glimpse()
  carp("pour avoir les nodes une seule fois")
  df1 <- df0 %>%
    distinct(node) %>%
    arrange(node) %>%
    glimpse()
  k <- 0
  carp("recherche des ways des nodes par interrogation de l'api")
  df <- data.frame(node=character(), ways=character())
  for (i in 1:nrow(df1)) {
    node <- df1[[i, "node"]]
    res <- osrm_get_node_ways(node)
    elements <- res$elements
    if(length(elements) < 1) {
      next
    }
    ways <- ""
    for (j in 1:length(elements)) {
      element <<- elements[[j]]
      if ( is.null(element$tags) ) {
#        stop("***** tags");
        next;
      }
      if ( is.null(element$tags$highway) ) {
 #       stop("***** highway");
        next;
      }
      ways <- sprintf("%s,%s", ways, elements[[j]]$id)
    }
    k <- k +1
    df[k , ] <- c(node, ways)
  }
  df <- df0 %>%
    left_join(df, by = c("node")) %>%
    glimpse()
  saveRDS(df, file = dsn)
  return(invisible(df))
}
#
# source("geo/scripts/transport.R");osrm_get_nodes_ways()
osrm_get_nodes_ways <- function(
    ref = "12627",
    type = "shape",
    polyline = "_gzfH||rPKWgC_HaBuEGc@?g@?ST@XBb@?|CIxAEhB[b@Ol@?|@HL??LPlCJj@B^",
    force = TRUE) {
  carp()
  res <- osrm_get_json(ref = ref, type = type, polyline = polyline, force = force)
  df1 <- osrm_get_legs_nodes(res) %>%
    glimpse()
  nodes <- paste(df1$node, collapse = ',')
  query <- sprintf("node(id:%s);
(way(bn);node(w););
out;", nodes)
#  print(query); stop("***")
  od <- osmdata_query(query, "test", force = force) %>%
    glimpse()
  return(invisible(od$osm_lines))
}
#
# source("geo/scripts/transport.R");res <- osrm_get_json()
#
# services possibles : route, match
osrm_get_json <- function(
    service = "match",
    ref = "12627",
    type = "shape",
    polyline = "_gzfH||rPKWgC_HaBuEGc@?g@?ST@XBb@?|CIxAEhB[b@Ol@?|@HL??LPlCJj@B^",
    force = FALSE) {
  library(httr)
  library(tidyverse)
  library(rjson)
  dsn <- sprintf("%s/%s_%s.json", osrmDir, type, ref)
  carp("dsn: %s", dsn)
  if (! file.exists(dsn) || force == TRUE) {
    url <- sprintf("http://192.168.0.2:5000/%s/v1/driving", service)
    if (service == "route") {
      query <- sprintf("%s/polyline(%s)?overview=full&annotations=nodes", url, polyline)
    }
    if (service == "match") {
      query <- sprintf("%s/polyline(%s)?overview=full&annotations=true", url, polyline)
    }
    carp("query: %s", query)
    res <- httr::GET(url = query, encoding = "UTF-8", type = "application/json", verbose(), httr::write_disk(dsn, overwrite = TRUE))
    if ( http_error(res) ) {
      print(http_status(res))
      return(invisible(FALSE))
    }
  }
  res <- rjson::fromJSON(file = dsn)
  return(invisible(res))
}
#
# source("geo/scripts/transport.R");nodes.df <- osrm_get_legs_nodes(res)
osrm_get_legs_nodes <- function(res) {
  df <- data.frame(node=character(), leg=double())
  legs <- res$routes[[1]]$legs
  if (length(legs) == 0) {
    legs <- res$matchings[[1]]$legs
  }
  if (length(legs) == 0) {
    confess("length legs")
  }
  k <- 0
  for (i in 1:length(legs)) {
    carp("i: %s", i)
    nodes <- legs[[i]]$annotation$nodes
    for (j in 1:length(nodes)) {
#      carp("j: %s", j)
      node <- as.character(nodes[[j]])
      l <- as.character(i)
      k <- k +1
      df[k , ] <- c(node, i)
    }
  }
  return(invisible(df))
}




osrm_extract <- function() {
  library(osmextract)
  osm_lines <- oe_get("Bretagne", stringsAsFactors = FALSE)
  ht <- c("primary", "secondary", "residential", "tertiary")
  osm_major_roads <- osm_lines[osm_lines$highway %in% ht, ]
  plot(osm_major_roads["highway"], key.pos = 1)
}
#
# récupération des ways via l'api
# source("geo/scripts/transport.R");res <- osrm_get_node_ways(1111578020)
osrm_get_node_ways <- function(node, force = FALSE) {
  library(httr)
  library(tidyverse)
  library(rjson)
  query <- sprintf("https://www.openstreetmap.org/api/0.6/node/%s/ways.json", node)
  res <- content(GET(query), as = "parsed")
#  res <- rjson::fromJSON(file = query)
  return(invisible(res))
}
#
## routage d'une relation à partir de ses stops
#
osrm_relation_stops <- function(ref, force = TRUE) {
  carp()
  library(tidyverse)
  library(stringr)
  library(httr)
  library(sf)
  library(sp)
  library(rgdal)
  library(rgeos)
#
# par interrogation de la relation avec l'api, détermination des platforms
  polyline1 <- osrm_platforms_polyline(ref = ref, force = force)
  sfc1 <- osrm_polyline_sfc(polyline1)
  plot(sfc1, add = FALSE, col = "blue", lwd = 2)
  res <- osrm_get_json(ref = ref, type = "relation", service = "route", polyline = polyline1, force = TRUE)
  polyline4 <- res$routes[[1]]$geometry
  sfc4 <- osrm_polyline_sfc(polyline4)
  plot(sfc4, add = TRUE, col = "red", lwd = 2)
  ways.df <- osrm_polyline_ways(ref = ref, type = "relation", polyline = polyline1, force = force)
}
#
## le serveur osrm
#
# source("geo/scripts/transport.R");osrm_ssh_session()
osrm_ssh_session <- function() {
  library(ssh)
  a <- sprintf('%s@%s', "marc", '192.168.0.2')
  carp('a: %s', a)
  if ( exists('session')) {
    oe <- try(ssh_session_info(session)) %>%
      glimpse()
  }
  if ( ! exists('session') || inherits(oe, "try-error")) {
    session <<- ssh_connect(a, passwd = "tgbtgb")
    print(session)
  }
  out <- ssh_exec_internal(session, "source ~/osrm.sh; _osrm_bzh")
}