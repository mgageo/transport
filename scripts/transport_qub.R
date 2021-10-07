# <!-- coding: utf-8 -->
# le réseau de bus de Quimper
# utilisation des données opendata
# auteur : Marc Gauthier
#
# source("geo/scripts/transport.R");qub_jour()
qub_jour <- function() {
  carp()
  config_xls('qub');
  mobibreizh_gtfs_reseau(config[1, "reseau"], config[1, "agency_id"])
}
# source("geo/scripts/transport.R");qub_stops_diff()
qub_stops_diff <- function() {
  library(tidyverse)
  library(rio)
  carp()
  config_xls('qub');
#  agency.df <- mobibreizh_agency_lire('qub')
  mobi.df <- gtfs_stops_verif() %>%
#    filter(grepl('^axeo', stop_id)) %>%
    glimpse()
  mobi.sf <- st_as_sf(mobi.df, coords=c("lon", "lat"), crs = 4326) %>%
    dplyr::select(stop_name) %>%
    st_transform(2154)
  osm.sf <- qub_osm_busstop_sf() %>%
#    filter(is.na(name)) %>%
    dplyr::select(name, osm_id) %>%
    st_transform(2154)
  df <- qub_proches(osm.sf, mobi.sf) %>%
    dplyr::select(-geometry.x, -geometry.y) %>%
    dplyr::select(osm_id, stop_name) %>%
    glimpse()
  dsn <- sprintf("%s/stops_diff.csv", cfgDir);
  export(df, dsn);
  carp('dsn: %s', dsn)
  carp('distance des arrêts mobi')
  df <- qub_proches(mobi.sf, osm.sf) %>%
    dplyr::select(-geometry.x, -geometry.y) %>%
    glimpse()
  View(df)
}
#
# qub_proche
qub_proches <- function(nc1, nc2) {
  library(sf)
  library(tidyverse)
  carp('nc1: %d', nrow(nc1))
  nc2$no <- 1:nrow(nc2)
  nc1$proche <- st_nearest_feature(nc1, nc2)
  df <- dplyr::left_join(nc1 %>% as.data.frame(), nc2 %>% as.data.frame(), by=c('proche'='no')) %>%
    glimpse()
#  stop('***')
  df$d <- st_distance(df$geometry.x, df$geometry.y, by_element = TRUE)
  units(df$d) <- 'm'
  return(invisible(df))
}
# source("geo/scripts/transport.R");qub_osm_busstop_sf()
qub_osm_busstop_sf <- function(force = FALSE) {
  carp()
  dsn <- 'network_busstop'
  requete <- sprintf("relation[network='%s']->.a;node(r.a);out meta;", config[1, "network"])
  dsn <- overpass_query(query = requete, fic = dsn, force = force)
  nc <- oapi_sf_lire(fic = dsn)
  return(invisible(nc))
}
qub_osm_get_xml <- function(force = FALSE) {
  carp()
  dsn <- 'qub_busstop.osm'
  requete <- "relation[network='qub']->.a;node(r.a);out meta;"
  oapi_requete_get(requete=requete, fic=dsn, force = force)
  od <- oapi_osmdata_lire_xml(fic=dsn) %>%
    glimpse()
}
# source("geo/scripts/transport.R");qub_nodes_busstop_valid()
qub_nodes_busstop_valid <- function(force = TRUE) {
  library(tidyverse)
  library(rio)
  carp()
  config_xls('qub');
  dsn <- 'network_nodes_busstop_valid'
  requete <- sprintf("relation[network='%s']->.a;node[!'public_transport'](r.a)->.b;(.b;way(bn.b););out meta;", config[1, "network"])
  dsn <- overpass_query(query = requete, fic = dsn, force = force)
  nc <- oapi_sf_lire(fic = dsn)
  return(invisible(nc))
}
#
# source("geo/scripts/transport.R");config_xls('qub');qub_stops_diff()
qub_stops_diff <- function(force = FALSE) {
  library(tidyverse)
  library(janitor)
  gtfs.sf <- txt_gtfs_stops_sf(force = force) %>%
    filter(location_type == 0) %>%
    glimpse()
#  gtfs2osm_stops_create(gtfs.sf);return()
  gtfs.df <- gtfs.sf %>%
    st_drop_geometry()
  osm.sf <- osm_area_busstop_get(force = force)
  k_ref <- config[1, "k_ref"]
  carp("k_ref: %s", k_ref)
  if (k_ref %in% names(osm.sf)) {
    osm.sf <- osm.sf %>%
    dplyr::select(ref = k_ref, name, osm_id) %>%
    mutate(stop_id = as.numeric(str_extract(ref, "\\d+")))
  } else {
    carp("pas de bus_stop")
    osm.sf$ref <- ""
    osm.sf$stop_id <- ""
  }
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
#
## la partie routage avec osrm
#
# source("geo/scripts/transport.R");dsn <- qub_relations_route_get()
#
qub_relations_route_get <- function(force = TRUE) {
  library(tidyverse)
  library(rio)
  library(xml2)
  carp()
  config_xls('qub')
  fic <- 'relations_route'
  requete <- sprintf("relation[network='%s'][type=route];
out meta;", Config[1, "network"])
  dsn <- overpass_query(query = requete, fic = fic, force = force)
  doc <- xml2::read_xml(dsn)
  has_relations <- osm_has_xpath(doc, "//relation")
  if (! has_relations) {
    stop("***")
  }
  relations <- xml2::xml_find_all(doc, "//relation")
  carp("relations nb: %s", length(relations))
  html_entete <- '<html>
<head>
  <meta charset="UTF-8">
  <title>transport QUB</title>
</head>
<body>'
  html_pied <- '
  </body>
</html>'
  html <- html_entete
  for (relation in relations) {
    id <- xml_attr(relation, "id")[[1]]
    if ( id != "12307588") {
#      next;
    }
    carp("id: %s", id)
    href <- sprintf("<br/><b>%s</b>", id)
    html <- append(html, href)
    tag_ref <<-  xml2::xml_attr(xml2::xml_find_first(relation, './/tag[@k="ref"]'), "v")
    tag_ref_network <<-  xml2::xml_attr(xml2::xml_find_first(relation, './/tag[@k="ref:network"]'), "v")
    tags <- sprintf(" %s %s", tag_ref, tag_ref_network)
    html <- append(html, tags)
    members_way <<-  xml2::xml_find_all(relation, './/member[@type="way"]')
    members_node <<-  xml2::xml_find_all(relation, './/member[@type="node"]')
    href <- sprintf("members_way: %s", length(members_way))
    html <- append(html, href)
    href <- sprintf(" members_node: %s", length(members_node))
    html <- append(html, href)
#    stop("****")
#    id <- xml_attr(relation, "id")
    carp("id : %s ways: %s nodes: %s", id, length(members_way), length(members_node))
    if (length(members_way) != 0) {
      carp("id: %s ways", id)
      next;
    }
    if (length(members_node) < 2) {
      carp("id: %s nodes", id)
      next;
    }
    type <- "relation"
    dsn <- qub_osrm(id, force = force)
    if ( ! file.exists(dsn) || force == TRUE) {
#      next
    }
#    stop("****")
    href <- sprintf("<a href='http://localhost:8111/import?url=https://api.openstreetmap.org/api/0.6/relation/%s/full'>josm</a>", id)
    html <- append(html, href)
    href <- sprintf("<a href='http://level0.osmz.ru/?url=relation/%s'>level0</a>", id)
    html <- append(html, href)
    href <- sprintf("<a href='http://bv/transport/qub/level0/relation_%s_level0.txt'>osrm</a>", id)
    html <- append(html, href)
  }
  html <- append(html, html_pied)
  dest <- "qub.html"
  dsn <- sprintf("%s/%s", webDir, dest)
  write(html, dsn)
  carp("dsn: %s", dsn)
  return()
}
#
# pour une ref
qub_osrm <- function(ref, force = TRUE) {
  library(clipr)
  carp()
  config_xls('qub');
  carp("josmDir: %s", josmDir)
  carp("level0Dir: %s", level0Dir)
#  osrm_relation_stops(12307545, force = force)
#  ref <- 12307555
#  ref <- 12307567 ; # 13-B
  type <- "relation"
  dsn <- sprintf("%s/%s_%s_level0.txt", level0Dir, type, ref)
  if ( ! file.exists(dsn) || force == TRUE) {
    osrm_relation_stops(ref, force = force)
  }
  return(dsn)
  carp(dsn)
  level0 <- read_lines(file = dsn)
  write_clip(level0)
}