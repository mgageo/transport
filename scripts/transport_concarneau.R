# <!-- coding: utf-8 -->
#
# le réseau de bus de Concarneau
#
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
# source("geo/scripts/transport.R");concarneau_jour()
concarneau_jour <- function() {
  library(tidyverse)
  library(rio)
  carp()
  config_xls('concarneau')
}

#
## la partie routage avec osrm
#
# source("geo/scripts/transport.R");dsn <- concarneau_relations_route_get(force = TRUE)
#
concarneau_relations_route_get <- function(network = 'concarneau', force = FALSE) {
  library(tidyverse)
  library(rio)
  library(xml2)
  carp()
  doc <- overpass_relations_route_get_xml(force = force, network = network)
  has_relations <- osm_has_xpath(doc, "//relation")
  if (! has_relations) {
    stop("***")
  }
  relations <- xml2::xml_find_all(doc, "//relation")
  carp("relations nb: %s", length(relations))
  html_entete <- '<html>
<head>
  <meta charset="UTF-8">
  <title>transport %s</title>
</head>
<body>'
  html_pied <- '
  </body>
</html>'
  html <- sprintf(html_entete, network)
  for (relation in relations) {
    id <- xml_attr(relation, "id")[[1]]
    if ( grepl("^11", id)) {
      next;
    }
    carp("id: %s", id)
    href <- sprintf("<br/><b>%s</b>", id)
    html <- append(html, href)
    tag_ref <<-  xml2::xml_attr(xml2::xml_find_first(relation, './/tag[@k="ref"]'), "v")
    tag_ref_network <<-  xml2::xml_attr(xml2::xml_find_first(relation, './/tag[@k="ref:network"]'), "v")
    tag_shape_id <-  xml2::xml_attr(xml2::xml_find_first(relation, './/tag[@k="gtfs:shape_id"]'), "v")
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
#      next;
    }
    if (length(members_node) < 2) {
      carp("id: %s nodes", id)
      next;
    }
    type <- "relation"
    dsn <- concarneau_osrm(id, network = network, force = force)
    if ( ! file.exists(dsn) || force == TRUE) {
#      next
    }
#    stop("****")
    href <- sprintf("<a href='http://localhost:8111/import?url=https://api.openstreetmap.org/api/0.6/relation/%s/full'>josm</a>", id)
    html <- append(html, href)
    href <- sprintf("<a href='http://level0.osmz.ru/?url=relation/%s'>level0</a>", id)
    html <- append(html, href)
    href <- sprintf("<a href='http://localhost/transport/%s/level0/relation_%s_level0.txt'>osrm</a>", network, id)
    html <- append(html, href)
    href <- sprintf("<a href='http://localhost:8111/open_file?filename=D:/web.heb/bv/transport/%s/level0/relation_%s.geojson'>josm geojson</a>", network, id)
    html <- append(html, href)
  }
  html <- append(html, html_pied)
  dest <- sprintf("%s.html", network)
  dsn <- sprintf("%s/%s", webDir, dest)
  write(html, dsn)
  carp("dsn: %s", dsn)
  return()
}
#
# pour une ref
concarneau_osrm <- function(ref, network = 'concarneau', force = TRUE) {
  library(clipr)
  carp("ref: %s", ref)
  config_xls(network);
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
#  stop("****")
  return(dsn)
  carp(dsn)
  level0 <- read_lines(file = dsn)
  write_clip(level0)
}
#
# pour les shapes : la conversion en gpx/geojson
# source("geo/scripts/transport.R");concarneau_gtfs_shapes()
concarneau_gtfs_shapes <- function(tt) {
  carp()
  config_xls('concarneau');txt_gtfs_shapes_sf()
}
#
# pour les stops
# source("geo/scripts/transport.R");concarneau_gtfs_stops()
concarneau_gtfs_stops <- function() {
  carp()
  config_xls('concarneau');txt_gtfs_stops_sf()
}
#
# pour les stops
# source("geo/scripts/transport.R");concarneau_gtfs_stop_times()
concarneau_gtfs_stop_times <- function(force = TRUE) {
  carp()
  config_xls('concarneau');
  stops.df <- txt_gtfs_stops(force = force)

  df1 <- stops.df %>%
    filter(stop_lat == 0)
  if(nrow(df1) > 0) {
    misc_print(df1)
  }
  df2 <- stops.df %>%
    dplyr::select(stop_id) %>%
    mutate(source = "stops") %>%
    filter(! grepl("\\:ST\\:", stop_id)) %>%
    glimpse()
  stop_times.df <- txt_gtfs_stop_times(force = force)
  df3 <- stop_times.df %>%
    distinct(stop_id) %>%
    mutate(source = "stop_times") %>%
    glimpse()
  df4 <- df2 %>%
    full_join(df3, by = c("stop_id")) %>%
    filter(is.na(source.x) | is.na(source.y)) %>%
    glimpse()
}
#
# pour les stops
# source("geo/scripts/transport.R");concarneau_nodes_busstop_valid()
concarneau_nodes_busstop_valid <- function(force = TRUE) {
  carp()
  config_xls('concarneau');
  nc <- overpass_nodes_busstop_network_get(force = force)
  nc1 <- nc %>%
    glimpse() %>%
    filter(is.na(ref.Coralie))
  misc_print(nc1)
}
#
## travail avec tidytransit
#
# setwd("d:/web");source("geo/scripts/transport.R");concarneau_tt_zip()
concarneau_tt_zip <- function(reseau = 'concarneau') {
  carp()
  config_xls(reseau)
  carp("gtfsDir: %s", gtfsDir)
#  misc_zip(dossier = gtfsDir, zipfile = "gtfs.zip", filtre = "*.txt")
  gtfs2mga_gtfs_lire(reseau)
  df <- tidytransit_shapes_stops()
  lignes <- knitr::kable(df, format = "pipe")
  txt <- paste(lignes,  collapse = "\n")
  txt <- sprintf('==les trips==
<pre>
%s
</pre>', txt)
  wiki_connect()
  page <- sprintf("User:Mga_geo/Transports_publics/%s/%s", Config["wiki"], "trips_shapes_stops")
#  wiki_page_init(page = page, article = txt, force = TRUE)
}