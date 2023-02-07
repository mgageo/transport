# <!-- coding: utf-8 -->
# le réseau de bus de Brest
# utilisation des données opendata
# auteur : Marc Gauthier
#
# source("geo/scripts/transport.R");bibus_jour()
bibus_jour <- function() {
  library(tidyverse)
  library(rio)
  carp()
  config_xls('bibus')
# pour les shapes : la conversion en gpx/geojson
  txt_gtfs_shapes_sf()
# pour les stops
  txt_gtfs_stops_sf()
}

#
## la partie routage avec osrm
#
# source("geo/scripts/transport.R");dsn <- bibus_relations_route_get()
#
bibus_relations_route_get <- function(force = TRUE, network = 'bibus') {
  library(tidyverse)
  library(rio)
  library(xml2)
  carp()
  config_xls(network)
  fic <- 'relations_route'
  requete <- sprintf("relation[network='%s'][type=route][route=bus];
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
  <title>transport %s</title>
</head>
<body>'
  html_pied <- '
  </body>
</html>'
  html <- sprintf(html_entete, network)
  for (relation in relations) {
    id <- xml_attr(relation, "id")[[1]]
    if ( id != "4011816") {
#      next;
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
      next;
    }
    if (length(members_node) < 2) {
      carp("id: %s nodes", id)
      next;
    }
    type <- "relation"
    dsn <- bibus_osrm(id, force = force)
    if ( ! file.exists(dsn) || force == TRUE) {
#      next
    }
#    stop("****")
    href <- sprintf("<a href='http://localhost:8111/import?url=https://api.openstreetmap.org/api/0.6/relation/%s/full'>josm</a>", id)
    html <- append(html, href)
    href <- sprintf("<a href='http://level0.osmz.ru/?url=relation/%s'>level0</a>", id)
    html <- append(html, href)
    href <- sprintf("<a href='http://bv/transport/%s/level0/relation_%s_level0.txt'>osrm</a>", network, id)
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
# conversion en format sf des relations "route=bus"
# source("geo/scripts/transport.R");dsn <- bibus_relations_route_sf()
bibus_relations_route_sf <- function(force = FALSE, network = 'bibus') {
  library(tidyverse)
  library(rio)
  library(xml2)
  library(sf)
  library(osmdata)
  carp()
  config_xls(network)
  fic <- 'relations_route'
  requete <- sprintf("relation[network='%s'][type=route][route=bus][ref=13];
out meta;", Config[1, "network"])
  dsn <- overpass_query(query = requete, fic = fic, force = force)
  carp("dsn: %s", dsn)
  q <- opq(bbox = c(51.1, 0.1, 51.2, 0.2))
  osm.sf <- osmdata(q, dsn) %>%
    glimpse()
}
#
# pour une ref
bibus_osrm <- function(ref, network = 'bibus', force = TRUE) {
  library(clipr)
  carp()
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
  return(dsn)
  carp(dsn)
  level0 <- read_lines(file = dsn)
  write_clip(level0)
}