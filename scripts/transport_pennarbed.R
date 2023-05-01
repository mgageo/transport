# <!-- coding: utf-8 -->
#
# le réseau de bus du Finistère ex pennarbed
#
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
# source("geo/scripts/transport.R");pennarbed_jour()
pennarbed_jour <- function() {
  library(tidyverse)
  library(rio)
  carp()
  config_xls('pennarbed')
}

#
## la partie routage avec osrm
#
# source("geo/scripts/transport.R");dsn <- pennarbed_relations_route_get()
#
pennarbed_relations_route_get <- function(force = TRUE, network = 'pennarbed') {
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
#      next;
    }
    if (length(members_node) < 2) {
      carp("id: %s nodes", id)
      next;
    }
    type <- "relation"
    dsn <- pennarbed_osrm(id, force = force)
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
# source("geo/scripts/transport.R");dsn <- pennarbed_relations_route_rmd()
pennarbed_relations_route_rmd <- function(force = FALSE, network = 'pennarbed') {
  library(tidyverse)
  library(rio)
  library(xml2)
  doc <- overpass_relations_route_get_xml(force = force, network = network)
  has_relations <- osm_has_xpath(doc, "//relation")
  if (! has_relations) {
    stop("***")
  }
  relations <- xml2::xml_find_all(doc, "//relation")
  for (relation in relations) {
    id <- xml_attr(relation, "id")[[1]]
    cat("  \n### id r",  id, "  \n")
    tag_ref <-  xml2::xml_attr(xml2::xml_find_first(relation, './/tag[@k="ref"]'), "v")
    tag_ref_network <<-  xml2::xml_attr(xml2::xml_find_first(relation, './/tag[@k="ref:network"]'), "v")
    tag_shape_id <-  xml2::xml_attr(xml2::xml_find_first(relation, './/tag[@k="gtfs:shape_id"]'), "v")
    tag_name <-  xml2::xml_attr(xml2::xml_find_first(relation, './/tag[@k="name"]'), "v")
    tag_from <-  xml2::xml_attr(xml2::xml_find_first(relation, './/tag[@k="from"]'), "v")
    tag_to <-  xml2::xml_attr(xml2::xml_find_first(relation, './/tag[@k="to"]'), "v")
    members_way <<-  xml2::xml_find_all(relation, './/member[@type="way"]')
    members_node <<-  xml2::xml_find_all(relation, './/member[@type="node"]')
    md <- sprintf("name: %s from: %s to: %s", tag_name, tag_from, tag_to)
    cat("\n",  md, "  \n")
    md <- sprintf("ref: %s ref:network: %s", tag_ref, tag_ref_network)
    cat("\n",  md, "  \n")
    md <- sprintf("members_way: %s members_node: %s", length(members_way), length(members_node))
    cat("\n",  md, "  \n")
    osmdata_relation_route_plot(id)
    cat("  \n")
  }
}
#
# conversion en format sf des relations "route=bus"
# source("geo/scripts/transport.R");dsn <- pennarbed_relation_route_sf()
pennarbed_relation_route_sf <- function(force = FALSE, network = 'pennarbed') {
  library(tidyverse)
  library(sf)
  library(osmdata)
  carp()
  relation <- 13436728
  dsn <- osmapi_object_full(relation, type = "relation", force = force)
  carp("dsn: %s", dsn)
  q <- opq(bbox = c(45, -6, 58, 0))
  osm.sf <- osmdata_sf(q, dsn) %>%
    glimpse()
  plot(st_geometry(osm.sf$osm_multilines))
  stops.sf <- osm.sf$osm_points %>%
    filter(! is.na(public_transport))
  plot(st_geometry(stops.sf), add = TRUE)
}
#
# pour une ref
pennarbed_osrm <- function(ref, network = 'pennarbed', force = TRUE) {
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