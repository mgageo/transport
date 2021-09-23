# <!-- coding: utf-8 -->
# <!-- coding: utf-8 -->
# le réseau de bus de Lorient
# utilisation des données opendata
# auteur : Marc Gauthier
#
# source("geo/scripts/transport.R");ctrl_jour()
ctrl_jour <- function() {
  carp()
  config_xls('ctrl');
}
#
# source("geo/scripts/transport.R");dsn <- ctrl_relations_route_get()
# https://gist.github.com/nuest/3ed3b0057713eb4f4d75d11bb62f2d66
#
ctrl_relations_route_get <- function(force = FALSE) {
  library(tidyverse)
  library(rio)
  library(xml2)
  carp()
  config_xls('ctrl');
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
  for (relation in relations) {
    id <- xml_attr(relation, "id")[[1]]
    if ( id != "12307588") {
      next;
    }
    carp("id: %s", id)
    members_way <<-  xml2::xml_find_all(relation, './/member[@type="way"]')
    members_node <<-  xml2::xml_find_all(relation, './/member[@type="node"]')
    if (length(members_way) != 0) {
      carp("id: %s ways", id)
      next;
    }
    if (length(members_node) < 2) {
      carp("id: %s nodes", id)
      next;
    }
    id <- xml_attr(relation, "id") %>%
      glimpse()
    carp("id : %s ways: %s nodes: %s", id, length(members_way), length(members_node))
    ctrl_osrm(id, force = force)
  }
  return()
  ids <- xml_attr(relations, "id") %>%
    glimpse()
  for (id in ids) {
    carp("id: %s", id)
#    ctrl_osrm(id, force = force)
  }
}
#
# source("geo/scripts/transport.R");ctrl_osrm()
ctrl_osrm <- function(ref, force = FALSE) {
  library(clipr)
  carp()
  config_xls('ctrl');
  carp("josmDir: %s", josmDir)
#  osrm_relation_stops(12307545, force = force)
#  ref <- 12307555
#  ref <- 12307567 ; # 13-B
  type <- "relation"
  dsn <- sprintf("%s/%s_%s_level0.txt", level0Dir, type, ref)
  if ( ! file.exists(dsn) || force == TRUE) {
    osrm_relation_stops(ref, force = force)
  }
  return()
  carp(dsn)
  level0 <- read_lines(file = dsn)
  write_clip(level0)
}