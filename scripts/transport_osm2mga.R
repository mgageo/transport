# <!-- coding: utf-8 -->
# le réseau de bus
# utilisation des données d'OpenStreetMap
# auteur : Marc Gauthier
#
# https://github.com/oscarperpinan/spacetime-vis/blob/master/osmar.R
#
#
# en xml2
# https://gist.github.com/nuest/3ed3b0057713eb4f4d75d11bb62f2d66
#
# source("geo/scripts/transport.R");osm2mga_jour()
osm2mga_jour <- function(force=TRUE) {
  library(tidyverse)
  library(rio)
  library(sf)
  library(xml2)
  carp()
  if( force==TRUE) {
    config_xls('star')
    osm_routes_get()
    osm_stops_get()
  }
# la conversion en format "mga"
  osm2mga_osm_lire()
# la vérification des platforms des relations
  osm2mga_platforms_verif()
}
# pour lire un fichier osm
# source("geo/scripts/transport.R");osm2mga_osm_lire()
osm2mga_osm_lire <- function(fic='relations_route_bus.osm') {
  library(xml2)
  library(tidyverse)
  dsn <- sprintf("%s/%s", transportDir, fic)
  carp("dsn: %s", dsn)
  doc <- read_xml(dsn, asText = TRUE, useInternal = TRUE, getDTD = FALSE)
  osm2mga_parse(doc, NODES=TRUE, RELATIONS=TRUE)
}
# osm_parse
osm2mga_parse <- function(doc, NODES=FALSE, RELATIONS=TRUE) {
  carp()
  osm2mga <- osm2mga_lire()
  has_nodes <- osm2mga_has_xpath(doc, "//node")
  has_ways <- osm2mga_has_xpath(doc, "//way")
  has_relations <- osm2mga_has_xpath(doc, "//relation")
  if ( has_nodes & NODES==TRUE) {
    osm2mga$nodes <- osm2mga_parse_nodes(doc)
    osm2mga_ecrire(osm2mga)
  }
#  return()
  if ( has_relations & RELATIONS == TRUE) {
    osm2mga$relations_platforms <- osm2mga_parse_relations_platforms(doc)
    osm2mga$relations <- osm2mga_parse_relations(doc)
    osm2mga$relations_stops <- osm2mga_platforms_stops()
    osm2mga_ecrire(osm2mga)
  }
}
osm2mga_parse_nodes <- function(doc) {
  carp()
  nodes <<- xml2::xml_find_all(doc, "//node")

# tous les arrêts de la star
  nodes <- xml2::xml_find_all(nodes, "tag[@k='ref:FR:STAR']")
  nodes <- xml2::xml_parent(nodes)
  nodes.df <- nodes %>%
    map(xml_attrs) %>%
    map_df(~as.list(.)) %>%
    glimpse()
  carp('nodes: %s', length(nodes))
  for (i in 1:length(nodes)) {
    tags <- xml2::xml_find_all(nodes[[i]], "tag")
    tags.df <- tags %>%
      map(xml_attrs) %>%
      map_df(~as.list(.)) %>%
      spread(k, v)
    for(a in c('name', 'ref:FR:STAR') ) {
      if( a %in% names(tags.df) ) {
        v <- tags.df[1, a]
      } else {
        v <- ''
      }
      nodes.df[i, a] <- v
    }
#    break
  }
  glimpse(nodes.df)
  return(invisible(nodes.df))
}
osm2mga_parse_relations <- function(doc) {
  carp()
  relations <<- xml2::xml_find_all(doc, "//relation")
  relations.df <- relations %>%
    map(xml_attrs) %>%
    map_df(~as.list(.))
  for (i in 1:length(relations)) {
    tags <- xml2::xml_find_all(relations[[i]], "tag")
    tags.df <- tags %>%
      map(xml_attrs) %>%
      map_df(~as.list(.)) %>%
      spread(k, v)
    for(a in c('name', 'ref:FR:STAR', 'from', 'to') ) {
      if( a %in% names(tags.df) ) {
        v <- tags.df[1, a]
      } else {
        v <- ''
      }
      relations.df[i, a] <- v
    }
#    break
  }
  glimpse(relations.df)
  return(invisible(relations.df))
}
osm2mga_parse_relations_platforms <- function(doc) {
  carp()
  relations <<- xml2::xml_find_all(doc, "//relation")
  relations.df <- relations %>%
    map(xml_attrs) %>%
    map_df(~as.list(.)) %>%
    mutate(toto='toto')
  for(i in 1:length(relations)) {
    platforms.df <- osm2mga_parse_relation(relations[[i]])
    if ( platforms.df == FALSE ) {
      next
    }
# on fait la "jointure"
    platforms.df <- platforms.df %>%
      mutate(stop_sequence=1:nrow(.)) %>%
      mutate(toto='toto') %>%
      left_join(relations.df[i,], by=c('toto')) %>%
      dplyr::select(-toto)
    if(exists('relations_platforms.df')) {
      relations_platforms.df <- rbind(relations_platforms.df, platforms.df)
    } else {
      relations_platforms.df <- platforms.df
    }
  }
  glimpse(relations_platforms.df)
  return(invisible(relations_platforms.df))
}
#
# pour une relation route
# on a les membres pour les arrêts et le parcours
osm2mga_parse_relation <- function(doc) {
  carp()
#  stop('***')
  platforms <<- xml2::xml_find_all(doc, "member[@role='platform']")
  if(length(platforms) == 0 ) {
    return(FALSE)
  }
  platforms.df <- platforms %>%
    map(xml_attrs) %>%
    map_df(~as.list(.))
  ways <<- xml2::xml_find_all(doc, "member[@role='']")
  members <<- xml2::xml_find_all(doc, "member")
  members.df <- members %>%
    map(xml_attrs) %>%
    map_df(~as.list(.))
#  View(members.df)
  tags <- xml2::xml_find_all(doc, "tag")
  tags.df <- tags %>%
    map(xml_attrs) %>%
    map_df(~as.list(.)) %>%
    spread(k, v)

  return(invisible(platforms.df))
}
# source("geo/scripts/transport.R");osm2mga_platforms_stops()
osm2mga_platforms_stops <- function() {
  library(tidyverse)
  osm2mga <- osm2mga_lire()
  relations.df <- osm2mga$relations
  if(nrow(relations.df) == 0 ) {
    stop('relations.df')
  }
  relations_platforms.df <- osm2mga$relations_platforms
  if(nrow(relations_platforms.df) == 0 ) {
    stop('relations_platforms.df')
  }
  nodes.df <- osm2mga$nodes
  if(nrow(nodes.df) == 0 ) {
    stop('nodes.df')
  }
  carp('les roles platform qui ne sont pas des noeuds')
  relations_platforms.df %>%
    filter(type != 'node') %>%
    glimpse()
  carp('les platforms qui ne sont pas des noeuds avec ref')
  df1 <- relations_platforms.df %>%
    left_join(nodes.df, by=c('ref'='id'))
  df1 %>%
    filter(is.na(name)) %>%
    dplyr::select(type, ref, role, stop_sequence, id, name, `ref:FR:STAR`) %>%
    glimpse()
  df2 <- df1 %>%
    filter(! is.na(name)) %>%
    dplyr::select(type, ref, role, stop_sequence, id, name, stop_id=`ref:FR:STAR`) %>%
    glimpse()
  df3 <- df2 %>%
    arrange(id, stop_sequence) %>%
    group_by(id) %>%
    summarize(stops=paste0(stop_id, collapse = ";")) %>%
    glimpse()
  carp('ajout infos de la relation')
  df4 <- df3 %>%
    left_join(relations.df, by=c('id')) %>%
    glimpse()
  return(invisible(df4))
}

# =====================================================================================
#
# fonctions utilitaires
#
# test if a given xpath exists in doc
osm2mga_has_xpath <- function(doc, xpath) {
  tryCatch(length(xml2::xml_find_all(doc, xpath)) > 0,
    error=function(err) { return(FALSE) },
    warning=function(wrn) { message(wrn$message) ; return(TRUE); }
  )
}
osm2mga_ecrire <- function(osm2mga, rds='osm2mga.Rds') {
  carp()
  dsn <- sprintf("%s/%s", transportDir, rds)
  carp("dsn: %s", dsn)
  saveRDS(osm2mga, file=dsn)
}
# source("geo/scripts/transport.R"); osm2mga <- osm2mga_lire();glimpse(osm2mga)
osm2mga_lire <- function(rds='osm2mga.Rds') {
  dsn <- sprintf("%s/%s", transportDir, rds)
  carp("dsn: %s", dsn)
  osm2mga <- list()
  if(file.exists(dsn)) {
    osm2mga <- readRDS(file=dsn)
  }
  return(invisible(osm2mga))
}