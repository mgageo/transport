# <!-- coding: utf-8 -->
#
# le réseau de bus
#
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
#
#
# source("geo/scripts/transport.R");config_xls('surf');osmar_jour()
#
osmar_jour <- function(fic='relations_route_bus.osm') {
  carp()
  osm <- osmar_osm_lire()
  osmar_relation_plan_ligne(osm)
}
# pour lire un fichier osm
# source("geo/scripts/transport.R");osm <- osmar_osm_lire()
osmar_osm_lire <- function(fic='relations_route_bus.osm') {
  library(osmar)
  library(tidyverse)
  dsn <- sprintf("%s/%s", osmDir, fic)
  carp("dsn: %s", dsn)
  osm <- get_osm(complete_file(), source = osmsource_file(dsn),  encoding = "UTF-8") %>%
    glimpse()
  return(invisible(osm))
}
#
# pour faire un plan de la ligne
# source("geo/scripts/transport.R");osm <- osmar_relation_plan_ligne(osm)
osmar_relation_plan_ligne <- function(osm) {
  library(osmar)
#  plot(osm)
  ajout <- FALSE
  idx_highway <- find(osm, way(tags(k=='highway')))
  sp_highway <- osmar_to_sp(osm, way(idx_highway))
  plot(sp_highway, add = ajout, col = "pink",lwd=5)
# les arrêts
  carp("les arrêts")
  idx_bus_stop <- find(osm, node(tags(k=='highway' & v=="bus_stop")))
  sp_bus_stop <- osmar_to_sp(osm, node(idx_bus_stop), 'points')
# pour ajouter le tag "name"
  tags <- subset(osm$nodes$tags, subset=(k=='name'), select=c('id', 'v'))
  tags_match <- match(idx_bus_stop, tags$id)
  tags <- tags[tags_match,]
  sp_bus_stop$name <- tags$v[tags_match]
  sp_bus_stop$name <- iconv(sp_bus_stop$name, "UTF-8")
  text(coordinates(sp_bus_stop), labels=sp_bus_stop@data$name)
}
#
# conversion en format sp
osmar_to_sp <- function(source, index, type='lines'){
  library(sp)
  idx <- find_down(source, index)
  obj <- subset(source, ids=idx)
  objSP <- as_sp(obj, type)
  return(invisible(objSP))
}