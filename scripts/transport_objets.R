# <!-- coding: utf-8 -->
#  interrogation des objets d'un réseau de transport
# utilisation des données opendata
# auteur : Marc Gauthier
#
# source("geo/scripts/transport.R");objets_jour()
objets_jour <- function(force = FALSE) {
  carp()
  config_xls('tub')
  routes.df <- objets_relations_route(force)
  masters.df <- objets_relations_route_master(force)
  carp("les masters sans route")
  df1 <- masters.df %>%
    filter(ref == "")
  if (nrow(df1) > 0) {
    print(knitr::kable(df1, format = "pipe"))
  }
  carp("les route hors master")
  df1 <- routes.df %>%
    filter(! id %in% masters.df$member)
  if (nrow(df1) > 0) {
    print(knitr::kable(df1, format = "pipe"))
  }

}
#
# les relation route avec le tag network
objets_relations_route <- function(force = FALSE) {
  carp()
  dsn <- sprintf("%s/objets_relations_route.Rds", transportDir)
  if (file.exists(dsn) && force == FALSE) {
    df <- readRDS(dsn)
    return(invisible(df))
  }
  requete <- sprintf("(relation[network='%s'][type=route][route=bus];);out meta;", config[1, 'network'])
  dsn1 <- overpass_query_json(requete, "relations_route", force = force)
  json1.list <- jsonlite::fromJSON(dsn1, simplifyVector = FALSE, simplifyDataFrame = FALSE)
  elements.list <- json1.list$elements
  relations.df <- data.frame(
    id = character(),
    user = character(),
    timestamp = character(),
    ref = character(),
    shape = character(),
    nodes = numeric(),
    ways = numeric()
  )
  for (i in 1:length(elements.list)) {
    element.list <- elements.list[[i]]
    tags <- element.list$tags
    shape <- ""
    if ( exists("note:mga_geo", where=tags)) {
      shape <- tags[["note:mga_geo"]]
    }
    nodes <- 0
    ways <- 0
    if ( exists("members", where=element.list)) {
      members.list <- element.list$members
      df1 <- do.call(rbind, lapply(members.list, data.frame))
      nodes <- nrow(filter(df1, type == "node"))
      ways <- nrow(filter(df1, type == "way"))    }
#    carp("shape: %s", shape)
#    glimpse(tags); stop("****")
    relations.df[i, ] = c(
      element.list$id,
      element.list$user,
      element.list$timestamp,
      tags$ref,
      shape,
      nodes,
      ways
    )
  }
  saveRDS(relations.df, file = dsn)
  return(invisible(relations.df))
}
#
# les relation route master_avec le tag network
objets_relations_route_master <- function(force = FALSE) {
  carp()
  library(jsonlite)
  dsn <- sprintf("%s/objets_relations_route_master.Rds", transportDir)
  if (file.exists(dsn) && force == FALSE) {
    df <- readRDS(dsn)
    return(invisible(df))
  }
  requete <- sprintf("(relation[network='%s'][type=route_master][route_master=bus];);out meta;", config[1, 'network'])
  dsn1 <- overpass_query_json(requete, "relations_route_master", force = force)
  json1.list <- jsonlite::fromJSON(dsn1, simplifyVector = FALSE, simplifyDataFrame = FALSE)
  elements.list <- json1.list$elements
  relations.df <- data.frame(
    id = character(),
    user = character(),
    timestamp = character(),
    ref = character(),
    member = character()
  )
  for (i in 1:length(elements.list)) {
    element.list <- elements.list[[i]]
    tags <- element.list$tags
    member <- ""
    if ( exists("members", where=element.list)) {
      members.list <- element.list$members
      df1 <- do.call(rbind, lapply(members.list, data.frame))
      member <- paste0(df1$ref, collapse = ",")
    }
#    carp("shape: %s", shape)
#    glimpse(tags); stop("****")
    relations.df[i, ] = c(
      element.list$id,
      element.list$user,
      element.list$timestamp,
      tags$ref,
      member
    )
  }
  relations.df <- tidyr::separate_rows(relations.df, member)
  saveRDS(relations.df, file = dsn)
  return(invisible(relations.df))
}