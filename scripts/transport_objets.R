# <!-- coding: utf-8 -->
#
# le réseau de bus
#
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================

#
# les relation route avec le tag network
# source("geo/scripts/transport.R");config_xls('bibus');df <- objets_relations_route() %>% glimpse()
objets_relations_route <- function(force = TRUE) {
  carp()
  dsn <- sprintf("%s/objets_relations_route.Rds", transportDir)
  if (file.exists(dsn) && force == FALSE) {
    df <- readRDS(dsn)
    return(invisible(df))
  }
  doc <- overpass_get(query = "relations_route_bus_network", format = "xml", force = force) %>%
    glimpse()
# les relations
  df <- objects_relations_df(doc)
  saveRDS(df, file = dsn)
  return(invisible(df))
}
#
# les relation route master_avec le tag network
# source("geo/scripts/transport.R");config_xls('bibus');df <- objets_relations_route_master() %>% glimpse()
objets_relations_route_master <- function(force = TRUE) {
  carp()
  dsn <- sprintf("%s/objets_relations_route_master.Rds", transportDir)
  if (file.exists(dsn) && force == FALSE) {
    df <- readRDS(dsn)
    return(invisible(df))
  }
  doc <- overpass_get(query = "relations_routemaster_bus_network", format = "xml", force = force) %>%
    glimpse()
# les relations
  df <- objects_relations_df(doc)
  saveRDS(df, file = dsn)
  return(invisible(df))
}
#
# conversion en dataframe
objects_relations_df <- function(doc) {
  carp()
  library(xml2)
# les relations
  relations <- xml2::xml_find_all(doc, ".//relation")
  carp("relations nb: %s", length(relations))
  df11 <- tibble()
  relations.df <- relations %>%
    map(xml_attrs) %>%
    map_df(~as.list(.))
  df <- data.frame()
  for (i in 1:length(relations)) {
    relation <- relations[[i]]
    members <-  xml2::xml_find_all(relation, './/member')
    members.df <- members %>%
      map(xml_attrs) %>%
      map_df(~as.list(.)) %>%
      mutate(id = relations.df[[i, "id"]])
    df11 <- bind_rows(df11, members.df)
#    carp("relations: %s ways: %s nodes: %s", length(members_relation), length(members_way), length(members_node))
    tags <- xml2::xml_find_all(relation, "tag")
    tags.df <- tags %>%
      map(xml_attrs) %>%
      map_df(~as.list(.)) %>%
      spread(k, v)
    df1 <- cbind(relations.df[i, ], tags.df) %>%
      mutate(nb_members = length(members))
    df <- bind_rows(df, df1)
  }
  return(invisible(list("relations.df" = df, "members.df" = df11)))
}
objets_relations_route_master_json <- function(force = TRUE) {
  carp()
  library(jsonlite)
  dsn <- sprintf("%s/objets_relations_route_master.Rds", transportDir)
  if (file.exists(dsn) && force == FALSE) {
    df <- readRDS(dsn)
    return(invisible(df))
  }
  json1.list <- overpass_get(query = "relations_routemaster_bus_network", format = "json", force = force) %>%
    glimpse()
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
      tags$network,
      tags$colour,
      tags$text_colour,
      member
    )
  }
  relations.df <- tidyr::separate_rows(relations.df, member)
  saveRDS(relations.df, file = dsn)
  return(invisible(relations.df))
}