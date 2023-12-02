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
# les traitements journaliers

# source("geo/scripts/transport.R");config_xls('star');osm_jour()
osm_jour <- function(force = TRUE) {
  carp()
  osm_valid_jour(force = force)
}
# les routes
#
osm_relations_route_bus_get <- function(fic = 'relations_route_bus', force = FALSE) {
  requete <- sprintf("(relation[network='%s'][type=route][route=bus];);out meta;", Config[1, 'network'])
  dsn <- sprintf("%s.osm", fic)
  carp("dsn: %s", dsn)
  oapi_requete_get(requete, dsn, force = force)
}
# lecture avec sf
# https://github.com/r-spatial/sf/blob/master/tests/read.Rout.save
osm_relations_route_bus_save <- function(fic='relations_route_bus') {
  library(sf)
  library(tidyverse)
  carp('fic: %s', fic)
  dsn <- sprintf("%s/%s.osm", osmDir, fic)
  osm.sf <- oapi_osmdata_lire_sf(dsn)
  dsn <- sprintf("%s/%s.Rds", transportDir, fic)
  saveRDS(osm.sf, dsn)
  return(invisible(osm.sf))
}
# version en xml
osm_relations_route_bus_save <- function(fic='relations_route_bus') {
  library(sf)
  library(tidyverse)
  carp('fic: %s', fic)
  dsn <- sprintf("%s/%s.osm", osmDir, fic)
  doc <- xml2::read_xml(dsn, encoding = "UTF-8")
  has_relations <- osm_has_xpath(doc, "//relation")
  if (! has_relations) {
    stop("***")
  }
  relations <- xml2::xml_find_all(doc, "//relation")
  carp("relations nb: %s", length(relations))
  dsn <- sprintf("%s/%s.Rds", transportDir, fic)
  saveRDS(df, dsn)
  return(invisible(df))
}
osm_relations_route_bus_read <- function(fic='relations_route_bus') {
  library(sf)
  library(tidyverse)
  dsn <- sprintf("%s/%s.Rds", transportDir, fic)
  osm.sf <- readRDS(dsn)
  carp('dsn: %s', dsn)
  return(invisible(osm.sf))
}
#
# l'historique des versions
# source("geo/scripts/transport.R"); df1 <- osm_relations_route_bus_history(force = FALSE)
osm_relations_route_bus_history <- function(force = FALSE) {
  library(tidyverse)
  carp()
  dsn_rds <- sprintf("%s/osm_relations_route_bus_history.rds", osmDir)
  if (file.exists(dsn_rds) && force == FALSE) {
    relations.df <- readRDS(dsn_rds)
    return(invisible(relations.df))
  }
  df1 <- overpass_get(query = "relations_route_bus_network", format = "csv", force = force) %>%
    clean_names() %>%
    glimpse()
  relations.df <- data.frame()
  for (i1 in 1:nrow(df1)) {
    carp("i1: %s/%s", i1, nrow(df1))
    relation.df <- osm_relation_route_bus_history(ref = df1[i1, "id"], type = df1[i1, "type"], force = force)
    relations.df <- bind_rows(relations.df, relation.df)
  }
  glimpse(relations.df)
  saveRDS(relations.df, dsn_rds)
  return(invisible(relations.df))
}
# source("geo/scripts/transport.R");osm_relation_route_bus_history(force = FALSE)
osm_relation_route_bus_history <- function(ref = "16017716", type = "relation", force = FALSE) {
  library(tidyverse)
  dsn <- osmapi_object_history(ref = ref, type = type, force = force)
  doc <- read_xml(dsn)
# les relations
  relations <- xml2::xml_find_all(doc, ".//relation")
  carp("relations nb: %s", length(relations))
  relations.df <- data.frame()
  i_relation <- 0
  for (relation in relations) {
#    carp("les attributs de la relation")
    relation.df <- xml_attrs(relation) %>%
      as_tibble_row()
    relations.df <- bind_rows(relations.df, relation.df)
  }
#  glimpse(relations.df)
  return(invisible(relations.df))
}
#
# le créateur de la route
# source("geo/scripts/transport.R"); df1 <- osm_relations_route_bus_history_stat(force = FALSE)
osm_relations_route_bus_history_stat <- function(force = FALSE) {
  library(tidyverse)
  carp()
  dsn_rds <- sprintf("%s/osm_relations_route_bus_history.rds", osmDir)
  relations.df <- readRDS(dsn_rds) %>%
    glimpse()
  df1 <- relations.df %>%
    filter(version == "1") %>%
    arrange(timestamp)
  misc_print(df1)
  df2 <- df1 %>%
    group_by(user) %>%
    summarize(nb = n()) %>%
    arrange(nb)
  misc_print(df2)
  df11 <- relations.df %>%
    group_by(user) %>%
    summarize(nb = n()) %>%
    arrange(nb)
  misc_print(df11)
}
#
# ajout d'un tag gtfs:stops
# tous les membres "platform.*" doivent avoir une référence du réseau
# source("geo/scripts/transport.R"); df1 <- osm_relations_route_bus_stops(force = FALSE)
osm_relations_route_bus_stops <- function(force = FALSE) {
  library(tidyverse)
  carp()
  dsn_rds <- sprintf("%s/osm_relations_route_bus_stops.rds", osmDir)
  if (file.exists(dsn_rds) && force == FALSE) {
    relations.df <- readRDS(dsn_rds)
    return(invisible(relations.df))
  }
  df1 <- overpass_get(query = "relations_route_bus_network", format = "csv", force = force) %>%
    clean_names() %>%
    glimpse()
  for (i1 in 1:nrow(df1)) {
    carp("i1: %s/%s", i1, nrow(df1))
    osm_relation_route_bus_stops(ref = df1[i1, "id"], type = df1[i1, "type"], force = force)
  }
}
# Bordeaux Lianes 4
# source("geo/scripts/transport.R");osm_relation_route_bus_stops(ref = "1601972", force = FALSE, force_osm = FALSE) %>% glimpse()
osm_relation_route_bus_stops <- function(ref = "16017716", type = "relation", force = TRUE, force_osm = TRUE) {
  library(tidyverse)
  library(digest)
  rc <- osm_relation_route_members(ref, force = force, force_osm = force_osm)
  platforms.df <- rc$members.sf %>%
    st_drop_geometry() %>%
    filter(grepl("platform", role))
  if(Config_k_ref %notin% names(platforms.df)) {
    carp("*** pas de tag k_ref: %s", Config_k_ref)
    return(invisible())
  }
  gtfs.df <- platforms.df %>%
    filter(! is.na(!!Config_k_ref))
  if (nrow(platforms.df) != nrow(gtfs.df)) {
    carp("*** manque des tags k_ref: %s", Config_k_ref)
    return(invisible())
  }
  misc_print(gtfs.df)
  stops <- paste(gtfs.df[, Config_k_ref], collapse = ",")
  tags <- list(
    "gtfs:stops" = digest::md5(stops)
  )
  type <- "relation"
  osmchange_object_modify_tags(id = ref, type = type, tags = tags)
#  stop("******")
}
#
## les relations route_master
#
# source("geo/scripts/transport.R"); df1 <- osm_relations_routemaster_bus_get(force = FALSE)
osm_relations_routemaster_bus_get <- function(force = TRUE) {
  doc <- overpass_get(query = "relations_routemaster_bus_network", format = "xml", force = force) %>%
    glimpse()
}
# lecture avec sf
# https://github.com/r-spatial/sf/blob/master/tests/read.Rout.save
osm_relations_routemaster_bus_save <- function(fic='relations_routemaster_bus') {
  library(sf)
  library(tidyverse)
  carp('fic: %s', fic)
  dsn <- sprintf("%s/%s.osm", osmDir, fic)
  osm.sf <- oapi_osmdata_lire_sf(dsn)
  dsn <- sprintf("%s/%s.Rds", transportDir, fic)
  saveRDS(osm.sf, dsn)
  return(invisible(osm.sf))
}
osm_relations_routemaster_bus_read <- function(fic = 'relations_routemaster_bus') {
  library(sf)
  library(tidyverse)
  dsn <- sprintf("%s/%s.Rds", transportDir, fic)
  osm.sf <- readRDS(dsn)
  carp('dsn: %s', dsn)
  return(invisible(osm.sf$osm_multilines))
}
# source("geo/scripts/transport.R"); df1 <- osm_relations_routemaster_bus_area(force = FALSE)
osm_relations_routemaster_bus_area <- function(fic = 'relations_routemaster_bus_area', force = TRUE) {
  carp()
  carp("osm: les relations")
  doc <- overpass_get(query = "relations_routemaster_bus_area", format = "xml", force = force)
  objects <- xml2::xml_find_all(doc, "//relation")
  osm.df <- osmapi_objects_members(objects) %>%
    glimpse()
  carp("les networks")
  df1 <- osm.df %>%
    filter(! is.na(route_master)) %>%
    group_by(network) %>%
    summarize(nb = n())
  misc_print(df1)
#
# les relations membres plusieurs fois
  carp("les doublons")
  df1 <- osm.df %>%
    group_by(Mtype, Mrole) %>%
    summarize(nb = n()) %>%
    filter(nb > 1)
  if (nrow(df1) > 1) {
    misc_print(df1)
  }
}
#
#
#
## les validations
###############################################################
#
# source("geo/scripts/transport.R");osm_valid_jour()
osm_valid_jour <- function(force = TRUE) {
  carp()
  osm_valid_stops()
  osm_valid_relations(); # master # route
  osm_valid_relations_route_bus()
}
#
# les route_master et les route
# source("geo/scripts/transport.R");osm_valid_relations()
osm_valid_relations <- function(force = TRUE) {
  carp()
  titre <- sprintf("osm_valid_relations_%s", Config[1, "reseau"])
  html <- misc_html_titre(titre)
  html <- misc_html_append(html, sprintf("<h1>%s</h1>", titre))
  rc <- objets_relations_route(force)
  routes.df <- rc[["relations.df"]]
  rc <- objets_relations_route_master(force)
  masters.df <- rc[["relations.df"]] %>%
    glimpse()
  members.df <- rc[["members.df"]] %>%
    glimpse()
#
  html <- misc_html_append(html, sprintf("<h2>%s</h2>", "cohérence route_master # route"))
  html <- misc_html_append(html, sprintf("<h3>%s</h3>", "les masters sans route"))
  df1 <- masters.df %>%
    filter(nb_members == 0)
  if (nrow(df1) > 0) {
    html <- misc_html_append_df(html, df1)
  }
#
  html <- misc_html_append(html, sprintf("<h3>%s</h3>", "les relations route hors master"))
  df1 <- routes.df %>%
    filter(! id %in% members.df$ref)
  if (nrow(df1) > 0) {
    df2 <- df1 %>%
      dplyr::select(id, version, timestamp, user, name)
    html <- misc_html_append_df(html, df2)
  }
#
  html <- misc_html_append(html, sprintf("<h3>%s</h3>", "les relations route dans plusieurs master"))
  df1 <- members.df %>%
    group_by_all() %>%
    summarize(nb = n()) %>%
    filter(nb > 1)
  if (nrow(df1) > 0) {
    html <- misc_html_append_df(html, df1)
  }

  transport_html_browse(html, titre)
}
#
# les route_master et les route
# source("geo/scripts/transport.R");osm_valid_relations_bus(force = FALSE)
osm_valid_relations_bus <- function(force = TRUE) {
  carp()
  titre <- sprintf("osm_valid_relations_bus_%s", Config[1, "reseau"])
  html <- misc_html_titre(titre)
  html <- misc_html_append(html, sprintf("<h1>%s</h1>", titre))
  rc <- objets_relations_bus(force)
  routes.df <- rc[["relations.df"]] %>%
    (function(.df){
      cls <- c("disused:route", "disused:route_master")
      .df[cls[!(cls %in% colnames(.df))]] = NA
      return(.df)
    }) %>%
    mutate(level0 = sprintf("<a href='http://level0.osmz.ru/?url=%s%s'>level0</a>", "r", id)) %>%
    glimpse()
  members.df <- rc[["members.df"]] %>%
    rename_with( ~ paste0(.x, ".member"))
  html <- misc_html_append(html, sprintf("<h3>%s%s</h3>", "les relations hors network: ", Config_network))
  df11 <- routes.df %>%
    filter(network != Config_network) %>%
    dplyr::select(id, level0, ref, name) %>%
    glimpse()
  html <- misc_html_append_df(html, df11)
  html <- misc_html_append(html, sprintf("<h3>%s</h3>", "les relations disused:route"))
  df1 <- routes.df %>%
    filter(!is.na(`disused:route`)) %>%
    dplyr::select(id, level0, ref, name) %>%
    glimpse()
  html <- misc_html_append_df(html, df1)
  html <- misc_html_append(html, sprintf("<h3>%s</h3>", "les relations disused:route_master"))
  df2 <- routes.df %>%
    filter(!is.na(`disused:route_master`)) %>%
    dplyr::select(id, level0, ref, name) %>%
    glimpse()
  html <- misc_html_append_df(html, df2)
  html <- misc_html_append(html, sprintf("<h3>%s</h3>", "les relations route hors route_master"))
  df21 <- routes.df %>%
    filter(route == "bus") %>%
    left_join(members.df, by = c("id" = "ref.member")) %>%
    glimpse()
  df22 <- df21 %>%
    filter(is.na(id.member)) %>%
    dplyr::select(id, level0, ref, name) %>%
    glimpse()
  html <- misc_html_append_df(html, df22)
#
## pour valider les membres des route_master
#
  carp("les relations route=bus")
  df31 <- routes.df %>%
    filter(route == "bus") %>%
    arrange(ref) %>%
    dplyr::select(ref, id) %>%
    rename_with( ~ paste0(.x, ".route")) %>%
    glimpse()
# pour retouver les membres des route_master
  df32 <- routes.df %>%
    filter(route_master == "bus") %>%
    arrange(ref) %>%
    dplyr::select(ref, id) %>%
    left_join(members.df, by = c("id" = "id.member")) %>%
    glimpse()
  df33 <- df32 %>%
    filter(is.na(type.member))
  if(nrow(df33) > 0) {
    confess("bug interrogation overpass")
  }
# la comparaison
  df34 <- df32 %>%
    full_join(df31, by = c("ref.member" = "id.route")) %>%
    glimpse()
  df35 <- df34 %>%
    filter(ref != ref.route)
  html <- misc_html_append(html, sprintf("<h3>%s</h3>", "les deltas"))
  html <- misc_html_append_df(html, df35)
  transport_html_browse(html, titre)
}
#
## *************************************************
# pour avoir la liste des relations route
# source("geo/scripts/transport.R");osm_valid_relations_route_bus()
osm_valid_relations_route_bus <- function(force = TRUE) {
  library(tidyverse)
  library(data.table)
  library(janitor)
  carp()
  titre <- sprintf("osm_valid_relations_route_bus_%s", Config[1, "reseau"])
  html <- misc_html_titre(titre)
  html <- misc_html_append(html, sprintf("<h1>%s</h1>", titre))
  dsn <- osm_relations_route_bus_csv(force = force)
  df <- fread(dsn, encoding = "UTF-8") %>%
    clean_names() %>%
    glimpse()
  df10 <- df %>%
    dplyr::select(id, gtfs_shape_id, ref, ref_network, from, to, name) %>%
    mutate(josm = sprintf("<a href='http://localhost:8111/import?url=https://api.openstreetmap.org/api/0.6/relation/%s/full'>josm</a>", id)) %>%
    mutate(level0 = sprintf("<a href='http://level0.osmz.ru/?url=%s%s'>level0</a>", "r", id)) %>%
    arrange(ref, ref_network, gtfs_shape_id)
  html <- misc_html_append(html, sprintf("<h3>%s</h3>", "les relations route"))
  html <- misc_html_append_df(html, df10)
  df11 <- df %>%
    filter(!grepl("^\\w", gtfs_shape_id)) %>%
    dplyr::select(id, ref, from, to, name) %>%
    arrange(ref)
  html <- misc_html_append(html, sprintf("<h3>%s</h3>", "les relations route avec gtfs:shape_id"))
  html <- misc_html_append_df(html, df11)
  df1 <- df %>%
    group_by(ref) %>%
    summarize(nb = n()) %>%
    filter(nb != 2) %>%
    glimpse()
  df2 <- df %>%
    filter(ref %in% df1$ref) %>%
    arrange(ref, ref_network)
  misc_print(df2)
  df3 <- df %>%
    filter(ref %in% df1$ref) %>%
    group_by(ref_network) %>%
    summarize(josm = paste0(id, collapse = ","))
  html <- misc_html_append(html, sprintf("<h3>%s</h3>", "les relations route ref_network # 2"))
  html <- misc_html_append_df(html, df3)
  df5 <- df %>%
    group_by(gtfs_shape_id) %>%
    summarize(nb = n()) %>%
    filter(nb > 1) %>%
    glimpse()
  df6 <- df %>%
    filter(gtfs_shape_id %in% df5$gtfs_shape_id) %>%
    dplyr::select(ref_network, id, name, from, to, ref, gtfs_shape_id) %>%
    arrange(gtfs_shape_id) %>%
#    mutate(josm = sprintf("<a href='http://localhost:8111/import?url=https://api.openstreetmap.org/api/0.6/relation/%s/full'>josm</a>", id)) %>%
    mutate(level0 = sprintf("<a href='http://level0.osmz.ru/?url=%s%s'>level0</a>", "r", id)) %>%
    glimpse()
  html <- misc_html_append(html, sprintf("<h3>%s</h3>", "les relations route gtfs:shape_id # 1"))
  html <- misc_html_append_df(html, df6)
  transport_html_browse(html, titre)
  return(invisible())
}
#
## les relations route
#
# source("geo/scripts/transport.R"); df1 <- osm_valid_relations_route_bus_network(force = FALSE) %>% glimpse()
osm_valid_relations_route_bus_network <- function(force = TRUE) {
  doc <- overpass_get(query = "relations_route_bus_network", format = "xml", force = force)
  objects <- xml2::xml_find_all(doc, "//relation")
  kref <- "gtfs:shape_id"
  osm.df <- osmapi_objects_tags(objects) %>%
    glimpse()
  osm.df[, "kref"] <- osm.df[, "gtfs:shape_id"]
  titre <- sprintf("osm_valid_relations_route_bus_network_%s", Reseau)
  html <- misc_html_titre(titre)
  df1 <- osm.df %>%
    group_by(kref) %>%
    summarize(nb = n()) %>%
    filter(nb > 1)
  if (nrow(df1) > 0) {
    glimpse(df1)
    df2 <- osm.df %>%
      filter(kref %in% df1$kref) %>%
      arrange(kref, `ref:network`) %>%
      mutate(josm = sprintf("<a href='http://localhost:8111/import?url=https://api.openstreetmap.org/api/0.6/relation/%s/full'>josm</a>", `@id`)) %>%
      dplyr::select(kref, `@id`, josm, `ref:network`, from, to, name) %>%
      mutate(`@id` = sprintf("<a href=http://level0.osmz.ru/?url=relation/%s>%s</a>", `@id`, `@id`))

    misc_print(df2)
    html <- misc_html_append(html, "<h2>Les doublons</h2>")
    html <- misc_html_append_df(html, df2)
    transport_html_browse(html, titre)
  }
#
# les tags définis dans agency.xls
  tags <- c("operator", "website", "wikidata", "wikipedia", "operator:wikidata", "operator:wikipedia", "network:wikidata", "network:wikipedia")
  for (tag in tags) {
    col <- sprintf("Config_%s", tag)
    col <- get(col)
    carp("tag: %s col: %s", tag, col)
    if(tag %notin% names(osm.df)) {
      carp("\ttag absent")
      next
    }
    df2 <- osm.df %>%
      rename(tag = !!tag) %>%
      dplyr::select(`@id`, tag, name) %>%
      filter(tag != col)
    if ( nrow(df2) > 0) {
      misc_print(df2)
#      stop("*****")
    }
  }

  return(invisible())
}
#
## les nodes platform
#
# source("geo/scripts/transport.R"); osm_valid_nodes_platform_kref(force = FALSE)
osm_valid_nodes_platform_kref <- function(rds = "osm_valid_nodes_platform_kref", force = TRUE, force_osm = TRUE) {
  if (force == TRUE) {
    doc <- overpass_get(query = "nodes_platform_kref", format = "xml", force_osm = force_osm)
    objects <- xml2::xml_find_all(doc, "//node")
    osm.df <- osmapi_objects_tags(objects) %>%
      glimpse()
    transport_ecrire(osm.df, rds)
  }
  osm.df <- transport_read(rds)
#
# les tags potentiels
  tags_delete <- c("source", "route_ref", "local_ref", "ref", "network", "not:network:wikidata", "operator", "network:wikidata")
  tags_osm <- names(osm.df)
  tags <- list()
  for (tag in tags_delete) {
    if(tag %notin% tags_osm) {
      carp("\ttag absent")
      next
    }
    tags <- append(tags, tag)
  }
  level0 <- list()
  for (i in 1:nrow(osm.df)) {
    carp("i: %s/%s", i, nrow(osm.df))
    row.list <- purrr::transpose(osm.df[i, ])[[1]]
    ok <- TRUE
    for (tag in tags) {
      if(! is.na(row.list[[tag]])) {
        carp("\ttag présent: %s", tag)
        ok <- FALSE
        next
      }
    }
    if (ok == FALSE) {
      level0 <- append(level0, row.list[["@id"]])
    }
#    stop("****")
  }
  glimpse(level0)
  level0 <- paste(level0, collapse = ",n")
  print(level0)
}
# https://forum.openstreetmap.fr/t/comment-optimiser-une-ligne-de-car-pour-affichage-sur-carte-osm-et-traitement-automatique-des-donnees/2418

#
# lecture en csv
# source("geo/scripts/transport.R");osm_valid_stops(force_osm = FALSE)
osm_valid_stops <- function(force_osm = TRUE) {
  library(tidyverse)
  library(data.table)
  library(janitor)
  k_ref <- Config[[1, 'k_ref']]
  carp("k_ref: %s", k_ref)
  titre <- sprintf("osm_valid_stops_%s", Config[1, 'reseau'])
  html <- misc_html_titre(titre)
  html <- misc_html_append(html, "<h1>OSM</h1>")
  html <- misc_html_append(html, "<h2>PTv2</h2>")
  html <- misc_html_append(html, "<h3>public_transport=platform + highway=bus_stop</h3>")
  html <- misc_html_append(html, "<h3>public_transport=stop_position + bus=yes</h3>")
#
#
  html <- misc_html_append(html, "<h2>Les stops du réseau</h2>")
  html <- misc_html_append(html, "<h3>Répartition</h3>")
  network.df <- overpass_get(query = "bus_stop_network", format = "csv", force = force_osm) %>%
    filter(ferry == "" & tram == "" & rail == "" & train == "") %>%
    dplyr::select(-ferry, -tram, -rail,  -train) %>%
    glimpse()
  df1 <- network.df %>%
    group_by(`@type`, public_transport, highway, bus) %>%
    summarize(nb = n()) %>%
    glimpse()
  misc_print(df1)
  html <- misc_html_append_df(html, df1)
  html <- misc_html_append(html, "<h3>Non conforme PTv2</h3>")
  tags.df <- read.table(text="type,public_transport,highway,bus
node,platform,bus_stop,
node,stop_position,,yes
", header=TRUE, sep=",", blank.lines.skip = TRUE, stringsAsFactors = FALSE, quote="")
  misc_print(tags.df)
  zoom <- "left=%0.5f&right=%0.5f&top=%0.5f&bottom=%0.5f"
  select <- "%s%s"

  df1 <- network.df %>%
    filter(!(`@type` == "node" & public_transport == "platform" & highway == "bus_stop" & bus == "")) %>%
    filter(!(`@type` == "node" & public_transport == "stop_position" & highway == "" & bus == "yes")) %>%
    filter(!(`@type` == "relation" & public_transport == "stop_area" & highway == "" & bus == "")) %>%
    filter(!(railway != "")) %>%
    arrange(`@type`, public_transport, highway, bus) %>%
    mutate(select = sprintf(select, `@type`, `@id`)) %>%
    mutate(zoom = sprintf(zoom, `@lon` - .001, `@lon` + .001, `@lat` + .001, `@lat` - .001)) %>%
    mutate(josm = sprintf("<a href='http://127.0.0.1:8111/load_and_zoom?%s&select=%s'>josm</a>", zoom, select)) %>%
    dplyr::select(-select, -zoom)
  df2 <- df1  %>%
    group_by(`@type`, public_transport, highway, bus) %>%
    summarize(nb = n()) %>%
    glimpse()
  misc_print(df2)
  html <- misc_html_append_df(html, df2)
  html <- misc_html_append_df(html, df1)
#
#
  html <- misc_html_append(html, "<h2>Les stops de la zone</h2>")
  html <- misc_html_append(html, "<h3>Répartition</h3>")
  area.df <- overpass_get(query = "bus_stop_area", format = "csv", force = force_osm) %>%
    filter(aerialway == "" & ferry == "" & rail == "" & train == "" & tram == "") %>%
    dplyr::select(-aerialway, -ferry, -rail,  -train, -tram) %>%
    glimpse()
  df1 <- area.df %>%
    group_by(`@type`, public_transport, highway, bus) %>%
    summarize(nb = n()) %>%
    glimpse()
  misc_print(df1)
  html <- misc_html_append_df(html, df1)
  html <- misc_html_append(html, "<h3>Non conforme PTv2</h3>")
  tags.df <- read.table(text="type,public_transport,highway,bus
node,platform,bus_stop,
node,stop_position,,yes
", header=TRUE, sep=",", blank.lines.skip = TRUE, stringsAsFactors = FALSE, quote="")
  misc_print(tags.df)
  zoom <- "left=%0.5f&right=%0.5f&top=%0.5f&bottom=%0.5f"
  select <- "%s%s"

  df1 <- area.df %>%
    filter(!(`@type` == "node" & public_transport == "platform" & highway == "bus_stop" & bus == "")) %>%
    filter(!(`@type` == "node" & public_transport == "stop_position" & highway == "" & bus == "yes")) %>%
    filter(!(`@type` == "relation" & public_transport == "stop_area" & highway == "" & bus == "")) %>%
    filter(!(railway != "")) %>%
    arrange(`@type`, public_transport, highway, bus) %>%
    mutate(select = sprintf(select, `@type`, `@id`)) %>%
    mutate(zoom = sprintf(zoom, `@lon` - .001, `@lon` + .001, `@lat` + .001, `@lat` - .001)) %>%
    mutate(josm = sprintf("<a href='http://127.0.0.1:8111/load_and_zoom?%s&select=%s'>josm</a>", zoom, select)) %>%
    dplyr::select(-select, -zoom)
  df2 <- df1  %>%
    group_by(`@type`, public_transport, highway, bus) %>%
    summarize(nb = n()) %>%
    glimpse()
  misc_print(df2)
  html <- misc_html_append_df(html, df2)
  html <- misc_html_append_df(html, df1)
#
  transport_html_browse(html, titre)
}
#
# source("geo/scripts/transport.R"); od <- osm_bus_stop_network(force = TRUE) %>% glimpse
osm_bus_stop_network <- function(fic = 'osm_bus_stop_network', force = TRUE) {
  requete <- overpass_query_bus_stop_network()
  fic <- sprintf("%s", fic)
  od <- osmdata_query(requete, fic, force = force)
  return(invisible(od))
}
# source("geo/scripts/transport.R");osm_bus_stop_network_csv(force = TRUE)
osm_bus_stop_network_csv <- function(fic = 'osm_bus_stop_network', force = FALSE) {
  requete <- overpass_query_bus_stop_network_csv()
  dsn <- overpass_query_csv(requete, fic, force = force)
  carp("dsn: %s", dsn)
  return(invisible(dsn))
}
# source("geo/scripts/transport.R"); od <- osm_bus_stop_area(force = TRUE) %>% glimpse
osm_bus_stop_area <- function(fic = 'osm_bus_stop_area', force = FALSE) {
  requete <- overpass_query_bus_stop_area()
  fic <- sprintf("%s", fic)
  od <- osmdata_query(requete, fic, force = force)
#  od <- st_query(requete, fic, force = force)
  return(invisible(od))
}
# source("geo/scripts/transport.R");osm_bus_stop_area_csv(force = TRUE)
osm_bus_stop_area_csv <- function(fic = 'osm_bus_stop_area', force = FALSE) {
  requete <- sprintf('[out:csv(::type,::id,::version,::timestamp,::user,::lat,::lon,name,highway,public_transport,rail,"%s";true;"|")];
area[name="%s"]->.a;
(
  nwr(area.a)[highway=bus_stop];
  nwr(area.a)[public_transport];
);
out meta;', Config[1, 'k_ref'], Config[1, 'zone'])
  dsn <- overpass_query_csv(requete, fic, force = force)
  carp("dsn: %s", dsn)
  return(invisible(dsn))
}
# lecture avec sf
# https://github.com/r-spatial/sf/blob/master/tests/read.Rout.save
osm_nodes_stop_save <- function(fic = 'nodes_stop') {
  library(sf)
  library(tidyverse)
  dsn <- sprintf("%s/%s.osm", osmDir, fic)
  nc <- st_read(dsn, layer = "points") %>%
    glimpse()
  osm.sf <- oapi_osmdata_lire_sf(dsn) %>%
    glimpse()
  dsn <- sprintf("%s/%s.Rds", transportDir, fic)
  saveRDS(osm.sf, dsn)
  return(invisible(osm.sf))
}
osm_nodes_stop_read <- function(fic = 'nodes_stop') {
  library(sf)
  library(tidyverse)
  dsn <- sprintf("%s/%s.Rds", transportDir, fic)
  osm.sf <- readRDS(dsn)
  carp('dsn: %s', dsn)
  return(invisible(osm.sf$osm_points))
}
#
## les interrogations avec réponse en csv
#
osm_bretagne_relations_bus_csv <- function(fic = 'bretagne_relations_bus_csv', force = FALSE) {
  requete <- sprintf('// les relations route/route_master
[out:csv(::id, ::type, network, name; true; "|")];
//area[admin_level=4][name="%s"]->.a;
area[name="%s"]->.a;
(
relation(area.a)[type=route][route=bus];
relation(area.a)[type=route_master][route_master=bus];
);
out body;', Config[1, 'zone'])
  overpass_query_csv(requete, fic, force = force)
}
osm_relations_routemaster_bus_csv <- function(fic = 'relations_routemaster_bus', force = FALSE) {
  requete <- sprintf('[out:csv(::type,::id,network,name,ref,"ref:network";true;"|")];
relation[route_master=bus][network="%s"];
out;', Config[1, 'network'])
  overpass_query_csv(requete, fic, force = force)
}
osm_relations_route_bus_csv <- function(fic = 'relations_route_bus', force = FALSE) {
  requete <- sprintf('[out:csv(::type,::id,::version,::timestamp,::user,network,name,ref,"ref:network","gtfs:shape_id",from,to;true;"|")];
area[name="%s"]->.a;
relation(area.a)[type=route][route=bus][network~"%s"];
out meta;', Config[1, 'zone'], Config[1, 'network'])
  dsn <- overpass_query_csv(requete, fic, force = force)
  carp("dsn: %s", dsn)
  return(invisible(dsn))
}
osm_relations_route_bus_area_csv <- function(fic = 'relations_route_bus_area', force = FALSE) {
  requete <- sprintf('[out:csv(::type,::id,::version,::timestamp,::user,network,name,ref,"ref:network","gtfs:shape_id",from,to;true;"|")];
relation(%s);map_to_area->.a;
relation(area.a)[type=route][route=bus];
out meta;', Config[1, 'zone_relation'])
  dsn <- overpass_query_csv(requete, fic, force = force)
  carp("dsn: %s", dsn)
  return(invisible(dsn))
}
osm_relations_routes_bus_csv <- function(fic = 'relations_routes_bus', force = FALSE) {
  requete <- sprintf('[out:csv(::type,::id,::version,::timestamp,::user,network,ref,"ref:network",type,colour,text_colour,operator;true;"|")];
area[name="%s"]->.a;
(
relation(area.a)[type=route][route=bus][network~"%s"];
relation["type"="route_master"]["route_master"="bus"][network="%s"];
);
out meta;', Config[1, 'network'], Config[1, 'network'], Config[1, 'network'])
  dsn <- overpass_query_csv(requete, fic, force = force)
  carp("dsn: %s", dsn)
  return(invisible(dsn))
}
# source("geo/scripts/transport.R");config_xls(Reseau);osm_nodes_bus_csv()
osm_nodes_bus_csv <- function(fic = 'nodes_bus', force = FALSE) {
  requete <- sprintf('[out:csv(::type,::id,::version,::timestamp,::user,name;true;"|")];
relation[type=route][route=bus][network="%s"]->.a;
(
  node[highway=bus_stop](r.a);
  node[public_transport](r.a);
);
out meta;', Config[1, 'network'])
  dsn <- overpass_query_csv(requete, fic, force = force)
  carp("dsn: %s", dsn)
  return(invisible(dsn))
}


#
# pour avoir la liste des relations route disused
# source("geo/scripts/transport.R");config_xls('star');osm_relations_route_bus_disused()
osm_relations_route_bus_disused <- function(fic = 'osm_relations_route_bus_disused', force = FALSE) {
  library(tidyverse)
  library(stringr)
  library(janitor)
  carp()
  requete <- '
relation[type=route][network~"STAR"][~"disused:route"~"^bus$"];
out meta;'
  dsn <- sprintf("%s/%s.osm", osmDir, fic);
  osm_oapi(requete, dsn, force)
  osm <- read_file(dsn)
  osm <- str_replace_all(osm, "\\<U\\+\\d{4}\\>", "?")
  doc <- xml2::read_xml(osm)
  relations <- xml2::xml_find_all(doc, "//relation")
  carp("relations nb: %s", length(relations))
  relations.df <- relations %>%
    map(xml_attrs) %>%
    map_df(~as.list(.)) %>%
    glimpse()
  df <- tibble()
  for (i in 1:length(relations)) {
    relation <- relations[[i]]
    members_way <-  xml2::xml_find_all(relation, './/member[@type="way"]')
    members_node <-  xml2::xml_find_all(relation, './/member[@type="node"]')
    carp("ways: %s nodes: %s", length(members_way), length(members_node))
    tags <- xml2::xml_find_all(relation, "tag")
    tags.df <- tags %>%
      map(xml_attrs) %>%
      map_df(~as.list(.)) %>%
      spread(k, v)
    df1 <- cbind(relations.df[i, ], tags.df) %>%
      mutate(nb_ways = length(members_way)) %>%
      mutate(nb_nodes = length(members_node))
    df <- bind_rows(df, df1)
  }
  df <- df %>%
    clean_names() %>%
    glimpse()
  dsn <- sprintf("%s/%s.Rds", transportDir, fic)
  saveRDS(df, dsn)
}
#
#
# source("geo/scripts/transport.R");osm_relations_route_bus_disused_tex()
osm_relations_route_bus_disused_tex <- function(fic = 'osm_relations_route_bus_disused', force = FALSE) {
  library(tidyverse)
  library(stringr)
  library(janitor)
  carp()
  dsn <- sprintf("%s/%s.Rds", transportDir, fic)
  df <- readRDS(dsn)
  texFic <- sprintf("%s/%s", imagesDir, "osm_relations_route_bus_disused.tex")
  TEX <- file(texFic)
  tex <- "% <!-- coding: utf-8 -->"
#
# le template tex
  dsn <- sprintf("%s/osm_relations_route_bus_disused_tpl.tex", tplDir)
  template <- readLines(dsn) %>%
    glimpse()
  for (i in 1:nrow(df)) {
    carp("i: %s/%s", i, nrow(df))
    tpl <- tex_df2tpl(df, i, template)
    tpl <- escapeLatexSpecials(tpl)
    tex <- append(tex, tpl)
  }
  write(tex, file = TEX, append = FALSE)
  close(TEX)
  carp(" texFic: %s", texFic)
}
# source("geo/scripts/transport.R");osm_relations_route_members(reseau = Reseau, force = FALSE, force_osm = FALSE, osrm = FALSE)
osm_relations_route_members <- function(reseau = Reseau, force = TRUE, force_osm = TRUE, osrm = FALSE) {
  library(tidyverse)
  library(data.table)
  library(sf)
  library(janitor)
  carp()
  config_xls(reseau)
  dsn <- osm_relations_route_bus_csv(force = force_osm)
  df <- fread(dsn, encoding = "UTF-8") %>%
    clean_names() %>%
    mutate(reseau = !!reseau) %>%
    arrange(ref, ref_network) %>%
    glimpse()
# https://ctan.math.illinois.edu/macros/latex/contrib/tabularray/tabularray.pdf
  df1 <- df %>%
    mutate(level0 = sprintf("\\href{http://level0.osmz.ru/?url=relation/%s}{level0}", id)) %>%
    mutate(josm = sprintf("\\href{http://localhost:8111/import?url=https://api.openstreetmap.org/api/0.6/relation/%s/full}{josm}", id)) %>%
    mutate(col1 = sprintf("{%s \\\\%s \\\\%s}", id, level0, josm)) %>%
    mutate(col2 = sprintf("{%s\\\\from: %s to: %s\\\\ref: %s ref\\_network: %s\\\\%s %s}", name, from, to, ref, ref_network, version, timestamp)) %>%
    mutate(row = sprintf("%s & %s \\\\", col1, col2)) %>%
#    mutate(row = sprintf("%s & {%s\\\\from: %s to: %s\\\\ref: %s} \\\\", id, name, from, to, ref)) %>%
    glimpse()
  rows <- paste(df1$row, collapse = "\n")
  tex <- sprintf("\\begin{longtblr}[
caption = {Tableau des relations},
]{colspec={ll},hlines}
%s
\\end{longtblr}", rows)
  texFic <- sprintf("%s/%s", imagesDir, "osm_relations_route_members_lst.tex")
  TEX <- file(texFic)
  write(tex, file = TEX, append = FALSE)
  close(TEX)
  return()
  tex_df2kable(df1, suffixe = "lst", longtable = TRUE)
  texFic <- sprintf("%s/%s", imagesDir, "osm_relations_route_members.tex")
  TEX <- file(texFic)
  tex <- "% <!-- coding: utf-8 -->"
#
# le template tex
  dsn <- sprintf("%s/relations_route_members_tpl.tex", tplDir)
  template <- readLines(dsn) %>%
    glimpse()
  if (reseau == "star") {
    star.df <- star202210_supprime()
    df <- df %>%
      filter(ref %notin% star.df$ligne) %>%
#      filter(grepl("^0006", gtfs_shape_id)) %>%
      arrange(gtfs_shape_id) %>%
      glimpse()
  }
  if (reseau == "kiceo") {
    df <- df %>%
#      filter(ref_network %in% c("4-B")) %>%
#      filter(grepl("^D3", ref_network)) %>%
      arrange(gtfs_shape_id) %>%
      glimpse()
  }
  if (reseau == "bordeaux") {
    df <- df %>%
#      filter(id == 13528658) %>%
      glimpse()
  }
#  df <- df %>%
#    filter(grepl("^P8", ref_network))
  members_loins.df <- tibble()
  members_kref.df <- tibble()
  for (i in 1:nrow(df)) {
    carp("i: %s/%s", i, nrow(df))
    id <-  df[[i, "id"]]
    shape <- df[[i, "gtfs_shape_id"]]
    ref_network <- df[[i, "ref_network"]]
#    glimpse(df[i, ]); stop("****");
    osm.sf <- osmapi_get_ways(ref = id, force = force, force_osm = force_osm) %>%
      st_transform(2154)
    if (st_is_empty(osm.sf[1,]) ) {
      carp("***id: %s", id)
      next;
    }
    if (osrm == TRUE) {
      osrm_relation_get(id, reseau = reseau, force = force_osm)
    }
    rc <- osm_relation_route_members(id = df[i, "id"], force = force, force_osm = force_osm)
    rc <- osm_relation_route_members_valid(rc = rc, id = df[i, "id"], force = force, force_osm = force_osm)
#    osm_relation_route_stops_order(rc = rc, id = df[i, "id"], force = force, force_osm = force_osm)
    platforms.sf <- rc$platforms.sf
    df1 <- st_distance(platforms.sf, osm.sf)
    platforms.sf <- cbind(platforms.sf, df1) %>%
      mutate(distance = as.integer(df1)) %>%
      dplyr::select(-df1)
    loins.sf <- platforms.sf %>%
      filter(distance > 100)
    if (nrow(loins.sf) > 0) {
#      plot(st_geometry(loins.sf), col = "red", lwd = 4, pch = 19, add = TRUE)
      members_loins.df <- bind_rows(members_loins.df, st_drop_geometry(loins.sf))
#      stop("****")
    }
    if (1 == 2) {
      kref.sf <- platforms.sf %>%
        filter(is.na(k_ref))
      if (nrow(kref.sf) > 0) {
        plot(st_geometry(kref.sf), col = "black", lwd = 2, pch = 19, add = TRUE)
        members_kref.df <- rbind(members_kref.df, st_drop_geometry(kref.sf))
#      stop("****")
      }
    }
    osm_relation_route_members_mapsf(rc)
    dsn <- dev2pdf(suffixe = id, dossier = "images")
# pour le latex
#    tex <- append(tex, sprintf("\\mongraphique{images/reseau_routes_shapes_%s.pdf}", id))
    glimpse(rc)
    df[i, "shape"] <- shape
    df[i, "platforms"] <- rc$platforms
    df[i, "stops"] <- rc$stops
    tpl <- tex_df2tpl(df, i, template)
    tpl <- escapeLatexSpecials(tpl)
    tex <- append(tex, tpl)
#    break
  }
  write(tex, file = TEX, append = FALSE)
  close(TEX)
  carp("texFic: %s", texFic)
  tex_df2kable(members_loins.df, suffixe = "loin", longtable = TRUE)
  tex_df2kable(members_kref.df, suffixe = "kref", longtable = TRUE)
  return(invisible())
}
#
## différence entre deux versions
# requete via l'overpass
# source("geo/scripts/transport.R");osm_relation_route_versions(id = "3440638", force = TRUE)
osm_relation_route_versions <- function(id, force = FALSE, force_osm = FALSE) {
  library(tidyverse)
  library(data.table)
  library(sf)
  library(janitor)
  carp("id: %s", id)
  dsn <- overpass_relation_get_history(id = id, force = force_osm)
  df <- osmapi_objects_get_attrs(dsn) %>%
    glimpse()
  osmapi_object_history_version(dsn = dsn, ref = id, type = "relation", version = "11")
}

# Quimper 6-A
# Vannes 4754448 6b
# source("geo/scripts/transport.R");osm_relation_route_members(id = "11984170", force = TRUE) %>% glimpse()
# Bordeaux 2422223 6b
# source("geo/scripts/transport.R");osm_relation_route_members(id = "2422224", force = TRUE) %>% glimpse()
osm_relation_route_members <- function(id, force = TRUE, force_osm = TRUE) {
  library(tidyverse)
  library(data.table)
  library(sf)
  library(janitor)
  carp("id: %s", id)
  dsn_rds <- sprintf("%s/osm_relation_route_members_%s.rds", osmDir, id)
  if (file.exists(dsn_rds) && force == FALSE) {
    rc <- readRDS(dsn_rds)
    return(invisible(rc))
  }
  rc <- FALSE
  members_loins.df <- tibble()
  members_kref.df <- tibble()
  rc <- osmapi_get_transport(ref = id, force = force, force_osm = force_osm)
  ways.sf <- rc$ways.sf
# st_is_empty(ways.sf[1,]
  if (nrow(ways.sf) == 0) {
    carp("***id: %s", id)
    return(invisible(rc))
  }
  rc[["ways_points.sf"]] <- st_sf(st_cast(st_geometry(ways.sf), "POINT"))
  if (rc$relation[1, "route"]  != "bus") {
    saveRDS(rc, dsn_rds)
    return(invisible(rc))
  }
# pour remettre dans l'ordre de la relation route les ways
#  osm_relation_route_members_ways(rc)
  ref_network <- rc$relation[1, "ref:network"]
  platforms.df <- rc$members.df %>%
    filter(grepl("platform", role)) %>%
    mutate(Id = row_number()) %>%
    mutate(Name = sprintf("%s-%s", Id, name)) %>%
    mutate(lat = as.numeric(lat)) %>%
    mutate(lon = as.numeric(lon))
  platforms <- paste0(platforms.df$Name, collapse = ", ")
  platforms.sf <- st_as_sf(platforms.df, coords = c("lon", "lat"), crs = 4326, remove = TRUE) %>%
    st_transform(2154) %>%
    glimpse()
  nodes.df <- rc$members.df %>%
    filter(grepl("stop", role)) %>%
    mutate(Id = row_number()) %>%
    mutate(Name = sprintf("%s-%s", Id, name)) %>%
    mutate(lat = as.numeric(lat)) %>%
    mutate(lon = as.numeric(lon))
  stops.sf <- st_as_sf(nodes.df, coords = c("lon", "lat"), crs = 4326, remove = TRUE) %>%
    st_transform(2154) %>%
    glimpse()
  stops <- paste0(nodes.df$Name, collapse = ", ")
  rc[["platforms"]] <- platforms
  rc[["stops"]] <- stops
  rc[["platforms.sf"]] <- platforms.sf
  rc[["stops.sf"]] <- stops.sf
# pour récupérer les infos de l'objet
  rc$members.sf <- tibble()
  if (nrow(stops.sf) > 0) {
    rc <- osm_relation_route_members_nodes(rc, role = "stop")
    rc$members.sf <- rc$stops.sf
  }
  if (nrow(platforms.sf) > 0) {
    rc <- osm_relation_route_members_nodes(rc, role = "platform")
    if (nrow(stops.sf) > 0) {
      rc$members.sf <- bind_rows(rc$members.sf, rc$platforms.sf)
    } else {
      rc$members.sf <- rc$platforms.sf
  }
# les départ/arrivée
    d <- as.integer(st_distance(platforms.sf[1, ], rc$ways.sf[1, ]))
    rc[["distance_depart"]] <- d
    n1 <- nrow(platforms.sf)
    n2 <- nrow(rc$ways.sf)
    d <- as.integer(st_distance(platforms.sf[n1, ], rc$ways.sf[n2, ]))
    rc[["distance_arrivee"]] <- d
  }
  saveRDS(rc, dsn_rds)
  return(invisible(rc))
}
#
# la carte de la ligne
# https://rgeomatic.hypotheses.org/2077
# https://riatelab.github.io/mapsf/
osm_relation_route_members_mapsf <- function(rc) {
  library(tidyverse)
  library(sf)
  library(mapsf)
  carp()
  mf_init(rc$ways.sf, expandBB = c(0.1, 0.1, 0.1, 0.1))
  mf_map(x = rc$ways.sf, col = "black", lwd = 3, add = TRUE)
#  mf_map(x = rc$platforms.sf, col = "blue", lwd = 3, pch = 19, add = TRUE)
  if (nrow(rc$members.sf) > 0) {
    mf_map(x = rc$members.sf, col = rc$members.sf$couleur, lwd = 3, pch = 19, add = TRUE)
    mf_label(x = rc$members.sf,
      var = "Id",
      cex = 1.5,
      overlap = FALSE,
      lines = TRUE,
      col = rc$members.sf$couleur
    )
  }
  if (! "ref:network" %in% names(rc$relation)) {
    rc$relation[1, "ref:network" ] <- NA
  }
  titre <- sprintf("relation: %s ref_network: %s", rc$relation[1, "id"], rc$relation[1, "ref:network"])
  mf_title(titre)
  mf_annotation(rc$ways_points.sf[1, ], txt = "Départ")
}
#
# détermination de la distance et du segment le plus proche
osm_relation_route_members_nodes <- function(rc, role = "stop") {
  carp()
  if (role == "stop") {
    nodes.sf <- rc$stops.sf
  }
  if (role == "platform") {
    nodes.sf <- rc$platforms.sf
    nodes.sf$couleur <- "blue"
  }
  nodes.df <- st_drop_geometry(nodes.sf)
  ways.sf <- rc$ways.sf %>%
    glimpse()
  ways.df <- st_drop_geometry(ways.sf)
  nodes <- paste0(nodes.sf$Name, collapse = ", ")
  for (i in 1:nrow(nodes.sf)) {
    d <- as.integer(st_distance(nodes.sf[i, ], ways.sf))
    df1 <- data.frame(d = d, way_id = ways.df$id, way_no = ways.df$way_no) %>%
      mutate(way = row_number()) %>%
      arrange(d)
    nodes.df[i, "way"] <- df1[1, "way"]
    nodes.df[i, "distance"] <- df1[1, "d"]
    nodes.df[i, "way_id"] <- df1[1, "way_id"]
    nodes.df[i, "way_no"] <- df1[1, "way_no"]
  }
  nodes.df <- nodes.df %>%
    arrange(way) %>%
    mutate(ordre = row_number()) %>%
    dplyr::select(type, ref, name, Id, ordre, public_transport, distance, way_id, way_no) %>%
    glimpse()
#  misc_print(nodes.df)
  err.df <- tibble()
  if (role == "platform") {
    df1 <- nodes.df %>%
      filter(distance > 150) %>%
      mutate(err = "distance")
    err.df <- bind_rows(err.df, df1)
    df1 <- nodes.df %>%
      filter(Id != ordre) %>%
      mutate(err = "ordre")
    err.df <- bind_rows(err.df, df1)
  }
  if (role == "stop") {
    df1 <- nodes.df %>%
      filter(distance > 0) %>%
      mutate(err = "distance")
    err.df <- bind_rows(err.df, df1)
  }
  if (nrow(err.df) > 0) {
    err.df$relation <- rc$relation[1, "id"]
    err.df$ref_network <- rc$relation[1, "ref_network"]
    err.df$gtfs_shape_id <- rc$relation[1, "gtfs_shape_id"]
    misc_print(err.df)
    stops.df <<- bind_rows(stops.df, err.df)
#    stop("****")
  }
  if (role == "stop") {
    nodes.sf$couleur <- "green"
    rc$stops.df <- nodes.df
    rc$stops.sf <- nodes.sf
  }
  if (role == "platform") {
    nodes.sf$couleur <- "blue"
    rc$platforms.df <- nodes.df
    rc$platforms.sf <- nodes.sf
  }
  return(invisible(rc))
}
# liste des informations des ways de la relation
# osmapi_get_transport ligne 290
osm_relation_route_members_ways <- function(rc) {
  carp()
  ways.sf <- rc$ways.sf %>%
    glimpse()
  ways.df <- st_drop_geometry(ways.sf)
  df1 <- ways.df %>%
    mutate(tags = highway) %>%
    mutate(tags = ifelse(is.na(junction), tags, sprintf("%s,%s", tags, junction))) %>%
    mutate(tags = ifelse(is.na(oneway), tags, sprintf("%s,%s", tags, oneway))) %>%
    select(id, tags, node1, node9, name)
  misc_print(df1)
  return(invisible(rc))
}
# source("geo/scripts/transport.R");osm_relations_route_members_tex()
osm_relations_route_members_tex <- function(force = FALSE, force_osm = FALSE) {
  carp()
  osm_relations_route_members(force = force, force_osm = force_osm)
  tex_pdflatex(sprintf("%s_relations_route_members.tex", Reseau))
}
#
## les arrêts qui ne sont plus en service
# source("geo/scripts/transport.R");osm_arrets_disused()
osm_arrets_disused <- function(force = FALSE, force_osm = FALSE) {
  carp()
  df <-  transport_read("reseau_osm_stops_gtfs_stops_osm_inc") %>%
    glimpse()
  df1 <- tibble()
  df2 <- tibble()
  for (i in 1:nrow(df)) {
    dsn <- osmapi_get_object_relations(ref = df[[i, "osm_id"]], type = df[[i, "osm_type"]], force = force_osm)
    objects.df <- osmapi_objects_get_tags(dsn)
    if (length(objects.df) == 0) {
      next
    }
    if ("route" %in% names(objects.df)) {
      glimpse(df[i, ])
      glimpse(objects.df)
      df1 <- rbind(df1, df[i, ])
    } else {
      df2 <- rbind(df2, df[i, ])
    }
  }
  return(invisible(list(autre = df1, hs = df2)))
}
#
## les platforms qui n'ont pas de tag ref
# source("geo/scripts/transport.R")osm_platforms_untag()
osm_platforms_untag <- function(force = FALSE, force_osm = FALSE) {
  carp()
  fic <- "overpass_relations_route_node"
  dsn <- sprintf("%s/%s.Rds", transportDir, fic)
    if (! file.exists(dsn) || force == TRUE) {
    dsn_osm <- overpass_relations_route_node(network = Reseau, force  = force_osm)
    df <- osmapi_objects_get_tags(dsn_osm)
    saveRDS(df, dsn)
  } else {
    df <- readRDS(dsn)
  }
  df1 <- df %>%
    glimpse() %>%
    filter(osm_type == "node") %>%
    group_by(public_transport) %>%
    summarize(nb = n()) %>%
    glimpse()
  df2 <- df %>%
    filter(osm_type == "node") %>%
    filter(public_transport == "platform") %>%
    filter(is.na(`ref:KICEO`)) %>%
    dplyr::select(id, name) %>%
    glimpse()
  misc_print(df2)
  return(invisible(df))
}
#
## la recherche des relations bus existantes sur une zone
#
# source("geo/scripts/transport.R");osm_relations_bus()
osm_relations_bus <- function(force = TRUE, force_osm = TRUE) {
  library(tidyverse)
  library(data.table)
  library(janitor)
  carp()
  titre <- sprintf("osm_relations_bus_%s", Reseau)
  html <- misc_html_titre(titre)
  html <- misc_html_append(html, sprintf("<h1>%s</h1>", titre))
  osm.df <- overpass_get(query = "relations_bus_area", format = "csv", force = force) %>%
    glimpse()
  df1 <- osm.df %>%
    filter(network %notin% c("FR:STAR")) %>%
    filter(route != "bus") %>%
    dplyr::select("network", "@id", "name", "from", "to") %>%
    mutate(level0 = sprintf("<a href='http://level0.osmz.ru/?url=relation/%s' target='diff'>%s</a>", `@id`, `@id`))
  html <- misc_html_append_df(html, "<h2>osm</h2>")
  html <- misc_html_append_df(html, df1)
  transport_html_browse(html, titre, Exit = FALSE)

}
#
## l'analyse fine d'une relation
#
# source("geo/scripts/transport.R");Reseau <- "bordeaux";config_xls(Reseau);osm_relations_route_members_valid(force = FALSE, force_osm = FALSE)
# source("geo/scripts/transport.R");osm_relations_route_members_valid()
osm_relations_route_members_valid <- function(force = TRUE, force_osm = TRUE) {
  library(tidyverse)
  library(data.table)
  library(sf)
  library(janitor)
  carp()
  dsn <- osm_relations_route_bus_csv(force = force_osm)
  df <- fread(dsn, encoding = "UTF-8") %>%
    as.data.table() %>%
    clean_names() %>%
    glimpse()
#  stop("*****")
  ptv2.df <<- tibble()
  stops.df <<- tibble()
  relations.list <- list()
  for (i in 1:nrow(df)) {
    id <- as.character(df[[i, "id"]])
    carp("i: %s/%s id: %s", i, nrow(df), id)
    rc <- osm_relation_route_members(id = id, force = force, force_osm = force_osm)
    if (nrow(rc$ways.sf) > 0) {
      rc <- osm_relation_route_members_valid(rc = rc, id = df[i, "id"], force = force, force_osm = force_osm)
      rc <- osm_relation_route_stops_order(rc = rc, id = df[i, "id"], force = force, force_osm = force_osm)
#      osm_relation_route_members_mapsf(rc)
    }
    relations.list[[id]] <- rc
#    break
#    stop("****")
  }
  misc_print(ptv2.df)
  carp("les erreurs ordre/distance sur les members")
  misc_print(stops.df)
  dsn_rds <- sprintf("%s/osm_relations_route_members_valid.rds", osmDir)
  saveRDS(relations.list, dsn_rds)
  carp("dsn_rds: %s", dsn_rds)
  glimpse(relations.list)
}
#
# source("geo/scripts/transport.R");osm_relations_route_members_valid_tex(force = FALSE, force_osm = FALSE)
osm_relations_route_members_valid_tex <- function(force = TRUE, force_osm = TRUE) {
  library(tidyverse)
  library(janitor)
  dsn_rds <- sprintf("%s/osm_relations_route_members_valid.rds", osmDir)
  carp("dsn_rds: %s", dsn_rds)
  relations.list <- readRDS(dsn_rds)
  df <- tribble(
    ~id,
    ~shape_id,
    ~distance_depart,
    ~distance_arrivee
  )
  err_role.df <- tibble()
  platforms.df <- tibble()
  for (id in names(relations.list)) {
    carp("id: %s", id)
    relation <- relations.list[[id]]
    if (nrow(relation$ways.sf) == 0) {
      next
    }
#    glimpse(relation);stop("*****")
    if (nrow(relation$err_role.df) > 0) {
      err_role.df <- relation$err_role.df
    }
    df <- add_row(df,
      id = id,
      shape_id = relation$relation[[1, "gtfs:shape_id"]],
      distance_depart = relation$distance_depart,
      distance_arrivee = relation$distance_arrivee,
    )
    if (nrow(relation$platforms.sf) > 0) {
      relation$platforms.df$relation <- id
      relation$platforms.df$ref_network <- relation$relation[[1, "ref:network"]]
#      glimpse(relation$platforms.df)
      platforms.df <- bind_rows(platforms.df, relation$platforms.df)
    }
#    break
  }
  carp("les erreurs de distance")
  df1 <- df %>%
    filter(distance_depart > 150 | distance_arrivee > 150) %>%
    arrange(shape_id) %>%
    glimpse()
  misc_print(df1)
  carp("les erreurs de role")
  misc_print(err_role.df)
  df2 <- platforms.df %>%
    filter(grepl("stop", public_transport)) %>%
    filter(distance > 1) %>%
    glimpse()
  df3 <- platforms.df %>%
    filter(grepl("platform", public_transport)) %>%
    filter(distance > 100)
  misc_print(df3)
}

# source("geo/scripts/transport.R"); osm_relation_route_members_valid(id = 4756509, force = TRUE, force_osm = TRUE)
# source("geo/scripts/transport.R"); osm_relation_route_members_valid(id = 13528658, force = TRUE, force_osm = TRUE)
osm_relation_route_members_valid <- function(rc, id = 3184038, force = FALSE, force_osm = FALSE) {
  carp()
  df <- tibble()
  df1 <- rc$members.df
  carp("bus=yes")
  df2 <- df1 %>%
    dplyr::bind_rows(dplyr::tibble(bus = character())) %>%
    filter(bus %in% c("yes", "on_demand"))
  if (nrow(df2) > 0) {
    df2$err <- "bus=yes"
#    df <- rbind(df, df2)
  }
  roles <- c("stop", "platform", "stop_entry_only", "stop_exit_only", "platform_entry_only", "platform_exit_only")
  df2 <- df1 %>%
    filter(role %notin% roles)
  if (nrow(df2) > 0) {
    df2$err <- "role"
    df <- rbind(df, df2)
  }
  pts <- c("stop_position", "platform")
  df2 <- df1 %>%
    filter(public_transport %notin% pts)
  if (nrow(df2) > 0) {
    df2$err <- "public_transport"
    df <- rbind(df, df2)
  }
  if (nrow(df) > 0) {
    df$relation <- rc$relation[1, "id"]
    df$ref_network <- rc$relation[1, "ref_network"]
    df$gtfs_shape_id <- rc$relation[1, "gtfs_shape_id"]
    rc$members_err <- df
    misc_print(df)
    ptv2.df <<- bind_rows(ptv2.df, df)
#    stop("*****")
  }
  df2 <- df1 %>%
    mutate(type = dplyr::recode(type,
      "way" = "wy",
      "node" = "nd"
    )) %>%
    mutate(member = sprintf("  %s %s %s", type, ref, role))
  rc$members_level0 <- paste(df2$member,  collapse = "\n")
  rc$err_role.df <- df
  return(invisible(rc))
}
#
## pour remettre dans l'ordre les stops
# source("geo/scripts/transport.R"); osm_relation_route_stops_order(force = TRUE)
osm_relation_route_stops_order <- function(rc, id = 3184038, force = FALSE, force_osm = FALSE) {
  carp()
  if (nrow(rc$stops.sf) == 0) {
    carp("id: %s pas de stops", id)
    return(invisible(rc))
  }
  carp("le rapprochement entre les platforms et les stops")
  nc1 <- rc$platforms.sf %>%
    mutate(type = dplyr::recode(type,
      "way" = "wy",
      "node" = "nd"
    )) %>%
    dplyr::select(type, ref, role, Id)
  nc2 <- rc$stops.sf %>%
    mutate(type = dplyr::recode(type,
      "way" = "wy",
      "node" = "nd"
    )) %>%
    dplyr::select(type, ref, role, Id)
  nc1$proche <- st_nearest_feature(nc1, nc2)
  df <- dplyr::left_join(
    nc1 %>% as.data.frame()
    , nc2 %>% as.data.frame()
    , by = c('proche' = 'Id')
    , suffix = c(".platform", ".stop")
  ) %>%
    glimpse()
#  stop('***')
  df$d <- st_distance(df$geometry.platform, df$geometry.stop, by_element = TRUE)
  df <- df %>%
    mutate(d = as.integer(d)) %>%
    glimpse()
#
  carp("les stops trop loin de platforms")
  df1 <- df %>%
    filter(d > 50) %>%
    glimpse()
  carp("les stops non rattachés")
  df2 <- rc$stops.sf %>%
    st_drop_geometry() %>%
    filter(Id %notin% df$proche) %>%
    glimpse()
  level0 <- list()
  df <- df %>%
    mutate(platform = sprintf("  %s %s %s", type.platform, ref.platform, role.platform)) %>%
    mutate(stop = sprintf("  %s %s %s", type.stop, ref.stop, role.stop))

  for (i in 1:nrow(df)) {
    level0 <- append(level0, df[i, "platform"])
    if (df[i, "d"] < 50) {
      level0 <- append(level0, df[i, "stop"])
    }
  }
  level0 <- paste(level0,  collapse = "\n")
  if (level0 != rc$members_level0) {
    n <- str_split(rc$members_level0, "\n")
    s <- str_split(level0, "\n")
    sq <- seq(max(length(n), length(s)))
#    df5 <- data.frame(n[sq], s[sq])
    dsn <- sprintf("%s/osm_relation_route_members_%s_level0.txt", osmDir, id)
    write(level0, dsn)
    carp("dsn: %s", dsn)
  }
  return(invisible(rc))
}
#
# les trous
# source("geo/scripts/transport.R");osm_relations_route_gap(force = FALSE, force_osm = FALSE)
osm_relations_route_gap <- function(force = TRUE, force_osm = TRUE) {
  library(tidyverse)
  library(data.table)
  library(sf)
  library(janitor)
  carp()
  dsn <- osm_relations_routes_bus_csv(force = force_osm)
  df <- fread(dsn, encoding = "UTF-8") %>%
    as.data.table() %>%
    clean_names() %>%
    filter(type_2 == "route") %>%
    glimpse()
  for (i in 1:nrow(df)) {
    osm_relation_route_gap(id = df[i, "id"], force = TRUE, force_osm = TRUE)
  }
}

#
# cohérence route route_master
# même operator, colour, text_colour
# source("geo/scripts/transport.R");osm_relations_routes_bus(force = FALSE, force_osm = FALSE)
osm_relations_routes_bus <- function(force = TRUE, force_osm = TRUE) {
  library(tidyverse)
  library(data.table)
  library(sf)
  library(janitor)
  carp()
  dsn <- osm_relations_routes_bus_csv(force = force_osm)
  df <- fread(dsn, encoding = "UTF-8") %>%
    as.data.table() %>%
    clean_names() %>%
    glimpse()
  df1 <- df %>%
    group_by(operator) %>%
    summarize(nb = n())
  if (nrow(df1) != 1) {
    glimpse(df1)
#    stop("*****")
  }
  carp("différences colour, text_colour")
  df2 <- df %>%
    dplyr::select(id, ref, colour, text_colour)
  df5 <- df2 %>%
    filter(text_colour == "") %>%
    glimpse()
  df3 <- df2 %>%
    group_by(ref, colour, text_colour) %>%
    summarize(nb = n()) %>%
    group_by(ref) %>%
    summarize(nb = n()) %>%
    filter(nb > 1) %>%
    glimpse()
  df4 <- df2 %>%
    filter(ref %in% df3$ref)
  misc_print(df4)

}
#
## interrogation avec level0
#
# source("geo/scripts/transport.R");osm_relations_level0(force = FALSE, force_osm = FALSE)
# cd /d/web.var/transport/kiceo/osm
# cat osm_relation_level0_*.txt
osm_relations_level0 <- function(force = TRUE, force_osm = TRUE) {
  library(tidyverse)
  library(data.table)
  library(sf)
  library(janitor)
  carp()
  dsn <- osm_relations_route_bus_csv(force = force_osm)
  df <- fread(dsn, encoding = "UTF-8") %>%
    as.data.table() %>%
    clean_names() %>%
    glimpse()
  for (i in 1:nrow(df)) {
    carp("i: %s/%s", i, nrow(df))
    level0 <- level0_get(type = "relation", id = df[i, "id"])
    df1 <- data.frame(ligne = unlist(str_split(level0, "\\n"))) %>%
      mutate(no = 1:n()) %>%
      filter(grepl(" stop", ligne)) %>%
      glimpse()
    if (nrow(df1) > 1) {
      for (j in 2:nrow(df1)) {
        if (df1[j-1, "no"] +1 == df1[j, "no"]) {
          stop("****")
        }
      }
    }
    dsn <- sprintf("%s/osm_relation_level0_%s.txt", osmDir, df[i, "id"])
    write(level0, dsn)
    carp("dsn: %s", dsn)
  }
}
#
## les gaps dans une relation
#
# source("geo/scripts/transport.R");osm_relation_route_gap(id = 4260060, force = FALSE, force_osm = FALSE)
# source("geo/scripts/transport.R");osm_relation_route_gap(id = 14632216, force = FALSE, force_osm = FALSE)
# source("geo/scripts/transport.R");osm_relation_route_gap(id = 8319385, force = FALSE, force_osm = FALSE)
# en erreur dans osmose
# source("geo/scripts/transport.R");osm_relation_route_gap(id = 14194690, force = FALSE, force_osm = FALSE)
# oneway:bus = no
# ligne courte
# le deuxième est un rond-point
# source("geo/scripts/transport.R");osm_relation_route_gap(id = 6528783, force = FALSE, force_osm = FALSE)
osm_relation_route_gap <- function(id = 4260060, force = TRUE, force_osm = TRUE) {
  library(tidyverse)
  library(rlist)
  DEBUG <<- TRUE
  gaps.df <- data.frame()
  rc <- osm_relation_route_members(id = id, force = force, force_osm = force_osm) %>%
    glimpse()
  if ("note:mga_geo" %in% names(rc$relation)) {
    carp("note:mga_geo id: %s", id)
#    return(invisible(gaps.df))
  }
  carp("les ways")
  ways.df <- rc$ways.sf %>%
    st_drop_geometry() %>%
   (function(.df){
      cls <- c("oneway_bus") # columns I need
      .df[cls[!(cls %in% colnames(.df))]] = NA
      return(.df)
    }) %>%
    clean_names() %>%
    glimpse()
  if (nrow(ways.df) == 0) {
    carp("****id: %s pas de ways", id)
    return(invisible(gaps.df))
  }
  df1 <- ways.df %>%
    mutate(no = 1:n()) %>%
    mutate(tags = highway) %>%
    mutate(tags = ifelse(is.na(junction), tags, sprintf("%s,%s", tags, junction))) %>%
    mutate(tags = ifelse(is.na(oneway), tags, sprintf("%s,%s", tags, oneway))) %>%
    dplyr::select(no, id, tags, node1, node9, name)
#  misc_print(df1)
  if (nrow(ways.df) < 2) {
    confess("pas assez de voies")
  }
#  glimpse(ways.df); stop("fghjklm")
  ways.df <- ways.df %>%
    mutate(no = 1:n()) %>%
    rowwise() %>%
    mutate(wRP = grepl("roundabout|circular", junction)) %>%
    mutate(wRP = ifelse(node1 == node9, TRUE, FALSE)) %>%
    mutate(wOW = grepl("yes", oneway)) %>%
    mutate(wOW = ifelse(grepl("yes", oneway_bus), TRUE, FALSE)) %>%
    mutate(name = ifelse(is.na(name), ref_y, name)) %>%
    mutate(name = sprintf("%s(%s)", name, nb_nodes)) %>%
    dplyr::select(no, id, node1, node9, wRP, wOW, name, nodes, wkt)
#    filter(! is.na(junction)) %>%
#    glimpse()
  nAvant <- "-1"
  carp("****id: %s", id)
  for (i in 2:nrow(ways.df)) {
    if (i > 2) {
      glimpse(nodes.list)
      if (anyNA(nodes.list) > 0) {
        confess("na")
      }
    }
    if (i > 30) {
#      break
    }
    Carp("***i: %s %s wRP: %s nAvant: %s", i, ways.df[i, "name"], ways.df[i, "wRP"], nAvant)
    x <- unname(unlist(ways.df[i, "nodes"]))
    x1 <- x[2:length(x)]
    x9 <- rev(x[1:(length(x)-1)])

#    misc_print(ways.df[c(i -1, i), ])

# le début du tracé
    if (i == 2) {
      y <- unname(unlist(ways.df[i - 1, "nodes"]))
      if (ways.df[i, "wRP"] == TRUE) {
        Carp("rond-point i=2")
        xn <- x[1:length(x)-1]
        j <- match(ways.df[i - 1 , "node9"], xn)
        if (! is.na(j)) {
          Carp("node9 j: %s xn: %s", j, length(xn))
          nodes.list <- y
          if (j < length(xn)) {
            nodes <- c(xn[(j + 1):length(xn)], xn)
          } else {
            nodes <- xn
          }
          next
        }
        j <- match(ways.df[i - 1 , "node1"], xn)
        if (! is.na(j)) {
          Carp("node1 j: %s xn: %s", j, length(xn))
          nodes.list <- list.reverse(y)
          if (j < length(xn)) {
            nodes <- c(xn[(j + 1):length(xn)], xn)
          } else {
            nodes <- xn
          }
          next
        }
        Carp( "gap avant rond-point")
        gaps.df <- ways.df[c(i -1, i), ]
        gaps.df$r_id <- id
        gaps.df$err <- "un rond-point avant"
        break
      }
      if (ways.df[i - 1 , "node1"] == ways.df[i , "node1"] ) {
        if (ways.df[i - 1, "wOW"]  == TRUE) {
          Carp( "sens unique i-1")
          gaps.df <- ways.df[c(i -1, i), ]
          gaps.df$r_id <- id
          gaps.df$err <- "sens unique i-1"
          break
        }
        nAvant <- ways.df[i , "node9"]
        Carp("**** 9-1 1-9")
        nodes.list <- append(list.reverse(y), x1)
        next;
      }
      if (ways.df[i - 1 , "node1"] == ways.df[i , "node9"] ) {
        if (ways.df[i, "wOW"]  == TRUE) {
          Carp( "sens unique i")
          gaps.df <- ways.df[c(i -1, i), ]
          gaps.df$r_id <- id
          gaps.df$err <- "sens unique i"
          break
        }
        nAvant <- ways.df[i , "node1"]
        Carp("**** 9-1 9-1")
        nodes.list <- append(list.reverse(y), x9)
        next;
      }
      if (ways.df[i - 1 , "node9"] == ways.df[i , "node1"]) {
        nAvant <- ways.df[i , "node9"]
        Carp("**** 1-9 1-9")
        nodes.list <- c(y, x1)
        next;
      }
      if (ways.df[i - 1 , "node9"] == ways.df[i , "node9"]) {
        if (ways.df[i, "wOW"]  == TRUE) {
          Carp( "sens unique i-1")
          gaps.df <- ways.df[c(i -1, i), ]
          gaps.df$r_id <- id
          gaps.df$err <- "sens unique i-1"
          break
        }
        nAvant <- ways.df[i , "node1"]
        Carp("**** 1-9 9-1")
        nodes.list <- append(y, x9)
        next;
      }
      gaps.df <- ways.df[c(i -1, i), ]
      gaps.df$r_id <- id
      gaps.df$err <- "gap"
      break
    }

# la suite
    if (i > 2) {
# c'est un rond-point ?
      if (ways.df[i , "wRP"] == TRUE) {
# précédé d'un rond-point ?
        if (ways.df[i - 1 , "wRP"] == TRUE) {
          Carp("deux ronds-points")
          gaps.df <- ways.df[c(i -1, i), ]
          gaps.df$r_id <- id
          gaps.df$err <- "deux ronds-points"
          break
        }
        Carp("rond-point")
        xn <- x[1:length(x)-1]
        j <- match(nAvant, xn)
        if (is.na(j)) {
          Carp( "gap avant rond-point")
          next
        }
        Carp("rond-point j: %s xn: %s", j, length(xn))
        if (j < length(xn)) {
          nodes <- c(xn[(j + 1):length(xn)], xn)
        } else {
          nodes <- xn
        }
        next
      }
# précédé d'un rond-point ?
      if (ways.df[i - 1 , "wRP"] == TRUE) {
        Carp("rond-point i-1")
        j <- match(ways.df[i , "node1"], nodes)
        if (! is.na(j)) {
          Carp("node1")
          nodes <- nodes[1:j]
          nodes.list <- c(nodes.list, nodes)
          nAvant <- ways.df[i , "node9"]
          next;
        }
        j <- match(ways.df[i , "node9"], nodes)
        if (! is.na(j)) {
          Carp("node9")
          nodes <- nodes[1:j]
          nodes.list <- c(nodes.list, nodes, x9)
          nAvant <- ways.df[i , "node1"]
          next;
        }
        gaps.df <- ways.df[c(i -1, i), ]
        gaps.df$r_id <- id
        gaps.df$err <- "un rond-point avant"
        break
      }
      if (nAvant == ways.df[i , "node1"]) {
        nAvant <- ways.df[i , "node9"]
        Carp("**** 1-9")
        nodes.list <- c(nodes.list, x1)
        next;
      }
      if (nAvant == ways.df[i , "node9"]) {
        if (ways.df[i, "wOW"]  == TRUE) {
          Carp("sens unique i")
          gaps.df <- ways.df[c(i -1, i), ]
          gaps.df$r_id <- id
          gaps.df$err <- "sens unique"
          break
        }
        nAvant <- ways.df[i , "node1"]
        Carp("**** 9-1")
        nodes.list <- c(nodes.list, x9)
        next;
      }

      gaps.df <- ways.df[c(i -1, i), ]
      gaps.df$r_id <- id
      gaps.df$err <- "gap"
      break
    }
  }
  if (nrow(gaps.df) > 0) {
    misc_print(gaps.df)
  }
  carp("fin id: %s gaps.df nrow: %s", id, nrow(gaps.df))
  glimpse(ways.df)
  ways.sf <- st_as_sf(ways.df, geometry = st_as_sfc(ways.df$wkt, crs = st_crs(4326))) %>%
    st_transform(2154)
#  print(nodes.list)
  nodes.df <- data.frame(id = nodes.list) %>%
    left_join(rc$nodes.df, by = c("id")) %>%
    glimpse()
  df21 <- nodes.df %>%
    filter(is.na(lat))
  if (nrow(df21) > 0) {
    glimpse(df21)
    confess("erruer node")
  }
  trace.sf <- nodes.df %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    mutate(ID = "toto") %>%
    group_by(ID) %>%
    dplyr::summarize(do_union = FALSE) %>%
    sf::st_cast("LINESTRING") %>%
    st_transform(2154) %>%
    glimpse()
  plot(st_geometry(trace.sf), add = FALSE, lwd = 5, col = "blue")
  plot(st_geometry(ways.sf), add = TRUE, lwd = 2, col = "black")
  dsn <- sprintf("%s/trace.geojson", josmDir)
  st_write(st_transform(trace.sf, 4326), dsn, delete_dsn = TRUE)
  carp("dsn: %s", dsn)
  return()
#
# pour savoir si les stations sont du bon côté
# https://github.com/r-spatial/sf/issues/1001
  ways.sf <- st_as_sf(ways.df, geometry = st_as_sfc(ways.df$wkt, crs = st_crs(4326))) %>%
    st_transform(2154)
  b50.sf <- ways.sf %>%
    st_buffer(-50, singleSide = T)
  plot(st_geometry(b50.sf), add = TRUE, border = "red")
  return(invisible(gaps.df))
}
#
# source("geo/scripts/transport.R");osm_routes_refs(reseau = Reseau, force = TRUE)
osm_routes_refs <- function(reseau = "star", force = TRUE) {
  library(tidyverse)
  library(data.table)
  library(sf)
  library(janitor)
  carp()
  config_xls(reseau)
  dsn <- osm_relations_route_bus_csv(force = force)
  df <- fread(dsn, encoding = "UTF-8") %>%
    as.data.table() %>%
    clean_names() %>%
    glimpse()
  df1 <- df %>%
    filter(is.na(ref_network)) %>%
    glimpse()
  tex_df2kable(df1, suffixe = "absent", longtable = TRUE)
  gtfs.df <- transport_lire("gtfs_routes_stops") %>%
    glimpse()
  df2 <- df %>%
    filter(ref_network %notin% gtfs.df$ref_network) %>%
    glimpse()
  tex_df2kable(df2, suffixe = "inconnu", longtable = TRUE)
  texFic <- sprintf("%s/%s", imagesDir, "osm_routes_refs.tex")
  TEX <- file(texFic)
  tex <- sprintf("<!-- coding: utf-8 -->
%s ", format(Sys.time(), "%d/%m/%Y %H:%M:%S"))
#
# le template tex
  dsn <- sprintf("%s/routes_refs_tpl.tex", tplDir)
  template <- readLines(dsn) %>%
    glimpse()
#  stop("****")
  df1 <- df %>%
    dplyr::select(id, timestamp, user, ref_network) %>%
    arrange(ref_network)
  tex_df2kable(df1, suffixe = "lst", longtable = TRUE)
  for (i in 1:nrow(df)) {
    carp("i: %s/%s", i, nrow(df))
    id <-  df[i, "id"]
    ref <- df[[i, "ref_network"]]
    dsn <- sprintf("%s/osm_routes_refs_%s.pdf", imagesDir, id)
#    glimpse(df[i, ]);stop("****")
# les données osm
    rc <- osmapi_get_members_platform(ref = id, force = TRUE)
    route.df <- rc$relation %>%
      glimpse() %>%
      mutate(first_stop = sub(";.*$", "", stops_id)) %>%
      mutate(last_stop = sub("^.*;", "", stops_id))
    df1 <- rbind(colnames(route.df), route.df) %>%
      t() %>%
      as.data.frame()
    rownames(df1) <- NULL
    t <- tex_df2table(df1, suffixe = id, dossier = "images", entete = "lp{4cm}p{15cm}")
# les données gtfs
    df2 <- gtfs.df %>%
      filter(ref_network == !!ref)
    df3 <- df2 %>%
      dplyr::select(first_stop, last_stop)
    tex_df2table(df3, suffixe = id, dossier = "stop", entete = "lp{4cm}p{4cm}")
    if (1 == 2) {
      t2 <- ""
      for (i2 in 1:nrow(df2)) {
        df3 <- df2[i2, ]
        df3 <- rbind(colnames(df3), df3) %>%
          t() %>%
          as.data.frame()
        rownames(df3) <- NULL
        t <- tex_df2table(df3, suffixe = id, dossier = "gtfs", entete = "lp{4cm}p{15cm}")
        t2 <- append(t2, t)
      }
      dsn <- tex_dsn(suffixe = id, dossier = "gtfs")
      carp("dsn: %s", dsn)
      f <- file(dsn, open="w")
      writeLines(t2, f)
      close(f)
    }
    tpl <- tex_df2tpl(df, i, template)
    tpl <- escapeLatexSpecials(tpl)
    tex <- append(tex, tpl)
    next
    break
    osm.sf <- osmapi_get_ways(ref = id, force = force, force_osm = FALSE) %>%
      st_transform(2154)
    add <- FALSE
    if (! st_is_empty(osm.sf[1,]) ) {
      plot(st_geometry(osm.sf), col = "blue", lwd = 3)
      add <- TRUE;
    }
    dsn <- dev2pdf(suffixe = id, dossier = "images")
  }
  write(tex, file = TEX, append = FALSE)
  close(TEX)
  carp("texFic: %s", texFic)
}