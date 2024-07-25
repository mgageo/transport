# <!-- coding: utf-8 -->
#
# utilisation des données d'OpenStreetMap
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
# auteur : Marc Gauthier
#
# https://wiki.openstreetmap.org/wiki/API_v0.6
#
# source("geo/scripts/transport.R"); res <- osmapi_get_node_ways(node = "1123340557")
osmapi_get_node_ways <- function(node, force = FALSE) {
  carp("node: %s", node)
  library(rjson)
  query <- sprintf("https://www.openstreetmap.org/api/0.6/node/%s/ways.json", node)
  res <- rjson::fromJSON(file = query)
  return(invisible(res))
}
# source("geo/scripts/transport.R"); res <- osmapi_get_objet(ref = "11920346"); print(res)
osmapi_get_objet <- function(ref, type = "relation", force = FALSE) {
  library(rjson)
  dsn <- sprintf("%s/%s_%s.json", osmDir, type, ref)
#  carp("dsn: %s force: %s", dsn, force)
  if (! file.exists(dsn) || force == TRUE) {
    query <- sprintf("https://www.openstreetmap.org/api/0.6/%s/%s.json", type, ref)
    res <- httr::GET(url = query, encoding = "UTF-8", type = "application/json", httr::write_disk(dsn, overwrite = TRUE))
    stop_for_status(res)
  }
  if (2 == 1) {
    tx  <- readLines(dsn)
    tx2  <- stringi::stri_enc_toascii(tx)
    writeLines(tx2, con = dsn)
  }
  res <- rjson::fromJSON(file = dsn)
  # res <- jsonlite::fromJSON(dsn)
  return(invisible(res))
}
# source("geo/scripts/transport.R"); res <- osmapi_get_object_full(ref = "4021035"); print(res)
osmapi_get_object_full <- function(ref, type = "relation", force = FALSE) {
  library(readr)
  library(tidyverse)
  dsn <- sprintf("%s/%s_%s_full.osm", osmDir, type, ref)
  carp("dsn: %s", dsn)
  if (! file.exists(dsn) || force == TRUE) {
    url <- sprintf("https://www.openstreetmap.org/api/0.6/%s/%s/full", type, ref)
# , verbose()
    res <- httr::GET(url = url, encoding = "UTF-8", type = "application/json", httr::write_disk(dsn, overwrite = TRUE))
    stop_for_status(res)
  }
  return(invisible(dsn))
}
osmapi_get_object_full_xml <- function(ref, type = "relation", force = FALSE, force_osm = TRUE) {
  dsn_rds <- sprintf("%s/%s_%s_full.Rds", osmDir, type, ref)
  if (file.exists(dsn_rds) & force == FALSE) {
    doc <- readRDS(dsn_rds)
  } else {
    dsn <- osmapi_get_object_full(ref = ref, type = type, force = force_osm)
    doc <- read_xml(dsn)
    saveRDS(doc, dsn_rds)
  }
  return(invisible(doc))
}
# source("geo/scripts/transport.R"); res <- osmapi_get_object_relations(ref = "269138982"); print(res)
osmapi_get_object_relations <- function(ref, type = "node", force = FALSE) {
  library(readr)
  library(tidyverse)
  dsn <- sprintf("%s/%s_%s_relations.osm", osmDir, type, ref)
  carp("dsn: %s", dsn)
  if (! file.exists(dsn) || force == TRUE) {
    url <- sprintf("https://www.openstreetmap.org/api/0.6/%s/%s/relations", type, ref)
# , verbose()
    res <- httr::GET(url = url, encoding = "UTF-8", type = "application/json", httr::write_disk(dsn, overwrite = TRUE))
    stop_for_status(res)
  }
  return(invisible(dsn))
}
osmapi_object_txt <- function(ref, type = "relation", force = FALSE) {
  library(readr)
  library(tidyverse)
  library(httr)
  dsn <- sprintf("%s/%s_%s.osm", osmDir, type, ref)
  if (! file.exists(dsn) | force == TRUE) {
    carp("dsn: %s", dsn)
    url <- sprintf("https://www.openstreetmap.org/api/0.6/%s/%s", type, ref)
    res <- httr::GET(url = url, encoding = "UTF-8", type = "application/xml", httr::write_disk(dsn, overwrite = TRUE))
    txt <- content(res, "text")
  } else {
    txt <- readr::read_file(dsn)
  }
  return(invisible(txt))
}
osmapi_object_full <- function(ref, type = "relation", force = FALSE) {
  library(readr)
  library(tidyverse)
  library(httr)
  dsn <- sprintf("%s/%s_%s_full.osm", osmDir, type, ref)
  carp("dsn: %s", dsn)
  if (! file.exists(dsn) | force == TRUE) {
    url <- sprintf("https://www.openstreetmap.org/api/0.6/%s/%s/full", type, ref)
    res <- httr::GET(url = url, encoding = "UTF-8", type = "application/json", httr::write_disk(dsn, overwrite = TRUE))
    stop_for_status(res)
  }
  return(invisible(dsn))
}
osmapi_object_history <- function(ref, type = "relation", force = FALSE) {
  library(tidyverse)
  library(httr)
  dsn <- sprintf("%s/%s_%s_history.osm", osmDir, type, ref)
  carp("dsn: %s", dsn)
  if (! file.exists(dsn) | force == TRUE) {
    url <- sprintf("https://www.openstreetmap.org/api/0.6/%s/%s/history", type, ref)
    res <- httr::GET(url = url, encoding = "UTF-8", type = "application/xml", httr::write_disk(dsn, overwrite = TRUE))
    stop_for_status(res)
  }
  return(invisible(dsn))
}
#
# il y a plusieurs versions des nodes/ways/relations
osmapi_object_history_version <- function(dsn, ref, type = "relation", version, force = FALSE) {
  library(readr)
  library(tidyverse)
  library(httr)
  library(mapsf)
  dsn_rds <- sprintf("%s/osmapi_object_history_version_%s.rds", osmDir, ref)
  if (file.exists(dsn_rds) && force == FALSE) {
    rc <- readRDS(dsn_rds)
    return(invisible(rc))
  }
  carp("dsn: %s", dsn)
  doc <- read_xml(dsn)
#
# les nodes
  carp("les nodes")
  nodes <- xml2::xml_find_all(doc, ".//node")
  carp("nodes nb: %s", length(nodes))
  nodes.df <- osmapi_objects_tags(nodes) %>%
    dplyr::select(id, version, lat, lon, name, public_transport, matches("^(bus|ref)")) %>%
    mutate(v = as.numeric(version)) %>%
    arrange(id, v) %>%
    group_by(id) %>%
    summarise_all(last) %>%
    mutate(lat = as.numeric(lat)) %>%
    mutate(lon = as.numeric(lon)) %>%
    glimpse()
#  misc_print(nodes.df); stop("*****")

#
# les ways
  carp("les ways")
  ways <- xml2::xml_find_all(doc, ".//way")
  carp("ways nb: %s", length(ways))
  ways.df <- osmapi_objects_tags(ways) %>%
    dplyr::select(id, version, matches("^(name|highway|junction|oneway|oneway\\:bus|ref)")) %>%
    mutate(v = as.numeric(version)) %>%
    arrange(id, v) %>%
    group_by(id) %>%
    summarise_all(last) %>%
    (function(.df){
      cls <- c("name", "highway", "junction", "oneway", "oneway:bus", "ref", "nodes")
      .df[cls[!(cls %in% colnames(.df))]] = NA
      return(.df)
    }) %>%
    glimpse()
#  stop("*****")
#  print(xml_structure(ways[[1]]))
  ls.v <- c()
  for (i in 1:nrow(ways.df)) {
    carp("i: %s/%s", i, nrow(ways.df))
    nodes <- xml_find_all(ways[[i]], ".//nd") %>%
      xml_attr("ref")
    df1 <- data.frame(id = nodes) %>%
      left_join(nodes.df, by = ("id")) %>%
      mutate(point = sprintf("%s %s", lon, lat)) %>%
      summarize(points = paste(point, collapse = ",")) %>%
      mutate(wkt = sprintf("LINESTRING(%s)", points))
     ways.df[i, "wkt"] <- df1[[1, "wkt"]]
#    ls1 <- st_linestring(matrix(c(df1$lon, df1$lat), nrow(df1), 2))
#    ways.df[i, "ls"][[1]] <- list(ls1)
#    ls.v <- append(ls.v, ls1)
# https://ryouready.wordpress.com/2016/07/18/populating-data-frame-cells-with-more-than-one-value/
    ways.df[i, "nb_nodes"] <- length(nodes)
    ways.df$nodes[i] <- list(nodes)
    ways.df[i, "node1"] <- nodes[1]
    ways.df[i, "node9"] <- nodes[length(nodes)]
#    mga <<- nodes
#    glimpse(nodes); stop("*****")
  }
#  glimpse(ways.df); stop("*****")
  ways.sf <- st_as_sf(ways.df, geometry = st_as_sfc(ways.df$wkt, crs = 4326)) %>%
    st_transform(2154) %>%
    glimpse()
#
# les relations
  relations <- xml2::xml_find_all(doc, ".//relation")
  carp("relations nb: %s", length(relations))
  relations.df <- data.frame()
  i_relation <- 0
  for (relation in relations) {
    carp("les attributs de la relation")
    relation.df <- xml_attrs(relation) %>%
      as_tibble_row() %>%
      glimpse()
    if (relation.df$version[1] %notin% c("10", "11")) {
      next
    }
    i_relation <- i_relation + 1
#  print(as.character(relation))
    members <- xml2::xml_find_all(relation, ".//member")
    carp("members nb: %s", length(members))
    members.list <- list()
    for (attr in c("type", "ref", "role")) {
      members.list[[attr]] <- members %>% xml_attr(attr)
    }
    members.df <- as_tibble(members.list) %>%
      glimpse()
#
# on peut s'attaquer au rapprochement pour les nodes stop/platform
    df1 <- members.df %>%
      filter(type == "node") %>%
      left_join(nodes.df, by = c("ref" = "id")) %>%
      clean_names()
    df2 <- df1 %>%
      filter(grepl("platform", role)) %>%
      glimpse()
    platforms <- paste(df2$name, collapse = ";")
#    carp("platforms: %s", platforms); stop("*****")
    carp("les arrêts")
    df3 <- df1 %>%
      filter(grepl("(stop|platform)", role))
#
# on peut s'attaquer au rapprochement pour les ways
#  misc_print(members.df)
    df11 <- members.df %>%
      filter(role == "" & type == "way") %>%
#    glimpse();stop("*****")
      left_join(ways.sf, by = c("ref" = "id")) %>%
      rename(id = ref) %>%
      clean_names()
    sf11 <- st_as_sf(df11, geometry = st_as_sfc(df11$wkt, crs = 4326)) %>%
      mutate(way_no = row_number()) %>%
      st_transform(2154) %>%
      glimpse()
    rc <- list("platforms_name" = platforms, "members.df" <- df3, "ways.sf" = sf11)
    couleur <- "red"
    largeur <- 1
    if (i_relation == 1) {
      mf_init(rc$ways.sf, expandBB = c(0.1, 0.1, 0.1, 0.1))
      couleur <- "blue"
      largeur <- 4
    }
    mf_map(x = rc$ways.sf, col = couleur, lwd = largeur, add = TRUE)
    next
    relation.df$rc <- list()
    relation.df$rc[1] <- rc
    relations.df <- bind_rows(relations.df, relation.df)
  }
  return(invisible())
#  glimpse(relations.df);stop("*****")
  saveRDS(relations.df, dsn_rds)
  carp("mapsf")
  rc <- relations.df[1, "rc"]
  rc <- relations.df[2, "rc"]
  mf_map(x = rc$ways.sf, col = "red", lwd = 3, add = TRUE)
  return(invisible())
}

#
# récupération des coordonnées des arrêts d'une relation transport
# version json
# source("geo/scripts/transport.R"); osmapi_get_members_platform(ref = "3184038", force = TRUE)
osmapi_get_members_platform_json <- function(ref = "11920346", type = "relation", force = FALSE) {
  library(readr)
  library(tidyverse)
  carp("ref: %s", ref)
  json1.list <- osmapi_get_objet(ref = ref, type = type, force)
  members.list <- json1.list$elements[[1]]$members
  if (length(members.list) == 0 ) {
    confess("**** pas de membres")
  }
  carp("length(members.list): %s", length(members.list))
  df1 <- do.call(rbind, lapply(members.list, data.frame))
  df2 <- df1 %>%
    filter(type == "node")
  df4 <- tibble()
  for (i in 1:nrow(df2)) {
    ref <- df2[i, "ref"]
    res <- osmapi_get_objet(ref = ref, type = "node", force = force);
    elements <- res$elements[[1]]

#    glimpse(res$elements[[1]]); stop("*****")
    tags <- elements$tags
    tags.df <- as_tibble(tags) %>%
      rename_all(~ paste0("tags.", .x))
    elements$tags <- NULL
    elements$type <- NULL
    elements.df <- as_tibble(elements)
    df3 <- cbind(df2[i, ], elements.df[1, ], tags.df[1, ])
    df4 <- bind_rows(df4, df3)
  }
  return(invisible(df4))
}
#
# récupération des coordonnées des arrêts d'une relation transport
# version xml
# source("geo/scripts/transport.R"); rc <- osmapi_get_members_platform(ref = "3184038", force = FALSE)
osmapi_get_members_platform <- function(ref = "11920346", type = "relation", force = FALSE, force_osm = FALSE) {
  library(readr)
  library(tidyverse)
  library(janitor)
  library(xml2)
  carp("ref: %s", ref)
  dsn_rds <- sprintf("%s/%s_%s_members_platform.rds", osmDir, type, ref)
  if (file.exists(dsn_rds) && force == FALSE) {
    rc <- readRDS(dsn_rds)
    return(invisible(rc))
  }
  dsn <- osmapi_get_object_full(ref = ref, type = type, force = force)
  doc <- read_xml(dsn)
  relations <- xml2::xml_find_all(doc, "//relation")
  carp("relations nb: %s", length(relations))
  nodes <- xml2::xml_find_all(doc, "//node")
  carp("nodes nb: %s", length(nodes))
#
# analyse de la relation
  relation <- relations[[1]]
  relation.df <- xml_attrs(relation) %>%
    as_tibble_row()
#  print(as.character(relation))
  members <- xml2::xml_find_all(relation, "//member")
  carp("members nb: %s", length(members))
  members.list <- list()
  for (attr in c("type", "ref", "role")) {
    members.list[[attr]] <- members %>% xml_attr(attr)
  }
  members.df <- as_tibble(members.list)
#  misc_print(members.df); stop("*****")
  tags <- xml2::xml_find_all(relation, "./tag")
#  carp("tags nb: %s", length(tags))
#  print(as.character(tags[[1]]))
  tags.list <- list()
  for (attr in c("k", "v")) {
    tags.list[[attr]] <- tags %>% xml_attr(attr)
  }
  tags.df <- as_tibble(tags.list) %>%
    pivot_wider(names_from = k, values_from = v)
  relation.df <- cbind(relation.df, tags.df)
#
# analyse des nodes
# ils doivent avoir des tags, sauf certains cas
  nodes.df <- tibble()
  nodes <- xml2::xml_find_all(doc, "//node")
  carp("nodes nb: %s", length(nodes))
  for (node in nodes) {
    node.df <- xml_attrs(node) %>%
      as_tibble_row()
    tags <- xml2::xml_find_all(node, "./tag")
#    carp("tags nb: %s", length(tags))
    if (length(tags) > 0) {
      tags.list <- list()
      for (attr in c("k", "v")) {
        tags.list[[attr]] <- tags %>% xml_attr(attr)
      }
      tags.df <- as_tibble(tags.list) %>%
        pivot_wider(names_from = k, values_from = v)
#    glimpse(tags.df)
      node.df <- cbind(node.df, tags.df)
    }
    nodes.df <- bind_rows(nodes.df, node.df)
  }
  k_ref <- Config[[1, "k_ref"]]
#  carp("k_ref: %s", k_ref)
  nodes.df <- nodes.df %>%
#    filter(! is.na(`ref:FR:STAR`)) %>%
    dplyr::select(id, name, k_ref = !!k_ref, lat, lon)
#
# on peut enfin s'attaquer au rapprochement
  df1 <- members.df %>%
    left_join(nodes.df, by = c("ref" = "id")) %>%
    clean_names()
  df2 <- df1 %>%
    filter(grepl("platform", role))
  relation.df[1, "stops_id"] <- paste(df2$k_ref, collapse = ";")
  relation.df[1, "stops_name"] <- paste(df2$name, collapse = ";")
  saveRDS(relation.df, dsn_rds)
  carp("les arrêts")
  df3 <- df1 %>%
    filter(grepl("(stop|platform)", role)) %>%
    filter(type == "node") %>%
    mutate(id = !!ref)
  rc <- list("relation" = relation.df, "members" = df3)
  saveRDS(rc, dsn_rds)
  return(invisible(rc))
}
# source("geo/scripts/transport.R"); osmapi_get_tags(ref = "14704778", force = FALSE) %>% glimpse()
osmapi_get_tags <- function(ref = "11920346", type = "relation", force = FALSE) {
  library(readr)
  library(tidyverse)
  library(janitor)
  library(xml2)
  carp("ref: %s", ref)
  dsn_rds <- sprintf("%s_%s_get_tags", type, ref)
  relation.df <- misc.lire(dsn_rds)
  if (! is.logical(relation.df) & force == FALSE) {
    return(invisible(relation.df))
  }
  dsn <- osmapi_get_object_full(ref = ref, type = type, force = force)
  doc <- read_xml(dsn)
  relations <- xml2::xml_find_all(doc, "//relation")
#  carp("relations nb: %s", length(relations))
#
# analyse de la relation
  relation <- relations[[1]]
  relation.df <- xml_attrs(relation) %>%
    as_tibble_row()
#  print(as.character(relation))
 tags <- xml2::xml_find_all(relation, "./tag")
#  carp("tags nb: %s", length(tags))
#  print(as.character(tags[[1]]))
  tags.list <- list()
  for (attr in c("k", "v")) {
    tags.list[[attr]] <- tags %>% xml_attr(attr)
  }
  tags.df <- as_tibble(tags.list) %>%
    pivot_wider(names_from = k, values_from = v)
  relation.df <- cbind(relation.df, tags.df)
  misc.ecrire(relation.df, dsn_rds)
  return(invisible(relation.df))
}

# source("geo/scripts/transport.R"); osmapi_get_ways(ref = "8356872", force = TRUE)
osmapi_get_ways <- function(ref = "11920346", type = "relation", force = FALSE, force_osm = FALSE) {
  library(httr)
  library(readr)
  library(tidyverse)
  library(janitor)
  library(xml2)
  carp("ref: %s", ref)
  dsn_rds <- sprintf("%s/%s_%s_full_sf.rds", osmDir, type, ref)
  if (file.exists(dsn_rds) && force == FALSE) {
    nc <- readRDS(dsn_rds)
    return(invisible(nc))
  }
  dsn <- osmapi_get_object_full(ref = ref, type = type, force = force_osm)
# https://portailsig.org/content/recuperer-des-donnees-openstreetmap-gdalogr.html
  ini_new <- "#configuration osm import
[multilinestrings]
osm_id=yes
osm_version=yes
osm_timestamp=yes
osm_uid=yes
osm_user=yes
osm_changeset=yes
other_tags=yes
attributes=name,ref,ref:network,gtfs:shape_id,from,to,colour,text_colour,description,operator,route,type,network,public_transport:version
"
  writeLines(ini_new, "ini_new.ini")
  nc <- st_read(dsn, "multilinestrings", options = "CONFIG_FILE=ini_new.ini", quiet = TRUE) %>%
    glimpse()
  saveRDS(nc, dsn_rds)
  return(invisible(nc))
}
#
# les ways qui réfèrent un node
osmapi_get_node_ways <- function(node, force = FALSE) {
  carp("node: %s", node)
  library(httr)
  library(tidyverse)
  library(rjson)
  query <- sprintf("https://www.openstreetmap.org/api/0.6/node/%s/ways.json", node)
  res <- rjson::fromJSON(file = query)
  return(invisible(res))
}
#
# récupération des informations d'une relation transport
# version xml
# https://rdrr.io/github/mdlincoln/bigosm/src/R/parse.R
# source("geo/scripts/transport.R"); rc <- osmapi_get_transport(ref = "2422224", force = TRUE)
#
# bug, une platform de type way
# source("geo/scripts/transport.R"); rc <- osmapi_get_transport(ref = "2171048", force = TRUE) %>% glimpse()
osmapi_get_transport <- function(ref = "11920346", force = FALSE, force_osm = FALSE) {
  library(readr)
  library(tidyverse)
  library(janitor)
  library(xml2)
  library(httr)
  carp("ref: %s", ref)
  dsn_rds <- sprintf("%s/transport_%s.rds", osmDir, ref)
  if (file.exists(dsn_rds) && force == FALSE) {
    rc <- readRDS(dsn_rds)
    return(invisible(rc))
  }
  dsn <- osmapi_get_object_full(ref = ref, type = "relation", force = force_osm)
  doc <- read_xml(dsn)
  relations <- xml2::xml_find_all(doc, "./relation")
  carp("relations nb: %s", length(relations))
  nodes <- xml2::xml_find_all(doc, "./node")
  carp("nodes nb: %s", length(nodes))
  ways <- xml2::xml_find_all(doc, "./way")
  carp("ways nb: %s", length(ways))
#
# les nodes
  carp("les nodes")
  nodes.df <- osmapi_objects_tags(nodes) %>%
# pas de tag name, public_transport
    dplyr::bind_rows(dplyr::tibble(name = character(), public_transport = character())) %>%
    dplyr::select(id = "@id", lat = "@lat", lon = "@lon", name, public_transport, matches("^(bus|ref)")) %>%
    mutate(lat = as.numeric(lat)) %>%
    mutate(lon = as.numeric(lon)) %>%
    glimpse()
#
# les ways
  carp("les ways")
  ways.df <- osmapi_objects_tags(ways)
  if (nrow(ways.df) > 0) {
    ways.df <- ways.df %>%
      dplyr::select(id = "@id", matches("^(name|public_transport|bus|ref|highway|junction|oneway|oneway\\:bus|ref)")) %>%
      glimpse() %>%
      (function(.df){
        cls <- c("name", "highway", "junction", "oneway", "oneway:bus", "ref")
        .df[cls[!(cls %in% colnames(.df))]] = NA
        return(.df)
      }) %>%
      glimpse()
#  stop("*****")
#  print(xml_structure(ways[[1]]))
    ls.v <- c()
    for (i in 1:nrow(ways.df)) {
      nodes <- xml_find_all(ways[[i]], "./nd") %>%
        xml_attr("ref")
      df1 <- data.frame(id = nodes) %>%
        left_join(nodes.df, by = ("id")) %>%
        mutate(point = sprintf("%s %s", lon, lat)) %>%
        summarize(points = paste(point, collapse = ",")) %>%
        mutate(wkt = sprintf("LINESTRING(%s)", points))
       ways.df[i, "wkt"] <- df1[[1, "wkt"]]
#    ls1 <- st_linestring(matrix(c(df1$lon, df1$lat), nrow(df1), 2))
#    ways.df[i, "ls"][[1]] <- list(ls1)
#    ls.v <- append(ls.v, ls1)
# https://ryouready.wordpress.com/2016/07/18/populating-data-frame-cells-with-more-than-one-value/
      ways.df[i, "nb_nodes"] <- length(nodes)
      ways.df[i, "nodes"][[1]] <- list(nodes)
      ways.df[i, "node1"] <- nodes[1]
      ways.df[i, "node9"] <- nodes[length(nodes)]
#    mga <<- nodes
#    glimpse(nodes); stop("*****")
  }
#  glimpse(ways.df); stop("*****")
    ways.sf <- st_as_sf(ways.df, geometry = st_as_sfc(ways.df$wkt, crs = 4326))
    cc.df <- st_coordinates(st_centroid(ways.sf)) %>%
    dplyr::as_tibble() %>%
      dplyr::select(X, Y) %>%
      dplyr::rename(lon = X, lat = Y)
    ways.df <- cbind(ways.df, cc.df)
    ways.sf <- ways.sf %>%
      st_transform(2154)
  }
#
# la relation
  relation <- relations[[1]]
  relation.df <- osmapi_objects_tags(relations) %>%
    glimpse()
#  print(as.character(relation))
  members <- xml2::xml_find_all(relation, "//member")
  carp("members nb: %s", length(members))
  members.list <- list()
  for (attr in c("type", "ref", "role")) {
    members.list[[attr]] <- members %>% xml_attr(attr)
  }
  df10 <- as_tibble(members.list) %>%
    mutate(member_no = row_number()) %>%
    glimpse()
#
# on peut enfin s'attaquer au rapprochement pour les nodes
  df11 <- df10 %>%
    filter(type == "node") %>%
    left_join(nodes.df, by = c("ref" = "id"))
  df12 <- df10 %>%
    filter(type == "way") %>%
    left_join(ways.df, by = c("ref" = "id")) %>%
    dplyr::select(-nb_nodes, -node1, -node9, -nodes)
  df1 <- bind_rows(df11, df12)
  df2 <- df1 %>%
    filter(grepl("platform", role))
  relation.df[1, "stops_name"] <- paste(df2$name, collapse = ";")
  saveRDS(relation.df, dsn_rds)
  carp("les arrêts")
  members.df <- df1 %>%
    filter(grepl("(stop|platform)", role)) %>%
    arrange(member_no)
#  glimpse(df3);stop("****")
#
# on peut enfin s'attaquer au rapprochement pour les ways
  if (nrow(ways.df) > 0) {
    df11 <- df10 %>%
      filter(role == "" & type == "way") %>%
#    glimpse();stop("*****")
      left_join(ways.sf, by = c("ref" = "id")) %>%
      rename(id = ref)
    sf11 <- st_as_sf(df11, geometry = st_as_sfc(df11$wkt, crs = 4326)) %>%
      mutate(way_no = row_number()) %>%
      st_transform(2154) %>%
      glimpse()
  } else {
    sf11 <- tribble()
  }
  rc <- list("relation" = relation.df, "members.df" = members.df, "ways.sf" = sf11, "nodes.df" = nodes.df)
  saveRDS(rc, dsn_rds)
  return(invisible(rc))
}
# source("geo/scripts/transport.R"); osmapi_objects_get_tags(dsn = "d:/web.var/TRANSPORT/STAR/OSM/node_7883832201_relations.osm", force = FALSE)
# source("geo/scripts/transport.R"); osmapi_objects_get_tags(dsn = "d:/web.var/TRANSPORT/STAR/OSM/node_269138982_relations.osm", force = FALSE)
osmapi_objects_get_tags <- function(dsn = "d:/web.var/TRANSPORT/STAR/OSM/node_7883832201_relations.osm", force = FALSE) {
  library(readr)
  library(tidyverse)
  library(janitor)
  library(xml2)
  osm <- read_file(dsn)
# les caractères exotiques
  osm <- str_replace_all(osm, "\\<U\\+[0-9a-fA-F]{4}\\>", "?")
#  write(osm, "d:osm.txt");
  doc <- read_xml(osm)
  objects <- xml2::xml_children(doc)
  carp("objects nb: %s", length(objects))
#  objects <- xml2::xml_find_all(doc, xpath = "//node")
#  carp("objects nb: %s", length(objects))
  objects.df <- data.frame()
  if (length(objects) == 0) {
    return(invisible(objects.df))
  }
  objects.df <- osmapi_objects_tags(objects)
  return(invisible(objects.df))
}
#
# les attrs et tags
osmapi_objects_tags <- function(objects) {
#  carp("objets : %s", length(objects))
  objects.df <- data.frame()
  for (object in objects) {
    object.df <- xml_attrs(object) %>%
      as_tibble_row() %>%
      rename_with(.fn = function(.x){paste0("@", .x)})
#    Carp("id: %s", object.df[[1, "@id"]])
    tags <- xml2::xml_find_all(object, "./tag")
    if (length(tags) > 0) {
#    carp("tags nb: %s", length(tags))
      tags.list <- list()
      for (attr in c("k", "v")) {
        tags.list[[attr]] <- tags %>% xml_attr(attr)
      }
      tags.df <- as_tibble(tags.list) %>%
        pivot_wider(names_from = k, values_from = v)
#    glimpse(tags.df)
      object.df <- cbind(object.df, tags.df) %>%
      mutate(osm_type = xml_name(object))
    }
    objects.df <- bind_rows(objects.df, object.df)
  }
#  carp("objets.df : %s", length(objects.df))
  return(invisible(objects.df))
}
#
# les members, cas des relations route_master
osmapi_objects_members <- function(objects) {
  objects.df <- data.frame()
  for (object in objects) {
    object.df <- xml_attrs(object) %>%
      as_tibble_row() %>%
      rename_with(.fn = function(.x){paste0("@", .x)})
    tags <- xml2::xml_find_all(object, "./tag")
    if (length(tags) > 0) {
#    carp("tags nb: %s", length(tags))
      tags.list <- list()
      for (attr in c("k", "v")) {
        tags.list[[attr]] <- tags %>% xml_attr(attr)
      }
      tags.df <- as_tibble(tags.list) %>%
        pivot_wider(names_from = k, values_from = v)
#    glimpse(tags.df)
      object.df <- cbind(object.df, tags.df) %>%
      mutate(osm_type = xml_name(object))
    }
    members <- xml2::xml_find_all(object, "./member")
    if (length(members) > 0) {
#      carp("members nb: %s", length(members))
      members.list <- list()
      for (attr in c("type", "ref", "role")) {
        members.list[[attr]] <- members %>% xml_attr(attr)
      }
      members.df <- as_tibble(members.list) %>%
        rename_with(.fn = function(.x){paste0("M", .x)})

      object.df <- cross_join(object.df, members.df) %>%
        mutate(osm_type = xml_name(object))
    }
    objects.df <- bind_rows(objects.df, object.df)
  }
  return(invisible(objects.df))
}
osmapi_objects_get_attrs <- function(dsn = "d:/web.var/TRANSPORT/STAR/OSM/node_7883832201_relations.osm", force = FALSE) {
  library(readr)
  library(tidyverse)
  library(janitor)
  library(xml2)
  osm <- read_file(dsn)
# les caractères exotiques
  osm <- str_replace_all(osm, "\\<U\\+[0-9a-fA-F]{4}\\>", "?")
#  write(osm, "d:osm.txt");
  doc <- read_xml(osm)
  objects <- xml2::xml_children(doc)
  carp("objects nb: %s", length(objects))
  objects.df <- data.frame()
  if (length(objects) == 0) {
    return(invisible(objects.df))
  }
  objects.df <- osmapi_objects_attrs(objects)
  return(invisible(objects.df))
}
#
# les attrs
osmapi_objects_attrs <- function(objects) {
  objects.df <- data.frame()
  for (object in objects) {
    object.df <- xml_attrs(object) %>%
      as_tibble_row()
    object.df$xml_name <- xml_name(object)
    objects.df <- bind_rows(objects.df, object.df)
  }
  return(invisible(objects.df))
}
# Cast the character vector list into a matrix and then convert to a data.frame
osmapi_attrs_to_df <- function(nodes) {
  raw_node_attrs <- xml_attrs(nodes)
  first_node <- raw_node_attrs[[1]]
  node_matrix <- matrix(unlist(raw_node_attrs), nrow = length(nodes),
                        ncol = length(first_node), byrow = TRUE,
                        dimnames = list(NULL, names(raw_node_attrs[[1]])))
  node_df <- as.data.frame(node_matrix, stringsAsFactors = FALSE)
  colnames(node_df) <- names(first_node)
  node_df
}

#
## la partie avec authentification (GET, POST et PUT)
# https://wiki.openstreetmap.org/wiki/API_v0.6
#
# pour vérifier la connexion
# source("geo/scripts/transport.R"); osmapi_api("user/details", methode = "GET")
#
# la version OAuth2 01/07/2024
# https://github.com/r-lib/httr2/blob/main/vignettes/articles/oauth.Rmd
# découvert après https://github.com/ropensci/osmapiR
api_url <- "https://www.openstreetmap.org"
osmapi_api <- function(path, xml = '', methode = "PUT", debug = FALSE) {
  library(httr2)
  carp("path: %s methode: %s", path, methode)
  dsn <- "d:/osmapi_url.txt"
  write(xml, dsn)
  url <- sprintf("%s/api/0.6/%s", api_url, path)
  carp("url: %s", url)
  auth_url <- "https://www.openstreetmap.org/oauth2/authorize"
  client <- httr2::oauth_client(
    id = client_id,
    token_url = "https://www.openstreetmap.org/oauth2/token",
    secret = client_secret,
    auth = "header",
    name = "mgaOsmApi"
  )
  if (methode == "PUT" | methode == "POST") {
    req <- request(url) |>
      req_method(methode) |>
      req_body_raw(xml)
    resp <- req |>
      httr2::req_oauth_auth_code(
        client = client,
        auth_url = auth_url,
        scope = paste(c("write_api"), collapse = " "),
        redirect_uri = client_redirect
      ) |>
      req_perform()
  }
  if (methode == "GET") {
    req <- request(url) |>
      req_method(methode)
    resp <- req |>
      req_perform()
  }
  status <- resp |> resp_status_desc()
  if (status != "OK") {
    writeLines(xml)
    stop(
      sprintf(
        "osm API request failed [%s]",
        status
      ),
      call. = FALSE
    )
  }
  txt <- ""
  if (resp |> resp_has_body()) {
    txt <- resp |> resp_body_string()
  }
  return(invisible(txt))
}
#
# la version OAuth1 -> 01/07/2024
api_url <- "https://www.openstreetmap.org"
osmapi_api_oauth1 <- function(path, xml = '', methode = "PUT", debug = FALSE) {
  library(httr)
  url <- sprintf("%s/api/0.6/%s", api_url, path)
  carp("url: %s", url)
  username <- mes_options("openstreetmap_user")
  password <- mes_options("openstreetmap_password")
  dsn <- "d:/osmapi_url.txt"
  write(xml, dsn)
  ua <- user_agent("http://github.com/hadley/httr")
  if (methode == "PUT") {
    resp <- httr::PUT(
      url,
      body = xml,
      authenticate(username, password, type = "basic"),
      content_type_xml(),
      ua
#      verbose()
    )
  }
  if (methode == "POST") {
    resp <- httr::POST(
      url,
      body = xml,
      authenticate(username, password, type = "basic"),
      content_type_xml(),
      ua
    )
  }
  if (methode == "GET") {
    resp <- httr::GET(
      url,
      authenticate(username, password, type = "basic"),
      accept(".xml"),
#      verbose(),
      ua
    )
  }
  if (http_error(resp)) {
    writeLines(xml)
    txt <- httr::content(resp, as = "text")
    writeLines(txt)
    stop(
      sprintf(
        "osm API request failed [%s]",
        status_code(resp)
      ),
      call. = FALSE
    )
  }
  txt <- httr::content(resp, as = "text")
  if (debug == TRUE) {
    if (http_type(resp) == "application/json") {
      print(jsonlite::prettify(txt, indent = 2))
    }
    if (http_type(resp) == "application/xml") {
      xml <- str_split(txt, "\\n+", simplify = TRUE)
      xml <- head(xml[-1:-2], -2)
      xml <- paste(xml,  collapse = "\n")
      print(xml)
    }
  }
  return(invisible(txt))
}

#
# source("geo/scripts/transport.R"); res <- osmapi_get_object_xml(); print(res)
osmapi_get_object_xml <- function(id = "11920346", type = "relation", force = FALSE) {
  library(stringi)
  path <- "{type}/{id}"
  path <- str_glue(path)
  carp("path: %s", path)
  xml <- osmapi_api(path, methode = "GET")
  xml <- str_split(xml, "\\n+", simplify = TRUE)
  xml <- head(xml[-1:-2], -2)
  xml <- paste(xml, collapse = "\n")
  return(invisible(xml))
}
# source("geo/scripts/transport.R"); res <- osmapi_get_object_version_xml(); print(res)
osmapi_get_object_version_xml <- function(id = "11920346", type = "relation", version = 1, force = FALSE) {
  library(stringi)
  path <- "{type}/{id}/{version}"
  path <- str_glue(path)
  carp("path: %s", path)
  xml <- osmapi_api(path, methode = "GET")
  xml <- str_split(xml, "\\n+", simplify = TRUE)
  xml <- head(xml[-1:-2], -2)
  xml <- paste(xml, collapse = "\n")
  return(invisible(xml))
}
#
## interrogation d'un changeset par son id
# le node créé
# source("geo/scripts/transport.R"); osmapi_get_changeset(id = "132833877")
# modify
# source("geo/scripts/transport.R"); osmapi_get_changeset(id = "132571776")
# ref en double
# source("geo/scripts/transport.R"); osmapi_get_changeset(id = "132572055")
osmapi_get_changeset <- function(id = "132833877") {
  library(stringi)
  library(xml2)
  path <- "changeset/{id}/download"
  path <- str_glue(path)
  txt <- osmapi_api(path, methode = "GET", debug = TRUE)
  xml <- xml2::read_xml(txt)
  actions <- xml_children(xml)
  actions.df <- data.frame()
  for (action in actions) {
    objects <- xml_children(action)
    action.df <- osmapi_objects_attrs(objects)
    action.df$xml_action <- xml_name(action)
    actions.df <- bind_rows(actions.df, action.df)
  }
  return(invisible(actions.df))
}
# https://www.openstreetmap.org/api/0.6/changesets?display_name=mga_geo
# source("geo/scripts/transport.R"); osmapi_get_changesets()
osmapi_get_changesets <- function(display_name = "mga_geo") {
  library(stringi)
  library(xml2)
  path <- "changesets/?display_name={display_name}"
  path <- str_glue(path)
  txt <- osmapi_api(path, methode = "GET", debug = TRUE)
  xml <- xml2::read_xml(txt)
  text <- xml_children(xml) %>%
    xml_name()
  print(text)
}
# source("geo/scripts/transport.R"); osmapi_get_changeset_first()
osmapi_get_changeset_first <- function(display_name = "mga_geo") {
  library(stringi)
  library(jsonlite)
  path <- "changesets/?display_name={display_name}"
  path <- str_glue(path)
  txt <- osmapi_api(path, methode = "GET", debug = FALSE)
  json <- jsonlite::fromJSON(txt, flatten = TRUE)
  id <- json$changesets[1, "id"]
  carp("changeset: %s", id)
  osmapi_get_changeset(id)
}
# https://gist.github.com/typebrook/d166d5e8d0a293c30f697b0f403b3c0e
#
# source("geo/scripts/transport.R"); osmapi_put()
osmapi_put <- function(osmchange = "create", text = "osm", comment = "", force = FALSE) {
  library(tidyverse)
  carp("osmapi_put")
  xml <- '<osm>
  <changeset>
    <tag k="created_by" v="R 0.6"/>
    <tag k="comment" v="%s"/>
  </changeset>
</osm>'
  if (comment == "") {
    comment <- sprintf("maj gtfs %s", Config_gtfs_source)
  }
  xml <- sprintf(xml, comment)
  writeLines(text, sprintf("%s/osmapi_put_%s.txt", cfgDir, Reseau))
  if (OsmChange != TRUE) {
    return(invisible(-1))
  }
#  return(invisible(-2))
  changeset_id <- osmapi_api("changeset/create", xml, methode = "PUT")
  carp("changeset_id: %s", changeset_id)
#  stop("****")
  text <- osmapi_osmchange(changeset_id, change = osmchange, osm = text)
  path <- sprintf("changeset/%s/upload", changeset_id)
  res <- osmapi_api(path, xml = text, methode = "POST")
  path <- sprintf("changeset/%s/close", changeset_id)
  res <- osmapi_api(path, methode = "PUT") %>%
    glimpse()
  carp("changeset: %s/changeset/%s", api_url, changeset_id)
  return(invisible(changeset_id))
}
# source("geo/scripts/transport.R"); osmapi_osmchange()
osmapi_osmchange <- function(changeset_id = 200, change = "create", osm = "<node>") {
  timestamp <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S.0+02:00")
  osm <- str_replace_all(osm, 'changeset="\\d+"', 'changeset="{changeset_id}"')
  text <- '<osmChange version="0.6" generator="R mga_geo">
<{change}>
{osm}
</{change}>
</osmChange>'
  text <- str_glue(text)
  text <- str_glue(text)
#  print(text);exit;
  return(text)
}
#
# create_node : création d'un node
# source("geo/scripts/transport.R");osmapi_create_node()
osmapi_create_node <- function() {
  text <- '<node id="-1" timestamp="{timestamp}" lat="48.0875052" lon="-1.6445175" changeset="1" version="1">
  <tag k="name" v="mga_geo"/>
</node>
'
  OsmChange <<- TRUE
  changeset_id <- osmapi_put("create", text = text)
  txt <- osmapi_get_changeset(id = changeset_id)
  return(invisible(changeset_id))
}
#
# test
# - création
# - modification
# - suppression
# source("geo/scripts/transport.R");osmapi_test()
osmapi_test <- function() {
  library(stringi)
# création d'un node
  id <- osmapi_create_node()
#  id <- "132833877"
#
  df <- osmapi_get_changeset(id = id)
  text <- osmapi_get_object_xml(id = df[[1, "id"]], type = df[[1, "xml_name"]])
  text <- stri_replace(text, 'v="mga_geo_2"', regex = 'v="mga_geo"')
  print(text)
  changeset_id <- osmapi_put("modify", text = text)
  text <- osmapi_get_object_xml(id = df[[1, "id"]], type = df[[1, "xml_name"]])
  changeset_id <- osmapi_put("delete", text = text)
  return(invisible())
}