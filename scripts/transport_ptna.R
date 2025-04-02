# <!-- coding: utf-8 -->
#
# le réseau de bus
#
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
#
# la configuration pour ptna
#
# source("geo/scripts/transport.R");ptna_jour()
ptna_jour <- function() {
  ptna_gtfs_lire()
  ptna_wiki_ptna()
  ptna_wiki_config()
}
#
# source("geo/scripts/transport.R");ptna_wiki_ptna()
ptna_wiki_ptna <- function() {
  df1 <- ptna_gtfs_lire() %>%
    dplyr::select(wikidata = item, entité = itemLabel)
  wikidata <- wiki_df2table(df1)
  tpl <- sprintf("%s/transport_wiki_ptna.txt", cfgDir)
  parametres <- readLines(tpl)
  variables <- list(
    name = Config_name,
    nom = Config_nom,
    PTNA = Config_PTNA,
    network_name = Config_name,
    gtfs = Config_gtfs_source,
    k_ref = Config_k_ref,
    datawiki = wikidata,
    gestionnaire = Config_gestionnaire,
    network = Config_network,
    operator = Config_operator,
    wikidata = Config_wikidata,
    wiki = Config_wiki,
    url = Config_website
  )
  parametres <- misc_list2tpl(variables, parametres)
#  writeLines(parametres)
  Wiki <<- TRUE
  wiki <- paste(parametres,  collapse = "\n")
  page <- sprintf("User:Mga_geo/Transports en commun/%s/PTNA", Config_name)
  wiki_page_init(page = page, article = wiki, force = TRUE)
}
#
# source("geo/scripts/transport.R");ptna_wiki_config()
ptna_wiki_config <- function() {
  if ( ! exists("tt")) {
    dsn <- sprintf("%s/gtfs.zip", gtfsDir)
    carp("dsn: %s", dsn)
    tt <<- tidytransit::read_gtfs(dsn, quiet = FALSE)
  }
# https://gtfs.org/documentation/schedule/reference/#routestxt
# https://developers.google.com/transit/gtfs/reference/extended-route-types?hl=fr
  route_type.df <- tribble(
    ~type, ~libelle, ~code,
    0, "== Lignes de tram","tram",
    1, "== Lignes de métro","subway",
    3, "== Lignes de bus","bus",
    200, "== Lignes de bus","bus",
    204, "== Lignes de bus","bus",
    4, "== Lignes de ferry","ferry",
    6, "== Lignes de téléphérique","téléphérique"
  )
  routes.df <- tt$routes %>%
    glimpse()
  df1 <- routes.df %>%
    filter(route_type %notin% route_type.df$type)
  if (nrow(df1) > 0 ) {
    misc_print(df1)
    confess("route_type")
  }
#  stop("*****")
  if (Config_reseau == "saintmalo") {
    routes.df <- routes.df %>%
      filter(!grepl("^S", route_id)) %>%
#      filter(!grepl("[ABC]$", route_id)) %>%
      glimpse()
  }
  if (Config_reseau == "nantes") {
    routes.df <- routes.df %>%
      mutate(route_sort_order = as.numeric(route_sort_order)) %>%
      glimpse()
#    stop("*****")
  }
  if (Config_reseau == "rouen") {
    routes.df <- routes.df %>%
      mutate(route_sort_order = as.numeric(route_sort_order)) %>%
      arrange(route_sort_order) %>%
      glimpse()
#    stop("*****")
  }
#  stop("*****")
  config <- ""
  for (i in 1:nrow(route_type.df)) {
    df <- routes.df %>%
      filter(route_type == route_type.df[[i, "type"]])
    if (nrow(df) == 0) {
      next
    }
    config <- append(config, route_type.df[[i, "libelle"]])
    for (j in 1:nrow(df)) {
      route <- sprintf("%s;%s;;;;;%s;%s",
        df[[j, "route_short_name"]],
        route_type.df[[i, "code"]],
        Config_PTNA,
        df[[j, "route_id"]]
      )
      config <- append(config, route)
    }
  }
  config <- paste(config,  collapse = "\n")
#  writeLines(config);stop("****")
  dsn <- sprintf("%s/transport_wiki_ptna_config.txt", cfgDir)
  tpl <- readLines(dsn)
  variables <- list(
    name = Config_name,
    nom = Config_nom,
    gtfs = Config_gtfs_source,
    k_ref = Config_k_ref,
    gestionnaire = Config_gestionnaire,
    network = Config_network,
    operator = Config_operator,
    wikidata = Config_wikidata,
    wiki = Config_wiki,
    url = Config_website,
    lignes= config
  )
  parametres <- misc_list2tpl(variables, tpl)
#  writeLines(parametres)
  Wiki <<- TRUE
  wiki <- paste(parametres,  collapse = "\n")
  page <- sprintf("User:Mga_geo/Transports en commun/%s/PTNA csv", Config_name)
  wiki_page_init(page = page, article = wiki, force = TRUE)
}
#
# source("geo/scripts/transport.R");ptna_wiki()
ptna_wiki <- function() {
  df1 <- ptna_gtfs_lire() %>%
    dplyr::select(wikidata = item, entité = itemLabel)
  wikidata <- wiki_df2table(df1)
  stops.df <- tt$stops
  master.df <- misc.lire("gtfs2osm_relations_routemaster", dir = transportDir) %>%
    glimpse()
  route.df <- misc.lire("gtfs2osm_relations_route_stops", dir = transportDir) %>%
    glimpse()
  if (Config_reseau == "saintmalo") {
    route.df <- route.df %>%
      filter(!grepl("^S", ref)) %>%
      filter(!grepl("[AB]$", ref)) %>%
      glimpse()
  }
#  stop("*****")
  tpl <- sprintf("%s/transport_wiki.txt", cfgDir)
  parametres <- readLines(tpl)
  variables <- list(
    network_name = Config_name,
    gtfs = Config_gtfs_source,
    k_ref = Config_k_ref,
    datawiki = wikidata,
    stop_name = stops.df[[1, "stop_name"]],
    stop_id = stops.df[[1, "stop_id"]],
    colour = master.df[[1, "colour"]],
    text_colour = master.df[[1, "text_colour"]],
    master_ref = master.df[[1, "ref"]],
    master_name = master.df[[1, "name"]],
    master_description = master.df[[1, "description"]],
    gestionnaire = Config_gestionnaire,
    network = Config_network,
    operator = Config_operator,
    wikidata = Config_wikidata,
    wiki = Config_wiki,
    url = Config_website,
    route_from = route.df[[1, "from"]],
    route_to = route.df[[1, "to"]],
    route_ref = route.df[[1, "ref"]],
    route_ref_network = route.df[[1, "ref_network"]],
    route_name = route.df[[1, "name"]],
    route_description = route.df[[1, "description"]],
    shape_id = route.df[[1, "shape_id"]],
    "route_short_name" = route.df[[1, "gtfs:route_short_name"]],
    "route_long_name" = route.df[[1, "gtfs:route_long_name"]],
    "trip_id:sample" = route.df[[1, "gtfs:trip_id:sample"]],
    "route_id" = route.df[[1, "gtfs:route_id"]]
  )
  parametres <- misc_list2tpl(variables, parametres)
#  writeLines(parametres)
  Wiki <<- TRUE
  wiki <- paste(parametres,  collapse = "\n")
  page <- sprintf("User:Mga_geo/Transports_publics/%s/PTNA", Config_wiki)
  wiki_page_init(page = page, article = wiki, force = TRUE)
}
# source("geo/scripts/transport.R");ptna_gtfs_lire()
ptna_gtfs_lire <- function() {
  library(tidytransit)
  library(sf)
#  config_xls("landerneau")
  dsn <- sprintf("%s/gtfs.zip", gtfsDir)
  tt <<- tidytransit::read_gtfs(dsn, quiet = FALSE)
# un voyage pour tous les stops
  df1 <- tt$stop_times %>%
    dplyr::select(trip_id, stop_id) %>%
    group_by(stop_id) %>%
    filter(row_number() == 1) %>%
    glimpse()
  df2 <- tt$stops %>%
    filter(stop_id %in% df1$stop_id) %>%
    glimpse()
  df3 <- ptna_englobante(df2)
  misc_print(df3)
  return(invisible(df3))
}
pnta_config <- function() {
  df <- config_agency_lire() %>%
    filter(!is.na(reseau)) %>%
    filter(!grepl("^#", reseau)) %>%
    filter(agency_id %notin% c("BIBUS", "STAR", "TILT")) %>%
    filter(!is.na(network)) %>%
    filter(!is.na(gtfs_dir)) %>%
    filter(!is.na(route_ref)) %>%
    filter(!is.na(territoire)) %>%
    filter(grepl("korrigo", gtfs_source)) %>%
#    filter(!is.na(`network:wikidata`)) %>%
    dplyr::select(agency_id, network)
  misc_print(df)
  return(invisible(df))
}
ptna_gtfs_lire_v2 <- function() {
  library(tidytransit)
  config_xls("fougeres")
  if (! exists("communes.sf") ) {
    communes.sf <<- ign_adminexpress_lire_sf() %>%
      dplyr::select("city" = NOM, INSEE_COM, INSEE_CAN, INSEE_ARR, INSEE_DEP)
  }
  if ( ! exists("tt")) {
    dsn <- sprintf("%s/gtfs.zip", gtfsDir)
    carp("dsn: %s", dsn)
    tt <<- tidytransit::read_gtfs(dsn, quiet = FALSE)
  }
  wikidata.df <<- misc.lire("ptna_wikidata")
  config.df <- pnta_config()
# les stops sont préfixés avec l'agency_id
  df1 <- tt$agency %>%
    left_join(config.df) %>%
    filter(!is.na(network)) %>%
    arrange(agency_id) %>%
    dplyr::select(agency_id, network, agency_name)
  misc_print(df1)
  for (i1 in 1:nrow(df1)) {
    agency_id <- df1[[i1, "agency_id"]]
    carp("agency_id: %s network: %s name: %s", agency_id, df1[[i1, "network"]], df1[[i1, "agency_name"]])
#    next
    re <- sprintf("^%s\\:", agency_id)
    stops.df <- tt$stops %>%
      filter(grepl(re, stop_id)) %>%
      filter(stop_lat > 40)
    if (nrow(stops.df) < 50) {
      next
    }
    df2 <- ptna_englobante(stops.df)
    misc_print(df2)
  }
}
ptna_englobante <- function(stops.df) {
  library(tidytransit)
  if (! exists("communes.sf") ) {
    communes.sf <<- ign_adminexpress_lire_sf() %>%
      dplyr::select("city" = NOM, INSEE_COM, INSEE_CAN, INSEE_ARR, INSEE_DEP)
  }
  wikidata.df <- misc.lire("ptna_wikidata") %>%
    mutate(inseeCode = sprintf("%s", inseeCode))
  stops.sf <- st_as_sf(stops.df, coords = c("stop_lon", "stop_lat"), crs = 4326, remove = FALSE) %>%
    st_transform(2154)
  df1 <- stops.sf %>%
    st_join(communes.sf) %>%
    st_drop_geometry() %>%
    group_by(city, INSEE_COM, INSEE_CAN, INSEE_ARR, INSEE_DEP) %>%
    summarize(nb = n()) %>%
    ungroup()
  if (nrow(df1) < 3) {
    df1 <- df1 %>%
      left_join(wikidata.df, by = c("INSEE_COM" = "inseeCode"))
    return(invisible(df1))
  }
  df1 <- df1 %>%
    group_by(INSEE_CAN, INSEE_ARR, INSEE_DEP) %>%
    summarize(nb = sum(nb)) %>%
    ungroup()
  if (nrow(df1) < 3) {
#    glimpse(df1);confess()
    df2 <- df1 %>%
      mutate(INSEE = sprintf("%s%s", INSEE_DEP, INSEE_CAN)) %>%
      left_join(wikidata.df, by = c("INSEE" = "inseeCode"))
#    return(invisible(df2))
  }
  df1 <- df1 %>%
    group_by(INSEE_ARR, INSEE_DEP) %>%
    summarize(nb = sum(nb)) %>%
    ungroup()
  df2 <- df1 %>%
    mutate(INSEE = sprintf("%s%s", INSEE_DEP, INSEE_ARR)) %>%
    left_join(wikidata.df, by = c("INSEE" = "inseeCode"))
  return(invisible(df2))
}
#
# source("geo/scripts/transport.R");ptna_wiki()
ptna_wikidata_wiki <- function() {
  library(rio)
  library(tidyverse)
  wikidata.df <- misc.lire("ptna_wikidata")
  dsn <- sprintf("%s/%s.xlsx", cfgDir, "ptna")
  df1 <- rio::import(dsn) %>%
    separate_longer_delim(wikidata, ",") %>%
    distinct(wikidata) %>%
    mutate(wd = as.numeric(gsub("Q", "", wikidata))) %>%
    arrange(wd) %>%
    glimpse()
  df2 <- df1 %>%
    filter(wikidata %notin% wikidata.df$item) %>%
    glimpse()
  df3 <- df1 %>%
    left_join(wikidata.df, by = c("wikidata" = "item")) %>%
    dplyr::select(wikidata, item = itemLabel, entité) %>%
    glimpse()
  wiki <- wiki_df2table(df3, num = FALSE)
  page <- "Bretagne/Transports_en_commun/KorriGo/WikiData"
  wiki_page_init(page = page, article = wiki, force = TRUE)
}
# https://tableconvert.com/excel-to-mediawiki
#
# https://www.wikidata.org/wiki/Wikidata:Database_reports/List_of_properties/all/fr
# source("geo/scripts/transport.R");ptna_wikidata()
ptna_wikidata <- function() {
  library(tidyverse)
#  library(wikidataR)
  library(WikidataQueryServiceR)
# P2585 pour le code INSEE d'une région
  df1 <- tribble(
    ~entité, ~propriété,
    "région", "P2585",
    "département", "P2586",
    "arrondissement", "P3423",
    "canton", "P2506",
    "commune", "P374"
  )
  query <- 'SELECT ?item ?itemLabel ?inseeCode {
  ?item wdt:%s ?inseeCode .
  FILTER regex(?inseeCode, "^(22|29|35|56|50|53|44|49|76)", "i")
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],fr" }
}'
  df2 <- data.frame()
  for (i1 in 1:nrow(df1)) {
    q <- sprintf(query, df1[[i1, "propriété"]])
    writeLines(query)
    df <- query_wikidata(q) %>%
      mutate(entité = df1[[i1, "entité"]]) %>%
      glimpse()
    df2 <- bind_rows(df2, df)
  }
  glimpse(df2)
  misc.ecrire(df2, "ptna_wikidata")
}