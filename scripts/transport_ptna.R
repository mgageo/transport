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
}
ptna_gtfs_lire <- function() {
  library(tidytransit)
  config_xls("korrigo")
  dsn <- sprintf("%s/gtfs.zip", gtfsDir)
  tt <<- tidytransit::read_gtfs(dsn, quiet = FALSE)
# un voyage pour tous les stops
  df1 <- tt$stop_times %>%
    dplyr::select(trip_id, stop_id) %>%
    group_by(stop_id) %>%
    filter(row_number() == 1) %>%
    glimpse()
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
ptna_gtfs_lire <- function() {
  library(tidytransit)
  config_xls("korrigo")
  communes.sf <<- ign_adminexpress_lire_sf() %>%
    dplyr::select("city" = NOM, INSEE_COM, INSEE_CAN, INSEE_ARR, INSEE_DEP)
  dsn <- sprintf("%s/gtfs.zip", gtfsDir)
  if ( ! exists("tt")) {
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
ptna_wiki <- function() {
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
  FILTER regex(?inseeCode, "^(22|29|35|56|50|53)", "i")
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