# <!-- coding: utf-8 -->
# pour la cohérence route route_route_master
# utilisation des données opendata
# auteur : Marc Gauthier
#
# source("geo/scripts/transport.R");routes_jour(force = FALSE)
routes_jour <- function(force = TRUE) {
  library(tidyverse)
  config_xls("bretagne")
  df <- routes_relations_get(force = force)
  routes_coherence(df)
}
# source("geo/scripts/transport.R");dsn <- routes_relations_get(force = TRUE)
routes_relations_get <- function(force = FALSE) {
  library(tidyverse)
  carp()
  df <- bretagne_relations_routes_bus(force = force) %>%
    glimpse()
  return(invisible(df))
}
# source("geo/scripts/transport.R");dsn <- routes_relations_disused_get(force = FALSE)
routes_relations_disused_get <- function(force = FALSE) {
  library(tidyverse)
  carp()
  dsn <- bretagne_relations_routes_disused(force = force) %>%
    glimpse()
  osm <- read_file(dsn)
  osm <- str_replace_all(osm, "\\<U\\+\\d{4}\\>", "?")
  doc <- read_xml(osm)
# Note the difference between .// and //
# //  finds anywhere in the document (ignoring the current node)
# .// finds anywhere beneath the current node
  relations <- xml2::xml_find_all(doc, "//relation")
  carp("relations nb: %s", length(relations))
  for (relation in relations) {
    members_relation <-  xml2::xml_find_all(relation, './/member[@type="relation"]')
    if (length(members_relation) == 0) {
      next
    }
    members.list <- list()
    for (attr in c("type", "ref", "role")) {
      members.list[[attr]] <- members_relation %>% xml_attr(attr)
    }
    members.df <- as_tibble(members.list) %>%
      glimpse()
  }
  return(invisible())
}
#
routes_coherence <- function(df) {
  library(tidyverse)
  df <- df %>%
    filter(! is.na(id)) %>%
    dplyr::select(id, network, ref, type, colour, text_colour) %>%
    arrange(network, ref, type) %>%
    filter(network %in% c("STAR")) %>%
    glimpse()
  misc_print(df)
  df1 <- df %>%
    filter(! is.na(id)) %>%
    group_by(network) %>%
    summarize(nb = n()) %>%
    glimpse()
  misc_print(df1)
  agency.df <- config_agency_lire() %>%
    glimpse()
  df11 <- agency.df %>%
    filter(! is.na(agency_id)) %>%
    separate_longer_delim(agency_id, "|") %>%
    glimpse()
  carp("les agency_id de Linéotim")
  df12 <- df11 %>%
    filter(grepl("LINEO", agency_id)) %>%
    glimpse()
  df13 <- df11 %>%
    group_by(agency_id) %>%
    summarize(nb = n()) %>%
    filter(nb > 1) %>%
    glimpse()
  carp("les network non gérés")
  df2 <- df1 %>%
    filter(network %notin% df11$network)
  misc_print(df2)
  carp("mobibreizh")
  fic <- "agency"
  dsn <- sprintf("%s/%s.txt", odDir, fic)
  carp("dsn: %s", dsn)
  mobibreizh.df <- rio::import(dsn, encoding = "UTF-8") %>%
    glimpse()
  df3 <- mobibreizh.df %>%
    filter(! grepl("maritime", agency_name)) %>%
    filter(agency_id %notin% df11$agency_id)
  misc_print(df3)
  df4 <- mobibreizh.df %>%
    filter(! grepl("maritime", agency_name)) %>%
    left_join(df11, by = c("agency_id")) %>%
    dplyr::select(network, agency_id) %>%
    glimpse()
  return(invisible(df))
}