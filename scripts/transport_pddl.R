# <!-- coding: utf-8 -->
#
# les réseaux de bus de la région Pays de la Loire
#
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
#
# source("geo/scripts/transport.R");pddl_jour()
pddl_jour <- function(force = FALSE) {
  library(tidyverse)
  library(data.table)
  library(janitor)
  library(rio)
  library(tidyverse)
#  osmose_issues_gap()
}
#
# vérification de l'appartenance à un network connu
# source("geo/scripts/transport.R");pddl_relations_route_bus_network()
pddl_relations_route_bus_network <- function(force = FALSE) {
  library(tidyverse)
  library(data.table)
  library(janitor)
  library(rio)
  library(tidyverse)
  dsn <- sprintf("%s/agency.xls", cfgDir)
  carp("dsn: %s", dsn)
  xls.df <- rio::import(dsn) %>%
    glimpse()
  carp("osm")
  df <- overpass_get(query = "relations_route_bus_area", format = "csv", force = force) %>%
    glimpse()
  df1 <- df %>%
    group_by(network) %>%
    summarize(nb = n()) %>%
    glimpse()
  df2 <- df1 %>%
    filter(network %notin% xls.df$network)
  misc_print(df2)
  df3 <- df %>%
    filter(network %in% df2$network)
  misc_print(df3)
  misc_print(df1)
}
#
# pour essayer de faire le réseau de cars Aleop 44
# source("geo/scripts/transport.R");pddl_relations_route_bus_aleop44()
pddl_relations_route_bus_aleop44 <- function(force = TRUE) {
  library(tidyverse)
  library(data.table)
  library(janitor)
  library(rio)
  library(tidyverse)
  carp("osm")
  df <- overpass_get(query = "relations_route_bus_area", format = "csv", force = force) %>%
    glimpse()
  df11 <- df %>%
    group_by(network) %>%
    summarize(nb = n()) %>%
    as.data.frame() %>%
    glimpse()
  df21 <- df %>%
    filter(grepl("^(fr_lila|Lila|Aléop - Loire Atlantique)$", network))
  df22 <- df21 %>%
    group_by(network) %>%
    summarize(nb = n()) %>%
    as.data.frame()
  df23 <- df21 %>%
    mutate(id = sprintf("[http://level0.osmz.ru/?url=relation/%s %s]", `@id`, `@id`)) %>%
    dplyr::select(id, ref, name, from, to, network) %>%
    arrange(ref, name, from, to, network) %>%
    as.data.frame() %>%
    glimpse()
  Wiki <<- TRUE
  page <- sprintf("User:Mga_geo/Transports_publics/%s/osm/%s", Config["wiki"], "relations_route_bus_aleop44") %>%
    glimpse()
  wiki <- sprintf("==nombre par network==
%s
==aleop 44==
===nombre par network===
%s
===par référence===
%s",
    wiki_df2table(df11),
    wiki_df2table(df22),
    wiki_df2table(df23)
  )
  wiki_page_init(page = page, article = wiki, force = TRUE)
}