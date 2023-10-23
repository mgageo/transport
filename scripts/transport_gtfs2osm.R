# <!-- coding: utf-8 -->
#
# le réseau de bus
#
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
#
# préparation des données gtfs pour comparaison avec osm
#
Config_tags <- c("operator", "website", "network:wikidata", "network:wikipedia", "operator:wikidata", "operator:wikipedia")

# source("geo/scripts/transport.R");gtfs2osm_jour()
gtfs2osm_jour <- function(force = TRUE) {
  library(tidyverse)
  library(rio)
  library(sf)
  carp()
# mise au format interne des fichiers gtfs pour les relations route_master
  gtfs2osm_routemasters();
# mise au format interne des fichiers gtfs pour les routes
  gtfs2osm_routes_stops(force = force)
#  gtfs2osm_routes_wiki("gtfs2osm_routes_stops")
  if( Config[1, "shapes"] == "TRUE" ) {
    gtfs2osm_routes_shapes_stops(force = force)
    gtfs2osm_routes_wiki("gtfs2osm_routes_shapes_stops")
  }
}
#
# source("geo/scripts/transport.R");gtfs2osm_diff()
gtfs2osm_diff <- function() {
  library(tidyverse)
  library(rio)
  library(sf)
  carp()
  gtfs2mga <- gtfs2mga_lire() %>%
    glimpse()
  osm2mga <- osm2mga_lire() %>%
    glimpse()
  gtfs.df <- gtfs2mga$shapes_stops %>%
    ungroup() %>%
    glimpse()
  osm.df <- osm2mga$relations_stops %>%
    dplyr::rename(ref=`ref:FR:STAR`) %>%
    glimpse()
  df <- full_join(gtfs.df, osm.df, by=c('shape_id'='ref')) %>%
    filter(arrets != stops) %>%
    glimpse()
}
#
#################################################################################
#
# pour les stops
gtfs2osm_stops_create <- function(gtfs.sf) {
  carp()
  library(sf)
  library(tidyverse)
  library(knitr)
  library(kableExtra)
  df1 <- gtfs.sf %>%
    st_drop_geometry() %>%
    mutate(k_ref = Config[1, "k_ref"]) %>%
    mutate(v_ref = sprintf("%s", stop_id))
# le template level0
  dsn <- sprintf("%s/transport_level0_node.txt", cfgDir)
  carp("dsn: %s", dsn)
  template <- readLines(dsn)
  osm <- misc_df2tpl(df1, template)
  dsn <- sprintf("%s/transport_level0_node.txt", transportDir)
  carp("dsn: %s", dsn)
  writeLines(osm, dsn)
}
#
#################################################################################
#
# pour les relations route_master
#
# la conversion pour perl, fichier tsv
# source("geo/scripts/transport.R");gtfs2osm_routemasters()
gtfs2osm_routemasters <- function(rds = "gtfs2osm_routemasters", force_osm = TRUE) {
  library(tidyverse)
  library(tidytransit)
  carp()
  tt <- tidytransit_lire("gtfs")
  df <- tt$routes %>%
    mutate(Route_color = sprintf("#%s", toupper(route_color))) %>%
    mutate(Route_text_color = sprintf("#%s", toupper(route_text_color))) %>%
    mutate(Route_color = ifelse(grepl("^#[0-9A-F]{6}$", Route_color), Route_color, "#000000")) %>%
    mutate(Route_text_color = ifelse(grepl("^#[0-9A-F]{6}$", Route_text_color), Route_text_color, "#FFFFFF")) %>%
    mutate(Name = str_glue('{Config_route_name} {route_short_name} {route_long_name}'))
  if (grepl("breizhgo", Config_reseau)) {
    df <- df %>%
      mutate(Name = str_glue('{Config_route_name} {route_short_name} : {route_long_name}')) %>%
      glimpse()
  }
  df <- df %>%
    dplyr::select(
      description = route_long_name,
      colour = Route_color,
      name = Name,
      ref = route_short_name,
      text_colour = Route_text_color,
    ) %>%
    mutate(`public_transport:version` = 2) %>%
    mutate(`route_master` = "bus") %>%
    mutate(`type` = "route_master")
  tags <- c("network", "operator", "website", "network:wikidata", "network:wikipedia", "operator:wikidata", "operator:wikipedia")
  for (tag in tags) {
    col <- sprintf("Config_%s", tag)
    col <- get(col)
    if (! is.na(col)) {
      df[[tag]] <- col
    }
  }
  route_prefixe <- ""
  if (!is.na(Config_route_prefixe)) {
    route_prefixe <- Config_route_prefixe
  }
  df <- df %>%
    mutate(`ref:network` = sprintf("%s%s", route_prefixe, ref)) %>%
    glimpse()
  misc.ecrire(df, rds, dir = transportDir)
  dsn <- sprintf("%s/%s.csv", transportDir, rds)
  readr::write_tsv(df, file = dsn)
  carp("dsn: %s", dsn)
  page <- sprintf("User:Mga_geo/Transports_publics/%s/%s", Config["wiki"], "gtfs2osm_routemasters")
  wiki <- wiki_df2table(df)
#  page <- append(page, wiki)
  Wiki <<- TRUE
  wiki_page_init(page = page, article = wiki, force = TRUE)
}

#
# pour level0
# source("geo/scripts/transport.R");gtfs2osm_relations_routemaster_level0()
gtfs2osm_relations_routemaster_level0 <- function(rds = "gtfs2osm_routemasters", force_osm = TRUE) {
  library(stringr); # pour str_glue
  df <- misc.lire(rds, dir = transportDir) %>%
    arrange(ref) %>%
    glimpse()
#  stop("******")
  osm.df <- overpass_get(query = "relations_route_bus_network", format = "csv", force = force_osm) %>%
    clean_names() %>%
    mutate(id = sprintf("  rel %s", id)) %>%
    glimpse()
  cols <- colnames(df)
  tpl <- "
relation
  on_demand = yes
  contact:phone = +33 2 98 34 42 22
  opening_hours = Mo-Fr 07:30-19:00; Sa 09:00-17:00
"
  tpl <- "
relation
  colour = {gtfs_colour}
  name = {gtfs_name}
  network = {Config_network}
  public_transport:version = 2
  ref = {gtfs_ref}
  route_master = bus
  text_colour = {gtfs_text_colour}
  type = route_master"
  tags <- Config_tags
  for (tag in tags) {
    col <- sprintf("Config_%s", tag)
    val <- get(col)
    if (! is.na(val)) {
      tpl <- sprintf("%s
  %s = %s", tpl, tag, val)
    }
  }
  tpl <- sprintf("%s
{routes}", tpl)
#  writeLines(tpl); exit;
  wiki <- stringr::str_glue('==relations route: les fichiers de configuration level0==')
  for (i in 1:nrow(df)) {
    routes.df <- osm.df %>%
      filter(ref == df[[i, "ref"]])
    routes <- paste(routes.df$id, collapse = "\n")
#    carp("routes: %s", routes); stop("****")
    for (j in 1:ncol(df)) {
      col <- sprintf("gtfs_%s", cols[j])
      assign(col, df[[i, j]])
    }

    level0 <- str_glue(tpl)
    wiki <- stringr::str_glue('{wiki}
==={gtfs_ref} {gtfs_name}===
 <nowiki>
{level0}
</nowiki>')
#    print(level0);stop("*****")
    dsn <- sprintf("%s/reseau_relations_routemaster_level0_%s.txt", osmDir, gtfs_ref)
    write(level0, dsn)
    carp("dsn: %s", dsn)

  }
  page <- sprintf("User:Mga_geo/Transports_publics/%s/%s", Config["wiki"], "gtfs2osm_relations_routemaster_level0") %>%
    glimpse()
  wiki_page_init(page = page, article = wiki, force = TRUE)
}
#
#################################################################################
#
# pour les relations route
#
#
# la conversion
# source("geo/scripts/transport.R");gtfs2osm_routes_stops()
gtfs2osm_routes_stops <- function(rds = "gtfs2osm_routes_stops", force_osm = TRUE) {
  library(tidyverse)
  library(tidytransit)
  carp("selection de la route avec le plus de stops/voyages")
  df1 <- tidytransit_lire(rds = "tidytransit_routes_stops") %>%
    group_by(ref_network) %>%
    arrange(desc(nb_stops), desc(nb)) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    glimpse()
  df2 <- gtfs2osm_routes_osm(df1, force_osm = force_osm) %>%
    select(order(colnames(.))) %>%
    glimpse()
  df2 <- df2 %>%
    dplyr::select(colour, description, from, name, network, operator,`public_transport:version`
      ,ref, ref_network, shape_id, route, text_colour, to, type, stops, stops_code, stop_names = names
      ,stops_osm_id, stops_osm_name) %>%
    glimpse()
  transport_sauve(df2, rds)
  dsn <- sprintf("%s/%s.csv", transportDir, rds)
  readr::write_tsv(df2, file = dsn)
  carp("dsn: %s", dsn)
}
#
# la conversion
# source("geo/scripts/transport.R");gtfs2osm_routes_shapes_stops()
gtfs2osm_routes_shapes_stops <- function(rds = "gtfs2osm_routes_shapes_stops", force_osm = TRUE) {
  library(tidyverse)
  library(tidytransit)
  carp()
  df1 <- tidytransit_lire(rds = "tidytransit_routes_stops") %>%
    group_by(ref_network, shape_id) %>%
    arrange(desc(nb), desc(nb_stops)) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    arrange(ref_network) %>%
    glimpse()
  df2 <- df1 %>%
    dplyr::select(ref_network, shape_id, route_desc, nb, nb_stops) %>%
    arrange(ref_network, desc(nb), desc(nb_stops))
  misc_print(df2)
  df3 <- df1 %>%
    group_by(ref_network) %>%
    arrange(desc(nb), desc(nb_stops)) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    arrange(ref_network)
  df4 <- df3 %>%
    dplyr::select(ref_network, shape_id, route_desc, nb, nb_stops) %>%
    filter(ref_network == "C4-A") %>%
    arrange(ref_network, desc(nb), desc(nb_stops))
  misc_print(df4)
#  stop("*****")
  df4 <- gtfs2osm_routes_osm(df1, force_osm = force_osm)
  df4 <- df4 %>%
    dplyr::select(colour, description, from, name, network, operator,`public_transport:version`
      ,ref, ref_network, shape_id, route, text_colour, to, type, stops, stops_code, stop_names = names
      ,stops_osm_id, stops_osm_name
    ) %>%
    select(order(colnames(.))) %>%
    glimpse()
  transport_sauve(df4, rds)
  dsn <- sprintf("%s/%s.csv", transportDir, rds)
  readr::write_tsv(df4, file = dsn)
  carp("dsn: %s", dsn)
}
#
# les attributs de la relation route
gtfs2osm_routes_osm <- function(df1, force_osm) {
  library(tidyverse)
  library(tidytransit)
  library(stringr)
  carp()
  df2 <- df1 %>%
    mutate(Route_color = sprintf("#%s", toupper(route_color))) %>%
    mutate(Route_text_color = sprintf("#%s", toupper(route_text_color))) %>%
    mutate(Route_color = ifelse(grepl("^#[0-9A-F]{6}$", Route_color), Route_color, "#000000")) %>%
    mutate(Route_text_color = ifelse(grepl("^#[0-9A-F]{6}$", Route_text_color), Route_text_color, "#FFFFFF")) %>%
    mutate(from = str_glue('{first_city} ({first_name})')) %>%
    mutate(to = str_glue('{last_city} ({last_name})')) %>%
    mutate(from_city = ifelse(str_detect(first_name, first_city), first_name, str_glue('{first_city} ({first_name})'))) %>%
    mutate(to_city = ifelse(str_detect(last_name, last_city), last_name, str_glue('{last_city} ({last_name})'))) %>%
    mutate(name = str_glue('{Config_route_name} {route_short_name} : {first_name} -> {last_name}')) %>%
    mutate(description = str_glue('Ligne {route_short_name} : {first_city} ({first_name}) -> {last_city} ({last_name})')) %>%
    rename(colour = Route_color) %>%
    mutate(ref = route_short_name) %>%
    rename(text_colour = Route_text_color) %>%
    mutate(network = Config_network) %>%
    mutate(operator = Config_operator) %>%
    mutate(`public_transport:version` = "2") %>%
    mutate(route = "bus") %>%
    mutate(type = "route") %>%
    glimpse()

  if (Config_city == "Bordeaux") {
    carp("Config_city == Bordeaux")
    df2 <- df2 %>%
      mutate(from = ifelse(str_detect(first_name, first_city), first_name, str_glue('{first_city} ({first_name})'))) %>%
      mutate(to = ifelse(str_detect(last_name, last_city), last_name, str_glue('{last_city} ({last_name})'))) %>%
      mutate(name = str_glue('{Config_route_name} {route_long_name} : {from} -> {to}')) %>%
#      filter(ref == "87") %>%
      glimpse()
#    stop("***")
  }
  if (Config_city == "STAR") {
    carp("Config_city == STAR")
    df2 <- df2 %>%
      mutate(name = str_glue('{Config_route_name} {route_short_name} Direction {to_city}')) %>%
      glimpse()
  }
  if (grepl("breizhgo", Config_reseau)) {
    df2 <- df2 %>%
      mutate(name = str_glue('{Config_route_name} {route_short_name} {route_long_name} : {from_city} -> {to_city}')) %>%
      glimpse()
  }
#
# pour avoir les id osm des stops
  osm_stops.df <- overpass_get(query = "bus_stop_kref", format = "csv", force = force_osm) %>%
    rename(k_ref = Config[1, 'k_ref']) %>%
    mutate(k_ref = sprintf("%s", k_ref)) %>%
    clean_names()
  stops_zone <- "stops"
  if ( Reseau == "bordeaux") {
    stops_zone <- "stops_code"
  }
  for (i2 in 1:nrow(df2)) {
    stops <- df2[[i2, stops_zone]]
    stops.df <- data.frame(stop = unlist(strsplit(stops, ";"))) %>%
      left_join(osm_stops.df, by = c("stop" = "k_ref"))
    df3 <- stops.df %>%
      filter(is.na(id))
    if( nrow(df3) > 0) {
      glimpse(df3)
      glimpse(df2[i2, ])
      confess("***** stop inconnu nb: %s", nrow(df3))
    }
    df2[[i2, "stops_osm_id"]] <- paste0(stops.df$id, collapse = ",")
    df2[[i2, "stops_osm_name"]] <- paste0(stops.df$name, collapse = ",")
  }
  glimpse(df2)
  return(invisible(df2))
}
#
# production des fichiers pour level0
# !!! ne prend pas le bon fichier en entrée
# source("geo/scripts/transport.R");gtfs2osm_relations_route_level0()
gtfs2osm_relations_route_level0 <- function(force_osm = FALSE) {
  library(stringr); # pour str_glue
  df <- transport_read("gtfs2osm_routes_stops") %>%
    arrange(ref_network) %>%
    glimpse()
#  stop("******")
  wiki <-  stringr::str_glue('==relations route: les fichiers de configuration level0==')
  cols <- colnames(df)
  tpl <- "
relation
  on_demand = yes
  contact:phone = +33 2 98 34 42 22
  opening_hours = Mo-Fr 07:30-19:00; Sa 09:00-17:00
"
  tpl <- "
relation
  colour = {relation_colour}
  from = {relation_from}
  gtfs:shape_id = {relation_shape_id}
  name = {relation_name}
  network = {relation_network}
  public_transport:version = 2
  ref = {relation_ref}
  ref:network = {relation_ref_network}
  route = bus
  text_colour = {relation_text_colour}
  to = {relation_to}
  type = route
"
  tags <- Config_tags
  for (tag in tags) {
    col <- sprintf("Config_%s", tag)
    val <- get(col)
    if (! is.na(val)) {
      tpl <- sprintf("%s
  %s = %s", tpl, tag, val)
    }
  }
  tpl <- sprintf("%s
{nodes}", tpl)

  for (i in 1:nrow(df)) {
    for (j in 1:ncol(df)) {
      col <- sprintf("relation_%s", cols[j])
      assign(col, df[[i, j]])
    }

    nodes.df <- data.frame(node = unlist(str_split(df[[i, "stops_osm_id"]], ","))) %>%
      mutate(level0 = sprintf("  nd %s platform", node))
    nodes <-  paste(nodes.df$level0,  collapse = "\n")

    level0 <- str_glue(tpl)
    wiki <- stringr::str_glue('{wiki}
==={relation_ref_network} {relation_name}===
 <nowiki>
{level0}
</nowiki>')

#    print(level0);stop("*****")
    dsn <- sprintf("%s/gtfs2osm_relations_route_level0_%s.txt", osmDir, relation_shape_id)
    dsn <- sprintf("%s/gtfs2osm_relations_route_level0_%s.txt", osmDir, relation_ref_network)
    write(level0, dsn)
    carp("dsn: %s", dsn)
  }
  page <- sprintf("User:Mga_geo/Transports_publics/%s/%s", Config["wiki"], "gtfs2osm_relations_route_level0") %>%
    glimpse()
  wiki_page_init(page = page, article = wiki, force = TRUE)
}
#
# la production du fichier pour le wiki
# source("geo/scripts/transport.R");gtfs2osm_routes_wiki()
gtfs2osm_routes_wiki <- function(rds = "gtfs2osm_routes_stops") {
  library(tidyverse)
  carp()
  dsn <- sprintf("%s/gtfs.zip", gtfsDir)
  carp("dsn: %s", dsn)
  gtfs <- strftime(file.info(dsn)$mtime, format="%d.%m.%Y")
  df1 <- tidytransit_lire(rds = rds) %>%
    glimpse() %>%
    dplyr::select(ref_network, name, from, to, stop_names) %>%
    glimpse()
  wiki <- sprintf('==Les routes gtfs avec les shapes %s==
{|class="wikitable sortable"
|-', gtfs)
  for (col in colnames(df1)) {
    wiki <- stringr::str_glue('{wiki}
!scope="col" class="sortable"| {col}')
  }
  for (i1 in 1:nrow(df1)) {
    wiki <-  stringr::str_glue('{wiki}
|-
')
    for (col in colnames(df1)) {
      val <- df1[[i1, col]]
      wiki <-  stringr::str_glue('{wiki}
| {val}
')
    }
  }
  wiki <- stringr::str_glue('{wiki}
|}')
#  print(wiki)
  page <- sprintf("User:Mga_geo/Transports_publics/%s/%s", Config["wiki"], rds) %>%
    glimpse()
  wiki_page_init(page = page, article = wiki, force = TRUE)
  return(invisible())
}