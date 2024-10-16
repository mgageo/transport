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
# les tags du fichier de configuration Excel
Config_tags <- c("operator", "website", "network:wikidata", "network:wikipedia", "operator:wikidata", "operator:wikipedia")

# source("geo/scripts/transport.R");gtfs2osm_jour()
gtfs2osm_jour <- function(force = TRUE, force_osm = TRUE) {
  library(tidyverse)
  library(rio)
  library(sf)
  carp()
# mise au format interne des fichiers gtfs pour les relations route_master
  gtfs2osm_jour_routemasters(force = force, force_osm = force_osm)
# mise au format interne des fichiers gtfs pour les routes
  gtfs2osm_relations_route_stops(force_osm = force_osm)
#  gtfs2osm_relations_route_level0(force_osm = force_osm)
#  gtfs2osm_routes_wiki("gtfs2osm_routes_stops")
  if( Config[1, "shapes"] == "TRUE" ) {
    gtfs2osm_relations_route_shape_stops(force_osm = force_osm)
    gtfs2osm_relations_route_level0(dessertes = "toutes")
    gtfs2osm_relations_route_level0(dessertes = "arrets")
#    gtfs2osm_relations_route_shape_wiki("gtfs2osm_routes_shapes_stops")
  }
  return(invisible())
}
# mise au format interne des fichiers gtfs pour les relations route_master
# source("geo/scripts/transport.R");gtfs2osm_jour_routemasters()
gtfs2osm_jour_routemasters <- function(force = TRUE, force_osm = TRUE) {
  library(tidyverse)
  library(rio)
  library(sf)
  carp()
  gtfs2osm_relations_routemaster();
  gtfs2osm_relations_routemaster_level0(force_osm = force_osm)
  return(invisible())
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
  df <- full_join(gtfs.df, osm.df, by = c("shape_id" = "ref")) %>%
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
  template <- 'node: {{stop_lat}}, {{stop_lon}}
  highway = bus_stop
  public_transport = platform
  name = {{stop_name}}
  {{k_ref}} = {{stop_id}}
  gtfs:stop_id == {{stop_id}}
  gtfs:stop_name = {{stop_name}}
'
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
# source("geo/scripts/transport.R");gtfs2osm_relations_routemaster()
gtfs2osm_relations_routemaster <- function(rds = "gtfs2osm_relations_routemaster", force_osm = TRUE) {
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
  if (grepl("brest", Config_reseau)) {
    df <- df %>%
      mutate(Name = str_glue('{Config_route_name} {route_short_name} : {route_long_name}')) %>%
      glimpse()
  }
  if (grepl("rennes", Config_reseau)) {
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
      "gtfs:route_short_name" = route_short_name,
      "gtfs:route_long_name" = route_long_name,
      "gtfs:route_id" = route_id
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
  page <- sprintf("User:Mga_geo/Transports_publics/%s/%s", Config["wiki"], rds)
  wiki <- wiki_df2table(df)
  wiki_page_init(page = page, article = wiki, force = TRUE)
}

#
# pour level0
# source("geo/scripts/transport.R");gtfs2osm_relations_routemaster_level0()
gtfs2osm_relations_routemaster_level0 <- function(rds = "gtfs2osm_relations_routemaster", force_osm = TRUE) {
  library(stringr); # pour str_glue
  df <- misc.lire(rds, dir = transportDir) %>%
    arrange(ref) %>%
    clean_names() %>%
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
  description = {gtfs_description}
  name = {gtfs_name}
  network = {Config_network}
  public_transport:version = 2
  ref = {gtfs_ref}
  ref:network = {gtfs_ref_network}
  gtfs:route_id = {gtfs_gtfs_route_id}
  gtfs:route_short_name = {gtfs_gtfs_route_short_name}
  gtfs:route_long_name = {gtfs_gtfs_route_long_name}
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
    dsn <- sprintf("%s/gtfs2osm_relations_routemaster_level0_%s.txt", osmDir, gtfs_ref)
    write(level0, dsn)
    carp("dsn: %s", dsn)

  }
  page <- sprintf("User:Mga_geo/Transports_publics/%s/gtfs2osm/%s", Config["wiki"], "relations_routemaster_level0") %>%
    glimpse()
#  wiki_page_init(page = page, article = wiki, force = TRUE)
}
#
#################################################################################
#
# pour les relations route
#
#
# la conversion
# source("geo/scripts/transport.R");gtfs2osm_relations_route()
gtfs2osm_relations_route <- function(rds = "gtfs2osm_relations_route", force_osm = TRUE) {
  library(tidyverse)
  library(tidytransit)
  carp("selection de la route avec le plus de stops/voyages")
  df1 <- tidytransit_lire(rds = "tidytransit_routes_stops") %>%
    group_by(ref_network) %>%
    arrange(desc(nb_stops), desc(nb)) %>%
#    filter(row_number() == 1) %>%
    mutate(Ordre = row_number()) %>%
    ungroup() %>%
    glimpse()
  df2 <- gtfs2osm_routes_osm(df1, stops_osm = FALSE, force_osm = force_osm) %>%
    select(order(colnames(.))) %>%
    glimpse()
  df2 <- df2 %>%
    dplyr::select(colour, description, from, name, network, operator,`public_transport:version`
      ,ref, ref_network, Ordre, shape_id, route, text_colour, to, type, stops, stop_names = names
      ) %>%
    glimpse()
  tags <- Config_tags
  for (tag in tags) {
    col <- sprintf("Config_%s", tag)
    val <- get(col)
    if (! is.na(val)) {
      df2[, tag] <- val
    }
  }
  transport_sauve(df2, rds)
  dsn <- sprintf("%s/%s.csv", transportDir, rds)
  readr::write_tsv(df2, file = dsn)
  carp("dsn: %s", dsn)
}
# source("geo/scripts/transport.R");gtfs2osm_relations_route_stops()
gtfs2osm_relations_route_stops <- function(rds = "gtfs2osm_relations_route_stops", force_osm = TRUE) {
  library(tidyverse)
  library(tidytransit)
  carp("selection de la route avec le plus de stops/voyages")
  df1 <- tidytransit_lire(rds = "tidytransit_routes_stops") %>%
    group_by(ref_network) %>%
    arrange(desc(nb_stops), desc(nb)) %>%
    mutate(Ordre = row_number()) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    glimpse()
  df2 <- gtfs2osm_routes_osm(df1, force_osm = force_osm) %>%
    select(order(colnames(.))) %>%
    glimpse()
  df2 <- df2 %>%
    dplyr::select(colour, description, from, name, network, operator,`public_transport:version`
      ,ref, ref_network, Ordre, shape_id, route, text_colour, to, type, route_short_name, route_long_name
      , stops, stops_code, stop_names = names
      , stops_osm_id, stops_osm_name, nb, nb_stops
      , "gtfs:route_short_name" = route_short_name
      , "gtfs:route_long_name" = route_long_name
      , "gtfs:trip_id:sample" = trip_id
      , "gtfs:route_id" = route_id
    )
  tags <- Config_tags
  for (tag in tags) {
    col <- sprintf("Config_%s", tag)
    val <- get(col)
    if (! is.na(val)) {
      df2[, tag] <- val
    }
  }
  transport_sauve(df2, rds)
  dsn <- sprintf("%s/%s.csv", transportDir, rds)
  readr::write_tsv(df2, file = dsn)
  carp("dsn: %s", dsn)
}
#
# la conversion
# source("geo/scripts/transport.R");gtfs2osm_relations_route_shape_stops()
gtfs2osm_relations_route_shape_stops <- function(rds = "gtfs2osm_relations_route_shape_stops", force_osm = TRUE) {
  library(tidyverse)
  library(tidytransit)
  carp()
  df1 <- tidytransit_lire(rds = "tidytransit_shapes_stops") %>%
    arrange(ref_network, shape_id) %>%
    glimpse()
  df4 <- gtfs2osm_routes_osm(df1, force_osm = force_osm) %>%
    glimpse()
  df4 <- df4 %>%
    dplyr::select(colour, description, from, name, network, operator,`public_transport:version`
      ,ref, ref_network, Ordre, shape_id, route, text_colour, to, type
      , stops, stops_code, stop_names = names
      , stops_osm_id, stops_osm_name, nb_stops, nb,
      , "gtfs:route_short_name" = route_short_name
      , "gtfs:route_long_name" = route_long_name
      , "gtfs:trip_id:sample" = trip_id
      , "gtfs:route_id" = route_id
    )
  tags <- Config_tags
  for (tag in tags) {
    col <- sprintf("Config_%s", tag)
    val <- get(col)
    if (! is.na(val)) {
      df4[, tag] <- val
    }
  }
  df4 <- df4 %>%
    select(order(colnames(.))) %>%
    glimpse()
  transport_sauve(df4, rds)
  dsn <- sprintf("%s/%s.csv", transportDir, rds)
  readr::write_tsv(df4, file = dsn)
  carp("dsn: %s", dsn)
}
#
# les attributs de la relation route
gtfs2osm_routes_osm <- function(df1, stops_osm = TRUE, force_osm = TRUE) {
  library(tidyverse)
  library(tidytransit)
  library(stringr)
  carp("Config_reseau: %s", Config_reseau)
#  stop("*****")
#  if (!is.na(Config_route_prefixe)) {
#    df1 <- df1 %>%
#      mutate(ref_network = sprintf("%s%s", Config_route_prefixe, ref_network)) %>%
#      glimpse()
#  }

  df2 <- df1 %>%
    mutate(Route_color = sprintf("#%s", toupper(route_color))) %>%
    mutate(Route_text_color = sprintf("#%s", toupper(route_text_color))) %>%
    mutate(Route_color = ifelse(grepl("^#[0-9A-F]{6}$", Route_color), Route_color, "#000000")) %>%
    mutate(Route_text_color = ifelse(grepl("^#[0-9A-F]{6}$", Route_text_color), Route_text_color, "#FFFFFF")) %>%
    mutate(from = str_glue('{first_city} ({first_name})')) %>%
    mutate(to = str_glue('{last_city} ({last_name})')) %>%
    mutate(from_city = ifelse(str_detect(first_name, first_city), first_name, str_glue('{first_city} ({first_name})'))) %>%
    mutate(to_city = ifelse(str_detect(last_name, last_city), last_name, str_glue('{last_city} ({last_name})'))) %>%
    mutate(name = str_glue('{Config_route_name} {route_short_name} : {first_name} => {last_name}')) %>%
    mutate(description = str_glue('Ligne {route_short_name} : {first_city} ({first_name}) => {last_city} ({last_name})')) %>%
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
      mutate(name = str_glue('{Config_route_name} {route_long_name} : {from} => {to}')) %>%
#      filter(ref == "87") %>%
      glimpse()
#    stop("***")
  }
  if (Config_city == "STAR") {
    carp("Config_city == STAR")
    df2 <- df2 %>%
      mutate(description = str_glue('{route_long_name}')) %>%
      mutate(name = str_glue('{Config_route_name} {route_short_name} Direction {to_city}')) %>%
      glimpse()
  }
  if (grepl("breizhgo", Config_reseau)) {
    df2 <- df2 %>%
      mutate(name = str_glue('{Config_route_name} {route_short_name} {route_long_name} : {from_city} => {to_city}')) %>%
      glimpse()
  }
  if (Reseau == "lorient") {
    df2 <- df2 %>%
#      mutate(name = str_glue('{Config_route_name} {route_short_name} : {first_name} => {last_name}')) %>%
      mutate(name = str_glue('{Config_route_name} {route_short_name} : {first_city} ({first_name}) => {last_city} ({last_name})')) %>%
      glimpse()
  }
  if (grepl("semo", Config_reseau)) {
    df2 <- df2 %>%
      mutate(description = str_glue('Ligne {route_short_name} : {route_long_name}')) %>%
      glimpse()
  }
  if (grepl("nomad", Config_reseau)) {
    carp("Config_reseau: %s", Config_reseau)
#    stop("*****")
    df2 <- df2 %>%
      mutate(description = str_glue('Ligne {route_short_name} : {route_long_name}')) %>%
      mutate(from = str_glue('{first_name}')) %>%
      mutate(to = str_glue('{last_name}')) %>%
      glimpse()

  }
  if (stops_osm == FALSE) {
    return(invisible(df2))
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
  if ( Reseau == "vannes") {
    stops_zone <- "stops_code"
  }
  for (i2 in 1:nrow(df2)) {
    stops <- df2[[i2, stops_zone]]
    stops.df <- data.frame(stop = unlist(strsplit(stops, ";"))) %>%
      left_join(osm_stops.df, by = c("stop" = "k_ref")) %>%
      mutate(type = dplyr::recode(type,
        "way" = "wy",
        "node" = "nd"
      )) %>%
      mutate(id = sprintf("%s %s", type, id))
    df3 <- stops.df %>%
      filter(is.na(id))
    if( nrow(df3) > 0) {
      glimpse(df3)
      glimpse(df2[i2, ])
      confess("***** stop inconnu nb: %s", nrow(df3))
    }
#    glimpse(stops.df); stop("****")
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
gtfs2osm_relations_route_level0 <- function(dessertes = "stops", force_osm = TRUE) {
  library(stringr); # pour str_glue
  if( Config[1, "shapes"] == "TRUE" ) {
    rds <- "gtfs2osm_relations_route_shape_stops"
  } else {
    rds <- "gtfs2osm_relations_route_stops"
  }
  df <- transport_read(rds)
  if (dessertes == "toutes") {
    df <- df %>%
      arrange(ref_network, desc(nb), desc(nb_stops)) %>%
      glimpse()
  }
  if (dessertes == "stops") {
    df <- df %>%
      group_by(ref_network) %>%
      arrange(desc(nb), desc(nb_stops)) %>%
      filter(row_number() == 1) %>%
      ungroup() %>%
      arrange(ref_network) %>%
      glimpse()
  }
#  stop("******")
  wiki <-  stringr::str_glue('==relations route: les fichiers de configuration level0==')
  wiki <- ""
  tpl <- "
relation
  on_demand = yes
  contact:phone = +33 2 98 34 42 22
  opening_hours = Mo-Fr 07:30-19:00; Sa 09:00-17:00
"
  tpl <- "
relation
  colour = {relation_colour}
  description = {relation_description}
  from = {relation_from}
  gtfs:route_id = {relation_gtfs_route_id}
  gtfs:route_long_name = {relation_gtfs_route_long_name}
  gtfs:route_short_name = {relation_gtfs_route_short_name}
  gtfs:shape_id = {relation_shape_id}
  name = {relation_name}
  network = {relation_network}
  public_transport:version = 2
  ref = {relation_ref}
  ref:network = {relation_ref_network}
  route = bus
  text_colour = {relation_text_colour}
  to = {relation_to}
  type = route"
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
  df <- df %>%
    clean_names() %>%
    select(order(colnames(.))) %>%
    glimpse()
  cols <- colnames(df)
  for (i in 1:nrow(df)) {
    for (j in 1:ncol(df)) {
      col <- sprintf("relation_%s", cols[j])
#      Carp("%s => %s", col, df[[i, j]])
      assign(col, df[[i, j]])
    }
#    glimpse(df[i, ])
    nodes.df <- data.frame(node = unlist(str_split(df[[i, "stops_osm_id"]], ","))) %>%
      mutate(level0 = sprintf("  %s platform", node))
    nodes <-  paste(nodes.df$level0,  collapse = "\n")

    level0 <- str_glue(tpl)
    wiki <- stringr::str_glue('{wiki}
==={relation_ref_network} {relation_name}===
nb_stops: {relation_nb_stops} nb_services: {relation_nb}

{relation_stops_osm_name}
 <nowiki>
{level0}
</nowiki>')

#    print(level0);stop("*****")
    dsn <- sprintf("%s/gtfs2osm_relations_route_level0_%s.txt", osmDir, relation_shape_id)
    dsn <- sprintf("%s/gtfs2osm_relations_route_level0_%s.txt", osmDir, relation_ref_network)
    write(level0, dsn)
    carp("dsn: %s", dsn)
  }
  page <- sprintf("User:Mga_geo/Transports_publics/%s/gtfs2osm/%s_%s", Config["wiki"], "relations_route_level0", dessertes) %>%
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