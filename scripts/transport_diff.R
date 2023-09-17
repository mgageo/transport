# <!-- coding: utf-8 -->
#
# le réseau de bus
#
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
## script pour les comparaisons
#
# le réseau est configuré en variable globale dans transport.R
#
# source("geo/scripts/transport.R");diff_jour()
diff_jour <- function(reseau = Reseau, force = TRUE) {
  library(tidyverse)
  library(rio)
  library(sf)
  carp()
  config_xls(reseau)
#  tidytransit_jour(force = force)
  tidytransit_routes_stops(force = FALSE)
  gtfs2osm_routes_stops(force = force)
  diff_routes_shapes_tags(force = force)
}
#
# source("geo/scripts/transport.R");diff_routes()
diff_routes <- function(reseau = Reseau, force = TRUE) {
  library(tidyverse)
  library(rio)
  library(sf)
  carp()
  config_xls(reseau)
  gtfs2osm_routes_stops(force = force)
  diff_routes_tags(force = force)
  if( Config[1, "shapes"] == "TRUE" ) {
    gtfs2osm_routes_shapes_stops(force = force)
  }
}
#
# l'enchainement pour un réseau avec shapes
# source("geo/scripts/transport.R");diff_toto()
diff_toto <- function(force = FALSE) {
  library(tidyverse)
  library(rio)
  library(sf)
  carp()
  tidytransit_shapes_stops()
  tidytransit_routes_shapes_stops()
  gtfs2osm_routes_stops(force_osm = force)
  if( Config[1, "shapes"] == "TRUE" ) {
    gtfs2osm_routes_shapes_stops(force = force)
    diff_routes_shapes_tags(force = force)
  } else {
    diff_routes_tags(force = force)
  }
  return(invisible())
}
#
# comparaison pour les stops
# source("geo/scripts/transport.R");diff_stops(force_osm = FALSE)
diff_stops <- function(force_osm = TRUE) {
  library(tidyverse)
  library(data.table)
  library(janitor)
  carp()
  titre <- "diff_stops_gtfs"
  html <- misc_html_titre(titre)
  html <- misc_html_append(html, "<h1>GTFS</h1>")
  gtfs_stops.sf <- tidytransit_stops_sf() %>%
    glimpse() %>%
    mutate(gtfs = "gtfs") %>%
    glimpse()
  zoom <- "left=%0.5f&right=%0.5f&top=%0.5f&bottom=%0.5f"
  gtfs1.df <- gtfs_stops.sf %>%
    st_drop_geometry() %>%
    mutate(zoom = sprintf(zoom, `stop_lon` - .002, `stop_lon` + .002, `stop_lat` + .001, `stop_lat` - .001)) %>%
    mutate(josm = sprintf("<a href='http://127.0.0.1:8111/load_and_zoom?%s'>josm</a>", zoom)) %>%
    dplyr::select(city, stop_name, stop_id, stop_code, josm) %>%
    arrange(city, stop_name) %>%
    glimpse()
#  html <- misc_html_append_df(html, gtfs1.df)

  osm.df <- overpass_get(query = "bus_stop_kref", format = "csv", force = force_osm) %>%
    rename(k_ref = Config[1, 'k_ref']) %>%
    mutate(k_ref = sprintf("%s", k_ref)) %>%
    clean_names() %>%
    glimpse()
#
  gtfs1.df$k_ref <- gtfs1.df$stop_code
  gtfs1.df$k_ref <- gtfs1.df$stop_id
  html <- misc_html_append(html, "<h1>les références inconnues dans le gtfs</h1>")
  df1 <- osm.df %>%
    filter(k_ref %notin% gtfs1.df$k_ref) %>%
    arrange(k_ref) %>%
    glimpse()
  html <- misc_html_append_df(html, df1)
  html <- misc_html_append(html, "<h1>les références inconnues dans osm</h1>")
  df2 <- gtfs1.df %>%
    filter(k_ref %notin% osm.df$k_ref) %>%
    arrange(k_ref) %>%
    glimpse()
  html <- misc_html_append_df(html, df2)
  dsn <- sprintf("%s/%s.html", webDir, titre)
  write(html, dsn)
  carp("dsn: %s", dsn)
  url <- sprintf("http://localhost/transport/%s.html", titre)
  browseURL(
    url,
    browser = "C:/Program Files/Mozilla Firefox/firefox.exe"
  )
}

#
# comparaison à partir des routes gtfs
# source("geo/scripts/transport.R");diff_routes_tags(reseau = Reseau, force = FALSE)
diff_routes_tags <- function(force = TRUE) {
  library(tidyverse)
  library(data.table)
  library(sf)
  library(janitor)
  carp()
# osm
  carp("osm: les relations")
  osm.df <- overpass_get(query = "relations_route_bus_network", format = "csv", force = force)
  carp("osm: les doublons")
  df1 <- osm.df %>%
    group_by(`ref:network`) %>%
    summarize(nb = n()) %>%
    filter(nb > 1)
  if (nrow(df1) > 0) {
    glimpse(df1)
    df2 <- osm.df %>%
      filter(`ref:network` %in% df1$`ref:network`) %>%
      arrange(`ref:network`) %>%
      dplyr::select(`ref:network`, `@id`)
    misc_print(df2)
    carp("**** doublons nb: %s", nrow(df1))
  }
  carp("osm: tri par ref_network")
  osm.df <- osm.df %>%
    arrange(`ref:network`) %>%
    rename_with( ~ paste0(.x, ".osm")) %>%
    glimpse()
# le gtfs
  carp("gtfs: les routes avec les stops")
  dsn <- sprintf("%s/%s", transportDir, "gtfs2osm_routes_stops.csv")
  routes.df <- fread(dsn, encoding = "UTF-8") %>%
    rename(`ref:network` = ref_network) %>%
    arrange(`ref:network`) %>%
    rename_with( ~ paste0(.x, ".gtfs")) %>%
    glimpse()
  carp("osm et gtfs: fusion par ref_network")
  df1 <- osm.df %>%
    full_join(routes.df, by = c("ref:network.osm" = "ref:network.gtfs")) %>%
    glimpse()
  carp("osm et gtfs: absent du gtfs")
  df1 %>%
    filter(is.na(name.gtfs)) %>%
    glimpse()
  carp("osm et gtfs: absent d'osm")
  df1 %>%
    filter(is.na(name.osm)) %>%
    glimpse()
  carp("osm et gtfs: les communs")
  df1 <- df1 %>%
    filter(!is.na(name.gtfs)) %>%
    filter(! is.na(name.osm)) %>%
    select(order(colnames(.))) %>%
    glimpse()
  carp("osm et gtfs: tri par ref_network")
  df2 <- df1 %>%
    dplyr::select(kref = `ref:network.osm`, from.gtfs, from.osm, to.gtfs, to.osm) %>%
    arrange(kref)
  misc_print(df2)
  df2 <- df1 %>%
    dplyr::select(kref = `ref:network.osm`, name.gtfs, name.osm) %>%
    arrange(kref)
  misc_print(df2)
}
#
# comparaison à partir des routes gtfs
# source("geo/scripts/transport.R");diff_routes_shapes_tags(force = FALSE)
diff_routes_shapes_tags <- function(rds = "diff_routes_shapes_tags", force = TRUE) {
  library(tidyverse)
  library(data.table)
  library(sf)
  library(janitor)
  carp()
  titre <- sprintf("diff_routes_shapes_tags_%s", Config[1, "reseau"])
  html <- misc_html_titre(titre)
  html <- misc_html_append(html, sprintf("<h1>%s</h1>", titre))
# osm
  carp("osm: les relations")
  osm.df <- osm_valid_relations_route_bus_network(force = force)
  carp("osm: tri par ref_network")
  osm.df <- osm.df %>%
    arrange(kref) %>%
    rename_with( ~ paste0(.x, ".osm")) %>%
    glimpse()
# le gtfs
  carp("gtfs: les routes avec les stops")
  dsn <- sprintf("%s/%s", transportDir, "gtfs2osm_routes_shapes_stops.csv")
  gtfs.df <- fread(dsn, encoding = "UTF-8") %>%
    mutate(kref = `shape_id`) %>%
    arrange(kref) %>%
    rename_with( ~ paste0(.x, ".gtfs")) %>%
    glimpse()
  carp("osm et gtfs: fusion par ref_network")
  df1 <- osm.df %>%
    full_join(gtfs.df, by = c("kref.osm" = "kref.gtfs")) %>%
    rename(kref = kref.osm) %>%
    glimpse()
  carp("osm et gtfs: absent du gtfs")
  df11 <- df1 %>%
    filter(is.na(name.gtfs)) %>%
    arrange(kref)
  if (nrow(df11) > 0) {
    html <- misc_html_append_df(html, "<h2>osm et gtfs: absent du gtfs</h2>")
    df12 <- df11 %>%
      select(-ends_with(".gtfs")) %>%
      rename_at(vars(matches(".osm")), ~str_remove(., ".osm")) %>%
      dplyr::select(ref_network, `gtfs:shape_id`, id = `@id`, version = `@version`, from, to) %>%
      mutate(level0 = sprintf("<a href='http://level0.osmz.ru/?url=%s%s'>level0</a>", "r", id)) %>%
      glimpse()
    html <- misc_html_append_df(html, df12)
  }
  html <- misc_html_append_df(html, "<h2>osm et gtfs: les communs</h2>")
  df11 <- df1 %>%
    filter(! is.na(name.gtfs)) %>%
    filter(! is.na(name.osm)) %>%
    select(order(colnames(.))) %>%
    glimpse()
  tidytransit_sauve(df11, rds)
  carp("osm et gtfs: tri par ref_network")
  df12 <- df11 %>%
    dplyr::select(kref, from.gtfs, from.osm, to.gtfs, to.osm) %>%
    arrange(kref)
  html <- misc_html_append_df(html, df12)
  df12 <- df11 %>%
    dplyr::select(kref, name.gtfs, name.osm) %>%
    arrange(kref)
#
# le fichier de sortie
  fic <- sprintf("%s.html", titre)
  dsn <- sprintf("%s/%s", webDir, fic)
  write(html, dsn)
  carp("dsn: %s", dsn)
  url <- sprintf("http://localhost/transport/%s", fic)
  browseURL(
    url,
    browser = "C:/Program Files/Mozilla Firefox/firefox.exe"
  )
}
# source("geo/scripts/transport.R");diff_routes_tags(force = FALSE)
diff_routes_tags_toto <- function(rds = "diff_routes_shapes_tags") {
  carp("osm et gtfs: les communs")
  df2 <- df1 %>%
    filter(!is.na(name.gtfs)) %>%
    filter(! is.na(name.osm)) %>%
    select(order(colnames(.))) %>%
    glimpse()
  carp("osm et gtfs: tri par ref_network")
  df3 <- df2 %>%
    dplyr::select(kref, from.gtfs, from.osm, to.gtfs, to.osm) %>%
    arrange(kref)
  misc_print(df2)
  df4 <- df2 %>%
    dplyr::select(kref, name.gtfs, name.osm) %>%
    arrange(kref)
  misc_print(df2)
#
  carp("tentative de rapprochement")
  df11 <- osm.df %>%
    full_join(gtfs.df, by = c("ref_network.osm" = "ref_network.gtfs")) %>%
    glimpse() %>%
    dplyr::select(id = "@id.osm", ref_network = ref_network.osm, kref.osm, kref.gtfs, from.osm, from.gtfs, to.osm, to.gtfs) %>%
    filter(kref.osm != kref.gtfs) %>%
    glimpse()
  misc_print(df11)
  tidytransit_sauve(df11, rds)
  df12 <- df11 %>%
    mutate(id = sprintf("<a href='http://level0.osmz.ru/?url=relation/%s' target='_blank'>%s</a>", id, id))
  titre <- rds
  misc_html_df2fic(df12, titre)
}
# source("geo/scripts/transport.R");diff_routes_tags(force = FALSE)
diff_routes_tag_shape_id <- function(rds = "diff_routes_shapes_tags") {
  carp("rds: %s", rds)
  df1 <- tidytransit_lire(rds) %>%
    glimpse()
  type <- "relation"
  for (i1 in 1:nrow(df1)) {
    carp("i1: %s", i1)
    tags.df <- tribble(
      ~name, ~value,
      "gtfs:shape_id", df1[[i1, "kref.gtfs"]],
    )
    nb_modifs <- osmchange_object_modify_tags(id = df1[[i1, "id"]], type = type, tags = tags.df)
    if (nb_modifs > 0) {
#      break
    }
  }
}
diff_routes_tags_tutu <- function(rds = "diff_routes_tags", force = FALSE) {
  carp("rds: %s", rds)
  df1 <- tidytransit_lire(rds) %>%
    glimpse()
  champs.df <- tribble(
    ~osm, ~gtfs,
    "name.osm", "name.gtfs",
#    "ref:network.osm", "ref:network.gtfs",
    "from.osm", "from.gtfs",
    "to.osm", "to.gtfs",
    "colour.osm", "colour.gtfs",
    "text_colour.osm", "text_colour.gtfs",
  )
  for (i1 in 1:nrow(df1)) {
    carp("i1: %s %s", i1, df1[[i1, "kref"]])
    tags.osm <- list()
    tags.gtfs <- list()
    tags.df <- champs.df
    tags.df$OSM <- NA
    tags.df$GTFS <- NA
    for (ic in 1:nrow(champs.df)) {
      tags.df[ic, "OSM"] <- df1[[i1, tags.df[[ic, "osm"]]]]
      tags.df[ic, "GTFS"] <- df1[[i1, tags.df[[ic, "gtfs"]]]]
    }
    if (identical(tags.df$OSM, tags.df$GTFS)) {
      next
    }
    misc_print(tags.df)
    if (force == "toto") {
      tags.df <- tags.df %>%
        dplyr::select(name = osm, value = GTFS)
      id <- df1[[i1, "@id"]]
      type <- "relation"
      osmchange_object_modify_tags(id = id, type = type, tags = tags.df)
    }
  }
}
