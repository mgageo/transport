# <!-- coding: utf-8 -->
#
# les réseaux de bus de la région Bretagne
#
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
#
# source("geo/scripts/transport.R");bretagne_jour()
bretagne_jour <- function(force = FALSE) {
  library(tidyverse)
  library(data.table)
  library(janitor)
  library(rio)
  library(tidyverse)
  config_xls("bretagne")
  bretagne_jour_osmose()
#  osmose_issues_gap()
}
#
# vérification de l'appartenance à un network connu
# source("geo/scripts/transport.R");bretagne_relations_route_bus_network()
bretagne_relations_route_bus_network <- function(force = FALSE) {
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
# source("geo/scripts/transport.R");bretagne_nodes_bus()
bretagne_nodes_bus <- function(fic = "bretagne_nodes_bus", force = FALSE, force_osm = FALSE) {
  config_xls("bretagne")
  dsn_rds <- sprintf("%s/%s.rds", osmDir, fic)
  if (file.exists(dsn_rds) && force == FALSE) {
    rc <- readRDS(dsn_rds)
    return(invisible(rc))
  }
  requete <- '
area[name="Bretagne"]->.a;
(
node(area.a)[highway=bus_stop];
node(area.a)[public_transport];
);
out meta;'
  fic <- sprintf("%s.osm", fic)
  dsn <- oapi_requete_get(requete, fic, force = force_osm)
  df <- osmapi_objects_get_tags(dsn) %>%
    glimpse()
  transport_ecrire(df, fic)
}

#
# source("geo/scripts/transport.R");bretagne_nodes_bus()
bretagne_nodes_bus_valid <- function(fic = "bretagne_nodes_bus", force = FALSE, force_osm = FALSE) {
  dsn_rds <- sprintf("%s/%s.rds", osmDir, fic)
  df <- readRDS(dsn_rds)
  df1 <- df %>%
    filter(! is.na(`train`)) %>%
    filter(! is.na(`public_transport`)) %>%
    glimpse()
}
#
# source("geo/scripts/transport.R");bretagne_nodes_bus_platform()
bretagne_nodes_bus_platform <- function(fic = "bretagne_nodes_bus_platform", force = FALSE, force_osm = FALSE) {
  config_xls("bretagne")
  df <- overpass_get(query = "nodes_bus_platform_area", format = "csv", force = force_osm) %>%
    glimpse()
  transport_ecrire(df, fic)
}
#
# source("geo/scripts/transport.R");bretagne_nodes_bus_platform_proche()
bretagne_nodes_bus_platform_proche <- function(fic = "bretagne_nodes_bus_platform") {
  library(sf)
  library(janitor)
  config_xls("bretagne")
  osm.df <- transport_lire(fic) %>%
    clean_names()
  carp("osm.df: %s", nrow(osm.df))
  osm.sf <- st_as_sf(osm.df, coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
    dplyr::select(-highway, -public_transport, -type) %>%
    st_transform(2154)
  depuis <- as.Date("20231001", format = "%Y%m%d")
  zoom <- "left=%0.5f&right=%0.5f&top=%0.5f&bottom=%0.5f"
  df1 <- st_proches(osm.sf, osm.sf, k = 2) %>%
    st_drop_geometry() %>%
    filter(dist < 10) %>%
    filter(timestamp > depuis) %>%
    filter(timestamp.1 > depuis) %>%
    filter(name == name.1) %>%
    filter(version == 1) %>%
    filter(user == "mga_geo") %>%
    filter(version.1 != 1) %>%
    filter(user.1 == "mga_geo") %>%
    mutate(select = sprintf("node%s,node%s", id, id.1)) %>%
    mutate(zoom = sprintf(zoom, `lon` - .002, `lon` + .002, `lat` + .001, `lat` - .001)) %>%
    mutate(josm = sprintf("<a href='http://127.0.0.1:8111/load_and_zoom?%s&select=%s'>josm</a>", zoom, select)) %>%
#    mutate(bash = sprintf("wget 'http://127.0.0.1:8111/load_and_zoom?%s&select=%s';sleep 2", zoom, select)) %>%
    dplyr::select(-select, -zoom) %>%
    glimpse()
#  bash <- paste(df1$bash, "\n", collapse="")
#  writeLines(bash, "d:/toto.sh")
  html <- misc_html_titre(fic)
#  h <- html_df2gt(df1)
  html <- misc_html_append_df(html, df1)
  transport_html_browse(html, fic)
}
#
# source("geo/scripts/transport.R");bretagne_nodes_bus_platform_mga()
bretagne_nodes_bus_platform_mga <- function(fic = "bretagne_nodes_bus_platform_mga", force = FALSE, force_osm = FALSE) {
  config_xls("bretagne")
  df <- overpass_get(query = "nodes_bus_platform_mga", format = "csv", force = force_osm) %>%
    glimpse()
  transport_ecrire(df, fic)
}
#
# source("geo/scripts/transport.R");bretagne_nodes_bus_platform_mga_proche()
bretagne_nodes_bus_platform_mga_proche <- function(fic = "bretagne_nodes_bus_platform_mga") {
  library(sf)
  library(janitor)
  config_xls("bretagne")
  depuis <- as.Date("20231010", format = "%Y%m%d")
  df1 <- transport_lire(fic) %>%
    clean_names() %>%
    filter(timestamp > depuis) %>%
    filter(version == 1) %>%
    mutate(osm = sprintf('<node lat="%s" lon="%s" id="%s" changeset="%s" version="%s"></node>', lat, lon, id, changeset, version)) %>%
    glimpse()
  carp("df1: %s", nrow(df1))
  osm <- paste(df1$osm, "\n", collapse = "")
  changeset_id <- osmapi_put("delete", text = osm)
}
#
## nettoyage des arrêts non référencés
#
# et proche d'aun autre arrêt
# source("geo/scripts/transport.R");bretagne_nodes_bus_platform_ref_get()
bretagne_nodes_bus_platform_ref_get <- function(fic = "bretagne_nodes_bus_platform_ref", force_osm = FALSE) {
  library(sf)
  library(janitor)
  config_xls("bretagne")
  ref.od <- overpass_get(query = "nodes_bus_platform_ref", format = "od", force = force_osm)
  non.od <- overpass_get(query = "nodes_bus_platform_nonref", format = "od", force = force_osm)
  ref.sf <- ref.od$osm_points
  non.sf <- non.od$osm_points
  points.list <- list(
    ref = ref.sf,
    non = non.sf
  )
  saveRDS(points.list, fic)
}
# source("geo/scripts/transport.R");bretagne_nodes_bus_platform_ref()
bretagne_nodes_bus_platform_ref <- function(fic = "bretagne_nodes_bus_platform_ref", force_osm = FALSE) {
  library(sf)
  library(janitor)
  config_xls("bretagne")
  points.list <- readRDS(fic)
  non.sf <- points.list$non %>%
    st_transform(2154)
  ref.sf <- points.list$ref %>%
    st_transform(2154)
  df1 <- st_proches(non.sf, ref.sf) %>%
    st_drop_geometry() %>%
    filter(dist < 1) %>%
    arrange(dist) %>%
#    filter(grepl("Village Collec", name)) %>%
    glimpse()
  df2 <- df1 %>%
    pivot_longer(
      cols = starts_with("ref"),
      names_to = "Ref",
      values_to = "val",
      values_drop_na = TRUE
    ) %>%
    dplyr::select(osm_id, name, Ref, val) %>%
    glimpse()

}
#
# les arrêts osm avec leur attribut ref
# source("geo/scripts/transport.R"); df <- bretagne_nodes_stop_get(force_osm = FALSE) %>% glimpse()
bretagne_nodes_stop_get <- function(force_osm = TRUE) {
  rc <- overpass_get(query = "bus_stop_area", format = "od", force = force_osm)
  nc <- rc$osm_points %>%
    st_drop_geometry() %>%
    filter(public_transport != "stop_position") %>%
    dplyr::select(starts_with("ref:")) %>%
    pivot_longer(
    cols = starts_with("ref"),
      names_to = "Ref",
      values_to = "val",
      values_drop_na = TRUE
    ) %>%
    group_by(Ref) %>%
    summarize(nb = n())
  return(invisible(nc))
}
#
# source("geo/scripts/transport.R"); rc <- bretagne_relations_route_bus()
bretagne_relations_route_bus <- function(fic = "bretagne_relations_route_bus", force = TRUE, force_osm = TRUE) {
  dsn_rds <- sprintf("%s/%s.rds", osmDir, fic)
  if (file.exists(dsn_rds) && force == FALSE) {
    rc <- readRDS(dsn_rds)
    return(invisible(rc))
  }
  rc <- overpass_get(query = "relations_route_bus_area", format = "od", force = force_osm)
  saveRDS(rc, dsn_rds)
  return(invisible(rc))
}
#
# source("geo/scripts/transport.R");bretagne_relations_route_bus_valid()
bretagne_relations_route_bus_valid <- function(fic = "bretagne_relations_route_bus", force = FALSE) {
  dsn_rds <- sprintf("%s/%s.rds", osmDir, fic)
  df <- readRDS(dsn_rds)
  df1 <- df %>%
    filter(! is.na(`review_requested`)) %>%
    glimpse()
}
#
# source("geo/scripts/transport.R");bretagne_relations_route_master_bus()
bretagne_relations_route_master_bus <- function(fic = "bretagne_relations_route_master_bus", force = FALSE) {
  dsn_rds <- sprintf("%s/%s.rds", osmDir, fic)
  if (file.exists(dsn_rds) && force == FALSE) {
    rc <- readRDS(dsn_rds)
    return(invisible(rc))
  }
  requete <- '
area[name="Bretagne"]->.a;
relation(area.a)[route=bus];
rel(br);
out meta;'
  fic <- sprintf("%s.osm", fic)
  dsn <- oapi_requete_get(requete, fic, force = force)
  df <- osmapi_objects_get_tags(dsn) %>%
    glimpse()
  saveRDS(df, dsn_rds)
}
#
# source("geo/scripts/transport.R");bretagne_relations_routes_bus()
bretagne_relations_routes_bus <- function(fic = "bretagne_relations_routes_bus", force = FALSE) {
  dsn_rds <- sprintf("%s/%s.rds", osmDir, fic)
  if (file.exists(dsn_rds) && force == FALSE) {
    rc <- readRDS(dsn_rds)
    return(invisible(rc))
  }
  requete <- '
area[name="Bretagne"]->.a;
(
relation(area.a)[route=bus];
rel(br);
);
out meta;'
  fic <- sprintf("%s.osm", fic)
  dsn <- oapi_requete_get(requete, fic, force = force)
  df <- osmapi_objects_get_tags(dsn) %>%
    glimpse()
  saveRDS(df, dsn_rds)
  return(invisible(df))
}
#
# source("geo/scripts/transport.R");bretagne_relations_routes_disused()
bretagne_relations_routes_disused <- function(fic = "bretagne_relations_routes_disused", force = FALSE) {
  requete <- '
area[name="Bretagne"]->.a;
(
relation(area.a)[~"^(was|disused)"~"bus"];
rel(br);
rel(br);
);
out meta;'
  fic <- sprintf("%s.osm", fic)
  dsn <- oapi_requete_get(requete, fic, force = force)
  return(invisible(dsn))
}
#
## les erreurs transport détectés par osmose
#
# l'api fournit au maximum 500 réponses sur des requêtes géographiques
# donc découpage de la bretagne
# - par commune
# - par département : trop de réponses
# puis interrogation par issue
# source("geo/scripts/transport.R");bretagne_jour_osmose()
bretagne_jour_osmose <- function () {
  config_xls("bretagne")
  osmose_area_jour()
}
# contours simplifiés des communes
# https://www.data.gouv.fr/fr/datasets/contours-des-communes-de-france-simplifie-avec-regions-et-departement-doutre-mer-rapproches/
# https://www.icem7.fr/cartographie/un-fond-de-carte-france-par-commune-optimise-pour-le-web-et-lanalyse-statistique/
#
# https://github.com/osm-fr/osmose-backend/blob/master/analysers/analyser_osmosis_relation_public_transport.py#L579
#
# données ign
# source("geo/scripts/transport.R");bretagne_communes()
bretagne_communes <- function() {
  communes.sf <- ign_adminexpress_lire_sf("COMMUNE") %>%
    filter(INSEE_REG == "53") %>%
    st_transform(4326) %>%
    glimpse()
  for (i in 1:nrow(communes.sf)) {
    bbox <- st_bbox(communes.sf[i, ])
    communes.sf[i, "lon1"] = bbox[1]
    communes.sf[i, "lon2"] = bbox[3]
    communes.sf[i, "lat1"] = bbox[2]
    communes.sf[i, "lat2"] = bbox[4]
  }
  glimpse(communes.sf)
  dsn_rds <- sprintf("%s/%s.rds", varDir, "bretagne_communes")
  saveRDS(communes.sf, dsn_rds)
}
# source("geo/scripts/transport.R");bretagne_communes_osmose()
bretagne_communes_osmose <- function() {
  dsn_rds <- sprintf("%s/%s.rds", varDir, "bretagne_communes")
  communes.sf <- readRDS(dsn_rds)
  df1 <- data.frame()
  for (i in 1:nrow(communes.sf)) {
    carp("i: %s/%s", i, nrow(communes.sf))
    issues <- osmose_bbox_get(communes.sf[[i, "lon1"]], communes.sf[[i, "lat1"]], communes.sf[[i, "lon2"]], communes.sf[[i, "lat2"]])
    if (length(issues) == 0) {
      next;
    }
    issues.df <- issues %>%
      rbindlist()
    df1 <- rbind(df1, issues.df)
  }
  glimpse(df1)
  dsn_rds <- sprintf("%s/%s.rds", varDir, "bretagne_communes_osmose")
  saveRDS(df1, dsn_rds)
}
# source("geo/scripts/transport.R");bretagne_communes_osmose_issues()
bretagne_communes_osmose_issues <- function() {
  dsn_rds <- sprintf("%s/%s.rds", varDir, "bretagne_communes_osmose")
  df1 <- readRDS(dsn_rds) %>%
    glimpse() %>%
    distinct(id) %>%
    glimpse()
  df3 <- data.frame()
  for (i in 1:nrow(df1)) {
    carp("i: %s/%s", i, nrow(df1))
    lst1 <- osmose_issue_get(df1[[i, "id"]])
    lst11 <- lst1[["elems"]]
    lst1["elems"] <- NULL
    df11 <- data.frame()
    for (i11 in 1:length(lst11)) {
      lst3 <- lst11[[i11]]
#    glimpse(lst3)
      lst4 <- list()
      for (j in 1:length(lst3)) {
        if (class(lst3[[j]]) == "list") {
          next
        }
#        carp("j: %s %s",j, class(lst3[[j]]))
        lst4[[names(lst3)[j]]] <- lst3[[j]]
      }
#    glimpse(lst4)

      df4 <- unlist(lst4) %>%
        enframe() %>%
        pivot_wider(names_from = name, values_from = value) %>%
        rename_with( ~ paste("elems", i11, .x, sep = "."))
#      glimpse(df4)
      if (i11 == 1) {
        df11 <- df4
      } else {
        df11 <- cbind(df11, df4)
      }
      lst5 <- lst3[["tags"]]
#    glimpse(lst5)
      lst6 <- list()
      for (j in 1:length(lst5)) {
        lst6[[lst5[[j]]$k]] <- lst5[[j]]$v
      }
#    glimpse(lst6)
      df6 <- unlist(lst6) %>%
        enframe() %>%
        pivot_wider(names_from = name, values_from = value) %>%
        rename_with( ~ paste("tags", i11, .x, sep = "."))
#      glimpse(df6)
      df11 <- cbind(df11, df6)
    }
    df2 <- unlist(lst1) %>%
      enframe() %>%
      pivot_wider(names_from = name, values_from = value)
    df2 <- cbind(df2, df11)
#    glimpse(df2);stop("*****")
    df3 <- bind_rows(df3, df2)
#    break
  }
  dsn_rds <- sprintf("%s/%s.rds", varDir, "bretagne_communes_osmose_issues")
  saveRDS(df3, dsn_rds)
#  glimpse(df3);misc_print(df3)
}
# source("geo/scripts/transport.R");df <- bretagne_communes_osmose_issues_()
bretagne_communes_osmose_issues_ <- function() {
  library(knitr)
  library(kableExtra)
  html <- misc_html_titre("bretagne_communes_osmose")
  dsn_rds <- sprintf("%s/%s.rds", varDir, "bretagne_communes_osmose_issues")
  df1 <- readRDS(dsn_rds) %>%
    glimpse()
  carp("titre: %s", df1[[1, "title.auto"]])
  df2 <- df1 %>%
    group_by(class, title.auto) %>%
    summarize(nb = n()) %>%
    glimpse()
  misc_print(df2)
  html <- misc_html_append(html, "<h1>Stats</h1>")
  html <- misc_html_append_df(html, df2)
  df3 <- df1 %>%
    filter(class == "5") %>%
    dplyr::select(uuid, subtitle = subtitle.auto, elems.1.id,  elems.1.type,  elems.2.id,  elems.2.type
      , tags.1.network, tags.2.network
    ) %>%
    glimpse()
  df4 <- df3 %>%
    mutate(josm = if_else(elems.1.type == "relation", sprintf("%s/%s/full", elems.1.type,  elems.1.id), sprintf("%s/%s",  elems.1.type,  elems.1.id))) %>%
    mutate(josm = sprintf("<a href='http://localhost:8111/import?url=https://api.openstreetmap.org/api/0.6/%s'>josm</a>", josm)) %>%
    mutate(osmose = sprintf("<a href='http://osmose.openstreetmap.fr/api/0.3/issue/%s'>osmose</a>", uuid)) %>%
    mutate(level0 = sprintf("<a href='http://level0.osmz.ru/?url=%s%s, %s%s'>level0</a>"
      , str_sub(elems.1.type,1, 1),  elems.1.id
      , str_sub(elems.2.type,1, 1),  elems.2.id
    )) %>%
    glimpse()
# https://sebastien-foulle.github.io/hebdor_mise_forme_tables.html
  html <- misc_html_append(html, "<h1>Diff route route_master</h1>")
  html <- misc_html_append_df(html, df4)
  df3 <- df1 %>%
    filter(class == "4") %>%
    dplyr::select(uuid, subtitle = subtitle.auto, elems.1.id,  elems.1.type
      , tags.1.network
    ) %>%
    glimpse()
  df4 <- df3 %>%
    mutate(josm = if_else(elems.1.type == "relation", sprintf("%s/%s/full", elems.1.type,  elems.1.id), sprintf("%s/%s",  elems.1.type,  elems.1.id))) %>%
    mutate(josm = sprintf("<a href='http://localhost:8111/import?url=https://api.openstreetmap.org/api/0.6/%s'>josm</a>", josm)) %>%
    mutate(osmose = sprintf("<a href='http://osmose.openstreetmap.fr/api/0.3/issue/%s'>osmose</a>", uuid)) %>%
    mutate(level0 = sprintf("<a href='http://level0.osmz.ru/?url=%s%s'>level0</a>"
      , str_sub(elems.1.type,1, 1),  elems.1.id
    )) %>%
    glimpse()
  html <- misc_html_append(html, "<h1>Diff route sans route_master</h1>")
  html <- misc_html_append_df(html, df4)
  df3 <- df1 %>%
    filter(class == "11") %>%
    dplyr::select(uuid, subtitle = subtitle.auto, elems.1.id,  elems.1.type
      , tags.1.network
    ) %>%
    glimpse()
  df4 <- df3 %>%
    mutate(josm = if_else(elems.1.type == "relation", sprintf("%s/%s/full", elems.1.type,  elems.1.id), sprintf("%s/%s",  elems.1.type,  elems.1.id))) %>%
    mutate(josm = sprintf("<a href='http://localhost:8111/import?url=https://api.openstreetmap.org/api/0.6/%s'>josm</a>", josm)) %>%
    mutate(osmose = sprintf("<a href='http://osmose.openstreetmap.fr/api/0.3/issue/%s'>osmose</a>", uuid)) %>%
    mutate(level0 = sprintf("<a href='http://level0.osmz.ru/?url=%s%s'>level0</a>", str_sub(elems.1.type,1, 1), elems.1.id)) %>%
    mutate(PTNA = sprintf("<a href='https://ptna.openstreetmap.de/relation.php?id=%s&lang=fr'>PTNA</a>", elems.1.id)) %>%
    glimpse()
  html <- misc_html_append(html, "<h1>Diff ordre des stops</h1>")
#  html <- misc_html_append_df(html, df4)
  df3 <- df1 %>%
    filter(class == "1") %>%
    filter(tags.1.route == "bus") %>%
    dplyr::select(uuid, subtitle = subtitle.auto, elems.1.id,  elems.1.type
      , tags.1.network, tags.1.route
    ) %>%
    glimpse()
  df4 <- df3 %>%
    mutate(josm = if_else(elems.1.type == "relation", sprintf("%s/%s/full", elems.1.type,  elems.1.id), sprintf("%s/%s",  elems.1.type,  elems.1.id))) %>%
    mutate(josm = sprintf("<a href='http://localhost:8111/import?url=https://api.openstreetmap.org/api/0.6/%s'>josm</a>", josm)) %>%
    mutate(osmose = sprintf("<a href='http://osmose.openstreetmap.fr/api/0.3/issue/%s'>osmose</a>", uuid)) %>%
    mutate(level0 = sprintf("<a href='http://level0.osmz.ru/?url=%s%s'>level0</a>", str_sub(elems.1.type,1, 1), elems.1.id)) %>%
    mutate(PTNA = sprintf("<a href='https://ptna.openstreetmap.de/relation.php?id=%s&lang=fr'>PTNA</a>", elems.1.id)) %>%
    glimpse()
  html <- misc_html_append(html, "<h1>Diff gap</h1>")
  html <- misc_html_append_df(html, df4)
  dsn_rds <- sprintf("%s/%s.rds", varDir, "bretagne_communes_osmose_issues_gap")
  saveRDS(df4, dsn_rds)
#
  df3 <- df1 %>%
    filter(class %in% c("6", "10")) %>%
    dplyr::select(uuid, class, elems.1.id,  elems.1.type
      , tags.1.network
    ) %>%
    glimpse()
  df4 <- df3 %>%
    mutate(josm = if_else(elems.1.type == "relation", sprintf("%s/%s/full", elems.1.type,  elems.1.id), sprintf("%s/%s",  elems.1.type,  elems.1.id))) %>%
    mutate(josm = sprintf("<a href='http://localhost:8111/import?url=https://api.openstreetmap.org/api/0.6/%s'>josm</a>", josm)) %>%
    mutate(osmose = sprintf("<a href='http://osmose.openstreetmap.fr/api/0.3/issue/%s'>osmose</a>", uuid)) %>%
    mutate(level0 = sprintf("<a href='http://level0.osmz.ru/?url=%s%s'>level0</a>"
      , str_sub(elems.1.type,1, 1),  elems.1.id
    )) %>%
    glimpse()
  html <- misc_html_append(html, "<h1>Tag stop/platform/bus_stop</h1>")
  html <- misc_html_append_df(html, df4)
# The stop_position is not part of a way
  df3 <- df1 %>%
    filter(class %in% c("7")) %>%
    dplyr::select(uuid, class, elems.1.id,  elems.1.type
      , tags.1.network
    ) %>%
    glimpse()
  df4 <- df3 %>%
    mutate(josm = if_else(elems.1.type == "relation", sprintf("%s/%s/full", elems.1.type,  elems.1.id), sprintf("%s/%s",  elems.1.type,  elems.1.id))) %>%
    mutate(josm = sprintf("<a href='http://localhost:8111/import?url=https://api.openstreetmap.org/api/0.6/%s'>josm</a>", josm)) %>%
    mutate(osmose = sprintf("<a href='http://osmose.openstreetmap.fr/api/0.3/issue/%s'>osmose</a>", uuid)) %>%
    mutate(level0 = sprintf("<a href='http://level0.osmz.ru/?url=%s%s'>level0</a>"
      , str_sub(elems.1.type,1, 1),  elems.1.id
    )) %>%
    glimpse()
  html <- misc_html_append(html, "<h1>stop/platform part of a way</h1>")
  html <- misc_html_append_df(html, df4)
# The platform is part of a way, it should have the role stop
  df3 <- df1 %>%
    filter(class == "8") %>%
    dplyr::select(uuid, subtitle = subtitle.auto, elems.1.id,  elems.1.type,  elems.2.id,  elems.2.type
      , tags.1.network, tags.2.network
    ) %>%
    glimpse()
  df4 <- df3 %>%
    mutate(josm = if_else(elems.2.type == "relation", sprintf("%s/%s/full", elems.2.type,  elems.2.id), sprintf("%s/%s",  elems.2.type,  elems.2.id))) %>%
    mutate(josm = sprintf("<a href='http://localhost:8111/import?url=https://api.openstreetmap.org/api/0.6/%s'>josm</a>", josm)) %>%
    mutate(osmose = sprintf("<a href='http://osmose.openstreetmap.fr/api/0.3/issue/%s'>osmose</a>", uuid)) %>%
    mutate(level0 = sprintf("<a href='http://level0.osmz.ru/?url=%s%s, %s%s'>level0</a>"
      , str_sub(elems.1.type,1, 1),  elems.1.id
      , str_sub(elems.2.type,1, 1),  elems.2.id
    )) %>%
    glimpse()
  html <- misc_html_append(html, "<h1>stop/platform part of a way</h1>")
  html <- misc_html_append_df(html, df4)
# Non route relation member in route_master relation
  df3 <- df1 %>%
    filter(class %in% c("3")) %>%
    distinct(class, elems.1.id,  elems.1.type
      , tags.1.network
    ) %>%
    glimpse()
  df4 <- df3 %>%
    mutate(josm = if_else(elems.1.type == "relation", sprintf("%s/%s/full", elems.1.type,  elems.1.id), sprintf("%s/%s",  elems.1.type,  elems.1.id))) %>%
    mutate(josm = sprintf("<a href='http://localhost:8111/import?url=https://api.openstreetmap.org/api/0.6/%s'>josm</a>", josm)) %>%
#    mutate(osmose = sprintf("<a href='http://osmose.openstreetmap.fr/api/0.3/issue/%s'>osmose</a>", uuid)) %>%
    mutate(level0 = sprintf("<a href='http://level0.osmz.ru/?url=%s%s'>level0</a>"
      , str_sub(elems.1.type,1, 1),  elems.1.id
    )) %>%
    glimpse()
  html <- misc_html_append(html, "<h1>Non route relation member in route_master relation</h1>")
  html <- misc_html_append_df(html, df4)
  dsn <- sprintf("%s/bretagne_communes_osmose_issues.html", webDir)
  write(html, dsn)
  carp("dsn: %s", dsn)
  url <- sprintf("http://localhost/transport/bretagne_communes_osmose_issues.html")
  browseURL(
    url,
    browser = "C:/Program Files/Mozilla Firefox/firefox.exe"
  )
  return(invisible(df4))
}
# source("geo/scripts/transport.R");df <- bretagne_communes_osmose_issues_gap()
bretagne_communes_osmose_issues_gap <- function() {
  dsn_rds <- sprintf("%s/%s.rds", varDir, "bretagne_communes_osmose_issues_gap")
  df <- readRDS(dsn_rds) %>%
    glimpse() %>%
# https://stackoverflow.com/questions/45857787/adding-column-if-it-does-not-exist
    (function(.df){
      cls <- c("tags.1.route", "tags.1.network") # columns I need
      # adding cls columns with NAs if not present in the piped data.frame
      .df[cls[!(cls %in% colnames(.df))]] = NA
      return(.df)
    }) %>%
    glimpse() %>%
    filter(tags.1.route == "bus") %>%
    filter(tags.1.network != "QUB")
  if (nrow(df) == 0) {
    carp("**** pas de gap")
    return(invisible(df))
  }
  gaps.df <- data.frame()
  for (i in 1:nrow(df)) {
    df1 <- osm_relation_route_gap(id = df[i, "elems.1.id"], force = TRUE, force_osm = TRUE)
     if (nrow(df1) > 0) {
      misc_print(df1)
#      stop("*****")
      gaps.df <- rbind(gaps.df, df1)
    }
  }
  misc_print(gaps.df)
}