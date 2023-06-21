# <!-- coding: utf-8 -->
#
# utilisation d'osmose
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
# auteur : Marc Gauthier
#
# https://wiki.openstreetmap.org/wiki/Osmose/api/0.3
# https://github.com/osm-fr/osmose-backend/blob/master/analysers/analyser_osmosis_relation_public_transport.py#L592
#
# http://osmose.openstreetmap.fr/api/0.3/items/1260/class/4?langs=auto
# http://osmose.openstreetmap.fr/api/0.3/issues?item=1260&bbox=-3.41043,47.72423,-3.35011,47.77449&limit=500
# http://osmose.openstreetmap.fr/api/0.3/issues?item=1260&class=4&country=france_bretagne*&limit=500
#
# http://osmose.openstreetmap.fr/api/0.3/issue/8676c5fa-fd69-5cb8-c021-0e5c23ae1454
osmose_host <- "http://osmose.openstreetmap.fr/api/0.3"
#
## la recherche de tous les signalements d'une zone
# détermination avec l'overpass de la zone
# utilisation de la bounding box
# découpage en carrés pour avoir moins de 500 signalements
# interrogation par carré
# interrogation par signalement
# production page html
#
# 2140 : Jungle Bus – validation ruleset
# https://github.com/Jungle-Bus/transport_mapcss/blob/master/transport.validator.mapcss
#
# source("geo/scripts/transport.R"); osmose_area_jour(force = FALSE)
osmose_area_jour <- function(force = TRUE) {
#  osmose_area_get(force = FALSE)
  osmose_country_get(country = Config[1, "zone_osmose"], force = TRUE)
  osmose_area_issues()
  osmose_area_issues_html()
}
#
# source("geo/scripts/transport.R"); json <- osmose_area_get(force = FALSE)
osmose_area_get <- function(force = TRUE) {
  library(tidyverse)
  library(xml2)
  fic <- "osmose_area"
  requete <- sprintf('
area[name="%s"]["boundary"="administrative"]["admin_level"="%s"];
nwr(pivot);
out geom;', Config[1, 'zone'], Config[1, 'zone_level'])
  requete <- sprintf('
rel(%s);
out ids bb;', Config[1, 'zone_relation'])
  doc <- overpass_query_xml(requete, fic, force = force)
  bounds <- osm_has_xpath(doc, "//relation/bounds")
  if (!bounds) {
    stop("***")
  }
  df <- xml2::xml_find_all(doc, "//relation/bounds") %>%
    map(xml_attrs) %>%
    map(as.list) %>%
    map_df(as_tibble) %>%
    mutate_if(is.character, as.numeric) %>%
    glimpse()
  mga <<- df
  steplon <- 0.2
  steplat <- 0.2
  df1 <- data.frame()
  for (lon in seq(from=df[[1, "minlon"]], to = df[[1, "maxlon"]], by = steplon)) {
    for (lat in seq(from=df[[1, "minlat"]], to = df[[1, "maxlat"]], by = steplat)) {
      issues <- osmose_bbox_get(lon1 = lon, lat1 = lat, lon2 = lon + steplon, lat2 = lat + steplat, force = TRUE)
      if (length(issues) == 0) {
        next;
      }
      issues.df <- issues %>%
        rbindlist()
      df1 <- rbind(df1, issues.df)
    }
  }
  glimpse(df1)
  dsn_rds <- sprintf("%s/%s_%s.rds", varDir, "osmose_area", Config[1, "reseau"])
  saveRDS(df1, dsn_rds)
}
# source("geo/scripts/transport.R");osmose_area_issues()
osmose_area_issues <- function() {
  dsn_rds <- sprintf("%s/%s_%s.rds", varDir, "osmose_area", Config[1, "reseau"])
  df1 <- readRDS(dsn_rds)
  df2 <- df1 %>%
    group_by(item) %>%
    summarize(nb = n()) %>%
    glimpse()
#  misc_print(df2);return()
  df1 <- df1 %>%
    distinct(id)
  df3 <- data.frame()
  for (i in 1:nrow(df1)) {
    carp("i: %s/%s", i, nrow(df1))
    df2 <- osmose_issue_parse(id = df1[[i, "id"]])
    df3 <- bind_rows(df3, df2)
  }
  dsn_rds <- sprintf("%s/%s_%s.rds", varDir, "osmose_area_issues", Config[1, "reseau"])
  saveRDS(df3, dsn_rds)
#  glimpse(df3);misc_print(df3)
}
# source("geo/scripts/transport.R"); osmose_issue_get(id, force = TRUE)
osmose_issue_get <- function(id, force = TRUE) {
  library(tidyverse)
  library(jsonlite)
  library(httr)
  library(data.table)
  url <- sprintf("%s/issue/%s", osmose_host, id)
  carp("url: %s", url)
  resp <- RETRY("GET", url)
  if ( resp$status_code %notin% c("200", "410") ) {
    confess("erreur: %s", resp$status_code)
  }
  lst <- content(resp, as = "parsed")
  return(invisible(lst))
}
#
# analyse de la réponse d'osmose
# source("geo/scripts/transport.R"); df2 <- osmose_issue_parse(id = "d993af3b-58e7-2c03-cd84-e2d6b09e36bf") %>% glimpse()
osmose_issue_parse <- function(id) {
  lst1 <- osmose_issue_get(id)
  if (! exists("elems", where = lst1)) {
    return(invisible(data.frame()))
  }
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
#    glimpse(lst3)
    lst5 <- lst3[["tags"]]
#    glimpse(lst5)
    lst6 <- list()
    if (length(lst5) > 0) {
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
  }
  df2 <- unlist(lst1) %>%
    enframe() %>%
    pivot_wider(names_from = name, values_from = value)
  df2 <- cbind(df2, df11)
#    glimpse(df2);stop("*****")
  return(invisible(df2))
}
# source("geo/scripts/transport.R");df <- osmose_area_issues_html()
osmose_area_issues_html <- function(force = TRUE) {
  library(knitr)
  library(kableExtra)
  titre <- sprintf("osmose_area_issues_%s", Config[1, "reseau"])
  html <- misc_html_titre(titre)
  dsn_rds <- sprintf("%s/%s.rds", varDir, titre)
  issues.df <- readRDS(dsn_rds) %>%
    mutate(lat = as.numeric(lat)) %>%
    mutate(lon = as.numeric(lon)) %>%
    glimpse()
  carp("titre: %s", issues.df[[1, "title.auto"]])
  df2 <- issues.df %>%
    group_by(item, class, title.auto) %>%
    summarize(nb = n()) %>%
    glimpse()
  misc_print(df2)
  html <- misc_html_append(html, sprintf("<h1>%s</h1>", titre))
  html <- misc_html_append_df(html, df2)
#
## les items 2140
  html <- misc_html_append(html, sprintf("<h1>2140</h1>"))
  df1 <- issues.df %>%
    filter(item == "2140")
# 21401 	Missing public_transport:version tag on a public_transport route relation
  df3 <- df1 %>%
    filter(class == "21401") %>%
    dplyr::select(uuid, subtitle = subtitle.auto, elems.1.id,  elems.1.type, tags.1.network
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
  html <- misc_html_append(html, "<h2>21401 	Missing public_transport:version tag on a public_transport route relation</h2>")
  html <- misc_html_append_df(html, df4)
# 21402 	Missing network tag on a public_transport relation
  df3 <- df1 %>%
    filter(class == "21402") %>%
    dplyr::select(uuid, subtitle = subtitle.auto, elems.1.id,  elems.1.type, tags.1.network
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
  html <- misc_html_append(html, "<h2>21402 	Missing network tag on a public_transport relation</h2>")
  html <- misc_html_append_df(html, df4)
# 21403 	Missing operator tag on a public_transport relation
  df3 <- df1 %>%
    filter(class == "21403") %>%
    dplyr::select(uuid, subtitle = subtitle.auto, elems.1.id,  elems.1.type, tags.1.network
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
  html <- misc_html_append(html, "<h2>21403 	Missing operator tag on a public_transport relation</h2>")
  html <- misc_html_append_df(html, df4)
  dsn_rds <- sprintf("%s/%s_21403.rds", varDir, titre)
  saveRDS(df4, dsn_rds)
# 21405 	Missing from/to tag on a public_transport route relation
  df3 <- df1 %>%
    filter(class == "21405") %>%
    filter(tags.1.route == "bus") %>%
    dplyr::select(uuid, subtitle = subtitle.auto, elems.1.id,  elems.1.type, tags.1.network
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
  html <- misc_html_append(html, "<h2>21405 	Missing from/to tag on a public_transport route relation</h2>")
  html <- misc_html_append_df(html, df4)
# 21411 	Missing public_transport tag on a public transport stop
  df3 <- df1 %>%
    filter(class == "21411") %>%
    dplyr::select(uuid, subtitle = subtitle.auto, elems.1.id,  elems.1.type,  elems.2.id,  elems.2.type
      , tags.1.network, tags.2.network
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
  html <- misc_html_append(html, "<h2>21411 	Missing public_transport tag on a public transport stop</h2>")
  html <- misc_html_append_df(html, df4)
# 21412 	Missing legacy tag on a public transport stop
  df3 <- df1 %>%
    filter(class == "21412") %>%
    dplyr::select(uuid, subtitle = subtitle.auto, elems.1.id,  elems.1.type,  elems.2.id,  elems.2.type
      , tags.1.network, tags.2.network
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
  html <- misc_html_append(html, "<h2>21412 	Missing legacy tag on a public transport stop</h2>")
  html <- misc_html_append_df(html, df4)
  dsn_rds <- sprintf("%s/%s_21412.rds", varDir, titre)
  saveRDS(df4, dsn_rds)
#
## les items 1260
#
  html <- misc_html_append(html, sprintf("<h1>1260</h1>"))
  df1 <- issues.df %>%
    filter(item == "1260")
#
# 3 Non route relation member in route_master relation
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
  html <- misc_html_append(html, "<h2>Non route relation member in route_master relation</h2>")
  html <- misc_html_append_df(html, df4)
#
# 5 Diff route route_master
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
  html <- misc_html_append(html, "<h2>Diff route route_master</h2>")
  html <- misc_html_append_df(html, df4)
#
# 4 Diff route route_master
  df3 <- df1 %>%
    filter(class == "4") %>%
    glimpse() %>%
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
  html <- misc_html_append(html, "<h2>Diff route sans route_master</h2>")
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
  html <- misc_html_append(html, "<h2>Diff ordre des stops</h2>")
#  html <- misc_html_append_df(html, df4)
#
# 1 Diff gap
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
  html <- misc_html_append(html, "<h2>Diff gap</h2>")
  html <- misc_html_append_df(html, df4)
  dsn_rds <- sprintf("%s/%s_gap.rds", varDir, titre)
  saveRDS(df4, dsn_rds)
#
# 6 The bus stop is part of a way
  zoom <- "left=%0.5f&right=%0.5f&top=%0.5f&bottom=%0.5f"
  select1 <- "%s%s"
  df3 <- df1 %>%
    filter(class %in% c("6")) %>%
    mutate(select = sprintf(select1, elems.1.type, elems.1.id)) %>%
    mutate(zoom = sprintf(zoom, lon - .005, lon + .005, lat + .005, lat - .005)) %>%
    mutate(josm = sprintf("<a href='http://127.0.0.1:8111/load_and_zoom?%s&select=%s'>josm</a>", zoom, select)) %>%
    dplyr::select(-select, -zoom) %>%
    dplyr::select(uuid, class, elems.1.id,  elems.1.type
      , tags.1.network, josm
    ) %>%
    glimpse()
  df4 <- df3 %>%
    mutate(osmose = sprintf("<a href='http://osmose.openstreetmap.fr/api/0.3/issue/%s'>osmose</a>", uuid)) %>%
    mutate(level0 = sprintf("<a href='http://level0.osmz.ru/?url=%s%s'>level0</a>"
      , str_sub(elems.1.type,1, 1),  elems.1.id
    )) %>%
    glimpse()
  html <- misc_html_append(html, "<h2>The bus stop is part of a way</h2>")
  html <- misc_html_append_df(html, df4)
#
# 10 Stop position without platform nor bus stop
  df3 <- df1 %>%
    filter(class %in% c("10")) %>%
    dplyr::select(uuid, class, elems.1.id,  elems.1.type
      , tags.1.network, title.auto
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
  html <- misc_html_append(html, "<h2>Stop position without platform nor bus stop</h2>")
  html <- misc_html_append_df(html, df4)
#
# 7 The stop_position is not part of a way
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
  html <- misc_html_append(html, "<h2>Le stop_position ne fait pas partie d'un chemin</h2>")
  html <- misc_html_append_df(html, df4)
#
# 8 The platform is part of a way, it should have the role stop
  zoom <- "left=%0.5f&right=%0.5f&top=%0.5f&bottom=%0.5f"
  select1 <- "%s%s"
  df3 <- df1 %>%
    filter(class == "8") %>%
    mutate(select = sprintf(select1, elems.2.type, elems.2.id)) %>%
    mutate(zoom = sprintf(zoom, lon - .005, lon + .005, lat + .005, lat - .005)) %>%
    mutate(josm = sprintf("<a href='http://127.0.0.1:8111/load_and_zoom?%s&select=%s'>josm</a>", zoom, select)) %>%
    dplyr::select(-select, -zoom) %>%
    dplyr::select(uuid, subtitle = subtitle.auto, elems.1.id,  elems.1.type,  elems.2.id,  elems.2.type
      , tags.1.network, tags.2.network, josm
    ) %>%
    glimpse()
  df4 <- df3 %>%
#    mutate(josm = if_else(elems.2.type == "relation", sprintf("%s/%s/full", elems.2.type,  elems.2.id), sprintf("%s/%s",  elems.2.type,  elems.2.id))) %>%
#    mutate(josm = sprintf("<a href='http://localhost:8111/import?url=https://api.openstreetmap.org/api/0.6/%s'>josm</a>", josm)) %>%
    mutate(osmose = sprintf("<a href='http://osmose.openstreetmap.fr/api/0.3/issue/%s'>osmose</a>", uuid)) %>%
    mutate(level0 = sprintf("<a href='http://level0.osmz.ru/?url=%s%s, %s%s'>level0</a>"
      , str_sub(elems.1.type,1, 1),  elems.1.id
      , str_sub(elems.2.type,1, 1),  elems.2.id
    )) %>%
    glimpse()
  html <- misc_html_append(html, "<h2>L'arrêt fait partie d'un chemin, il devrait avoir le rôle 'stop'</h2>")
  html <- misc_html_append_df(html, df4)

  fic <- sprintf("%s.html", titre)
  dsn <- sprintf("%s/%s", webDir, fic)
  write(html, dsn)
  carp("dsn: %s", dsn)
  url <- sprintf("http://localhost/transport/%s", fic)
  browseURL(
    url,
    browser = "C:/Program Files/Mozilla Firefox/firefox.exe"
  )
  return(invisible(df4))
}
#
# http://localhost:8111/load_and_zoom?left=-2.8351908&bottom=47.6589548&right=-2.8311908000000003&top=47.6629548
# source("geo/scripts/transport.R"); osmose_bbox_get(lon1=-2.87,lat1=47.58,lon2=-2.66,lat2=47.68, force = TRUE)
osmose_bbox_get <- function(lon1 = -2.87, lat1 = 47.58,lon2 = -2.66,lat2 = 47.68, force = TRUE) {
  library(tidyverse)
  library(jsonlite)
  library(httr)
  library(data.table)
  url <- sprintf("%s/issues?item=1260&bbox=%0.5f,%0.5f,%0.5f,%0.5f&limit=500", osmose_host, lon1, lat1, lon2, lat2)
  carp("url: %s", url)
  resp <- GET(url)
  stop_for_status(resp)
  json <- content(resp, as = "parsed")
#  glimpse(json$issues)
  carp("issues nb: %s", length(json$issues))
  if (length(json$issues) > 50) {
#    carp("url: %s", url);stop("****")
  }
  return(invisible(json$issues))
}
#
# source("geo/scripts/transport.R"); osmose_country_get(country = "france_bretagne*", force = TRUE)
osmose_country_get <- function(country, force = TRUE) {
  library(tidyverse)
  df1 <- osmose_country_items_get(country = country, item = 1260, force = TRUE) %>%
    glimpse()
  issues <- osmose_country_item_get(country = country, item = 2140, force = TRUE) %>%
    glimpse()
  issues.df <- issues %>%
      rbindlist()
  df1 <- rbind(df1, issues.df)
  dsn_rds <- sprintf("%s/%s_%s.rds", varDir, "osmose_area", Config[1, "reseau"])
  saveRDS(df1, dsn_rds)
  carp("dsn_rds: %s", dsn_rds)
  return(invisible(df1))
}
# source("geo/scripts/transport.R"); df1 <- osmose_country_lire(country = "france_bretagne*", force = TRUE) %>% glimpse()
osmose_country_lire <- function(country, force = TRUE) {
  library(tidyverse)
  dsn_rds <- sprintf("%s/%s_%s.rds", varDir, "osmose_area", Config[1, "reseau"])
  df1 <- readRDS(dsn_rds)
  carp("dsn_rds: %s", dsn_rds)
  return(invisible(df1))
}
#
# source("geo/scripts/transport.R"); osmose_country_items_get(country = "france_bretagne*", item = 1260, force = TRUE)
osmose_country_items_get <- function(country, item, force = TRUE) {
  library(tidyverse)
  df1 <- data.frame()
  for (class in seq(1, 12)) {
    if ( class %in% c(2, 11, 12)) {
      next
    }
    issues <- osmose_country_item_class_get(country = country, item = item, class = class, force = force)
    if (length(issues) == 0) {
      next;
    }
    issues.df <- issues %>%
      rbindlist()
    df1 <- rbind(df1, issues.df)
  }
  return(invisible(df1))
}
# https://wiki.openstreetmap.org/wiki/Osmose/api/0.3
# source("geo/scripts/transport.R"); osmose_country_item_get(country = "france_bretagne*", item = 2140, force = TRUE)
osmose_country_item_get <- function(country, item, force = TRUE) {
  library(tidyverse)
  library(jsonlite)
  library(httr)
  library(data.table)
  url <- sprintf("%s/issues?country=%s&item=%s&limit=500", osmose_host, country, item)
  carp("url: %s", url)
  resp <- GET(url)
  stop_for_status(resp)
  json <- content(resp, as = "parsed")
#  glimpse(json$issues)
  carp("issues nb: %s", length(json$issues))
  if (length(json$issues) >= 500) {
#    carp("url: %s", url);stop("****")
  }

  return(invisible(json$issues))
}
# source("geo/scripts/transport.R"); osmose_county_item_class_get(country = "france_bretagne*", item = 1260, class = 1, force = TRUE)
osmose_country_item_class_get <- function(country, item, class, force = TRUE) {
  library(tidyverse)
  library(jsonlite)
  library(httr)
  library(data.table)
  url <- sprintf("%s/issues?country=%s&item=%s&class=%s&limit=500", osmose_host, country, item, class)
  carp("url: %s", url)
  resp <- GET(url)
  stop_for_status(resp)
  json <- content(resp, as = "parsed")
#  glimpse(json$issues)
  carp("issues nb: %s", length(json$issues))
  if (length(json$issues) >= 500) {
#    carp("url: %s", url);stop("****")
  }
  return(invisible(json$issues))
}
# source("geo/scripts/transport.R"); osmose_issues_get(json, force = TRUE)
osmose_issues_get <- function(issues, force = TRUE) {
  library(tidyverse)
  library(jsonlite)
  library(httr)
  library(data.table)
  for (issue in json$issues) {
#    glimpse(issue)
    url <- sprintf("%s/issue/%s", osmose_host, issue$id)
#    carp("url: %s", url)
    resp <- GET(url)
    stop_for_status(resp)
    lst <- content(resp, as = "parsed")
#
    carp("lst: %s", lst$title)
#    if (grepl("should be the same on route", lst$title)) {
#    if (grepl("The bus stop is part of a way", lst$title)) {
    if (grepl("Public transport relation route not in route_master relation", lst$title)) {
      glimpse(lst)
      df1 <- as.data.frame(do.call(cbind, lst)) %>%
        glimpse()
      df2 <- as.data.frame(do.call(cbind, df1[[1, "elems"]])) %>%
        glimpse()
#      stop("hjklm")
    }
  }
}
# source("geo/scripts/transport.R");df <- osmose_issues_gap()
osmose_issues_gap <- function(force = TRUE, force_osm = TRUE) {
  dsn <- sprintf("%s/%s_%s.rds", varDir, "osmose_area_issues_gap", Config[1, "reseau"])
  mtime <- file.info(dsn)$mtime
  carp("dsn: %s %s", dsn, mtime)
  df <- readRDS(dsn) %>%
    glimpse() %>%
# https://stackoverflow.com/questions/45857787/adding-column-if-it-does-not-exist
    (function(.df){
      cls <- c("tags.1.route", "tags.1.network") # columns I need
      # adding cls columns with NAs if not present in the piped data.frame
      .df[cls[!(cls %in% colnames(.df))]] = NA
      return(.df)
    }) %>%
    filter(tags.1.route == "bus")
  if (nrow(df) == 0) {
    carp("**** pas de gap")
    return(invisible(df))
  }
  dsn <- osm_relations_route_bus_area_csv(force = force_osm)
  relations.df <- fread(dsn, encoding = "UTF-8") %>%
    clean_names() %>%
    mutate(id = sprintf("%s", id)) %>%
    glimpse()
  df1 <- df %>%
    left_join(relations.df, by = c("elems.1.id" = "id")) %>%
# l'interrogation osmose couvre plus que la zone
    filter(! is.na(type)) %>%
    dplyr::select(network, name, user, timestamp, version, josm, osmose, PTNA) %>%
    glimpse()
  misc_print(df1)
  misc_html_df2fic(df1)
#  return()
  gaps.df <- data.frame()
  for (i in 1:nrow(df)) {
    df2 <- osm_relation_route_gap(id = df[i, "elems.1.id"], force = TRUE, force_osm = TRUE)
     if (nrow(df2) > 0) {
#      misc_print(df2)
#      stop("*****")
      gaps.df <- rbind(gaps.df, df2)
#      break
    }

  }
  gaps.df <- gaps.df %>%
    left_join(relations.df, by = c("r_id" = "id")) %>%
    dplyr::select(id = r_id, no, network, name.x, version, timestamp, user, name.y) %>%
# http://127.0.0.1:8111/load_object?new_layer=false&relation_members=true&objects=r11246461
    mutate(josm = sprintf("<a href='http://localhost:8111/load_object?new_layer=false&relation_members=true&objects=r%s'>josm</a>", id)) %>%
#    mutate(josm = sprintf("<a href='http://localhost:8111/import?url=https://api.openstreetmap.org/api/0.6/relation/%s/full'>josm</a>", id)) %>%
    mutate(level0 = sprintf("<a href='http://level0.osmz.ru/?url=r%s'>level0</a>", id)) %>%
    mutate(PTNA = sprintf("<a href='https://ptna.openstreetmap.de/relation.php?id=%s&lang=fr'>PTNA</a>", id)) %>%

    glimpse()
  misc_html_df2fic(gaps.df, suffixe = sprintf("gaps_%s", Config[1, "reseau"]))
  url <- sprintf("http://localhost/transport/osmose_issues_gap_gaps_%s.html", Config[1, "reseau"])
  browseURL(
    url,
    browser = "C:/Program Files/Mozilla Firefox/firefox.exe"
  )
}
# source("geo/scripts/transport.R");df <- osmose_issues_21403()
osmose_issues_21403 <- function(force = TRUE) {
  dsn_rds <- sprintf("%s/osmose_area_issues_%s_21403.rds", varDir, Config[1, "reseau"])
  df <- readRDS(dsn_rds) %>%
    dplyr::select(id = elems.1.id, type = elems.1.type, network = tags.1.network) %>%
    glimpse()
  config.df <- Config.df %>%
    dplyr::select(reseau, network, operator)
  df1 <- df %>%
    left_join(config.df, by = c("network")) %>%
    glimpse()
  objects.list <- list()
  carp("nrow: %s", nrow(df1))
#  for (i1 in 2:nrow(df1)) {
#  for (i1 in 51:70) {
  for (i1 in 71:nrow(df1)) {
    carp("i1: %s", i1)
    txt <- osmapi_object_txt(ref = df1[i1, "id"], type = df1[i1, "type"], force = TRUE)
    lst <- str_split(txt, "\n")[[1]]
#    mga <<- lst; exit
# https://osmose.openstreetmap.fr/api/0.3/issue/aa7f1dca-9e40-86df-6804-d621eeb37696/fix/0
    lst <- lst[grepl("^ ", lst)]
    lst <- head(lst, -1)
    lst[1] <- gsub(">", ' action="modify">', lst[1])
    tag <- sprintf('  <tag k="operator" v="%s"/>', df1[i1, "operator"])
    objects.list <- c(objects.list, lst, tag, ' </relation>')
  }
  objects.list <- c('<osm version="0.6" generator="OsmSax">', objects.list, '</osm>')

  fic <- sprintf("osmose_issues_21403.osm")
  dsn <- sprintf("%s/%s", webDir, fic)
  write_lines(objects.list, dsn)
  carp("dsn: %s", dsn)
}
#
## pour les noeuds
#
# source("geo/scripts/transport.R");df <- osmose_issues_21412()
osmose_issues_21412 <- function(force = TRUE) {
  dsn_rds <- sprintf("%s/osmose_area_issues_%s_21412.rds", varDir, Config[1, "reseau"])
  df1 <- readRDS(dsn_rds) %>%
    dplyr::select(id = elems.1.id, type = elems.1.type) %>%
    mutate(url = sprintf("https://www.openstreetmap.org/api/0.6/%s/%s", type, id)) %>%
    mutate(url_r = sprintf("https://www.openstreetmap.org/api/0.6/%s/%s/relations", type, id)) %>%
    mutate(url_w = sprintf("https://www.openstreetmap.org/api/0.6/%s/%s/ways", type, id)) %>%
    glimpse()
  df4 <- tibble()
  for (i1 in 1:nrow(df1)) {
    carp("i1: %s", i1)
    res <- httr::GET(url = df1[i1, "url"], encoding = "UTF-8", type = "application/xml")
    stop_for_status(res)
    doc <- content(res)
    nodes <- xml2::xml_find_all(doc, "//node")
    node <- nodes[[1]]
    node.df <- xml_attrs(node) %>%
      as_tibble_row()
    res <- httr::GET(url = df1[i1, "url_r"], encoding = "UTF-8", type = "application/xml")
    stop_for_status(res)
    doc <- content(res)
    relations <- xml2::xml_find_all(doc, "//relation")
    node.df$relations_nb <- length(relations)
    res <- httr::GET(url = df1[i1, "url_w"], encoding = "UTF-8", type = "application/xml")
    stop_for_status(res)
    doc <- content(res)
    ways <- xml2::xml_find_all(doc, "//way")
    node.df$ways_nb <- length(ways)
    df4 <- bind_rows(df4, node.df)
#    stop("****")
  }
  glimpse(df4)
  dsn_rds <- sprintf("%s/osmose_area_issues_%s_21412_osm.rds", varDir, Config[1, "reseau"])
  saveRDS(df4, dsn_rds)
  carp("dsn_rds: %s", dsn_rds)
}
#
# source("geo/scripts/transport.R");df <- osmose_issues_21412_osm()
osmose_issues_21412_osm <- function(force = TRUE) {
  dsn_rds <- sprintf("%s/osmose_area_issues_%s_21412_osm.rds", varDir, Config[1, "reseau"])
  df1 <- readRDS(dsn_rds) %>%
    mutate(level0 = sprintf("n%s", id)) %>%
#    filter(ways_nb == 0) %>%
#    filter(relations_nb == 0) %>%
    glimpse()
  misc_print(df1)
  level0 <- paste0(df1$level0, collapse = ",")
  print(level0)
}