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
# http://osmose.openstreetmap.fr/api/0.3/issues?item=1260&class=4&county=france_bretagne&limit=500
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
#
# source("geo/scripts/transport.R"); osmose_area_jour(force = FALSE)
osmose_area_jour <- function(force = TRUE) {
  osmose_area_get(force = FALSE)
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
area[name="%s"];
nwr(pivot);
out geom;', Config[1, 'zone'])
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
  dsn_rds <- sprintf("%s/%s.rds", varDir, "osmose_area")
  saveRDS(df1, dsn_rds)
}
# source("geo/scripts/transport.R");osmose_area_issues()
osmose_area_issues <- function() {
  dsn_rds <- sprintf("%s/%s.rds", varDir, "osmose_area")
  df1 <- readRDS(dsn_rds) %>%
    glimpse() %>%
    distinct(id) %>%
    glimpse()
  df3 <- data.frame()
  for (i in 1:nrow(df1)) {
    carp("i: %s/%s", i, nrow(df1))
    df2 <- osmose_issue_parse(id = df1[[i, "id"]])
    df3 <- bind_rows(df3, df2)
  }
  dsn_rds <- sprintf("%s/%s.rds", varDir, "osmose_area_issues")
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
  stop_for_status(resp)
  lst <- content(resp, as = "parsed")
  return(invisible(lst))
}
#
# analyse de la réponse d'osmose
# source("geo/scripts/transport.R"); df2 <- osmose_issue_parse(id = "d993af3b-58e7-2c03-cd84-e2d6b09e36bf") %>% glimpse()
osmose_issue_parse <- function(id) {
  lst1 <- osmose_issue_get(id)
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
  html <- misc_html_titre("osmose_area_issues")
  dsn_rds <- sprintf("%s/%s.rds", varDir, "osmose_area_issues")
  df1 <- readRDS(dsn_rds) %>%
    mutate(lat = as.numeric(lat)) %>%
    mutate(lon = as.numeric(lon)) %>%
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
  dsn_rds <- sprintf("%s/%s.rds", varDir, "osmose_area_issues_gap")
  saveRDS(df4, dsn_rds)
#
  df3 <- df1 %>%
    filter(class %in% c("6")) %>%
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
  html <- misc_html_append(html, "<h1>The bus stop is part of a way</h1>")
  html <- misc_html_append_df(html, df4)
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
  html <- misc_html_append(html, "<h1>Stop position without platform nor bus stop</h1>")
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
  html <- misc_html_append(html, "<h1>Le stop_position ne fait pas partie d'un chemin</h1>")
  html <- misc_html_append_df(html, df4)
# The platform is part of a way, it should have the role stop
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
  html <- misc_html_append(html, "<h1>L'arrêt fait partie d'un chemin, il devrait avoir le rôle 'stop'</h1>")
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
  dsn <- sprintf("%s/osmose_area_issues.html", webDir)
  write(html, dsn)
  carp("dsn: %s", dsn)
  url <- sprintf("http://localhost/transport/osmose_area_issues.html")
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

