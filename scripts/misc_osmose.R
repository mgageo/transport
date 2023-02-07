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
# http://localhost:8111/load_and_zoom?left=-2.8351908&bottom=47.6589548&right=-2.8311908000000003&top=47.6629548
# source("geo/scripts/transport.R"); osmose_bbox_get(lon1=-2.87,lat1=47.58,lon2=-2.66,lat2=47.68, force = TRUE)
osmose_bbox_get <- function(lon1=-2.87,lat1=47.58,lon2=-2.66,lat2=47.68, force = TRUE) {
  library(tidyverse)
  library(jsonlite)
  library(httr)
  library(data.table)
  url <- sprintf("%s/issues?item=1260&bbox=%0.5f,%0.5f,%0.5f,%0.5f&limit=500", osmose_host, lon1, lat1, lon2, lat2)
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
# source("geo/scripts/transport.R"); osmose_issue_get(id, force = TRUE)
osmose_issue_get <- function(id, force = TRUE) {
  library(tidyverse)
  library(jsonlite)
  library(httr)
  library(data.table)
  url <- sprintf("%s/issue/%s", osmose_host, id)
#    carp("url: %s", url)
    resp <- GET(url)
  stop_for_status(resp)
  lst <- content(resp, as = "parsed")
  return(invisible(lst))
}
#
# source("geo/scripts/transport.R"); osmose_issue_parse(id = "8676c5fa-fd69-5cb8-c021-0e5c23ae1454", force = TRUE)
osmose_issue_parse <- function(id, force = TRUE) {
  lst1 <- osmose_issue_get(id, force = force)
  glimpse(lst1)
  lst2 <- lst1[["elems"]][1]
  lst1["elems"] <- NULL
  lst1["new_elems"] <- NULL
  lst1["title"] <- NULL
  df1 <- as.data.frame(do.call(cbind, lst1)) %>%
    glimpse()
  df2 <- as.data.frame(do.call(cbind, lst2)) %>%
    glimpse()
}
