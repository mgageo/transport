# <!-- coding: utf-8 -->
#
# utilisation des données d'OpenStreetMap
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
# auteur : Marc Gauthier
#
# https://wiki.openstreetmap.org/wiki/API_v0.6
#
# source("geo/scripts/transport.R"); res <- osmapi_get_node_ways(node = "1123340557")
osmapi_get_node_ways <- function(node, force = FALSE) {
  carp("node: %s", node)
  library(rjson)
  query <- sprintf("https://www.openstreetmap.org/api/0.6/node/%s/ways.json", node)
  res <- rjson::fromJSON(file = query)
  return(invisible(res))
}
# source("geo/scripts/transport.R"); res <- osmapi_get_objet(ref = "11920346"); print(res)
osmapi_get_objet <- function(ref, type = "relation", force = FALSE) {
  library(rjson)
  dsn <- sprintf("%s/%s_%s.json", transportDir, type, ref)
  carp("dsn: %s", dsn)
  if (! file.exists(dsn) || force == TRUE) {
    query <- sprintf("https://www.openstreetmap.org/api/0.6/%s/%s.json", type, ref)
    res <- httr::GET(url = query, encoding = "UTF-8", type = "application/json", verbose(), httr::write_disk(dsn, overwrite = TRUE))
    stop_for_status(res)
  }
  res <- rjson::fromJSON(file = dsn)
  return(invisible(res))
}
# source("geo/scripts/transport.R"); res <- osmapi_get_full(ref = "11920346"); print(res)
osmapi_get_object_full <- function(ref, type = "relation", force = FALSE) {
  library(readr)
  library(tidyverse)
  dsn <- sprintf("%s/%s_%s_full.osm", transportDir, type, ref)
  carp("dsn: %s", dsn)
  if (! file.exists(dsn) || force == TRUE) {
    url <- sprintf("https://www.openstreetmap.org/api/0.6/%s/%s/full", type, ref)
    res <- httr::GET(url = url, encoding = "UTF-8", type = "application/json", verbose(), httr::write_disk(dsn, overwrite = TRUE))
    stop_for_status(res)
    txt <- httr::content(res, as = "text")
  } else {
    txt <- read_file(dsn)
  }
  return(invisible(txt))
}
# source("geo/scripts/transport.R"); res <- osmapi_get_objet_v1(objet = "11920346"); print(res)
osmapi_get_object_v1 <- function(objet, type = "relation", force = FALSE) {
  library(readr)
  library(tidyverse)
  dsn <- sprintf("%s/%s_%s.txt", transportDir, type, objet)
  carp("dsn: %s", dsn)
  if (! file.exists(dsn) || force == TRUE) {
    url <- sprintf("https://www.openstreetmap.org/api/0.6/%s/%s.json", type, objet)
    res <- httr::GET(url = url, encoding = "UTF-8", type = "application/json", verbose(), httr::write_disk(dsn, overwrite = TRUE))
    stop_for_status(res)
    txt <- httr::content(res, as = "text")
  } else {
    txt <- read_file(dsn)
  }
  return(invisible(txt))
}
#
# récupération des coordonnées des arrêts d'une relation transport
# source("geo/scripts/transport.R"); osmapi_get_members_platform(ref = "11920346")
osmapi_get_members_platform <- function(ref = "11920346", type = "relation", force = FALSE) {
  library(readr)
  library(tidyverse)
  json1.list <- osmapi_get_objet(ref = ref, type = type, force) %>%
    glimpse()
  members.list <- json1.list$elements[[1]]$members
  df1 <- do.call(rbind, lapply(members.list, data.frame))
  df2 <- df1 %>%
    filter(type == "node")
  for (i in 1:nrow(df2)) {
    ref <- df2[i, "ref"]
    res <- osmapi_get_objet(ref = ref, type = "node");
    df2[i, "lat"] <- res$elements[[1]]$lat
    df2[i, "lon"] <- res$elements[[1]]$lon
  }
  return(invisible(df2))
}
#
# les ways qui réfèrent un node
osmapi_get_node_ways <- function(node, force = FALSE) {
  carp("node: %s", node)
  library(httr)
  library(tidyverse)
  library(rjson)
  query <- sprintf("https://www.openstreetmap.org/api/0.6/node/%s/ways.json", node)
  res <- rjson::fromJSON(file = query)
  return(invisible(res))
}
#
## la partie en écriture
#
osmapi_api <- function(path, xml = '') {
  url <- sprintf("https://www.openstreetmap.org/api/0.6/%s", path)
  carp("url: %s", url)
  username <- mes_options("openstreetmap_user")
  password <- mes_options("openstreetmap_password")
  res <- httr::PUT(
    url,
    body = xml,
    authenticate(username, password, type = "basic"),
    verbose()
  )
  stop_for_status(res)
  txt <- httr::content(res, as = "text")
  carp("text: %s", txt)
  return(invisible(txt))
}
# source("geo/scripts/transport.R"); osmapi_put()
osmapi_put <- function(osmchange = '', force = FALSE) {
  library(httr)
  library(tidyverse)
  xml <- '<osm>
  <changeset>
    <tag k="created_by" v="R 0.6"/>
    <tag k="comment" v="test"/>
  </changeset>
</osm>'
  changeset_id <- osmapi_api("changeset/create", xml)
  path <- sprintf("changeset/%s/close", changeset_id)
  res <- osmapi_api(path)

}