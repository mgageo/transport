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
# en direct de https://github.com/hrbrmstr/overpass/blob/master/R/overpass_query.r
#
# source("geo/scripts/transport.R"); res <- overpass_get_objet(ref = "11920346"); print(res)
# https://wiki.openstreetmap.org/wiki/Overpass_API#Public_Overpass_API_instances
overpass_base_url <- "http://overpass-api.de/api/interpreter"
#overpass_base_url <- "https://z.overpass-api.de/api/interpreter"
#overpass_base_url <- "https://lz4.overpass-api.de/api/interpreter"
#overpass_base_url <- "https://overpass.kumi.systems/api/interpreter"
#overpass_base_url <- "http://overpass.openstreetmap.fr/api/interpreter"
overpass_status <- function(quiet=FALSE) {
  status <- httr::GET(overpass_base_url)
  status <- httr::content(status)
  status_now <- strsplit(status, "\n")[[1]][4]
  if (!quiet) {
    message(status_now)
    print(status_now)
  }
  if (grepl("after", status_now)) {
    available <- FALSE
    status_now <- gsub("Slot available after: ", "", status_now)
    status_now <- gsub(", in.*", "", status_now)
    slot_time <- lubridate::ymd_hms(status_now)
    current_time <- strsplit(status, "\n")[[1]][2]
    current_time <- lubridate::ymd_hms(gsub("Current time: ", "", current_time))
    waiting_time <- difftime(current_time, slot_time, units = "secs")
  } else {
    available <- TRUE
    waiting_time <- 0
  }
  return(invisible(list(available=available, waiting_time=waiting_time, msg=status)))
}
#
# source("geo/scripts/transport.R");res <- overpass_query()
overpass_query <- function(query, fic = "test", force = FALSE) {
  library(httr)
  library(tidyverse)
  dsn <- sprintf("%s/%s.osm", osmDir, fic)
  carp("dsn: %s", dsn)
  if (! file.exists(dsn) || force == TRUE) {
    query <- sprintf("data=[timeout:600][maxsize:1073741824];%s", query)
    carp("query: %s", query)
    res <- httr::POST(overpass_base_url, body=query, httr::write_disk(dsn, overwrite = TRUE))
    if (status_code(res) != 200) {
      print(httr::content(res, as="text", encoding="UTF-8"));
      stop_for_status(res)
    }
  }
  return(invisible(dsn))
}
overpass_query_v1 <- function(query, quiet=FALSE) {
  carp("query: %s", query)
  res <- httr::POST(overpass_base_url, body=query)
  httr::stop_for_status(res)
  if (!quiet) message("Query complete!")
  if (res$headers$`content-type` == "text/csv") {
    return(httr::content(res, as="text", encoding="UTF-8"))
  }
  doc <- xml2::read_xml(httr::content(res, as="text", encoding="UTF-8"))
  return(invisible(doc))
}
overpass_query_json <- function(query, fic = "test", force = FALSE) {
  library(httr)
  library(tidyverse)
  library(rjson)
  dsn <- sprintf("%s/%s.json", osmDir, fic)
  carp("dsn: %s", dsn)
  if (! file.exists(dsn) || force == TRUE) {
    query <- sprintf("data=[timeout:600][maxsize:1073741824][out:json];%s", query)
    res <- httr::POST(overpass_base_url, body=query, httr::write_disk(dsn, overwrite = TRUE))
    stop_for_status(res)
  }
  return(invisible(dsn))
}
overpass_query_csv <- function(query, fic = "test", force = TRUE) {
  library(httr)
  library(tidyverse)
  dsn <- sprintf("%s/%s.csv", osmDir, fic)
  carp("dsn: %s", dsn)
  if (! file.exists(dsn) || force == TRUE) {
    carp("query: %s", query)
    res <- httr::POST(overpass_base_url, body=query, httr::write_disk(dsn, overwrite = TRUE))
    if (status_code(res) != 200) {
      print(httr::content(res, as="text", encoding="UTF-8"));
      stop_for_status(res)
    }
  }
  return(invisible(dsn))
}
#
## les requêtes
#
# source("geo/scripts/transport.R");overpass_nodes_busstop_network_get()
overpass_nodes_busstop_network_get <- function(network = "concarneau", force = FALSE) {
  library(tidyverse)
  library(rio)
  carp()
  config_xls(network);
  glimpse(Config)
  dsn <- 'nodes_busstop_network'
  requete <- sprintf('(
relation[network="%s"][route=bus]->.a;
node(r.a)[highway=bus_stop][public_transport=platform]-> .b;
node[highway=bus_stop][public_transport~platform]["%s"]-> .c;
);
out meta;', Config[1, "network"], Config[1, "k_ref"] )
  dsn <- overpass_query(query = requete, fic = dsn, force = force)
# https://github.com/r-spatial/sf/issues/1157
  ini_new <- "#configuration osm import
attribute_name_laundering=no
[points]
osm_id=yes
osm_version=yes
osm_timestamp=yes
osm_uid=yes
osm_user=yes
osm_changeset=yes
# keys to report as OGR fields
attributes=name,highway,ref:Coralie,public_transport,bus
all_tags=yes
"
  f_ini <- "nodes_busstop_network.ini"
  writeLines(ini_new, f_ini)
  nc <- oapi_sf_lire(fic = dsn , ini = f_ini)
  return(invisible(nc))
}
overpass_relations_route_get_xml <- function(force = TRUE, network = 'pennarbed') {
  library(tidyverse)
  library(rio)
  library(xml2)
  carp()
  config_xls(network)
  fic <- 'relations_route'
# ['ref:network'~'^29\\-']
  requete <- sprintf('relation[network~"%s"][type=route][route=bus];
out meta;', Config[1, "network"])
  dsn <- overpass_query(query = requete, fic = fic, force = force)
  carp("dsn: %s", dsn)
  doc <- xml2::read_xml(dsn)
  has_relations <- osm_has_xpath(doc, "//relation")
  if (! has_relations) {
    stop("***")
  }
  return(invisible(doc))
}
# source("geo/scripts/transport.R");overpass_relations_route_get_sf(network = 'star', force  = FALSE)
overpass_relations_route_get_sf <- function(network = 'pennarbed', force = TRUE ) {
  library(tidyverse)
  library(rio)
  library(sf)
  library(osmdata)
  carp()
  config_xls(network)
  fic <- 'relations_route'
  requete <- sprintf("relation[network='%s'][type=route][route=bus];
out meta;", Config[[1, "network"]])
  carp("requete: %s", requete);
  dsn <- overpass_query(query = requete, fic = fic, force = force)
  carp("dsn: %s", dsn)
  q <- opq(bbox = c(45, -6, 58, 0))
  od <- osmdata_sf(q, dsn) %>%
    glimpse()
  return(invisible(od))
}
#
## les platforms d'un réseau
#
# source("geo/scripts/transport.R"); od <- overpass_relations_route_node(network = 'kiceo', force  = TRUE)
overpass_relations_route_node <- function(network = 'pennarbed', force = TRUE ) {
  library(tidyverse)
  library(rio)
  library(sf)
  library(osmdata)
  carp()
  config_xls(network)
  fic <- 'overpass_relations_route_node'
  requete <- sprintf("(
relation[network='%s'][type=route][route=bus];
node(r);
);
out meta;", Config[[1, "network"]])
  carp("requete: %s", requete);
  dsn <- overpass_query(query = requete, fic = fic, force = force)
  carp("dsn: %s", dsn)
  return(invisible(dsn))
}
#
# source("geo/scripts/transport.R");overpass_relation_get()
overpass_relation_get <- function(id = "4013532", force = TRUE) {
  library(tidyverse)
  library(rio)
  carp()
  dsn <- sprintf("overpass_relation_get_%s", id)
  requete <- sprintf("
rel(%s);
way(r)[highway];
(._;>;);
out meta;", id)
  dsn <- overpass_query(query = requete, fic = dsn, force = force)
# https://github.com/r-spatial/sf/issues/1157
  ini_new <- "#configuration osm import
attribute_name_laundering=no
[points]
osm_id=yes
osm_version=yes
osm_timestamp=yes
osm_uid=yes
osm_user=yes
osm_changeset=yes
# keys to report as OGR fields
attributes=name,highway,public_transport,bus
all_tags=yes
"
  f_ini <- "nodes_busstop_network.ini"
  writeLines(ini_new, f_ini)
#  nc <- oapi_sf_lire(fic = dsn , ini = f_ini)
  nc <- oapi_sf_lire(fic = dsn )
  return(invisible(nc))
}
#
# source("geo/scripts/transport.R");overpass_relation_get()
#
overpass_relation_get <- function(id = "4013532", force = TRUE) {
  library(tidyverse)
  library(rio)
  carp()
  od_ways <- overpass_relation_get_ways(id = id, force = force)
  plot(st_geometry(od_ways$osm_multilines))
  od_stops <- overpass_relation_get_stops(id = id, force = force)
  plot(st_geometry(od_stops$osm_points), add = TRUE, col = "darkred")
}
# https://gis.stackexchange.com/questions/402766/querying-single-element-groups-from-openstreetmap-using-overpass-and-osmdata-in
overpass_relation_get_ways <- function(id = "4013532", force = TRUE) {
  library(tidyverse)
  library(rio)
  dsn <- sprintf("overpass_relation_get_%s_ways", id)
  carp("dsn: %s", dsn)
  requete <- sprintf("
rel(%s);
out;
way(r);
out body;
node(w);
out skel;", id)
  dsn <- overpass_query(query = requete, fic = dsn, force = force)
  q <- opq(bbox = c(51.1, 0.1, 51.2, 0.2))
  od <- osmdata_sf(q, dsn)
  return(invisible(od))
}
overpass_relation_get_stops <- function(id = "4013532", force = TRUE) {
  library(tidyverse)
  library(rio)
  dsn <- sprintf("overpass_relation_get_%s_stops", id)
  carp("dsn: %s", dsn)
  requete <- sprintf("
rel(%s);
node(r);
out body;", id)
  dsn <- overpass_query(query = requete, fic = dsn, force = force)
  q <- opq(bbox = c(51.1, 0.1, 51.2, 0.2))
  od <- osmdata_sf(q, dsn)
  return(invisible(od))
}