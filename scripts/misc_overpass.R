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
overpass_relation_get_history <- function(id = "4013532", force = TRUE) {
  library(tidyverse)
  library(rio)
  dsn <- sprintf("overpass_relation_get_%s_history", id)
  carp("dsn: %s", dsn)
  requete <- sprintf('
timeline(rel, %s);
foreach(
  retro(u(t["created"]))
  (
    (rel(%s);>>;);
    out meta;

  );
);', id, id)
  dsn <- overpass_query(query = requete, fic = dsn, force = force)
  return(invisible(dsn))
}
# source("geo/scripts/transport.R");overpass_object_get_history()
overpass_object_get_history <- function(id = "4294292361", type = "node", force = TRUE) {
  library(tidyverse)
#  type <- substr(type, 1, 3)
  dsn <- sprintf("overpass_object_get_history_%s_%s", type, id)
  carp("dsn: %s", dsn)
  requete <- sprintf('
timeline(%s, %s);
foreach(
  retro(u(t["created"]))
  (
    (%s(%s);>>;);
    out meta;

  );
);', type, id, type, id)
  dsn <- overpass_query(query = requete, fic = dsn, force = force)
  return(invisible(dsn))
}
#
## recherche des arrêts proches
#
# source("geo/scripts/transport.R");overpass_arrets_proches(force = FALSE)
overpass_arrets_proches <- function(force = TRUE) {
  library(tidyverse)
  library(nngeo)
  library(sf)
#  library(maggritr)
  carp()

  dsn2 <- sprintf("%s/overpass_arrets_proches.Rds", transportDir)
  if ( ! file.exists(dsn2) | force == TRUE) {
    dsn1 <- sprintf("%s/overpass_arrets_parse.Rds", transportDir)
    nc1 <- readRDS(dsn1) %>%
#    slice_head(n = 10) %>%
      glimpse()
    nc2 <- overpass_proches(nc1, nc1, k = 2)
    carp("dsn2: %s", dsn2)
    saveRDS(nc2, file = dsn2)
  } else {
    nc2 <- readRDS(dsn2)
  }
#  josm <- "http://127.0.0.1:8111/load_and_zoom?left=8.19&right=8.20&top=48.605&bottom=48.590&select=node413602999
  zoom <- "left=%0.5f&right=%0.5f&top=%0.5f&bottom=%0.5f"
  select <- "node%s,node%s"
  df2 <- nc2 %>%
    mutate(X=st_coordinates(.)[,'X']) %>%
    mutate(Y=st_coordinates(.)[,'Y']) %>%
    st_drop_geometry()
  names(df2) <- names(df2) %>%
    str_replace_all("^ref.*\\.1$", "kref1") %>%
    str_replace_all("^ref\\..*$", "kref") %>%
    glimpse()
  df2 <- df2 %>%
    filter(dist < 10) %>%
    filter(osm_id < osm_id.1) %>%
    filter(is.na(kref) | is.na(kref1)) %>%
#    filter(kref != kref1) %>%
    filter(name == name.1) %>%
    mutate(select = sprintf(select, osm_id, osm_id.1)) %>%
    mutate(zoom = sprintf(zoom, X - .001, X + .001, Y + .001, Y - .001)) %>%
    mutate(josm = sprintf("<a href='http://127.0.0.1:8111/load_and_zoom?%s&select=%s'>josm</a>", zoom, select)) %>%
    dplyr::select(-select, -zoom, -X, -Y) %>%
    arrange(name) %>%
    glimpse()
  html <- misc_html_titre(sprintf("%s_overpass_arrets_proches", Config["reseau"]))
  html <- misc_html_append_df(html, df2)
  dsn <- sprintf("%s/%s_overpass_arrets_proches.html", webDir, Config["reseau"])
  write(html, dsn)
  carp("dsn: %s", dsn)
  url <- sprintf("http://localhost/transport/%s_overpass_arrets_proches.html", Config["reseau"])
  browseURL(
    url,
    browser = "C:/Program Files/Mozilla Firefox/firefox.exe"
  )
  return(invisible())
}
overpass_proches <- function(nc1, nc2, k = 1) {
  library(nngeo)
  n <- st_nn(nc1, nc2, k = k, progress = TRUE, returnDist = TRUE)
  mga <<- n
  ids <-  sapply(n[[1]], "[", k)
  dists <- sapply(n[[2]], "[", k)
  df3 <- data.frame(nc1, st_drop_geometry(nc2)[ids, , drop = FALSE])
  nc3 <- st_sf(df3)
  nc3$dist <- dists
  return(invisible(nc3))
}
#
# source("geo/scripts/transport.R");overpass_arrets_query()
overpass_arrets_query <- function(force = TRUE) {
  library(tidyverse)
  library(sf)
  carp()
  dsn <- sprintf("overpass_arrets_query")
  requete <- sprintf('
area[name="%s"]->.a;
(
node(area.a)[highway=bus_stop];
node(area.a)[public_transport=platform];
);
out meta;', Config[1, "zone"] )
  dsn <- overpass_query(query = requete, fic = dsn, force = force)
  return(invisible(dsn))
}
#
# source("geo/scripts/transport.R");overpass_arrets_query()
overpass_arrets_query <- function(force = TRUE) {
  library(tidyverse)
  library(sf)
  carp()
  dsn <- sprintf("overpass_arrets_query")
  requete <- sprintf('
area[name="%s"]->.a;
(
node(area.a)[highway=bus_stop];
node(area.a)[public_transport=platform];
);
out meta;', Config[1, "zone"] )
  dsn <- overpass_query(query = requete, fic = dsn, force = force)
  return(invisible(dsn))
}
#
# source("geo/scripts/transport.R");overpass_arrets_parse()
overpass_arrets_parse <-  function(force = TRUE) {
  library(tidyverse)
  library(rio)
  library(sf)
  library(osmdata)
  carp()
  dsn <- overpass_arrets_query(force = force)
  carp("dsn: %s", dsn)
  q <- opq(bbox = c(45, -6, 58, 0))
  od <- osmdata_sf(q, dsn) %>%
    glimpse()
  points.sf <- od$osm_points %>%
    dplyr::select(osm_id, name, Config[["k_ref"]]) %>%
    glimpse()
  Encoding(points.sf$name) <- "UTF-8"
  dsn <- sprintf("%s/overpass_arrets_parse.Rds", transportDir)
  carp("dsn: %s", dsn)
  saveRDS(points.sf, file = dsn)
}
#
## nouvelle version de ma bibliothèque overpass
##########################################################################
#
overpass_get <- function(query, format = "xml", force_osm = TRUE) {
  library(osmdata)
  library(xml2)
  library(data.table)
  fic <- query
  carp("1 fic: %s", fic)
  query <- sprintf("overpass_query_%s", query)
  if (format == "csv") {
    query <- sprintf("%s_%s", query, format)
  }
  carp("query: %s", query)
  query <- do.call(query, list())
  carp("2 fic: %s", fic)
  if (format == "csv") {
    carp("fic: %s", fic)
#    query <- gsub(';true;"\t"', ';true;"µ"', query)
    dsn <- overpass_query_csv(query = query, fic = fic, force = force_osm)
# , sep = "\t"
#    res <- fread(dsn, encoding = "UTF-8", header = TRUE, sep = "|") %>%
    res <- fread(dsn, encoding = "UTF-8", header = TRUE, sep = "\t") %>%
      replace(is.na(.), "")
    return(invisible(res))
  }
  if (format == "od") {
    dsn <- overpass_query(query, fic, force = force_osm)
    res <- osmdata::osmdata_sf(, dsn)
    return(invisible(res))
  }
  if (format == "xml") {
    dsn <- overpass_query(query, fic, force = force_osm)
    doc <- xml2::read_xml(dsn)
    return(invisible(doc))
  }
  if (format == "json") {
    dsn <- overpass_query_json(query, fic, force = force_osm)
    json.list <- jsonlite::fromJSON(dsn, simplifyVector = FALSE, simplifyDataFrame = FALSE)
    return(invisible(json.list))
  }
  return(invisible(FALSE))
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
overpass_query_csv <- function(query, fic = "test", force = TRUE) {
  library(httr)
  library(tidyverse)
  dsn <- sprintf("%s/%s.csv", osmDir, fic)
  mga <<- force
  carp("dsn: %s", dsn)
  if (! file.exists(dsn) | force == TRUE) {
    carp("query: %s", query)
    res <- httr::POST(overpass_base_url, body=query, httr::write_disk(dsn, overwrite = TRUE))
    if (status_code(res) != 200) {
      print(httr::content(res, as="text", encoding="UTF-8"));
      stop_for_status(res)
    }
  }
  return(invisible(dsn))
}
overpass_query_json <- function(query, fic = "test", force = FALSE) {
  library(httr)
  library(tidyverse)
  library(rjson)
  dsn <- sprintf("%s/%s.json", osmDir, fic)
  carp("dsn: %s", dsn)
  if (! file.exists(dsn) || force == TRUE) {
    query <- sprintf("data=[timeout:600][maxsize:1073741824][out:json];%s", query)
    print(query);
    res <- httr::POST(overpass_base_url, body=query, httr::write_disk(dsn, overwrite = TRUE))
    stop_for_status(res)
  }
  return(invisible(dsn))
}
overpass_query_xml <- function(query, fic, force = TRUE, quiet = FALSE) {
  carp("query: %s", query)
  dsn <- sprintf("%s/%s.osm", osmDir, fic)
  carp("dsn: %s", dsn)
  res <- httr::POST(overpass_base_url, body = query, httr::write_disk(dsn, overwrite = TRUE))
  httr::stop_for_status(res)
  if (!quiet) message("Query complete!")
  doc <- xml2::read_xml(httr::content(res, as = "text", encoding = "UTF-8"))
  return(invisible(doc))
}
# requête en get
overpass_query_geojson_get <- function(data, dsn) {
  library(httr)
  carp("début data:%s", data)
  url <- 'http://overpass-api.de/api/interpreter'
  data <- sprintf("[out:GeoJson][timeout:180];%s", data)
  url <- sprintf('%s?data=%s', url, URLencode(data));
  carp("url: %s", url)
  res <- httr::GET(url = query, encoding = "UTF-8", type = "application/json", verbose(), httr::write_disk(dsn, overwrite = TRUE))
  json <- httr::content(res, as="text", encoding="UTF-8")
  return(invisible(dsn))
}
#
## les requêtes pour les "name" de la zone
#
overpass_query_name_area_csv <- function() {
  requete <- sprintf('[out:csv(name;true;"\t")];
relation(%s);map_to_area->.a;
(
  nwr(area.a)[name];
);
out center meta;', Config[1, 'zone_relation'])
  return(invisible(requete))
}
#
## les requêtes pour les arrêts
#
overpass_query_bus_stop_kref_csv <- function() {
  requete <- sprintf('[out:csv(::type,::id,::version,::timestamp,::changeset,::user,::lat,::lon,name,highway,public_transport,aerialway,bus,ferry,rail,train,tram,railway,walking_bus,"%s";true;"\t")];
(
  nwr[highway=bus_stop]["%s"];
  nwr[public_transport=platform][railway!=platform]["%s"];
);
out center meta;', Config[1, 'k_ref'], Config[1, 'k_ref'], Config[1, 'k_ref'])
  return(invisible(requete))
}
overpass_query_bus_stop_network <- function() {
  requete <- sprintf('
relation[type=route][route=bus][network="%s"]->.a;
(
  nwr[highway=bus_stop](r.a);
  nwr[public_transport=platform](r.a);
);
(._;>>;);
out meta;', Config[1, 'network'])
  return(invisible(requete))
}
overpass_query_bus_stop_network_csv <- function() {
  requete <- sprintf('[out:csv(::type,::id,::version,::timestamp,::user,::lat,::lon,name,highway,public_transport,aerialway,bus,ferry,rail,train,tram,railway,walking_bus,"%s";true;"\t")];
relation[type=route][route=bus][network="%s"]->.a;
(
  nwr[highway=bus_stop](r.a);
  nwr[public_transport=platform](r.a);
);
out center meta;', Config[1, 'k_ref'], Config[1, 'network'])
  return(invisible(requete))
}
overpass_query_bus_stop_area <- function() {
  requete <- sprintf('
relation(%s);map_to_area->.a;
(
  nwr(area.a)[highway=bus_stop];
  nwr(area.a)[public_transport=platform];
);
(._;>>;);
out center meta;', Config[1, 'zone_relation'])
  return(invisible(requete))
}
overpass_query_bus_stop_area_csv <- function() {
  requete <- sprintf('[out:csv(::type,::id,::version,::timestamp,::user,::lat,::lon,name,highway,public_transport,aerialway,bus,ferry,rail,train,tram,railway,walking_bus,"%s";true;"\t")];
relation(%s);map_to_area->.a;
(
  nwr(area.a)[highway=bus_stop];
  nwr(area.a)[public_transport];
);
out center meta;', Config[1, 'k_ref'], Config[1, 'zone_relation'])
  return(invisible(requete))
}
overpass_query_bus_stop_bbox_csv <- function() {
  requete <- sprintf('[out:csv(::type,::id,::version,::timestamp,::user,::lat,::lon,name,highway,public_transport,aerialway,bus,ferry,rail,train,tram,railway,walking_bus,"%s";true;"\t")];
(
  nwr[highway=bus_stop](%s);
  nwr[public_transport](%s);
);
out center meta;', Config[1, 'k_ref'], Config_bbox, Config_bbox)
  return(invisible(requete))
}
overpass_query_nodes_bus_platform_area_csv <- function() {
  requete <- sprintf('[out:csv(::type,::id,::version,::timestamp,::user,::lat,::lon,name,highway,public_transport;true;"\t")];
relation(%s);map_to_area->.a;
nwr(area.a)[highway=bus_stop][public_transport=platform];
out center meta;', Config[1, 'zone_relation'])
  return(invisible(requete))
}
overpass_query_nodes_bus_platform_mga_csv <- function() {
  requete <- sprintf('// les arrêts qui ne sont pas référencés
[out:csv(::type,::id,::version,::timestamp,::user,::lat,::lon,::changeset,name;true)];
relation(%s);map_to_area->.a;
// ensemble des arrêts
node(area.a)[highway=bus_stop][public_transport=platform](user:"mga_geo")->.na;
// les relations de ces arrêts
rel(bn.na)->.rbus;
// les noeuds des relations
node(r.rbus)->.nbus;
(node.na;-node.nbus;);
out meta;', Config[1, 'zone_relation'])
  return(invisible(requete))
}
overpass_query_nodes_platform_kref <- function() {
  requete <- sprintf('
(
node[highway=bus_stop]["%s"];
node[public_transport=platform]["%s"];
);
out meta;', Config[1, 'k_ref'], Config[1, 'k_ref'])
  return(invisible(requete))
}
#
## les requêtes pour les relations "route"
#
overpass_query_relations_route_bus_network <- function() {
  requete <- sprintf('area[name="%s"]->.a;
relation(area.a)[type=route][route=bus][network="%s"];
out meta;', Config[1, 'zone'], Config[1, 'network'])
  carp("requete: %s", requete)
  return(invisible(requete))
}
overpass_query_relations_route_bus_network_csv <- function() {
  requete <- sprintf('[out:csv(::type,::id,::version,::timestamp,::user,network,type,route,"disused:route",operator,name,description,ref,"ref:network","gtfs:shape_id",from,to,colour,text_colour,"network:wikidata","network:wikipedia";true;"\t")];
area[name="%s"]->.a;
relation(area.a)[type=route][route=bus][network="%s"];
out meta;', Config[1, 'zone'], Config[1, 'network'])
  return(invisible(requete))
}
overpass_query_relations_bus_area_csv <- function() {
  requete <- sprintf('[out:csv(::type,::id,::version,::timestamp,::user,network,type,route,"disused:route",name,ref,"ref:network","gtfs:shape_id",from,to;true;"\t")];
relation(%s);map_to_area->.a;
relation(area.a)[type=route][~"route"~"bus"];
out meta;', Config[1, 'zone_relation'])
  return(invisible(requete))
}
#
## les requêtes pour les relations "route_master"
#
overpass_query_relations_routemaster_bus_network <- function() {
  requete <- sprintf('relation[type=route_master][route_master=bus][network="%s"];
out meta;', Config[1, 'network'])
  return(invisible(requete))
}
overpass_query_relations_routemaster_bus_network_csv <- function() {
  requete <- sprintf('[out:csv(::type,::id,::version,::timestamp,::user,network,name,ref;true;"\t")];
relation[type=route_master][route_master=bus][network="%s"];
out meta;', Config[1, 'network'])
  return(invisible(requete))
}
#
overpass_query_relations_routemaster_bus_area <- function() {
  requete <- sprintf('area[name="%s"]->.a;
relation(area.a)[type=route][route=bus];
rel(br);
out meta;', Config[1, 'zone'])
  carp("requete: %s", requete)
  return(invisible(requete))
}
#
overpass_query_relations_bus_network_csv <- function() {
  requete <- sprintf('[out:csv(::type,::id,::version,::timestamp,::user,type,route,route_master,network,name,ref,operator,colour,text_colour,"network:wikidata","network:wikipedia";true;"\t")];
area[name="%s"]->.a;
(
relation(area.a)[type=route][route=bus][network="%s"];
rel(br);
relation[type=route_master][route_master=bus][network="%s"];
relation(r);
);
out meta;', Config[1, 'zone'], Config[1, 'network'], Config[1, 'network'])
  carp("requete: %s", requete)
  return(invisible(requete))
}
#
overpass_query_relations_bus_network <- function() {
  requete <- sprintf('area[name="%s"]->.a;
(
relation(area.a)[type=route][~"route"~"^bus$"][network="%s"];
rel(br);
relation[type=route_master][~"route_master"~"^bus$"][network="%s"];
relation(r);
);
out meta;', Config[1, 'zone'], Config[1, 'network'], Config[1, 'network'])
  carp("requete: %s", requete)
  return(invisible(requete))
}