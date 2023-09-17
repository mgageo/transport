# <!-- coding: utf-8 -->
#
# le réseau de bus de Quimper
#
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
#
# réseau sans shape
# source("geo/scripts/transport.R");qub_jour()
qub_jour <- function(reseau = "qub", force = FALSE) {
  carp()
#  mobibreizh_gtfs_reseau(config[1, "reseau"], config[1, "agency_id"])
  carp("reseau: %s", reseau)
  config_xls(reseau)
  reseau_tpl_tex(reseau = reseau)
#  qub_opendata()
  tidytransit_jour()
  return()
  tidytransit_routes_trips_stops()
#  osm_relations_route_bus_verif(reseau = reseau, force = force)
#  osm_relations_route_members(reseau = reseau, force = force)
  tex_pdflatex(sprintf("%s_osm.tex", reseau))
}
#
# téléchargement des données en opendata
#
# source("geo/scripts/transport.R");qub_opendata()
qub_opendata <- function(force = TRUE) {
  library(tidyverse)
  library(rio)
  library(archive)
  carp()
  config_xls("qub")
  gtfs_source <- "https://www.data.gouv.fr/fr/datasets/r/251cbf71-a4e7-4be4-9212-6c91f19bd71b"
  dsn_source <- sprintf("%s/gtfs.zip", gtfsDir)
  if (force == TRUE | ! file.exists(dsn_source) ) {
    download.file(gtfs_source, dsn_source)
    setwd(gtfsDir)
    archive::archive_extract("gtfs.zip", dir = ".")
    setwd(baseDir)
  }
}
# source("geo/scripts/transport.R");qub_stops_diff()
qub_stops_diff <- function() {
  library(tidyverse)
  library(rio)
  carp()
  config_xls('qub');
#  agency.df <- mobibreizh_agency_lire('qub')
  mobi.df <- gtfs_stops_verif() %>%
#    filter(grepl('^axeo', stop_id)) %>%
    glimpse()
  mobi.sf <- st_as_sf(mobi.df, coords=c("lon", "lat"), crs = 4326) %>%
    dplyr::select(stop_name) %>%
    st_transform(2154)
  osm.sf <- qub_osm_busstop_sf() %>%
#    filter(is.na(name)) %>%
    dplyr::select(name, osm_id) %>%
    st_transform(2154)
  df <- qub_proches(osm.sf, mobi.sf) %>%
    dplyr::select(-geometry.x, -geometry.y) %>%
    dplyr::select(osm_id, stop_name) %>%
    glimpse()
  dsn <- sprintf("%s/stops_diff.csv", cfgDir);
  export(df, dsn);
  carp('dsn: %s', dsn)
  carp('distance des arrêts mobi')
  df <- qub_proches(mobi.sf, osm.sf) %>%
    dplyr::select(-geometry.x, -geometry.y) %>%
    glimpse()
  View(df)
}
#
# qub_proche
qub_proches <- function(nc1, nc2) {
  library(sf)
  library(tidyverse)
  carp('nc1: %d', nrow(nc1))
  nc2$no <- 1:nrow(nc2)
  nc1$proche <- st_nearest_feature(nc1, nc2)
  df <- dplyr::left_join(nc1 %>% as.data.frame(), nc2 %>% as.data.frame(), by=c('proche'='no')) %>%
    glimpse()
#  stop('***')
  df$d <- st_distance(df$geometry.x, df$geometry.y, by_element = TRUE)
  units(df$d) <- 'm'
  return(invisible(df))
}
# source("geo/scripts/transport.R");qub_osm_busstop_sf()
qub_osm_busstop_sf <- function(force = FALSE) {
  carp()
  dsn <- 'network_busstop'
  requete <- sprintf("relation[network='%s']->.a;node(r.a);out meta;", config[1, "network"])
  dsn <- overpass_query(query = requete, fic = dsn, force = force)
  nc <- oapi_sf_lire(fic = dsn)
  return(invisible(nc))
}
qub_osm_get_xml <- function(force = FALSE) {
  carp()
  dsn <- 'qub_busstop.osm'
  requete <- "relation[network='qub']->.a;node(r.a);out meta;"
  oapi_requete_get(requete=requete, fic=dsn, force = force)
  od <- oapi_osmdata_lire_xml(fic=dsn) %>%
    glimpse()
}
# source("geo/scripts/transport.R");qub_nodes_busstop_valid()
qub_nodes_busstop_valid <- function(force = TRUE) {
  library(tidyverse)
  library(rio)
  carp()
  config_xls('qub');
  dsn <- 'network_nodes_busstop_valid'
  requete <- sprintf("relation[network='%s']->.a;node[!'public_transport'](r.a)->.b;(.b;way(bn.b););out meta;", config[1, "network"])
  dsn <- overpass_query(query = requete, fic = dsn, force = force)
  nc <- oapi_sf_lire(fic = dsn)
  return(invisible(nc))
}
#
# source("geo/scripts/transport.R");config_xls('qub');qub_stops_diff()
qub_stops_diff <- function(force = FALSE) {
  library(tidyverse)
  library(janitor)
  gtfs.sf <- txt_gtfs_stops_sf(force = force) %>%
    filter(location_type == 0) %>%
    glimpse()
#  gtfs2osm_stops_create(gtfs.sf);return()
  gtfs.df <- gtfs.sf %>%
    st_drop_geometry()
  osm.sf <- osm_area_busstop_get(force = force)
  k_ref <- config[1, "k_ref"]
  carp("k_ref: %s", k_ref)
  if (k_ref %in% names(osm.sf)) {
    osm.sf <- osm.sf %>%
    dplyr::select(ref = k_ref, name, osm_id) %>%
    mutate(stop_id = as.numeric(str_extract(ref, "\\d+")))
  } else {
    carp("pas de bus_stop")
    osm.sf$ref <- ""
    osm.sf$stop_id <- ""
  }
  gtfs2osm_stops(gtfs.sf, osm.sf)
  return()
  osm.df <- osm.sf %>%
    st_drop_geometry() %>%
    filter(! is.na(`ref:FR:TUB`)) %>%
    dplyr::select(name, `ref:FR:TUB`) %>%
    mutate(stop_id = as.numeric(str_extract(`ref:FR:TUB`, "\\d+"))) %>%
    glimpse()
  df1 <- gtfs.df %>%
    full_join(osm.df, by = c("stop_id"), suffix = c(".gtfs", ".osm")) %>%
    filter(is.na(name)) %>%
    arrange(stop_id) %>%
    glimpse()
  nc1 <- st_join(gtfs.sf, osm.sf, join = st_nearest_feature) %>%
    glimpse()
}
#
## la partie routage avec osrm
#
# source("geo/scripts/transport.R");dsn <- qub_relations_route_get()
#
qub_relations_route_get <- function(force = TRUE) {
  library(tidyverse)
  library(rio)
  library(xml2)
  carp()
  config_xls('qub')
  fic <- 'relations_route'
  requete <- sprintf("relation[network='%s'][type=route];
out meta;", Config[1, "network"])
  dsn <- overpass_query(query = requete, fic = fic, force = force)
  doc <- xml2::read_xml(dsn)
  has_relations <- osm_has_xpath(doc, "//relation")
  if (! has_relations) {
    stop("***")
  }
  relations <- xml2::xml_find_all(doc, "//relation")
  carp("relations nb: %s", length(relations))
  html_entete <- '<html>
<head>
  <meta charset="UTF-8">
  <title>transport QUB</title>
</head>
<body>'
  html_pied <- '
  </body>
</html>'
  html <- html_entete
  for (relation in relations) {
    id <- xml_attr(relation, "id")[[1]]
    if ( id != "4011816") {
      next;
    }
    carp("id: %s", id)
    href <- sprintf("<br/><b>%s</b>", id)
    html <- append(html, href)
    tag_ref <<-  xml2::xml_attr(xml2::xml_find_first(relation, './/tag[@k="ref"]'), "v")
    tag_ref_network <<-  xml2::xml_attr(xml2::xml_find_first(relation, './/tag[@k="ref:network"]'), "v")
    tags <- sprintf(" %s %s", tag_ref, tag_ref_network)
    html <- append(html, tags)
    members_way <<-  xml2::xml_find_all(relation, './/member[@type="way"]')
    members_node <<-  xml2::xml_find_all(relation, './/member[@type="node"]')
    href <- sprintf("members_way: %s", length(members_way))
    html <- append(html, href)
    href <- sprintf(" members_node: %s", length(members_node))
    html <- append(html, href)
#    stop("****")
#    id <- xml_attr(relation, "id")
    carp("id : %s ways: %s nodes: %s", id, length(members_way), length(members_node))
    if (length(members_way) != 0) {
      carp("id: %s ways", id)
      next;
    }
    if (length(members_node) < 2) {
      carp("id: %s nodes", id)
      next;
    }
    type <- "relation"
    dsn <- qub_osrm(id, force = force)
    if ( ! file.exists(dsn) || force == TRUE) {
#      next
    }
#    stop("****")
    href <- sprintf("<a href='http://localhost:8111/import?url=https://api.openstreetmap.org/api/0.6/relation/%s/full'>josm</a>", id)
    html <- append(html, href)
    href <- sprintf("<a href='http://level0.osmz.ru/?url=relation/%s'>level0</a>", id)
    html <- append(html, href)
    href <- sprintf("<a href='http://bv/transport/qub/level0/relation_%s_level0.txt'>osrm</a>", id)
    html <- append(html, href)
  }
  html <- append(html, html_pied)
  dest <- "qub.html"
  dsn <- sprintf("%s/%s", webDir, dest)
  write(html, dsn)
  carp("dsn: %s", dsn)
  return()
}
#
# pour une ref
# source("geo/scripts/transport.R");qub_osrm(ref = 12228687, force = TRUE)
qub_osrm <- function(ref, force = TRUE) {
  library(clipr)
  carp()
  config_xls('qub');
  carp("josmDir: %s", josmDir)
  carp("level0Dir: %s", level0Dir)
#  osrm_relation_stops(12307545, force = force)
#  ref <- 12307555
#  ref <- 12307567 ; # 13-B
  type <- "relation"
  dsn <- sprintf("%s/%s_%s_level0.txt", level0Dir, type, ref)
  if ( ! file.exists(dsn) || force == TRUE) {
    osrm_relation_stops(ref, force = force)
  }
  return(dsn)
  carp(dsn)
  level0 <- read_lines(file = dsn)
  write_clip(level0)
}
#
# comparaison gtfs wiki
# source("geo/scripts/transport.R");qub_gtfs_wiki()
qub_gtfs_wiki <- function() {
  library(rio)
  library(tidyverse)
  config_xls('qub');
  dsn <- sprintf("%s/routemaster_csv.txt", reseau_dir)
  carp("dsn: %s", dsn)
  wiki.df <- rio::import(dsn, encoding = "UTF-8") %>%
    dplyr::select(ref, name, description, colour, text_colour) %>%
    glimpse()
  dsn <- sprintf("%s/routes.txt", gtfsDir)
  carp("dsn: %s", dsn)
  gtfs.df <- rio::import(dsn, encoding = "UTF-8") %>%
    dplyr::select(route_short_name, route_long_name, route_color, route_text_color) %>%
    filter(route_text_color != "") %>%
    mutate(route_color = sprintf("#%s", route_color)) %>%
    mutate(route_text_color = sprintf("#%s", route_text_color)) %>%
    glimpse()
  df <- wiki.df %>%
    full_join(gtfs.df, by = c("ref" = "route_short_name")) %>%
    filter(! is.na(name)) %>%
    filter(! is.na(route_long_name)) %>%
    dplyr::select(-description, -route_long_name) %>%
    filter(colour != route_color) %>%
    glimpse()
  misc_print(df)
}
#
# comparaison des fichiers de référence
# source("geo/scripts/transport.R");qub_masters_routes()
qub_masters_routes <- function() {
  library(rio)
  library(tidyverse)
  config_xls('qub');
  dsn <- sprintf("%s/routes.txt", cfg_dir)
  carp("dsn: %s", dsn)
  routes.df <- rio::import(dsn, encoding = "UTF-8") %>%
    glimpse()
  dsn <- sprintf("%s/masters.txt", cfg_dir)
  carp("dsn: %s", dsn)
  masters.df <- rio::import(dsn, encoding = "UTF-8") %>%
    glimpse()
  carp("les ref routes absentes dans masters")
  df <- routes.df %>%
    filter(! ref %in% masters.df$ref)
  if (nrow(df) >0) {
    misc_print(df)
    confess()
  }
  carp("le name des routes")
  df <- routes.df %>%
    filter(! grepl("^Bus QUB", name))
  if (nrow(df) > 0) {
    misc_print(df)
    confess()
  }
  carp("la meme description")
  df <- routes.df %>%
    full_join(masters.df, by = c("ref")) %>%
    filter(description.x != description.y) %>%
    dplyr::select(ref, description.x, description.y)
  if (nrow(df) > 0) {
    misc_print(df)
    confess()
  }
  carp("routes, name : Bus QUB ...")
  df1 <- routes.df %>%
    extract(name, c("route_name", "route_ref", "route_desc"), "^(Bus\\s\\S+)\\s(\\S+)\\s(.*)$", remove = FALSE)
  df <- df1 %>%
    filter(ref_network != route_ref) %>%
    dplyr::select(ref, name)
  if (nrow(df) > 0) {
    misc_print(df)
    confess()
  }
  df <- df1 %>%
    filter(is.na(route_desc)) %>%
    dplyr::select(ref, name)
  if (nrow(df) > 0) {
    misc_print(df)
  }
  df2 <- df1 %>%
    filter(is.na(route_desc)) %>%
    mutate(name = sprintf("%s %s %s", "Bus QUB", ref_network, description))
  df3 <- df1 %>%
    filter(! is.na(route_desc)) %>%
    bind_rows(df2) %>%
    dplyr::select(-route_name, -route_ref, -route_desc) %>%
    arrange(ref) %>%
    glimpse()
  dsn <- sprintf("%s/routes_masters.txt", cfg_dir)
  carp("dsn: %s", dsn)
  rio::export(df3, file = dsn, sep = ";")
}
#
# bug du gtfs
# source("geo/scripts/transport.R");qub_gtfs_bug()
qub_gtfs_bug <- function() {
  library(rio)
  library(tidyverse)
  config_xls('qub');
  dsn <- sprintf("%s/stops.txt", gtfsDir)
  stops.df <- rio::import(dsn, encoding = "UTF-8") %>%
    filter(grepl("KERROUE", stop_name)) %>%
    glimpse()
  dsn <- sprintf("%s/stop_times.txt", gtfsDir)
  stop_times.df <- rio::import(dsn, encoding = "UTF-8") %>%
    filter(stop_id %in% stops.df$stop_id) %>%
    glimpse()
  dsn <- sprintf("%s/trips.txt", gtfsDir)
  trips.df <- rio::import(dsn, encoding = "UTF-8") %>%
    filter(trip_id %in% stop_times.df$trip_id) %>%
    glimpse()
  dsn <- sprintf("%s/routes.txt", gtfsDir)
  routes.df <- rio::import(dsn, encoding = "UTF-8") %>%
    filter(route_id %in% trips.df$route_id) %>%
    glimpse()
}
#
# bug stations de tram
# source("geo/scripts/transport.R");qub_gtfs_tram()
qub_gtfs_tram <- function() {
  library(rio)
  library(tidyverse)
  config_xls('qub');
  dsn <- sprintf("%s/stops.txt", gtfsDir)
  stops.df <- rio::import(dsn, encoding = "UTF-8") %>%
    filter(grepl("CHATEAU", stop_name)) %>%
    glimpse()
}
#
## pour mettre en minuscules le nom des arrêts
# source("geo/scripts/transport.R");config_xls(Reseau);osm_nodes_bus_topo()
qub_osm_nodes_bus_topo_v1 <- function(fic = 'nodes_bus', force = FALSE) {
  library(stringr)
  dsn <- "d:/web.var/TRANSPORT/QUB/OSM/nodes_bus.csv"
  df <- fread(dsn, encoding = "UTF-8") %>%
    clean_names() %>%
    dplyr::select(-type) %>%
    glimpse()
  df1 <- df %>%
    filter(name != "") %>%
    filter(!grepl("[a-z]", name)) %>%
    mutate(nom = str_to_title(name)) %>%
    glimpse()
  remplacements <- "src;dest;bid
 De La ; de la ;
 D E; d'E;
 De ; de ;
 Du ; du ;
 Des ; des ;
 D'; d';
 L'; l';
 An ; an ;
 Ar ; ar ;
 Au ; au ;
 Aux ; aux ;
 Ty ; ty ;
^Zac ;ZAC ;
^Zc ;ZC ;
^Zi ;ZI ;
^Epsm;EPSM;
^Cmpi;CMPI;
^C.c. ; C.C. ;
^Cc ;CC ;
'H$;'h;
^Cfa;CFA;
^Iut;IUT;
Rdpt ; RdPt ;
^A\\.m\\.;A.M.;
Sncf;SNCF;
^Caf;CAF;
D'auvergne;d'Auvergne;
 Andre; André;
Abbe$;Abbé;
Aeroport;Aéroport
Ampere;Ampère;
Becharles;Bécharles;
Bleriot;Blériot;
Boissiere;Boissière;
Chateau;Château;
^Cite;Cité;
College;Collège;
Congres;Congrès;
Creac'h;Créac'h;
Crenal;Crénal
Echange;Échange;
Ecole;École;
Eglise;Église;
Entree;Entrée;
Ergue;Ergué;
Eric;Éric;
Exupery;Exupéry;
Francois;François;
Freres;Frères;
Gaberic; Gabéric;
General;Général;
Genets;Genêts;
Glaieuls;Glaïeuls;
Guelen;Guélen;
Guenole;Guénolé;
Guepratte;Guépratte
Herve;Hervé;
Kerdevot;Kerdévot;
Kerdiles;Kerdilès;
Kerdrezec;Kerdrézec;
Kerelan;Kérélan;
Kerguelen;Kerguélen;
Kerroue;Kerroué;
Kerdevot;Kerdévot;
Leurgueric;Leurguéric;
Lezebel;Lézebel;
Liberation;Libération;
Lycee;Lycée;
Masse$;Massé;
Mendes;Mendès;
Menez;Ménez;
Metairie;Métairie;
Nevez;Névez;
Nominoe;Nominoë;
Pole;Pôle;
President;Président;
Pyrenees;Pyrénées;
Rene$;René;
Resistance;Résistance;
Routiere;Routière;
Thepot;Thépot;
Therese;Thérèse;
Treodet;Tréodet;
Universite;Université;
"
  r.df <- fread(remplacements, strip.white = FALSE) %>%
    glimpse()
  for (i in 1:nrow(r.df)) {
    df1$nom = str_replace(df1$nom, r.df[[i, "src"]], r.df[[i, "dest"]])
  }
  misc_print(df1)
  dsn <- "d:/web.var/TRANSPORT/QUB/OSM/nodes_bus_topo.csv"
  rio::export(df1, dsn, sep = ";")
}


qub_osm_nodes_bus_topo <- function(fic = 'nodes_bus', force = FALSE) {
  library(stringr)
  library(stringi)
  dsn <- "d:/web.var/TRANSPORT/QUB/OSM/nodes_bus.csv"
  df <- fread(dsn, encoding = "UTF-8") %>%
    clean_names() %>%
    dplyr::select(-type) %>%
    glimpse()
  df1 <- df %>%
    dplyr::select(name) %>%
    mutate(NAME = str_to_upper(name)) %>%
    mutate(NAME = stri_trans_general(NAME, id = "Latin-ASCII")) %>%
    arrange(NAME) %>%
    distinct(name, NAME) %>%
    filter(name != NAME) %>%
    mutate(topo = sprintf("%s;%s", NAME, name)) %>%
    dplyr::select(topo) %>%
    glimpse()
  dsn <- "d:/web.var/TRANSPORT/QUB/OSM/nodes_bus_topo.csv"
  rio::export(df1, dsn, sep = ";")
}
#
# source("geo/scripts/transport.R");config_xls(Reseau);osm_nodes_bus_mga()
qub_osm_nodes_bus_mga <- function(fic = 'nodes_bus', force = FALSE) {
  library(stringr)
  library(stringi)
  dsn <- "d:/web.var/TRANSPORT/QUB/OSM/nodes_bus.csv"
  osm.df <- fread(dsn, encoding = "UTF-8") %>%
    clean_names() %>%
    dplyr::select(-type) %>%
    mutate(NAME = str_to_upper(name)) %>%
    mutate(NAME = stri_trans_general(NAME, id = "Latin-ASCII")) %>%
    glimpse()
  dsn <- "d:/web.var/TRANSPORT/QUB/OSM/nodes_bus_topo_mga.csv"
  mga.df <- fread(dsn, encoding = "UTF-8") %>%
    glimpse()
  df1 <- osm.df %>%
    left_join(mga.df, by = c("NAME" = "NAME")) %>%
    filter(is.na(name.y)) %>%
    filter(NAME != "") %>%
    arrange(NAME) %>%
    distinct(NAME) %>%
    glimpse()
  df2 <- osm_topo(df1) %>%
    glimpse()
  dsn <- "d:/web.var/TRANSPORT/QUB/OSM/nodes_bus_topo.csv"
  fwrite(df2, dsn, sep = ";")
}
qub_osm_topo <- function(df1) {
  library(stringr)
  remplacements <- "src;dest;bid
 De La ; de la ;
 D E; d'E;
 De ; de ;
 Du ; du ;
 Des ; des ;
 D'; d';
 L'; l';
 An ; an ;
 Ar ; ar ;
 Au ; au ;
 Aux ; aux ;
 Ty ; ty ;
^Zac ;ZAC ;
^Zc ;ZC ;
^Zi ;ZI ;
^Epsm;EPSM;
^Cmpi;CMPI;
^C.c. ; C.C. ;
^Cc ;CC ;
'H$;'h;
^Cfa;CFA;
^Iut;IUT;
Rdpt ; RdPt ;
Sncf;SNCF;
^Caf;CAF;
D'auvergne;d'Auvergne;
 Andre; André;
Abbe$;Abbé;
Aeroport;Aéroport;
Ampere;Ampère;
Becharles;Bécharles;
Bleriot;Blériot;
Boissiere;Boissière;
Chateau;Château;
^Cite;Cité;
College;Collège;
Congres;Congrès;
Creac'h;Créac'h;
Crenal;Crénal;
Echange;Échange;
Ecole;École;
Eglise;Église;
Entree;Entrée;
Ergue;Ergué;
Eric;Éric;
Exupery;Exupéry;
Francois;François;
Freres;Frères;
Gaberic; Gabéric;
General;Général;
Genets;Genêts;
Glaieuls;Glaïeuls;
Guelen;Guélen;
Guenole;Guénolé;
Guepratte;Guépratte;
Herve;Hervé;
Kerdevot;Kerdévot;
Kerdiles;Kerdilès;
Kerdrezec;Kerdrézec;
Kerelan;Kérélan;
Kerguelen;Kerguélen;
Kerroue;Kerroué;
Kerdevot;Kerdévot;
Leurgueric;Leurguéric;
Lezebel;Lézebel;
Liberation;Libération;
Lycee;Lycée;
Masse$;Massé;
Mendes;Mendès;
Menez;Ménez;
Metairie;Métairie;
Nevez;Névez;
Nominoe;Nominoë;
Pole;Pôle;
President;Président;
Pyrenees;Pyrénées;
Rene$;René;
Resistance;Résistance;
Routiere;Routière;
Thepot;Thépot;
Therese;Thérèse;
Treodet;Tréodet;
Universite;Université;
"
  r.df <- fread(remplacements, strip.white = FALSE) %>%
    glimpse()
  df1 <- df1 %>%
    mutate(nom = str_to_title(NAME))
  for (i in 1:nrow(r.df)) {
    df1$nom = str_replace(df1$nom, r.df[[i, "src"]], r.df[[i, "dest"]])
  }
  return(invisible(df1))
}