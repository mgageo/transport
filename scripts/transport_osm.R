# <!-- coding: utf-8 -->
# le réseau de bus
# utilisation des données d'OpenStreetMap
# auteur : Marc Gauthier
#
# https://github.com/oscarperpinan/spacetime-vis/blob/master/osmar.R
#
#
# les traitements journaliers
# source("geo/scripts/transport.R");config_xls('star');osm_jour(routemaster = TRUE, route = TRUE, stop = TRUE)
osm_jour <- function(routemaster = FALSE, route = TRUE, stop = TRUE, force = FALSE) {
  carp()
  if( routemaster == TRUE ) {
    osm_relations_routemaster_bus_get()
    osm_relations_routemaster_bus_save()
    osm.sf <- osm_relations_routemaster_bus_read() %>%
      glimpse()
  }
  if( route == TRUE ) {
    osm_relations_route_bus_get(force = force)
    osm_relations_route_bus_save()
    osm.sf <- osm_relations_route_bus_read() %>%
      glimpse()
  }
  if( stop == TRUE ) {
    osm_nodes_stop_get()
    osm_nodes_stop_save()
    osm.sf <- osm_nodes_stop_read() %>%
      glimpse()
  }
}
# les routes
#
osm_relations_route_bus_get <- function(fic = 'relations_route_bus', force = FALSE) {
  requete <- sprintf("(relation[network='%s'][type=route][route=bus];);out meta;", Config[1, 'network'])
  dsn <- sprintf("%s.osm", fic)
  carp("dsn: %s", dsn)
  oapi_requete_get(requete, dsn, force = force)
}
# lecture avec sf
# https://github.com/r-spatial/sf/blob/master/tests/read.Rout.save
osm_relations_route_bus_save <- function(fic='relations_route_bus') {
  library(sf)
  library(tidyverse)
  carp('fic: %s', fic)
  dsn <- sprintf("%s/%s.osm", osmDir, fic)
  osm.sf <- oapi_osmdata_lire_sf(dsn)
  dsn <- sprintf("%s/%s.Rds", transportDir, fic)
  saveRDS(osm.sf, dsn)
  return(invisible(osm.sf))
}
# version en xml
osm_relations_route_bus_save <- function(fic='relations_route_bus') {
  library(sf)
  library(tidyverse)
  carp('fic: %s', fic)
  dsn <- sprintf("%s/%s.osm", osmDir, fic)
  doc <- xml2::read_xml(dsn, encoding = "UTF-8")
  has_relations <- osm_has_xpath(doc, "//relation")
  if (! has_relations) {
    stop("***")
  }
  relations <- xml2::xml_find_all(doc, "//relation")
  carp("relations nb: %s", length(relations))
  dsn <- sprintf("%s/%s.Rds", transportDir, fic)
  saveRDS(df, dsn)
  return(invisible(df))
}
osm_relations_route_bus_read <- function(fic='relations_route_bus') {
  library(sf)
  library(tidyverse)
  dsn <- sprintf("%s/%s.Rds", transportDir, fic)
  osm.sf <- readRDS(dsn)
  carp('dsn: %s', dsn)
  return(invisible(osm.sf))
}
# les route_master
#
osm_relations_routemaster_bus_get <- function(fic='relations_routemaster_bus') {
  requete <- sprintf("(relation[network='%s'][type=route_master][route_master=bus];);out meta;", Config[1, 'network'])
  dsn <- sprintf("%s.osm", fic)
  oapi_requete_get(requete, dsn)
}
# lecture avec sf
# https://github.com/r-spatial/sf/blob/master/tests/read.Rout.save
osm_relations_routemaster_bus_save <- function(fic='relations_routemaster_bus') {
  library(sf)
  library(tidyverse)
  carp('fic: %s', fic)
  dsn <- sprintf("%s/%s.osm", osmDir, fic)
  osm.sf <- oapi_osmdata_lire_sf(dsn)
  dsn <- sprintf("%s/%s.Rds", transportDir, fic)
  saveRDS(osm.sf, dsn)
  return(invisible(osm.sf))
}
osm_relations_routemaster_bus_read <- function(fic='relations_routemaster_bus') {
  library(sf)
  library(tidyverse)
  dsn <- sprintf("%s/%s.Rds", transportDir, fic)
  osm.sf <- readRDS(dsn)
  carp('dsn: %s', dsn)
  return(invisible(osm.sf$osm_multilines))
}
#
## les arrêts
#
osm_nodes_stop_get <- function(fic = 'nodes_stop') {
  requete <- "relation[network~'%s'][type=route][route=bus]->.a;node(r.a);out meta;"
  requete <- '
relation[type=route][route=bus][network="%s"]->.a;
(
  node[highway=bus_stop](r.a);
  node[public_transport](r.a);
);
out meta;'
  requete <- sprintf(requete, Config[1, 'network'])
  fic <- sprintf("%s.osm", fic)
  oapi_requete_get(requete, fic)
}
# source("geo/scripts/transport.R");osm_arrets_csv(force = TRUE)
osm_arrets_csv <- function(fic = 'osm_arrets', force = FALSE) {
  requete <- sprintf('[out:csv(::type,::id,::version,::timestamp,::user,::lat,::lon,name,highway,public_transport,"%s";true;"|")];
node[highway=bus_stop]["%s"];
out meta;', Config[1, 'k_ref'], Config[1, 'k_ref'])
  dsn <- overpass_query_csv(requete, fic, force = force)
  carp("dsn: %s", dsn)
  return(invisible(dsn))
}
# lecture avec sf
# https://github.com/r-spatial/sf/blob/master/tests/read.Rout.save
osm_nodes_stop_save <- function(fic = 'nodes_stop') {
  library(sf)
  library(tidyverse)
  dsn <- sprintf("%s/%s.osm", osmDir, fic)
  nc <- st_read(dsn, layer = "points") %>%
    glimpse()
  osm.sf <- oapi_osmdata_lire_sf(dsn) %>%
    glimpse()
  dsn <- sprintf("%s/%s.Rds", transportDir, fic)
  saveRDS(osm.sf, dsn)
  return(invisible(osm.sf))
}
osm_nodes_stop_read <- function(fic = 'nodes_stop') {
  library(sf)
  library(tidyverse)
  dsn <- sprintf("%s/%s.Rds", transportDir, fic)
  osm.sf <- readRDS(dsn)
  carp('dsn: %s', dsn)
  return(invisible(osm.sf$osm_points))
}
#
## les interrogations avec réponse en csv
#
osm_bretagne_relations_bus_csv <- function(fic = 'bretagne_relations_bus_csv', force = FALSE) {
  requete <- '// les relations route/route_master
[out:csv(::id, ::type, network, name; true; "|")];
area[admin_level=4][name="Bretagne"]->.a;
(
relation(area.a)[type=route][route=bus];
relation(area.a)[type=route_master][route_master=bus];
);
out body;'
  overpass_query_csv(requete, fic, force = force)
}
osm_relations_routemaster_bus_csv <- function(fic = 'relations_routemaster_bus', force = FALSE) {
  requete <- sprintf('[out:csv(::type,::id,network,name,ref,"ref:network";true;"|")];
relation[route_master=bus][network="%s"];
out;', Config[1, 'network'])
  overpass_query_csv(requete, fic, force = force)
}
osm_relations_route_bus_csv <- function(fic = 'relations_route_bus', force = FALSE) {
  requete <- sprintf('[out:csv(::type,::id,::version,::timestamp,::user,network,name,ref,"ref:network","gtfs:shape_id",from,to;true;"|")];
area[name="Bretagne"]->.a;
relation(area.a)[type=route][route=bus][network="%s"];
out meta;', Config[1, 'network'])
  dsn <- overpass_query_csv(requete, fic, force = force)
  carp("dsn: %s", dsn)
  return(invisible(dsn))
}
osm_relations_routes_bus_csv <- function(fic = 'relations_routes_bus', force = FALSE) {
  requete <- sprintf('[out:csv(::type,::id,::version,::timestamp,::user,network,ref,"ref:network",type,colour,text_colour,operator;true;"|")];
area[name="Bretagne"]->.a;
(
relation(area.a)[type=route][route=bus][network="%s"];
relation["type"="route_master"]["route_master"="bus"][network="%s"];
);
out meta;', Config[1, 'network'], Config[1, 'network'])
  dsn <- overpass_query_csv(requete, fic, force = force)
  carp("dsn: %s", dsn)
  return(invisible(dsn))
}
# source("geo/scripts/transport.R");config_xls(Reseau);osm_nodes_bus_csv()
osm_nodes_bus_csv <- function(fic = 'nodes_bus', force = FALSE) {
  requete <- sprintf('[out:csv(::type,::id,::version,::timestamp,::user,name;true;"|")];
relation[type=route][route=bus][network="%s"]->.a;
(
  node[highway=bus_stop](r.a);
  node[public_transport](r.a);
);
out meta;', Config[1, 'network'])
  dsn <- overpass_query_csv(requete, fic, force = force)
  carp("dsn: %s", dsn)
  return(invisible(dsn))
}
#
## pour mettre en minuscules le nom des arrêts
# source("geo/scripts/transport.R");config_xls(Reseau);osm_nodes_bus_topo()
osm_nodes_bus_topo_v1 <- function(fic = 'nodes_bus', force = FALSE) {
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


osm_nodes_bus_topo <- function(fic = 'nodes_bus', force = FALSE) {
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
osm_nodes_bus_mga <- function(fic = 'nodes_bus', force = FALSE) {
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
osm_topo <- function(df1) {
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
#
## *************************************************
# pour avoir la liste des relations route
# source("geo/scripts/transport.R");osm_relations_route_bus_verif("concarneau")
osm_relations_route_bus_verif <- function(reseau = "star", force = FALSE) {
  library(tidyverse)
  library(data.table)
  library(janitor)
  carp()
  config_xls(reseau)
  dsn <- osm_relations_route_bus_csv(force = force)
  df <- fread(dsn, encoding = "UTF-8") %>%
    clean_names() %>%
    glimpse()
  df10 <- df %>%
    dplyr::select(id, gtfs_shape_id, from, to, name) %>%
    arrange(gtfs_shape_id)
  tex_df2kable(df10, num = TRUE)
  df11 <- df %>%
    filter(!grepl("^\\w", gtfs_shape_id)) %>%
    dplyr::select(id, ref, from, to, name) %>%
    arrange(ref)
  tex_df2kable(df11, num = TRUE, suffixe = "gsi")
  df1 <- df %>%
    group_by(ref) %>%
    summarize(nb = n()) %>%
    filter(nb != 2) %>%
    glimpse()
  df2 <- df %>%
    filter(ref %in% df1$ref) %>%
    arrange(ref, ref_network)
  misc_print(df2)
  df3 <- df %>%
    filter(ref %in% df1$ref) %>%
    group_by(ref_network) %>%
    summarize(josm = paste0(id, collapse = ","))
  tex_df2kable(df3, num = TRUE, suffixe = "ar")
  df5 <- df %>%
    group_by(gtfs_shape_id) %>%
    summarize(nb = n()) %>%
    filter(nb > 1) %>%
    glimpse()
  df6 <- df %>%
    filter(gtfs_shape_id %in% df5$gtfs_shape_id) %>%
    dplyr::select(ref_network, id, name, from, to, ref, gtfs_shape_id) %>%
    arrange(gtfs_shape_id) %>%
    glimpse()
  tex_df2kable(df6, suffixe = "doublon", longtable = FALSE)
  return(invisible())
}
#
# pour avoir la liste des relations route disused
# source("geo/scripts/transport.R");config_xls('star');osm_relations_route_bus_disused()
osm_relations_route_bus_disused <- function(fic = 'osm_relations_route_bus_disused', force = FALSE) {
  library(tidyverse)
  library(stringr)
  library(janitor)
  carp()
  requete <- '
relation[type=route][network~"STAR"][~"disused:route"~"^bus$"];
out meta;'
  dsn <- sprintf("%s/%s.osm", osmDir, fic);
  osm_oapi(requete, dsn, force)
  osm <- read_file(dsn)
  osm <- str_replace_all(osm, "\\<U\\+\\d{4}\\>", "?")
  doc <- xml2::read_xml(osm)
  relations <- xml2::xml_find_all(doc, "//relation")
  carp("relations nb: %s", length(relations))
  relations.df <- relations %>%
    map(xml_attrs) %>%
    map_df(~as.list(.)) %>%
    glimpse()
  df <- tibble()
  for (i in 1:length(relations)) {
    relation <- relations[[i]]
    members_way <-  xml2::xml_find_all(relation, './/member[@type="way"]')
    members_node <-  xml2::xml_find_all(relation, './/member[@type="node"]')
    carp("ways: %s nodes: %s", length(members_way), length(members_node))
    tags <- xml2::xml_find_all(relation, "tag")
    tags.df <- tags %>%
      map(xml_attrs) %>%
      map_df(~as.list(.)) %>%
      spread(k, v)
    df1 <- cbind(relations.df[i, ], tags.df) %>%
      mutate(nb_ways = length(members_way)) %>%
      mutate(nb_nodes = length(members_node))
    df <- bind_rows(df, df1)
  }
  df <- df %>%
    clean_names() %>%
    glimpse()
  dsn <- sprintf("%s/%s.Rds", transportDir, fic)
  saveRDS(df, dsn)
}
#
#
# source("geo/scripts/transport.R");config_xls('star');osm_relations_route_bus_disused_tex()
osm_relations_route_bus_disused_tex <- function(fic = 'osm_relations_route_bus_disused', force = FALSE) {
  library(tidyverse)
  library(stringr)
  library(janitor)
  carp()
  dsn <- sprintf("%s/%s.Rds", transportDir, fic)
  df <- readRDS(dsn)
  texFic <- sprintf("%s/%s", imagesDir, "osm_relations_route_bus_disused.tex")
  TEX <- file(texFic)
  tex <- "% <!-- coding: utf-8 -->"
#
# le template tex
  dsn <- sprintf("%s/osm_relations_route_bus_disused_tpl.tex", tplDir)
  template <- readLines(dsn) %>%
    glimpse()
  for (i in 1:nrow(df)) {
    carp("i: %s/%s", i, nrow(df))
    tpl <- tex_df2tpl(df, i, template)
    tpl <- escapeLatexSpecials(tpl)
    tex <- append(tex, tpl)
  }
  write(tex, file = TEX, append = FALSE)
  close(TEX)
  carp(" texFic: %s", texFic)
}
# source("geo/scripts/transport.R");osm_relations_route_members(reseau = Reseau, force = FALSE, force_osm = FALSE, osrm = FALSE)
osm_relations_route_members <- function(reseau = "star", force = FALSE, force_osm = FALSE, osrm = TRUE) {
  library(tidyverse)
  library(data.table)
  library(sf)
  library(janitor)
  carp()
  config_xls(reseau)
  dsn <- osm_relations_route_bus_csv(force = force_osm)
  df <- fread(dsn, encoding = "UTF-8") %>%
    clean_names() %>%
    mutate(reseau = !!reseau) %>%
    arrange(ref_network)
  texFic <- sprintf("%s/%s", imagesDir, "osm_relations_route_members.tex")
  TEX <- file(texFic)
  tex <- "% <!-- coding: utf-8 -->"
#
# le template tex
  dsn <- sprintf("%s/relations_route_members_tpl.tex", tplDir)
  template <- readLines(dsn) %>%
    glimpse()
  if (reseau == "star") {
    star.df <- star202210_supprime()
    df <- df %>%
      filter(ref %notin% star.df$ligne) %>%
#      filter(grepl("^0006", gtfs_shape_id)) %>%
      arrange(gtfs_shape_id) %>%
      glimpse()
  }
  if (reseau == "kiceo") {
    df <- df %>%
#      filter(ref_network %in% c("4-B")) %>%
#      filter(grepl("^D3", ref_network)) %>%
      arrange(gtfs_shape_id) %>%
      glimpse()
  }
#  df <- df %>%
#    filter(grepl("^P8", ref_network))
  members_loins.df <- tibble()
  members_kref.df <- tibble()
  for (i in 1:nrow(df)) {
    carp("i: %s/%s", i, nrow(df))
    id <-  df[[i, "id"]]
    shape <- df[[i, "gtfs_shape_id"]]
    ref_network <- df[[i, "ref_network"]]
#    glimpse(df[i, ]); stop("****");
    osm.sf <- osmapi_get_ways(ref = id, force = force, force_osm = force_osm) %>%
      st_transform(2154)
    if (st_is_empty(osm.sf[1,]) ) {
      carp("***id: %s", id)
      next;
    }
    if (osrm == TRUE) {
      osrm_relation_get(id, reseau = reseau, force = force_osm)
    }
    rc <- osm_relation_route_members(id = df[i, "id"], force = force, force_osm = force_osm)
    rc <- osm_relation_route_members_valid(rc = rc, id = df[i, "id"], force = force, force_osm = force_osm)
#    osm_relation_route_stops_order(rc = rc, id = df[i, "id"], force = force, force_osm = force_osm)
    platforms.sf <- rc$platforms.sf
    df1 <- st_distance(platforms.sf, osm.sf)
    platforms.sf <- cbind(platforms.sf, df1) %>%
      mutate(distance = as.integer(df1)) %>%
      dplyr::select(-df1)
    loins.sf <- platforms.sf %>%
      filter(distance > 100)
    if (nrow(loins.sf) > 0) {
#      plot(st_geometry(loins.sf), col = "red", lwd = 4, pch = 19, add = TRUE)
      members_loins.df <- bind_rows(members_loins.df, st_drop_geometry(loins.sf))
#      stop("****")
    }
    if (1 == 2) {
      kref.sf <- platforms.sf %>%
        filter(is.na(k_ref))
      if (nrow(kref.sf) > 0) {
        plot(st_geometry(kref.sf), col = "black", lwd = 2, pch = 19, add = TRUE)
        members_kref.df <- rbind(members_kref.df, st_drop_geometry(kref.sf))
#      stop("****")
      }
    }
    osm_relation_route_members_mapsf(rc)
    dsn <- dev2pdf(suffixe = id, dossier = "images")
# pour le latex
#    tex <- append(tex, sprintf("\\mongraphique{images/reseau_routes_shapes_%s.pdf}", id))
    glimpse(rc)
    df[i, "shape"] <- shape
    df[i, "platforms"] <- rc$platforms
    df[i, "stops"] <- rc$stops
    tpl <- tex_df2tpl(df, i, template)
    tpl <- escapeLatexSpecials(tpl)
    tex <- append(tex, tpl)
#    break
  }
  write(tex, file = TEX, append = FALSE)
  close(TEX)
  carp("texFic: %s", texFic)
  tex_df2kable(members_loins.df, suffixe = "loin", longtable = TRUE)
  tex_df2kable(members_kref.df, suffixe = "kref", longtable = TRUE)
  return(invisible())
}
# Quimper 6-A
# Vannes 4754448 6b
# source("geo/scripts/transport.R");osm_relation_route_members(id = "4754448", force = TRUE)
osm_relation_route_members <- function(id, force = FALSE, force_osm = FALSE) {
  library(tidyverse)
  library(data.table)
  library(sf)
  library(janitor)
  carp("id: %s", id)
  dsn_rds <- sprintf("%s/osm_relation_route_members_%s.rds", osmDir, id)
  if (file.exists(dsn_rds) && force == FALSE) {
    rc <- readRDS(dsn_rds)
    return(invisible(rc))
  }
  rc <- FALSE
  members_loins.df <- tibble()
  members_kref.df <- tibble()
  rc <- osmapi_get_transport(ref = id, force = force, force_osm = force_osm) %>%
    glimpse()
  ways.sf <- rc$ways.sf
  if (st_is_empty(ways.sf[1,]) ) {
    carp("***id: %s", id)
    return(invisible(rc))
  }
  rc[["ways_points.sf"]] <- st_sf(st_cast(st_geometry(ways.sf), "POINT"))
  if (rc$relation[1, "route"]  != "bus") {
    saveRDS(rc, dsn_rds)
    return(invisible(rc))
  }
# pour remettre dans l'ordre de la relation route les ways
#  osm_relation_route_members_ways(rc)
  ref_network <- rc$relation[1, "ref:network"]
  platforms.df <- rc$members.df %>%
    filter(grepl("platform", role)) %>%
    mutate(Id = row_number()) %>%
    mutate(Name = sprintf("%s-%s", Id, name)) %>%
    mutate(lat = as.numeric(lat)) %>%
    mutate(lon = as.numeric(lon))
  platforms <- paste0(platforms.df$Name, collapse = ", ")
  platforms.sf <- st_as_sf(platforms.df, coords = c("lon", "lat"), crs = 4326, remove = TRUE) %>%
    st_transform(2154) %>%
    glimpse()
  nodes.df <- rc$members.df %>%
    filter(grepl("stop", role)) %>%
    mutate(Id = row_number()) %>%
    mutate(Name = sprintf("%s-%s", Id, name)) %>%
    mutate(lat = as.numeric(lat)) %>%
    mutate(lon = as.numeric(lon))
  stops.sf <- st_as_sf(nodes.df, coords = c("lon", "lat"), crs = 4326, remove = TRUE) %>%
    st_transform(2154) %>%
    glimpse()
  stops <- paste0(nodes.df$Name, collapse = ", ")
  rc[["platforms"]] <- platforms
  rc[["stops"]] <- stops
  rc[["platforms.sf"]] <- platforms.sf
  rc[["stops.sf"]] <- stops.sf
# pour récupérer les infos de l'objet
  nc3 <- tibble()
  if (nrow(stops.sf) > 0) {
    nc3 <- osm_relation_route_members_nodes(rc, role = "stop")
  }
  if (nrow(platforms.sf) > 0) {
    d <- as.integer(st_distance(platforms.sf[1, ], rc$ways_points.sf[1, ]))
    rc[["distance_depart"]] <- d
    nc2 <- osm_relation_route_members_nodes(rc, role = "platform")
    if (nrow(nc3) > 0) {
      nc3 <- bind_rows(nc3, nc2)
    } else {
      nc3 <- nc2
    }
  }
  rc[["members.sf"]] <- nc3
  saveRDS(rc, dsn_rds)
  return(invisible(rc))
}
#
# la carte de la ligne
# https://rgeomatic.hypotheses.org/2077
# https://riatelab.github.io/mapsf/
osm_relation_route_members_mapsf <- function(rc) {
  library(tidyverse)
  library(sf)
  library(mapsf)
  carp()
  mf_init(rc$ways.sf, expandBB = c(0.1, 0.1, 0.1, 0.1))
  mf_map(x = rc$ways.sf, col = "black", lwd = 3, add = TRUE)
#  mf_map(x = rc$platforms.sf, col = "blue", lwd = 3, pch = 19, add = TRUE)
  mf_map(x = rc$members.sf, col = rc$members.sf$couleur, lwd = 3, pch = 19, add = TRUE)
  mf_label(x = rc$members.sf,
    var = "Id",
    cex = 1.5,
    overlap = FALSE,
    lines = TRUE,
    col = rc$members.sf$couleur
  )
  if (! "ref:network" %in% names(rc$relation)) {
    rc$relation[1, "ref:network" ] <- NA
  }
  titre <- sprintf("relation: %s ref_network: %s", rc$relation[1, "id"], rc$relation[1, "ref:network"])
  mf_title(titre)
  mf_annotation(rc$ways_points.sf[1, ], txt = "Départ")
}

#
osm_relation_route_members_nodes <- function(rc, role = "stop") {
  carp()
  if ( role == "stop") {
    nodes.sf <- rc$stops.sf
    nodes.sf$couleur <- "green"
  }
  if ( role == "platform") {
    nodes.sf <- rc$platforms.sf
    nodes.sf$couleur <- "blue"
  }
  nodes.df <- st_drop_geometry(nodes.sf)
  ways.sf <- rc$ways.sf %>%
    glimpse()
  ways.df <- st_drop_geometry(ways.sf)
  nodes <- paste0(nodes.sf$Name, collapse = ", ")
  for (i in 1:nrow(nodes.sf)) {
    d <- as.integer(st_distance(nodes.sf[i, ], ways.sf))
    df1 <- data.frame(d = d, way_id = ways.df$id, way_no = ways.df$way_no) %>%
      mutate(way = row_number()) %>%
      arrange(d)
    nodes.df[i, "way"] <- df1[1, "way"]
    nodes.df[i, "distance"] <- df1[1, "d"]
    nodes.df[i, "way_id"] <- df1[1, "way_id"]
    nodes.df[i, "way_no"] <- df1[1, "way_no"]
  }
  nodes.df <- nodes.df %>%
    arrange(way) %>%
    mutate(ordre = row_number()) %>%
    dplyr::select(type, ref, name, Id, ordre, public_transport, distance, way_id, way_no) %>%
    glimpse()
  misc_print(nodes.df)
  return(invisible(nodes.sf))
}
# liste des informations des ways de la relation
# osmapi_get_transport ligne 290
osm_relation_route_members_ways <- function(rc) {
  carp()
  ways.sf <- rc$ways.sf %>%
    glimpse()
  ways.df <- st_drop_geometry(ways.sf)
  df1 <- ways.df %>%
    mutate(tags = highway) %>%
    mutate(tags = ifelse(is.na(junction), tags, sprintf("%s,%s", tags, junction))) %>%
    mutate(tags = ifelse(is.na(oneway), tags, sprintf("%s,%s", tags, oneway))) %>%
    select(id, tags, node1, node9, name)
  misc_print(df1)
  return(invisible(rc))
}
# source("geo/scripts/transport.R");osm_relations_route_members_tex()
osm_relations_route_members_tex <- function(reseau = "star", force = FALSE, force_osm = FALSE) {
  carp()
  config_xls(reseau)
  osm_relations_route_members(reseau = reseau, force = force, force_osm = force_osm)
  tex_pdflatex("star_relations_route_members.tex")
}
#
## les arrêts qui ne sont plus en service
# source("geo/scripts/transport.R");osm_arrets_disused()
osm_arrets_disused <- function(force = FALSE, force_osm = FALSE) {
  carp()
  df <-  transport_read("reseau_osm_stops_gtfs_stops_osm_inc") %>%
    glimpse()
  df1 <- tibble()
  df2 <- tibble()
  for (i in 1:nrow(df)) {
    dsn <- osmapi_get_object_relations(ref = df[[i, "osm_id"]], type = df[[i, "osm_type"]], force = force_osm)
    objects.df <- osmapi_objects_get_tags(dsn)
    if (length(objects.df) == 0) {
      next
    }
    if ("route" %in% names(objects.df)) {
      glimpse(df[i, ])
      glimpse(objects.df)
      df1 <- rbind(df1, df[i, ])
    } else {
      df2 <- rbind(df2, df[i, ])
    }
  }
  return(invisible(list(autre = df1, hs = df2)))
}
#
## les platforms qui n'ont pas de tag ref
# source("geo/scripts/transport.R")osm_platforms_untag()
osm_platforms_untag <- function(force = FALSE, force_osm = FALSE) {
  carp()
  fic <- "overpass_relations_route_node"
  dsn <- sprintf("%s/%s.Rds", transportDir, fic)
    if (! file.exists(dsn) || force == TRUE) {
    dsn_osm <- overpass_relations_route_node(network = Reseau, force  = force_osm)
    df <- osmapi_objects_get_tags(dsn_osm)
    saveRDS(df, dsn)
  } else {
    df <- readRDS(dsn)
  }
  df1 <- df %>%
    glimpse() %>%
    filter(osm_type == "node") %>%
    group_by(public_transport) %>%
    summarize(nb = n()) %>%
    glimpse()
  df2 <- df %>%
    filter(osm_type == "node") %>%
    filter(public_transport == "platform") %>%
    filter(is.na(`ref:KICEO`)) %>%
    dplyr::select(id, name) %>%
    glimpse()
  misc_print(df2)
  return(invisible(df))
}
# source("geo/scripts/transport.R");osm_relations_route_members_valid(force = FALSE, force_osm = FALSE)
osm_relations_route_members_valid <- function(force = TRUE, force_osm = TRUE) {
  library(tidyverse)
  library(data.table)
  library(sf)
  library(janitor)
  carp()

  dsn <- osm_relations_route_bus_csv(force = force_osm)
  df <- fread(dsn, encoding = "UTF-8") %>%
    as.data.table() %>%
    clean_names() %>%
    glimpse()
  for (i in 1:nrow(df)) {
    carp("i: %s/%s", i, nrow(df))
    rc <- osm_relation_route_members(id = df[i, "id"], force = force, force_osm = force_osm)
    rc <- osm_relation_route_members_valid(rc = rc, id = df[i, "id"], force = force, force_osm = force_osm)
    osm_relation_route_stops_order(rc = rc, id = df[i, "id"], force = force, force_osm = force_osm)
    osm_relation_route_members_mapsf(rc)
#    stop("****")
  }
}

# source("geo/scripts/transport.R"); osm_relation_route_members_valid(id = 4756509, force = TRUE, force_osm = TRUE)
osm_relation_route_members_valid <- function(rc, id = 3184038, force = FALSE, force_osm = FALSE) {
  carp()
  df <- tibble()
  df1 <- rc$members.df
  carp("bus=yes")
  df2 <- df1 %>%
    filter(bus != "yes")
  if (nrow(df2) > 0) {
    df2$err <- "bus=yes"
    df <- rbind(df, df2)
  }
  roles <- c("stop", "platform", "stop_entry_only", "stop_exit_only", "platform_entry_only", "platform_exit_only")
  df2 <- df1 %>%
    filter(role %notin% roles)
  if (nrow(df2) > 0) {
    df2$err <- "role"
    df <- rbind(df, df2)
  }
  pts <- c("stop_position", "platform")
  df2 <- df1 %>%
    filter(public_transport %notin% pts)
  if (nrow(df2) > 0) {
    df2$err <- "public_transport"
    df <- rbind(df, df2)
  }
  if (nrow(df) > 0) {
    df$relation <- rc$relation[1, "id"]
    misc_print(df)
    stop("*****")
  }
  df2 <- df1 %>%
    mutate(type = dplyr::recode(type,
      "way" = "wy",
      "node" = "nd"
    )) %>%
    mutate(member = sprintf("  %s %s %s", type, ref, role))
  rc$members_level0 <- paste(df2$member,  collapse = "\n")
  return(invisible(rc))
}
#
## pour remettre dans l'ordre les stops
# source("geo/scripts/transport.R"); osm_relation_route_stops_order(force = TRUE)
osm_relation_route_stops_order <- function(rc, id = 3184038, force = FALSE, force_osm = FALSE) {
  carp()
  if (nrow(rc$stops.sf) == 0) {
    carp("id: %s pas de stops", id)
    return(invisible(rc))
  }
  carp("le rapprochement entre les platforms et les stops")
  nc1 <- rc$platforms.sf %>%
    mutate(type = dplyr::recode(type,
      "way" = "wy",
      "node" = "nd"
    )) %>%
    dplyr::select(type, ref, role, Id)
  nc2 <- rc$stops.sf %>%
    mutate(type = dplyr::recode(type,
      "way" = "wy",
      "node" = "nd"
    )) %>%
    dplyr::select(type, ref, role, Id)
  nc1$proche <- st_nearest_feature(nc1, nc2)
  df <- dplyr::left_join(
    nc1 %>% as.data.frame()
    , nc2 %>% as.data.frame()
    , by = c('proche' = 'Id')
    , suffix = c(".platform", ".stop")
  ) %>%
    glimpse()
#  stop('***')
  df$d <- st_distance(df$geometry.platform, df$geometry.stop, by_element = TRUE)
  df <- df %>%
    mutate(d = as.integer(d)) %>%
    glimpse()
#
  carp("les stops trop loin de platforms")
  df1 <- df %>%
    filter(d > 50) %>%
    glimpse()
  carp("les stops non rattachés")
  df2 <- rc$stops.sf %>%
    st_drop_geometry() %>%
    filter(Id %notin% df$proche) %>%
    glimpse()
  level0 <- list()
  df <- df %>%
    mutate(platform = sprintf("  %s %s %s", type.platform, ref.platform, role.platform)) %>%
    mutate(stop = sprintf("  %s %s %s", type.stop, ref.stop, role.stop))

  for (i in 1:nrow(df)) {
    level0 <- append(level0, df[i, "platform"])
    if (df[i, "d"] < 50) {
      level0 <- append(level0, df[i, "stop"])
    }
  }
  level0 <- paste(level0,  collapse = "\n")
  if (level0 != rc$members_level0) {
    n <- str_split(rc$members_level0, "\n")
    s <- str_split(level0, "\n")
    sq <- seq(max(length(n), length(s)))
#    df5 <- data.frame(n[sq], s[sq])
    dsn <- sprintf("%s/osm_relation_route_members_%s_level0.txt", osmDir, id)
    write(level0, dsn)
    carp("dsn: %s", dsn)
  }
}
#
# cohérence route route_master
# même operator, colour, text_colour
# source("geo/scripts/transport.R");osm_relations_routes_bus(force = FALSE, force_osm = FALSE)
osm_relations_routes_bus <- function(force = TRUE, force_osm = TRUE) {
  library(tidyverse)
  library(data.table)
  library(sf)
  library(janitor)
  carp()
  dsn <- osm_relations_routes_bus_csv(force = force_osm)
  df <- fread(dsn, encoding = "UTF-8") %>%
    as.data.table() %>%
    clean_names() %>%
    glimpse()
  df1 <- df %>%
    group_by(operator) %>%
    summarize(nb = n())
  if (nrow(df1) != 1) {
    glimpse(df1)
#    stop("*****")
  }
  carp("différences colour, text_colour")
  df2 <- df %>%
    dplyr::select(id, ref, colour, text_colour)
  df5 <- df2 %>%
    filter(text_colour == "") %>%
    glimpse()
  df3 <- df2 %>%
    group_by(ref, colour, text_colour) %>%
    summarize(nb = n()) %>%
    group_by(ref) %>%
    summarize(nb = n()) %>%
    filter(nb > 1) %>%
    glimpse()
  df4 <- df2 %>%
    filter(ref %in% df3$ref)
  misc_print(df4)

}
#
## interrogation avec level0
#
# source("geo/scripts/transport.R");osm_relations_level0(force = FALSE, force_osm = FALSE)
# cd /d/web.var/transport/kiceo/osm
# cat osm_relation_level0_*.txt
osm_relations_level0 <- function(force = TRUE, force_osm = TRUE) {
  library(tidyverse)
  library(data.table)
  library(sf)
  library(janitor)
  carp()
  dsn <- osm_relations_route_bus_csv(force = force_osm)
  df <- fread(dsn, encoding = "UTF-8") %>%
    as.data.table() %>%
    clean_names() %>%
    glimpse()
  for (i in 1:nrow(df)) {
    carp("i: %s/%s", i, nrow(df))
    level0 <- level0_get(type = "relation", id = df[i, "id"])
    df1 <- data.frame(ligne = unlist(str_split(level0, "\\n"))) %>%
      mutate(no = 1:n()) %>%
      filter(grepl(" stop", ligne)) %>%
      glimpse()
    if (nrow(df1) > 1) {
      for (j in 2:nrow(df1)) {
        if (df1[j-1, "no"] +1 == df1[j, "no"]) {
          stop("****")
        }
      }
    }
    dsn <- sprintf("%s/osm_relation_level0_%s.txt", osmDir, df[i, "id"])
    write(level0, dsn)
    carp("dsn: %s", dsn)
  }
}
#
## les gaps dans une relation
#
# source("geo/scripts/transport.R");osm_relation_route_gap(id = 4260060, force = FALSE, force_osm = FALSE)
# source("geo/scripts/transport.R");osm_relation_route_gap(id = 14194690, force = FALSE, force_osm = FALSE)
# oneway:bus = no
osm_relation_route_gap <- function(id = 4260060, force = TRUE, force_osm = TRUE) {
  library(tidyverse)
  rc <- osm_relation_route_members(id = id, force = force, force_osm = force_osm)
  if ("note:mga_geo" %in% names(rc$relation)) {
    carp("note:mga_geo id: %s", id)
    return()
  }
  carp("les ways")
  ways.df <- rc$ways.sf %>%
    st_drop_geometry()
  df1 <- ways.df %>%
    mutate(no = 1:n()) %>%
    mutate(tags = highway) %>%
    mutate(tags = ifelse(is.na(junction), tags, sprintf("%s,%s", tags, junction))) %>%
    mutate(tags = ifelse(is.na(oneway), tags, sprintf("%s,%s", tags, oneway))) %>%
    select(no, id, tags, node1, node9, name)
  misc_print(df1)
  if (nrow(ways.df) < 2) {
    confess("pas assez de voies")
  }
#  glimpse(ways.df); stop("fghjklm")
  ways.df <- ways.df %>%
    mutate(no = 1:n()) %>%
    rowwise() %>%
    mutate(wRP = grepl("roundabout|circular", junction)) %>%
    mutate(wRP = ifelse(node1 == node9, TRUE, FALSE)) %>%
    mutate(wOW = grepl("yes", oneway)) %>%
#    mutate(wOW = ifelse(oneway_bus == "no", FALSE, TRUE)) %>%
    mutate(name = ifelse(is.na(name), ref_y, name)) %>%
    mutate(name = sprintf("%s(%s)", name, nb_nodes)) %>%
    dplyr::select(no, id, node1, node9, wRP, wOW, name, nodes) %>%
#    filter(! is.na(junction)) %>%
    glimpse()
  nAvant <- "-1"
  carp("****id: %s", id)
  for (i in 2:nrow(ways.df)) {
#    carp("***i: %s", i)
#    misc_print(ways.df[c(i -1, i), ])
# c'est un rond-point ?
    if (ways.df[i , "wRP"] == TRUE) {
# précédé d'un rond-point ?
      if (ways.df[i - 1 , "wRP"] == TRUE) {
        confess("deux ronds-points i: %s", i)
      }
      next
    }
# le début du chemin
    if (i == 2) {
      if (ways.df[i - 1 , "node1"] == ways.df[i , "node1"] ) {
        if (ways.df[i - 1, "wOW"]  == TRUE) {
          confess("sens unique i-1: %s", i)
        }
        nAvant <- ways.df[i , "node9"]
        next;
      }
      if (ways.df[i - 1 , "node1"] == ways.df[i , "node9"] ) {
        if (ways.df[i, "wOW"]  == TRUE) {
          misc_print(ways.df[c(i -1, i), ])
          confess("sens unique i: %s", i)
        }
        nAvant <- ways.df[i , "node1"]
        next;
      }
      if (ways.df[i - 1 , "node9"] == ways.df[i , "node1"]) {
        nAvant <- ways.df[i , "node9"]
        next;
      }
      if (ways.df[i - 1 , "node9"] == ways.df[i , "node9"]) {
       if (ways.df[i, "wOW"]  == TRUE) {
          misc_print(ways.df[c(i -1, i), ])
          confess("sens unique i: %s", i)
        }
        nAvant <- ways.df[i , "node1"]
        next;
      }
      misc_print(ways.df[c(i -1, i), ])
      confess("gap i: %s", i)
    }
# la suite
    if (i > 2) {
      if (nAvant == ways.df[i , "node1"]) {
        nAvant <- ways.df[i , "node9"]
        next;
      }
      if (nAvant == ways.df[i , "node9"]) {
        if (ways.df[i, "wOW"]  == TRUE) {
          misc_print(ways.df[c(i -1, i), ])
          confess("sens unique i: %s", i)
        }
        nAvant <- ways.df[i , "node1"]
        next;
      }
# précédé d'un rond-point ?
      if (ways.df[i - 1 , "wRP"] == TRUE) {
        nodes <- unlist(ways.df[i - 1 , "nodes"])
        if (ways.df[i , "node1"] %in% nodes) {
          nAvant <- ways.df[i , "node9"]
          next;
        }
        if (ways.df[i , "node9"] %in% nodes) {
          nAvant <- ways.df[i , "node1"]
          next;
        }
        glimpse(nodes)
        confess("un rond-point avant i: %s", i)
      }
      misc_print(ways.df[c(i -1, i), ])
      confess("gap i: %s %s", i, nAvant)
    }
  }
  carp("fin id: %s", id)
}