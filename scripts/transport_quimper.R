# <!-- coding: utf-8 -->
#
# le réseau de bus de Quimper
#
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
#
# passage en minuscule dans le gtfs
# source("geo/scripts/transport.R");quimper_stops()
quimper_stops <- function(stops.df, force = FALSE) {
  library(stringr)
  library(stringi)
  carp("le gtfs")
  stops.df <- stops.df %>%
#    distinct(stop_id, stop_name) %>%
    rename(STOP_NAME = stop_name) %>%
    arrange(STOP_NAME)
  carp("les arrêts du réseau dans osm")
  osm.df <- overpass_get(query = "bus_stop_kref", format = "csv", force = force) %>%
    glimpse() %>%
    rename(k_ref = `ref:QUB`) %>%
    distinct(k_ref, name) %>%
    mutate(NAME = str_to_upper(name)) %>%
    mutate(NAME = stri_trans_general(NAME, id = "Latin-ASCII")) %>%
    arrange(name)
  carp("la jointure")
  df1 <- stops.df %>%
    full_join(osm.df, by = c("stop_id" = "k_ref"))
# des arrêts gtfs non configurés dans osm
  df2 <- df1 %>%
    filter(is.na(name))
  if (nrow(df2) > 0) {
    glimpse(df2)
    confess("**** df2: %s", nrow(df2))
  }
  df3 <- df1 %>%
    filter(NAME != STOP_NAME)
  df1 <- df1 %>%
    rename(stop_name = name) %>%
    dplyr::select(-NAME)
#  misc_print(df1)
  return(invisible(df1))
}
#
## pour mettre en minuscules le nom des arrêts
# source("geo/scripts/transport.R");config_xls(Reseau);osm_nodes_bus_topo()
quimper_osm_nodes_bus_topo_v1 <- function(fic = 'nodes_bus', force = FALSE) {
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


quimper_osm_nodes_bus_topo <- function(fic = 'nodes_bus', force = FALSE) {
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
quimper_osm_nodes_bus_mga <- function(fic = 'nodes_bus', force = FALSE) {
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
quimper_osm_topo <- function(df1) {
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