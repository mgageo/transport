# <!-- coding: utf-8 -->
#
# quelques fonctions pour les réseaux de transport
#
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
Drive <- substr( getwd(),1,2)
baseDir <- sprintf("%s/web", Drive)
cfgDir <- sprintf("%s/web/geo/TRANSPORT", Drive)
imagesDir <- sprintf("%s/web/geo/TRANSPORT/images", Drive)
texDir <- sprintf("%s/web/geo/TRANSPORT", Drive)
tplDir <- sprintf("%s/web/geo/TRANSPORT", Drive)
varDir <- sprintf("%s/web.var/TRANSPORT", Drive)
odDir <- sprintf("%s/STAR", varDir)
transportDir <- sprintf("%s", varDir)
odDir <- sprintf("%s/MOBIBREIZH", varDir)
webDir <- sprintf("%s/web.heb/bv/transport", Drive)
osmDir <- sprintf("%s/OSM", varDir)
reseauDir <- sprintf("%s/SURF", varDir)
setwd(baseDir)
dir.create(cfgDir, showWarnings = FALSE, recursive = TRUE)
dir.create(varDir, showWarnings = FALSE, recursive = TRUE)
dir.create(osmDir, showWarnings = FALSE, recursive = TRUE)
dir.create(texDir, showWarnings = FALSE, recursive = TRUE)
dir.create(imagesDir, showWarnings = FALSE, recursive = TRUE)
DEBUG <- FALSE
source("geo/scripts/mga.R")
source("geo/scripts/misc.R")
source("geo/scripts/misc_docker.R")
source("geo/scripts/misc_datagouv.R")
source("geo/scripts/misc_geocode.R")
#source("geo/scripts/misc_gpx.R")
source("geo/scripts/misc_gtfs.R")
source("geo/scripts/misc_html.R")
source("geo/scripts/misc_ign.R")
source("geo/scripts/misc_level0.R")
source("geo/scripts/misc_md.R")
source("geo/scripts/misc_osm.R")
source("geo/scripts/misc_osmapi.R")
source("geo/scripts/misc_osmchange.R")
source("geo/scripts/misc_osmdata.R")
source("geo/scripts/misc_osmose.R")
source("geo/scripts/misc_osrm.R")
source("geo/scripts/misc_overpass.R")
source("geo/scripts/misc_ssh.R")
source("geo/scripts/misc_st.R"); # ajout de fonctions à sf dans st_proches
source("geo/scripts/misc_tex.R")
source("geo/scripts/misc_tidytransit.R")
source("geo/scripts/misc_transport.R")
source("geo/scripts/misc_unicode.R")
source("geo/scripts/transport_carto.R"); # production des cartes
source("geo/scripts/transport_config.R"); # lecture du fichier excel
source("geo/scripts/transport_diff.R"); # différence entre gtfs et osm
# source("geo/scripts/transport_gtfs.R")
source("geo/scripts/transport_gtfs2mga.R")
source("geo/scripts/transport_gtfs2osm.R"); # mise en format compatible perl
source("geo/scripts/transport_ign.R"); # pour la détermination des communes des arrêts
source("geo/scripts/transport_menu.R")
source("geo/scripts/transport_mapbox.R")
source("geo/scripts/transport_mapmatching.R")
source("geo/scripts/transport_misc.R")
source("geo/scripts/transport_mobibreizh.R"); # pour le gtfs de la région
source("geo/scripts/transport_oapi.R")
source("geo/scripts/transport_osm.R"); # les interrogations osm soit via l'api soit via l'overpass
source("geo/scripts/transport_osmar.R")
source("geo/scripts/transport_osmose.R")
source("geo/scripts/transport_osm2mga.R")
source("geo/scripts/transport_postgis.R"); # avec importation par osm2pgsql
source("geo/scripts/transport_reseau.R"); # les comparaisons osm gtfs
source("geo/scripts/transport_route.R")
source("geo/scripts/transport_routes.R"); # cohérence route route_master
source("geo/scripts/transport_tidytransit.R"); # lecture du fichier gtfs.zip
source("geo/scripts/transport_train.R");# les réseaux de train
source("geo/scripts/transport_txt.R")
source("geo/scripts/transport_valhalla.R")
source("geo/scripts/transport_wiki.R")
source("geo/scripts/transport_zone.R")
#
# les différents objets : node, way, relation
source("geo/scripts/transport_objets.R")

#
# les différents réseaux
source("geo/scripts/transport_axeo.R")
source("geo/scripts/transport_auray.R")
source("geo/scripts/transport_bibus.R"); # brest
source("geo/scripts/transport_bordeaux.R")
source("geo/scripts/transport_breizhgo.R")
source("geo/scripts/transport_bretagne.R")
source("geo/scripts/transport_concarneau.R")
source("geo/scripts/transport_ctrl.R")
source("geo/scripts/transport_distribus.R")
source("geo/scripts/transport_kiceo.R")
source("geo/scripts/transport_landerneau.R")
source("geo/scripts/transport_lila.R")
source("geo/scripts/transport_pennarbed.R")
source("geo/scripts/transport_pontivy.R")
#source("geo/scripts/transport_qub.R")
source("geo/scripts/transport_quimper.R")
source("geo/scripts/transport_ploermel.R"); # RIV
source("geo/scripts/transport_rmat.R")
source("geo/scripts/transport_star.R")
source("geo/scripts/transport_surf.R")
source("geo/scripts/transport_tbk.R")
source("geo/scripts/transport_tibus.R")
source("geo/scripts/transport_tilt.R")
source("geo/scripts/transport_tub.R")
source("geo/scripts/transport_tudbus.R")

Reseau <- "concarneau"
Reseau <- "kiceo"; # Vannes
Reseau <- "morlaix"; # LinéoTim
Reseau <- "breizhgo35"; # pseudo-réseau pour BreizhGo en Ille-et-Vilaine
Reseau <- "ter"; # pseudo-réseau pour BreizhGo SNCF
Reseau <- "landerneau"
Reseau <- "breizhgo56"; # Morbihan
Reseau <- "vitobus"; # Vitré/Chateaubourg
Reseau <- "gironde"; # les lignes régionales
Reseau <- "arcachon"; # Arcachon
Reseau <- "toulouse"; # Toulouse Métropole
Reseau <- "limoges"; # Limoges Métropole
Reseau <- "bordeaux"; # Bordeaux Métropole
Reseau <- "vannes"
Reseau <- "rmat"; # Saint-Malo
Reseau <- "surf"; # Fougères
Reseau <- "bibusM"; # Brest avec les données de MobiBreizh
Reseau <- "bibus"; # Brest
Reseau <- "ploermel"
Reseau <- "breizhgo22"; # pseudo-réseau pour BreizhGo en Côtes d'Armor
Reseau <- "morlaix"; #
Reseau <- "dinan"; # Dinan / Côtes d'Armor
Reseau <- "distribus"; # Lamballe / Côtes d'Armor
Reseau <- "breizhgo35"; # pseudo-réseau pour BreizhGo en Ille-et-Vilaine
Reseau <- "breizhgo29"; # pseudo-réseau pour BreizhGo en Finistère
Reseau <- "angers"; # Angers Irigo
Reseau <- "semo"; # SEMO (pour Seine-Eure MObilités)
Reseau <- "douarnenez"; # Douarnenez Tudbus
Reseau <- "lorient"; # Lorient CTRL
Reseau <- "breizhgo56"; # pseudo-réseau pour BreizhGo en Morbihan
Reseau <- "breizhgo"; # BreizhGo en Bretagne, les lignes régionales
Reseau <- "quimper"; # Quimper QUB
Reseau <- "bretagne"; # pseudo-réseau pour la Bretagne
Reseau <- "strasbourg"; # Strasbourg CTS
Reseau <- "lannion"; # Lannion TILT
Reseau <- "guingamp"; # Guingamp AXEOBUS
Reseau <- "saintbrieuc"; # Saint-Brieuc TUB
Reseau <- "rennes"; # Rennes STAR

config_xls(Reseau)
Tex <- TRUE
Wiki <- TRUE
HtmlBrowse <- FALSE
OsmChange <- FALSE
if ( interactive() ) {
  DEBUG <- TRUE
  graphics.off()
#  shapes()
} else {
#  cesson()
#  osm_fla()
#  shapes2kml()
#  shapes_partiel()
#  shapes2routes()
#  shapes2shape()
#  trips_stops()
#  transport_get()
}
