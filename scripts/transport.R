# <!-- coding: utf-8 -->
# lees réseaux de transport
# utilisation des données d'OpenStreetMap et des gtfs
# auteur : Marc Gauthier
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
source("geo/scripts/misc_couches.R")
source("geo/scripts/misc_datagouv.R")
source("geo/scripts/misc_gpx.R")
source("geo/scripts/misc_gtfs.R")
source("geo/scripts/misc_level0.R")
source("geo/scripts/misc_md.R")
source("geo/scripts/misc_osm.R")
source("geo/scripts/misc_osmapi.R")
source("geo/scripts/misc_osmdata.R")
source("geo/scripts/misc_osmose.R")
source("geo/scripts/misc_osrm.R")
source("geo/scripts/misc_overpass.R")
source("geo/scripts/misc_ssh.R")
source("geo/scripts/misc_tex.R")
source("geo/scripts/misc_tidytransit.R")
source("geo/scripts/misc_unicode.R")
source("geo/scripts/transport_config.R")
# source("geo/scripts/transport_gtfs.R")
source("geo/scripts/transport_gtfs2mga.R")
source("geo/scripts/transport_gtfs2osm.R")
source("geo/scripts/transport_misc.R")
source("geo/scripts/transport_mobibreizh.R"); # pour le gtfs de la région
source("geo/scripts/transport_oapi.R")
source("geo/scripts/transport_osm.R")
source("geo/scripts/transport_osmar.R")
source("geo/scripts/transport_osmose.R")
source("geo/scripts/transport_osm2mga.R")
source("geo/scripts/transport_reseau.R")
source("geo/scripts/transport_route.R")
source("geo/scripts/transport_tidytransit.R")
source("geo/scripts/transport_train.R");# les réseaux de train
source("geo/scripts/transport_txt.R")
source("geo/scripts/transport_wiki.R")
source("geo/scripts/transport_zone.R")
#
# les différents objets : node, way, relation
source("geo/scripts/transport_objets.R")

#
# les différents réseaux
source("geo/scripts/transport_axeo.R")
source("geo/scripts/transport_auray.R")
source("geo/scripts/transport_bibus.R")
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
source("geo/scripts/transport_qub.R")
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
Reseau <- "landerneau"
Reseau <- "star"; # Rennes
Reseau <- "kiceo"; # Vannes
Reseau <- "qub"; # Quimper
Reseau <- "douarnenez"; # Douarnenez
Reseau <- "bretagne"; # pseudo-réseau pour la Bretagne
Reseau <- "breizhgo56"; # pseudo-réseau pour BreizhGo en Morbihan
Reseau <- "morlaix"; # LinéoTim
Reseau <- "breizhgo35"; # pseudo-réseau pour BreizhGo en Ille-et-Vilaine
config_xls(Reseau)
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
