# <!-- coding: utf-8 -->
# lees réseaux de transport
# utilisation des données d'OpenStreetMap et des gtfs
# auteur : Marc Gauthier
#

Drive <- substr( getwd(),1,2)
baseDir <- sprintf("%s/web", Drive)
cfgDir <- sprintf("%s/web/geo/TRANSPORT", Drive)
odDir <- sprintf("%s/web.var/geo/STAR", Drive)
transportDir <- sprintf("%s/web/geo/TRANSPORT", Drive)
odDir <- sprintf("%s/web/geo/TRANSPORT/MOBIBREIZH", Drive)
webDir <- sprintf("%s/web.heb/bv/transport", Drive)
osmDir <- sprintf("%s/web/geo/TRANSPORT", Drive)
reseauDir <- sprintf("%s/web/geo/TRANSPORT/SURF", Drive)
setwd(baseDir)
DEBUG <- FALSE
source("geo/scripts/mga.R")
source("geo/scripts/misc.R")
source("geo/scripts/misc_couches.R")
source("geo/scripts/misc_datagouv.R")
source("geo/scripts/misc_gtfs.R")
source("geo/scripts/misc_osm.R")
source("geo/scripts/misc_osmapi.R")
source("geo/scripts/misc_osmdata.R")
source("geo/scripts/misc_osrm.R")
source("geo/scripts/misc_overpass.R")
source("geo/scripts/misc_unicode.R")
source("geo/scripts/transport_config.R")
source("geo/scripts/transport_gtfs.R")
source("geo/scripts/transport_gtfs2mga.R")
source("geo/scripts/transport_gtfs2osm.R")
source("geo/scripts/transport_misc.R")
source("geo/scripts/transport_mobibreizh.R")
source("geo/scripts/transport_oapi.R")
source("geo/scripts/transport_osm.R")
source("geo/scripts/transport_osmar.R")
source("geo/scripts/transport_osm2mga.R")
source("geo/scripts/transport_tidytransit.R")
source("geo/scripts/transport_txt.R")
source("geo/scripts/transport_wiki.R")
#
# les différents objets : node, way, relation
source("geo/scripts/transport_objets.R")

#
# les différents réseaux
source("geo/scripts/transport_axeo.R")
source("geo/scripts/transport_kiceo.R")
source("geo/scripts/transport_lila.R")
source("geo/scripts/transport_qub.R")
source("geo/scripts/transport_rmat.R")
source("geo/scripts/transport_star.R")
source("geo/scripts/transport_surf.R")
source("geo/scripts/transport_tibus.R")
source("geo/scripts/transport_tilt.R")
source("geo/scripts/transport_tub.R")
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
  shapes2shape()
#  trips_stops()
#  transport_get()
}
