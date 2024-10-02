# <!-- coding: utf-8 -->
#
# quelques fonctions pour OpenStreetMap
#
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===========À====================================================
#
DEBUG <- FALSE
source("geo/scripts/misc.R")
baseDir <- sprintf("%s/web", Drive)
cfgDir <- sprintf("%s/web/geo/OSM", Drive)
varDir <- sprintf("%s/web.var/OSM", Drive)
duckdbDir <- sprintf("%s/web/duckdb", Drive)
dir.create(cfgDir, showWarnings = FALSE, recursive = TRUE)
dir.create(varDir, showWarnings = FALSE, recursive = TRUE)

source("geo/scripts/mga.R")
source("geo/scripts/misc_overpass.R");
source("geo/scripts/osm_roundabout.R"); # pour les ronds-points
source("geo/scripts/osm_eumapper.R"); # pour les ronds-points modifiés par EUMapper
if ( interactive() ) {
  DEBUG <- TRUE
  graphics.off()
} else {
  menu()
}
