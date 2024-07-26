# <!-- coding: utf-8 -->
#
# quelques fonctions pour la comptage oiseaux des jardins
# auteur: Marc Gauthier
# licence: Creative Commons Paternité - Pas d"Utilisation Commerciale - Partage des Conditions Initiales à l"Identique 2.0 France
# ===============================================================
#
## nouvelle année
# mettre à jour coj_annee.R
# renommer les xlsx bvcoj_v2_****
#
# les traitements à enchainer suite à une mise à jour
# source("geo/scripts/coj.R");jour()
jour <- function() {
  carp()
  library(tidyverse)
  annee_jour()
}
Drive <- substr(getwd(), 1, 2)
baseDir <- sprintf("%s/web", Drive)
setwd(baseDir)

cfgDir <<- sprintf("%s/geo/COJ", baseDir)
tplDir <<- sprintf("%s/geo/scripts", baseDir)
texDir <<- sprintf("%s/geo/COJ", baseDir)
varDir <<- sprintf("%s/bvi35/CouchesCoj", Drive);
inseeDir <<- sprintf("%s/bvi35/CouchesINSEE", Drive);

source("geo/scripts/misc.R");
source("geo/scripts/mga.R");
source("geo/scripts/misc_actions.R");
source("geo/scripts/misc_apivn.R");
source("geo/scripts/misc_biolo.R");
source("geo/scripts/misc_couches.R");
source("geo/scripts/misc_db.R");
source("geo/scripts/misc_drive.R");
source("geo/scripts/misc_geocode.R");
source("geo/scripts/misc_ggplot.R");
source("geo/scripts/misc_insee.R");
source("geo/scripts/misc_psql.R");
source("geo/scripts/misc_tex.R");
# source("geo/scripts/coj_bastien.R");
source("geo/scripts/coj_actions.R");
source("geo/scripts/coj_annee.R");
source("geo/scripts/coj_apivn.R");
source("geo/scripts/coj_bastien.R");
source("geo/scripts/coj_bilans.R");
source("geo/scripts/coj_couches.R");
source("geo/scripts/coj_donnees.R");
source("geo/scripts/coj_doublons.R");
source("geo/scripts/coj_drive.R");
source("geo/scripts/coj_emmanuelle.R")
source("geo/scripts/coj_especes.R");
source("geo/scripts/coj_faune.R");
source("geo/scripts/coj_geoca.R");
source("geo/scripts/coj_guillaume.R");
source("geo/scripts/coj_jardin.R");
source("geo/scripts/coj_lpo.R");
source("geo/scripts/coj_lpo44.R");
source("geo/scripts/coj_marjorie.R")
source("geo/scripts/coj_odj.R"); # les données du site oiseauxdesjardins des années 2013-2015 transmises par Marjorie Poitevin
source("geo/scripts/coj_pvalue.R"); # test de la robustesse
source("geo/scripts/coj_sql.R");
source("geo/scripts/coj_sqlv2.R");
source("geo/scripts/coj_stat.R");
source("geo/scripts/coj_xl2012.R");


DEBUG <- FALSE


if (interactive()) {
  DEBUG <- TRUE
  graphics.off()
} else {
}
