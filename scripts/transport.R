# <!-- coding: utf-8 -->
#
# quelques fonctions pour les réseaux de transport
#
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===========À====================================================
#
DEBUG <- FALSE
source("geo/scripts/misc.R")
baseDir <- sprintf("%s/web", Drive)
scriptsDir <- sprintf("%s/web/geo/scripts", Drive)
cfgDir <- sprintf("%s/web/geo/TRANSPORT", Drive)
imagesDir <- sprintf("%s/web/geo/TRANSPORT/images", Drive)
texDir <- sprintf("%s/web/geo/TRANSPORT", Drive)
tplDir <- sprintf("%s/web/geo/TRANSPORT", Drive)
varDir <- sprintf("%s/web.var/TRANSPORT", Drive)
odDir <- sprintf("%s/STAR", varDir)
transportDir <- sprintf("%s", varDir)
odDir <- sprintf("%s/MOBIBREIZH", varDir)
webDir <- sprintf("%s/web.heb/transport", Drive)
web_dir <- sprintf("%s/web.heb/transport", Drive)
osmDir <- sprintf("%s/OSM", varDir)
reseauDir <- sprintf("%s/SURF", varDir)
dir.create(cfgDir, showWarnings = FALSE, recursive = TRUE)
dir.create(varDir, showWarnings = FALSE, recursive = TRUE)
dir.create(osmDir, showWarnings = FALSE, recursive = TRUE)
dir.create(texDir, showWarnings = FALSE, recursive = TRUE)
dir.create(imagesDir, showWarnings = FALSE, recursive = TRUE)
dir.create(webDir, showWarnings = FALSE, recursive = TRUE)
source("geo/scripts/mga.R")
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
source("geo/scripts/misc_osrm.R"); # le routage avec osrm
source("geo/scripts/misc_overpass.R")
source("geo/scripts/misc_ssh.R")
source("geo/scripts/misc_st.R"); # ajout de fonctions à sf dans st_proches
source("geo/scripts/misc_tex.R")
source("geo/scripts/misc_tidytransit.R")
source("geo/scripts/misc_transport.R")
source("geo/scripts/misc_unicode.R")
source("geo/scripts/transport_arrets.R"); # le nettoyage des arrêts
source("geo/scripts/transport_carto.R"); # production des cartes
source("geo/scripts/transport_config.R"); # lecture du fichier excel
source("geo/scripts/transport_diff.R"); # différence entre gtfs et osm
source("geo/scripts/transport_gtfs.R"); # la validité, les différences entre versions
source("geo/scripts/transport_gtfs2mga.R")
source("geo/scripts/transport_gtfs2osm.R"); # mise en format compatible perl
source("geo/scripts/transport_ign.R"); # pour la détermination des communes des arrêts
source("geo/scripts/transport_mapbox.R")
source("geo/scripts/transport_mapmatching.R")
source("geo/scripts/transport_menu.R"); # les différents traitements suite à mise à jour du gtfs
source("geo/scripts/transport_misc.R")
# source("geo/scripts/transport_mobibreizh.R"); # pour le gtfs de la région
source("geo/scripts/transport_oapi.R")
source("geo/scripts/transport_open.R"); # édition des fichiers, téléchargement gtfs
source("geo/scripts/transport_osm.R"); # les interrogations osm soit via l'api soit via l'overpass
source("geo/scripts/transport_osmar.R")
source("geo/scripts/transport_osmose.R")
source("geo/scripts/transport_osm2mga.R")
source("geo/scripts/transport_postgis.R"); # avec importation par osm2pgsql
source("geo/scripts/transport_ptna.R"); # pour configurer ptna
source("geo/scripts/transport_ptv2.R"); # migation en PTv2
source("geo/scripts/transport_reseau.R"); # les comparaisons osm gtfs
source("geo/scripts/transport_route.R")
source("geo/scripts/transport_routes.R"); # cohérence route route_master
source("geo/scripts/transport_shapes.R"); # validation des tracés du fichier shapes
source("geo/scripts/transport_tidytransit.R"); # lecture du fichier gtfs.zip
source("geo/scripts/transport_train.R");# les réseaux de train
source("geo/scripts/transport_txt.R")
source("geo/scripts/transport_valhalla.R"); # le routage avec les shapes
source("geo/scripts/transport_web.R"); # pour le site web local avec gtfs-to-html
source("geo/scripts/transport_wiki.R")
source("geo/scripts/transport_zone.R")
#
# les différents objets : node, way, relation
source("geo/scripts/transport_objets.R")

#
# les différents réseaux
source("geo/scripts/transport_atoumod.R"); # pour le gtfs de la région Normandie
source("geo/scripts/transport_auray.R")
source("geo/scripts/transport_axeo.R")
source("geo/scripts/transport_bibus.R"); # brest
source("geo/scripts/transport_bordeaux.R")
source("geo/scripts/transport_breizhgo.R")
source("geo/scripts/transport_bretagne.R")
source("geo/scripts/transport_concarneau.R")
source("geo/scripts/transport_ctrl.R")
source("geo/scripts/transport_datagouv.R"); # pour les gtfs sur transport.data.gouv.fr
source("geo/scripts/transport_destineo.R"); # pour le gtfs de la région Pays de la Loire
source("geo/scripts/transport_distribus.R")
source("geo/scripts/transport_izilo.R")
source("geo/scripts/transport_kiceo.R")
source("geo/scripts/transport_korrigo.R"); # pour le gtfs de la région Bretagne
source("geo/scripts/transport_landerneau.R")
source("geo/scripts/transport_orleans.R")
source("geo/scripts/transport_pontivy.R")
source("geo/scripts/transport_pddl.R"); # Pays de la Loire
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

Reseau <- "kiceo"; # Vannes
Reseau <- "ter"; # pseudo-réseau pour BreizhGo SNCF
Reseau <- "breizhgo56"; # Morbihan
Reseau <- "vitobus"; # Vitré/Chateaubourg
Reseau <- "gironde"; # les lignes régionales
Reseau <- "arcachon"; # Arcachon
Reseau <- "toulouse"; # Toulouse Métropole
Reseau <- "limoges"; # Limoges Métropole
Reseau <- "bordeaux"; # Bordeaux Métropole
Reseau <- "bibusM"; # Brest avec les données de MobiBreizh
Reseau <- "ploermel"
Reseau <- "morlaix"; #
Reseau <- "dinan"; # Dinan / Côtes d'Armor
Reseau <- "angers"; # Angers Irigo
Reseau <- "semo"; # SEMO (pour Seine-Eure MObilités)
Reseau <- "bretagne"; # pseudo-réseau pour la Bretagne
Reseau <- "strasbourg"; # Strasbourg CTS
Reseau <- "morlaix"; # Morlaix Linéotim
Reseau <- "pontivy"; # Pontivy PondiBus
Reseau <- "guingamp"; # Guingamp AXEOBUS
Reseau <- "quimperle"; # Quimperlé TBK
Reseau <- "morlaix"; # LinéoTim
Reseau <- "breizhgo"; # BreizhGo en Bretagne, les lignes régionales
Reseau <- "guingamp"; # Guingamp AXEOBUS
Reseau <- "korrigo"; # les réseaux gérés en gtfs régionalement
Reseau <- "aleop_44"; # Pays de la Loire, réseau départemental 44
Reseau <- "breizhgo_lrron"; # pseudo-réseau pour BreizhGo régional ouest nord
Reseau <- "breizhgo_lrrns"; # pseudo-réseau pour BreizhGo régional nord sud
Reseau <- "bretagne"; # les réseaux de la région Bretagne
Reseau <- "destineo"; # Pays de la Loire, les réseaux gérés au niveau de la région
Reseau <- "aleop"; # Pays de la Loire
Reseau <- "saintbrevin"; # Saint Brevin les Pins, Brévibus
Reseau <- "aleop44"; # Pays de la Loire, réseau départemental 44
Reseau <- "saintnazaire"; # Saint-Nazaire STRAN
Reseau <- "orleans"; # Orléans TAO
Reseau <- "atoumod"; # Normandie
Reseau <- "cosibus50"; # les cars du département de la Manche/Coutances
Reseau <- "nomad50"; # les cars du département de la Manche
Reseau <- "nomad61"; # les cars du département de l'Orne
Reseau <- "guingamp"; # Guingmap Paimpol AxeoBus
Reseau <- "fougeres"; # Fougères Surf
Reseau <- "landerneau"; # Landernaeau Le Bus/Ar Bus
Reseau <- "douarnenez"; # Douarnenez Tudbus
Reseau <- "coutances"; # Coutances Cosibus
Reseau <- "lannion"; # Lannion TILT
Reseau <- "concarneau"; # Concarneau Coralie
Reseau <- "vannes"; # Vannes Kicéo
Reseau <- "nantes"; # Nantes TAN/Naolib
Reseau <- "lamballe"; # Lamballe / Côtes d'Armor / Distribus
Reseau <- "breizhgo_tim"; # pseudo-réseau pour BreizhGo en Morbihan
Reseau <- "breizhgo_tibus"; # pseudo-réseau pour BreizhGo en Côtes d'Armor
Reseau <- "breizhgo_pennarbed"; # pseudo-réseau pour BreizhGo en Finistère
Reseau <- "saintmalo"; # Saint-Malo
Reseau <- "quimper"; # Quimper QUB
Reseau <- "rennes"; # Rennes STAR
Reseau <- "breizhgo_illenoo2"; # pseudo-réseau pour BreizhGo en Ille-et-Vilaine
Reseau <- "saintbrieuc"; # Saint-Brieuc TUB
Reseau <- "brest"; # Brest Bibus
Reseau <- "rennes"; # Rennes STAR
Reseau <- "rouen"; # Rouen Astuce
Reseau <- "breizhgo_bateau_29"; # les bateaux en 29
Reseau <- "lorient"; # Lorient CTRL IZILO
config_xls(Reseau)
Tex <- TRUE
Wiki <- FALSE
HtmlBrowse <- TRUE
OsmChange <- FALSE
if ( interactive() ) {
  DEBUG <- TRUE
  graphics.off()
#  menu()
} else {
}
