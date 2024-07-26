# <!-- coding: utf-8 -->
#
# quelques fonctions pour la comptage oiseaux des jardins
# auteur: Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
# source("geo/scripts/coj.R");couches_jour()
couches_jour <- function() {
  carp()
  libelleRef <<- 'bzha'
  deptRef <<- c('22', '35', '29', '56')
  couches_ign_ecrire(deptRef, libelleRef)
  return()
  libelleRef <<- 'bzh'
  deptRef <<- c('22', '35', '29', '56', '44')
  couches_ign_ecrire(deptRef, libelleRef)
  deptRef <<- '35'
  libelleRef <<- '35'
  couches_ign_ecrire(deptRef, libelleRef)
}
couches_ign_ecrire <- function(les_departements=c("35"), libelle='35') {
  require(sp)
  require(rgdal)
  carp()
  dsn <- sprintf('%s/web.var/geo/IGN/COMMUNE.SHP', Drive)
  if ( ! file.exists(dsn) ) {
    stop(carp(" dsn: %s", dsn))
  }
  layer <- ogrListLayers(dsn)
  spdf <- readOGR(dsn, layer=layer, stringsAsFactors=FALSE)
  spdf <- spdf[spdf@data$CODE_DEPT %in% les_departements, ]
  spdf <- spTransform(spdf, CRS("+init=epsg:2154"))
  carp(" nrow:%s", nrow(spdf@data))
  print(head(spdf@data))
  dsn <-  sprintf('%s/DEPARTEMENT_%s.shp', varDir, libelle)
  writeOGR(spdf, dsn, "DEPARTEMENT", driver="ESRI Shapefile", overwrite_layer=T)
  return(invisible(spdf))
}
couches_ign_lire <- function(libelle='35') {
  require(sp)
  require(rgdal)
  carp()
  dsn <-  sprintf('%s/DEPARTEMENT_%s.shp', varDir, libelle)
  if ( ! file.exists(dsn) ) {
    stop(carp("dsn: %s", dsn))
  }
  layer <- ogrListLayers(dsn)
  spdf <- readOGR(dsn, layer=layer, stringsAsFactors=FALSE)
  return(invisible(spdf))
}