# <!-- coding: utf-8 -->
#
# quelques fonctions de lecture de couches
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
#
#
############################################################################################
#
# les limites des zones Wetlands
wetlandsLire <- function(les_departements) {
  require(sp)
  require(rgdal)
  require(raster)
  dsn <- sprintf('%s/web.var/WETLANDS/polygone sites WI.shp', Drive)
  ogrInfo(dsn, layer="polygone sites WI")
  dfSP <- readOGR(dsn, layer="polygone sites WI", stringsAsFactors=FALSE)
  dfSP <- spTransform(dfSP, CRS("+init=epsg:2154"))
  dfSP <- dfSP[dfSP@data$DEPT %in% les_departements, ]
#  plot(dfSP)
  return(dfSP)
}
############################################################################################
#
# les fichiers de https://www.data.gouv.fr
# source("geo/scripts/wetlands.R"); df <- couches_datagouv_remembrement()
couches_datagouv_remembrement <- function() {
  require(readxl)
  dsn <- sprintf('%s/bvi35/CouchesDataGouv/BDRemembrement.xlsx', Drive)
  carp('dsn: %s', dsn)
  nc <- read_excel(dsn, sheet='AME_FONC_COR_2005_')
  return(nc)
}
############################################################################################
#
# les fichiers de la ddtm35
# source("geo/scripts/bmsm.R"); nc <- couches_ddtm35_pprsm()
couches_ddtm35_pprsm <- function() {
  require(sf)
  dsn <- sprintf('%s/bvi35/CouchesDDTM35/N_ZONE_REG_PPRSM_20100004_S_035.json', Drive)
  carp('dsn: %s', dsn)
  nc <- st_read(dsn) %>%
    glimpse
  return(nc)
}
#
# le contour du marais de Dol
# source("geo/scripts/bmsm.R"); nc <- couches_digues_et_marais()
couches_digues_et_marais <- function() {
  require(sf)
  dsn <- sprintf('%s/bvi35/CouchesBMSM/digues_et_marais/digues_et_marais.shp', Drive)
  carp('dsn: %s', dsn)
  nc <- st_read(dsn) %>%
    mutate(surface=as.numeric(round(st_area(.)/10000, 0))) %>%
    glimpse
  return(nc)
}
############################################################################################
# IGN : GeoFLA, précision moyenne
# limite des communes
communeLire <- function(les_departements) {
  require(sp)
  require(rgdal)
  dsn <- sprintf('%s/web.var/geo/COUCHES/COMMUNE.SHP', Drive)
  print(sprintf("communeLire() début %s", dsn))
  layer <- ogrListLayers(dsn)
  spdf <- readOGR(dsn, layer=layer, stringsAsFactors=FALSE)
  spdf <- spdf[spdf@data$CODE_DEPT %in% les_departements, ]
#  spdf <- spTransform(spdf, CRS("+init=epsg:2154"))
#  plot(spdf)
  print(sprintf("communeLire() fin"))
  return(spdf)
}
ign_commune_lire <- function(les_departements) {
  print(sprintf("ign_commune_lire() début %s", les_departements))
  require(sp)
  require(rgdal)
  if ( exists("ign_commune.spdf") ) {
    spdf <- ign_commune.spdf
  } else {
    dsn <- sprintf('%s/web.var/geo/IGN/COMMUNE.SHP', Drive)
    print(sprintf("ign_commune_lire() dsn %s", dsn))
    if ( ! file.exists(dsn) ) {
      stop(sprintf("ign_commune_lire dsn: %s", dsn))
    }
    layer <- ogrListLayers(dsn)
    spdf <- readOGR(dsn, layer=layer, stringsAsFactors=FALSE)
    ign_commune.spdf <<- spdf
  }
  spdf <- spdf[spdf@data$INSEE_COM %in% les_departements, ]
  spdf <- spTransform(spdf, CRS("+init=epsg:2154"))
  return(spdf)
}
ign_commune_lire_sf <- function(les_departements) {
 carp("début %s", les_departements)
  require(sf)
  if ( exists("ign_commune.sf") ) {
    nc <- ign_commune.sf
  } else {
    dsn <- sprintf('%s/web.var/geo/IGN/COMMUNE.SHP', Drive)
    carp("dsn %s", dsn)
    if ( ! file.exists(dsn) ) {
      stop(sprintf("ign_commune_lire dsn: %s", dsn))
    }
    nc <- st_read(dsn, stringsAsFactors=FALSE)
    ign_commune.sf <<- nc
  }
  nc <- nc[nc$INSEE_COM %in% les_departements, ]
  nc <- st_transform(nc, 2154)
  return(nc)
}
# limite des départements
departementLire <- function(les_departements= c("22","29","35","56"), force=FALSE) {
  if ( exists("departement.spdf") & force==FALSE) {
    return(invisible(departement.spdf))
  }
  carp()
  require(sp)
  require(rgdal)
  dsn <-  sprintf('%s/bvi35/CouchesIGN/ADE_1-1_SHP_LAMB93_FR/DEPARTEMENT.SHP', Drive)
  layer <- ogrListLayers(dsn)
  spdf <- readOGR(dsn, layer=layer, stringsAsFactors=FALSE)
  spdf <- spdf[spdf@data$CODE_DEPT %in% les_departements, ]
  spdf <- spTransform(spdf, CRS("+init=epsg:2154"))
  departement.spdf <<- spdf
#  plot(spdf)
  return(invisible(spdf))
}
# limite des départements
ign_departement_lire <- function(les_departements) {
  require(sp)
  require(rgdal)
  dsn <-  sprintf('%s/bvi35/CouchesIGN/ADE_1-1_SHP_LAMB93_FR/DEPARTEMENT.SHP', Drive)
  layer <- ogrListLayers(dsn)
  dfSP <- readOGR(dsn, layer=layer, stringsAsFactors=FALSE)
  dfSP <- dfSP[dfSP@data$CODE_DEPT %in% les_departements, ]
  dfSP <- spTransform(dfSP, CRS("+init=epsg:2154"))
#  plot(dfSP)
  return(dfSP)
}
# limite des départements
ign_departement_lire_sf_v1 <- function(les_departements) {
  require(sf)
  dsn <-  sprintf('%s/bvi35/CouchesIGN/ADE_1-1_SHP_LAMB93_FR/DEPARTEMENT.SHP', Drive)
  nc <- st_read(dsn, layer=layer, stringsAsFactors=FALSE) %>%
    glimpse()
  nc <- nc[nc$CODE_DEPT %in% les_departements, ]
  return(nc)
}
# limite des départements
ign_departement_lire_sf <- function(les_departements) {
  require(sf)
  dsn <-  sprintf('%s/bvi35/CouchesIGN/ADE_1-1_SHP_LAMB93_FR/DEPARTEMENT.SHP', Drive)
  nc <- st_read(dsn, stringsAsFactors=FALSE) %>%
    glimpse()
  nc <- nc[nc$INSEE_DEP %in% les_departements, ]
  return(nc)
}

ign_rgc_lire <- function(les_departements) {
  require(sp, quietly = TRUE)
  dsn <- sprintf('%s/web.var/geo/IGN/RGC2012/RGC_2012.txt', Drive)
  if ( ! file.exists(dsn) ) {
    stop("ign_rgc_lire() *** dsn:%s", dsn)
  }
  df <- read.csv(dsn, header = TRUE, sep="\t", fileEncoding="utf8")
  df <- df[df$DEP %in% les_departements, ]
# transformation en spatial
  df [,"YLAMB93"] <- sapply(df[,"YLAMB93"], as.character)
  df [,"YLAMB93"] <- sapply(df[,"YLAMB93"], as.numeric)
  df [,"XLAMB93"] <- sapply(df[,"XLAMB93"], as.character)
  df [,"XLAMB93"] <- sapply(df[,"XLAMB93"], as.numeric)
  df$XLAMB93 <- df$XLAMB93 * 100
  df$YLAMB93 <- df$YLAMB93 * 100
  coordinates(df) = ~ XLAMB93 + YLAMB93
  dfSP <- SpatialPointsDataFrame(df,data.frame(df[,]))
  proj4string(dfSP) <- CRS("+init=epsg:2154")
  if ( DEBUG ) {
    print(summary(dfSP))
    print(head(dfSP, 5))
  }
  return(dfSP)
}
#=============================================
# DC_ROUTE500.pdf
#
ign_route <- function() {
  ign_route_a_la()
#  ign_route_h_th()
#  ign_route_h_zos()
#  ign_route_rf_tvf()
#  ign_route_rr_tr()
}
# http://rstudio-pubs-static.s3.amazonaws.com/7953_4e3efd5b9415444ca065b1167862c349.html
ign_route_a_la <- function() {
  library(raster, quietly = TRUE)
  dfSP <- ign_route500_lire('ADMINISTRATIF', 'LIMITE_ADMINISTRATIVE')
  plot(dfSP, col= "black", add=TRUE, lwd=10, lty = "dotted" )
}
ign_route_rf_tvf <- function() {
  library(raster, quietly = TRUE)
  dfSP <- ign_route500_lire('RESEAU_FERRE', 'TRONCON_VOIE_FERREE')
  plot(dfSP, col= "blue", add=TRUE)
}
ign_route_h_th <- function() {
  library(raster, quietly = TRUE)
  dfSP <- ign_route500_lire('HABILLAGE', 'TRONCON_HYDROGRAPHIQUE')
  plot(dfSP, col= "blue", add=TRUE)
}
ign_route_h_zos <- function(add=TRUE) {
  library(raster, quietly = TRUE)
  dfSP <- ign_route500_lire('HABILLAGE', 'ZONE_OCCUPATION_SOL')
  dfSP@data$couleur <- "khaki"
  dfSP@data$NATURE  <- iconv(dfSP@data$NATURE , 'latin1','UTF-8')
  dfSP@data[dfSP@data$NATURE == 'Eau', 'couleur'] <- 'blue'
  dfSP@data[substr(dfSP@data$NATURE,1,1) == 'B', 'couleur'] <- 'grey'
  dfSP@data[substr(dfSP@data$NATURE,1,1) == 'F', 'couleur'] <- 'lightgreen'
  print(head(dfSP@data,50))
  plot(dfSP, col= dfSP@data$couleur,  add=add)
}
ign_route_rr_tr <- function() {
  library(raster, quietly = TRUE)
  dfSP <- ign_route500_lire('RESEAU_ROUTIER', 'TRONCON_ROUTE')
  dfSP@data$largeur <- 1
  dfSP@data[dfSP@data$VOCATION == 'Type autoroutier', 'largeur'] <- 4
  dfSP@data[dfSP@data$VOCATION == 'Liaison régionale', 'largeur'] <- 2
#  print(head(dfSP, 60))
  plot(dfSP, col= "orange", lwd=dfSP@data$largeur, add=TRUE)
  dfSP <- dfSP[dfSP@data$VOCATION == 'Type autoroutier', ]
#  print(head(dfSP, 60))
  plot(dfSP, col= "red", lwd=1, add=TRUE)
}
#
# découpage des fichiers ign ROUTE500
ign_clip <- function() {
  library(raster, quietly = TRUE)
  library(rgeos, quietly = TRUE)
#  dfSP <- ign_route500_lire('ADMINISTRATIF', 'COMMUNE')
#  dfSP <- ign_route500_lire('RESEAU_FERRE', 'TRONCON_VOIE_FERREE')
  dfSP <- ign_route500_lire('RESEAU_ROUTIER', 'TRONCON_ROUTE')
#  dfSP <- ign_route500_lire('HABILLAGE', 'TRONCON_HYDROGRAPHIQUE')
#  dfSP <- ign_route500_lire('HABILLAGE', 'ZONE_OCCUPATION_SOL')
  print(summary(dfSP))
  print(head(dfSP@data))
  xmin <- 350000; ymax <- 6797000; xmax <- 377000;  ymin <- 6778000
  clipSP <-as(extent(xmin, xmax, ymin, ymax), "SpatialPolygons")
  proj4string(clipSP) <- CRS(proj4string(dfSP))
  out <- gIntersection(dfSP, clipSP, byid=TRUE)
#
  out <- cropSP(dfSP, clipSP)
  plot(out, col= "blue", bg="azure2")
  stop("ign_clip()")
}
#
# les fichiers ign
ignDir <- sprintf("%s/bvi35/CouchesIGN", Drive);
#
# admin_express
#
ign_ade_lire_sf <- function(layer = 'COMMUNE') {
  library(sf)
  ignDir <-'D:/bvi35/CouchesIGN/ADE_1-1_SHP_LAMB93_FR'
  fname <- sprintf('%s/%s.shp', ignDir, layer)
  nc <- st_read(fname)
  return(invisible(nc))
}
#
# BD TOPO® Hydrographie par territoire édition septembre 2019
#
# source("geo/scripts/wetlands.R");nc <- ign_bdtopo_hydrographie_lire_sf('SURFACE_HYDROGRAPHIQUE')
ign_bdtopo_hydrographie_lire_sf <- function(layer='COURS_D_EAU') {
  library(sf)
  dossier <- 'BDTOPO_3-0_HYDROGRAPHIE_SHP_LAMB93_FXX_2019-09-19/BDTOPO/1_DONNEES_LIVRAISON_2019-09-00371/BDT_3-0_SHP_LAMB93_FXX_ED2019-09-19/HYDROGRAPHIE'
  dsn <- sprintf('%s/%s/%s.shp', ignDir, dossier, layer)
  nc <- st_read(dsn, stringsAsFactors=FALSE)
  return(invisible(nc))
}
#
# Geofla remplacé par Admin Express depuis 2017
# source("geo/scripts/wetlands.R");nc <- ign_geofla_1_lire_sf()
ign_geofla_1_lire_sf <- function(layer='COMMUNE') {
  carp()
  library(sf)
  dossier <- 'GEOFLA_1-0__SHP__FXX_2002-01-01/GEOFLA/1_DONNEES_LIVRAISON_2002-01-01/COM'
  dsn <- sprintf('%s/%s/%s.shp', ignDir, dossier, layer)
  nc <- st_read(dsn, stringsAsFactors=FALSE)
  return(invisible(nc))
}
# source("geo/scripts/wetlands.R");nc <- ign_adminexpress_lire_sf()
ign_adminexpress_lire_sf <- function(layer = "COMMUNE", force = FALSE) {
  if ( ! exists('ign_adminexpress.list')) {
    ign_adminexpress.list <<- list()
  }
  if ( exists(layer, where=ign_adminexpress.list) & force==FALSE) {
    return(invisible(ign_adminexpress.list[[layer]]))
  }
  library(sf)
  dossier <- 'ADMIN-EXPRESS_2-1__SHP__FRA_2019-11-15/ADMIN-EXPRESS/1_DONNEES_LIVRAISON_2019-11-15/ADE_2-1_SHP_LAMB93_FR'
  dossier <- 'ADMIN-EXPRESS_3-0__SHP__FRA_L93_2021-10-15/ADMIN-EXPRESS_3-0__SHP__FRA_2021-10-15/ADMIN-EXPRESS/1_DONNEES_LIVRAISON_2021-10-15/ADE_3-0_SHP_LAMB93_FR'
  dossier <- 'ADMIN-EXPRESS_3-1__SHP_LAMB93_FXX_2023-01-16/ADMIN-EXPRESS/1_DONNEES_LIVRAISON_2023-01-16/ADE_3-1_SHP_LAMB93_FXX'
  dsn <- sprintf('%s/%s/%s.shp', ignDir, dossier, layer)
  carp("dsn: %s", dsn)
  nc <- st_read(dsn, stringsAsFactors = FALSE, quiet = TRUE)
  ign_adminexpress.list[[layer]] <<- nc
  return(invisible(nc))
}

ign_adminexpress_cog_lire_sf <- function(layer='CHEF_LIEU', force=FALSE) {
  library(sf)
  carp()
  if ( ! exists('ign_adminexpress_cog.list')) {
    ign_adminexpress_cog.list <<- list()
  }
  if ( exists(layer, where=ign_adminexpress_cog.list) & force==FALSE) {
    return(invisible(ign_adminexpress_cog.list[[layer]]))
  }
  dossier <- 'ADMIN-EXPRESS-COG_2-0__SHP__FRA_L93_2019-09-24/ADMIN-EXPRESS-COG_2-0__SHP__FRA_2019-09-24/ADMIN-EXPRESS-COG/1_DONNEES_LIVRAISON_2019-09-24/ADE-COG_2-0_SHP_LAMB93_FR'
  dsn <- sprintf('%s/%s/%s.shp', ignDir, dossier, layer)
  nc <- st_read(dsn, stringsAsFactors=FALSE)
  ign_adminexpress_cog.list[[layer]] <<- nc
  return(invisible(nc))
}
#
# lecture des fichiers bruts
ign_route500_lire <- function(groupe = 'HABILLAGE', layer = 'ZONE_OCCUPATION_SOL') {
  require(sp, quietly = TRUE)
  require(rgdal, quietly = TRUE)
  print(sprintf("ign_route500_lire()"));
  dsn <- sprintf('%s/web.var/geo/IGN/ROUTE500_1-1_SHP_LAMB93_000_2012-11-23/ROUTE500_1-1_SHP_LAMB93_000_2012-11-23/ROUTE500/1_DONNEES_LIVRAISON_2012-11-00093/R500_1-1_SHP_LAMB93_FR-ED121/HABILLAGE/%s.SHP', Drive, layer)
  dsn <- sprintf('%s/web.var/geo/IGN/ROUTE500_1-1_SHP_LAMB93_000_2012-11-23/ROUTE500_1-1_SHP_LAMB93_000_2012-11-23/ROUTE500/1_DONNEES_LIVRAISON_2012-11-00093/R500_1-1_SHP_LAMB93_FR-ED121/HABILLAGE/%s.SHP', Drive, layer)
  dsn <- sprintf('%s/web.var/geo/IGN/ROUTE500_1-1_SHP_LAMB93_D035_2012-11-21/ROUTE500_1-1_SHP_LAMB93_D035_2012-11-21/ROUTE500/1_DONNEES_LIVRAISON_2012-11-00091/R500_1-1_SHP_LAMB93_D035-ED121/HABILLAGE/%s.SHP', Drive, layer)
  dsn <- sprintf('%s/web.var/geo/IGN/ROUTE500_1-1_SHP_LAMB93_D035_2012-11-21/ROUTE500/1_DONNEES_LIVRAISON_2012-11-00091/R500_1-1_SHP_LAMB93_D035-ED121/%s/%s.SHP', Drive, groupe, layer)
#  dsn <- sprintf('%s/web.var/geo/IGN/ROUTE500/%s/%s.SHP', Drive, groupe, layer)
  dfSP <- readOGR(dsn, layer=layer, stringsAsFactors=FALSE)
#  proj4string(dfSP) <- CRS("+init=epsg:2154")
#  dfSP <- spTransform(dfSP, CRS("+init=epsg:2154"))
  return(dfSP)
}
couches_ign_zos_lire <- function() {
  carp()
  dsn <- 'D:/bvi35/CouchesIKA/ign/ZONE_OCCUPATION_SOL.shp'
  nc <- read_sf(dsn, stringsAsFactors=FALSE) %>%
   glimpse()
  return(invisible(nc))
}
# http://professionnels.ign.fr/route500
# http://professionnels.ign.fr/rgc
ign_dl <- function() {
  sources <- '
https://wxs-telechargement.ign.fr/x02uy2aiwjo9bm8ce5plwqmr/telechargement/prepackage/ADMINEXPRESS-PACK_2018-06-14$ADMIN-EXPRESS_1-1__SHP__FRA_2018-06-14/file/ADMIN-EXPRESS_1-1__SHP__FRA_2018-06-14.7z
http://professionnels.ign.fr/sites/default/files/RGC2012.tar
https://wxs-telechargement.ign.fr/pfinqfa9win76fllnimpfmbi/telechargement/inspire/ROUTE500-France-2012_ROUTE500_1-1_SHP_LAMB93_000_2012-11-23/file/ROUTE500_1-1_SHP_LAMB93_000_2012-11-23.7z
https://wxs-telechargement.ign.fr/qar5zbaf189ecq7yz7tfgpe5/telechargement/inspire/ROUTE500-France-departements-2012_ROUTE500_1-1_SHP_LAMB93_D035_2012-11-21/file/ROUTE500_1-1_SHP_LAMB93_D035_2012-11-21.7z
'
  sourcesL <- unlist(strsplit(sources,"\n"))
  target_dir <- sprintf("%s/web.var/geo/IGN", Drive)
  dir.create(target_dir, showWarnings = FALSE, recursive = TRUE)
  for (source_file in sourcesL) {
    if ( source_file == "" ) {
      next
    }
    print(sprintf("ign_dl() source_file:%s", source_file))
    name <- basename(source_file)
    target_file <- sprintf("%s/%s", target_dir, name)
    ign_7z_dl(source_file, target_dir, target_file)
    name <- sub("-shp.zip$", "", name, perl=TRUE)
    dsn <- sprintf('%s/%s.shp',target_dir, name)
    print(sprintf("ign_dl() dsn:%s", dsn))
    if ( ! file.exists(dsn) ) {
      next;
    }
    layers <- ogrListLayers(dsn)
    print(sprintf("ign_dl() layers:%s", layers))
    dfSP <- readOGR(dsn, layer=layers, stringsAsFactors=FALSE, dropNULLGeometries=FALSE)
    print(sprintf("ign_dl() dfSP:%s", summary(dfSP)))
  }
}
ign_7z_dl_v1 <- function(source_file, target_dir, target_file) {
  library("RCurl")
  library("R.utils");
  print(sprintf("ign_7z_dl() début %s", source_file))
  if ( ! file.exists(target_file) ) {
    download.file(source_file, target_file)
  }
  extrait(target_file, target_dir, overwrite=TRUE)
  print(sprintf("ign_7z_dl() fin"))
}
#
# https://github.com/jburkhardt/RAdwords/issues/71
# curl certificat
# system.file("CurlSSL", "ca-bundle.crt", package = "RCurl")
ign_7z_dl <- function(source_file, target_dir, target_file) {
  library("RCurl")
  library("R.utils");
  print(sprintf("ign_7z_dl() début %s", source_file))
  if ( ! file.exists(target_file) ) {
    download.file(source_file, target_file)
  }
  unzip(target_file, exdir=target_dir, overwrite=TRUE)
  print(sprintf("ign_7z_dl() fin"))
}

############################################################################################
# OSM : précision variable
# limite des communes d'un département
osm_communes_lire <- function(les_communes) {
  require(sp)
  require(rgdal)
  dsn <- sprintf('%s/web.var/geo/OSM/communes-20131219-50m.shp', Drive)
  if ( ! file.exists(dsn) ) {
    stop(sprintf("osm_communes_lire() dsn:%s", dsn))
  }
  layers <- ogrListLayers(dsn)
  dfSP <- readOGR(dsn, layer=layers, stringsAsFactors=FALSE, dropNULLGeometries=TRUE)
  dfSP <- dfSP[dfSP@data$insee %in% les_communes, ]
  dfSP <- spTransform(dfSP, CRS("+init=epsg:2154"))
#  plot(dfSP)
  return(dfSP)
}
couches_osm_communes <- function(Couches) {
  require(sp)
  require(rgdal)
  dsn <- sprintf('%s/web.var/geo/COUCHES/%s/OSM_communes.SHP', Drive, Couches)
  if ( ! file.exists(dsn) ) {
    stop(sprintf("couches_osm_communes_lire() dsn:%s", dsn))
  }
  layer <- ogrListLayers(dsn)
  dfSP <- readOGR(dsn, layer=layer, stringsAsFactors=FALSE)
  dfSP <- spTransform(dfSP, CRS("+init=epsg:2154"))
#  plot(dfSP)
  return(dfSP)
}
couches_osm_communes_lire_sf <- function() {
  require(sf)
  dsn <- sprintf('%s/web.var/geo/OSM/communes-20180101.shp', Drive)
  carp("dsn: %s", dsn)
  if ( ! file.exists(dsn) ) {
    stop('***')
  }
  nc <- st_read(dsn)
  return(nc)
}
############################################################################################
#
# les téléchargements
#
osm_dl <- function() {
  library("rgdal")
  sources <- '
http://osm13.openstreetmap.fr/~cquest/openfla/export/communes-20180101-shp.zip
http://osm13.openstreetmap.fr/~cquest/openfla/export/communes-20131219-5m-shp.zip
http://osm13.openstreetmap.fr/~cquest/openfla/export/communes-20131219-50m-shp.zip
http://osm13.openstreetmap.fr/~cquest/openfla/export/communes-20131219-100m-shp.zip
http://osm13.openstreetmap.fr/~cquest/openfla/export/departements-20131219-5m-shp.zip
http://osm13.openstreetmap.fr/~cquest/openfla/export/departements-20131219-50m-shp.zip
http://osm13.openstreetmap.fr/~cquest/openfla/export/departements-20131219-100m-shp.zip
'
  sourcesL <- unlist(strsplit(sources,"\n"))
  target_dir <- sprintf("%s/web.var/geo/OSM", Drive)
  dir.create(target_dir, showWarnings = FALSE, recursive = TRUE)
  for (source_file in sourcesL) {
    if ( source_file == "" ) {
      next
    }
    carp("source_file:%s", source_file)
    name <- basename(source_file)
    target_file <- sprintf("%s/%s", target_dir, name)
    osm_fla_dl(source_file, target_dir, target_file)
    name <- sub("-shp.zip$", "", name, perl=TRUE)
    dsn <- sprintf('%s/%s.shp',target_dir, name)
    carp("dsn:%s", dsn)
    if ( ! file.exists(dsn) ) {
      next;
    }
    next;
    layers <- ogrListLayers(dsn)
    print(sprintf("osm_dl() layers:%s", layers))
    dfSP <- readOGR(dsn, layer=layers, stringsAsFactors=FALSE, dropNULLGeometries=FALSE)
    print(sprintf("osm_dl() dfSP:%s", summary(dfSP)))
  }
}
# les limites simplifiées
# http://openstreetmap.fr/blogs/cquest/limites-administratives-simplifiees
osm_fla_dl <- function(source_file, target_dir, target_file) {
  library("RCurl")
  library("R.utils");
  carp("target_file: %s", target_file)
  if ( ! file.exists(target_file) ) {
    download.file(source_file, target_file)
  }
  unzip(target_file, exdir=target_dir, overwrite=TRUE)
  carp("target_dir: %s", target_dir)
}

# https://www.ifremer.fr/sextant_doc/sextant/sextant/valorIG_2012/04_Cartographie_Avec_R.pdf
osm_map <-function() {
  library(OpenStreetMap)
  Nantes <- c(47.227962,-1.558113)
  map <- openmap(c(Nantes[1]+0.1,Nantes[2]-0.2), c(Nantes[1]-0.1,Nantes[2]+0.2),type='osm')
  plot(map,raster=TRUE)
}
#
# fonction générique d'extraction
extrait <- function(target_file, target_dir, overwrite=TRUE) {
  if ( ! file.exists(target_dir) ) {
    dir.create(target_dir, showWarnings = FALSE, recursive = TRUE)
  }
  ext <- sub('^.*\\.','', target_file, perl=TRUE)
  print(sprintf("extrait() ext:%s", ext))
  switch(ext,
  '7z' = {
    un7z(target_file, exdir=target_dir, overwrite=TRUE)
  },
  'zip' = {
    unzip(target_file, exdir=target_dir, overwrite=TRUE)
  },
  'tar' = {
    untar(target_file, exdir=target_dir)
  }
  )
}
# C:/Program Files/7-Zip/7z.exe
un7z <- function(target_file, exdir, overwrite=TRUE) {
  cmd <-sprintf('7z e -o %s %s', exdir, target_file)
  system(cmd)
}
