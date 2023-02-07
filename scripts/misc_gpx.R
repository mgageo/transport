# <!-- coding: utf-8 -->
#
# pour les fichiers gpx
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
#
gpx_ecrire <- function(vecteur, dsn) {
  gpx_debut <- '<?xml version="1.0" encoding="UTF-8" standalone="no" ?>
<gpx xmlns="http://www.topografix.com/GPX/1/1" creator="mga R" version="1.1" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"  xsi:schemaLocation="http://www.topografix.com/GPX/1/1 http://www.topografix.com/GPX/1/1/gpx.xsd">'

  gpx_fin <- '</gpx>'

  gpx <- c(gpx_debut, vecteur, gpx_fin)
  write(gpx, dsn)
  carp("dsn: %s", dsn)
  return(invisible(dsn))
}
#
# conversion en format gpx compatble osm
gpx_write <- function(nc, dsn, name) {
  library(rgdal)
  carp("début name: %s", name)
  spdf <- as_Spatial(nc)
  writeOGR(
    spdf,
    dsn,
    layer="tracks",
    driver="GPX",
    dataset_options="GPX_USE_EXTENSIONS=yes,FORCE_GPX_TRACK=true",
    overwrite_layer=TRUE,
    delete_dsn = TRUE
  )

  return(invisible(gpx))
}
#
# conversion en format gpx compatble osm
gpx_write <- function(nc, dsn, name) {
  library(terra)
  carp("début name: %s", name)
  st_write(nc, dsn, driver = "GPX", overwrite_layer = TRUE, delete_dsn = TRUE, dataset_options = "GPX_USE_EXTENSIONS=yes,FORCE_GPX_TRACK=true")
  return(invisible(dsn))
}
#
# conversion en format gpx compatble osm
gpx_write <- function(nc, dsn, name) {
  library(sf)
  carp("début name: %s", name)
  points <- st_cast(nc, "POINT")
  gpx <- '<?xml version="1.0"?>
<gpx version="1.1" creator="GDAL 3.4.1" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:ogr="http://osgeo.org/gdal" xmlns="http://www.topografix.com/GPX/1/1" xsi:schemaLocation="http://www.topografix.com/GPX/1/1 http://www.topografix.com/GPX/1/1/gpx.xsd">
'
  gpx <- append(gpx, '<trk>')
  name <- sprintf('<name>%s</name>', name)
  gpx <- append(gpx, name)

  gpx <- append(gpx, '<trkseg>')
  for(point in points) {
#    glimpse(point);stop("****")
    trkpt <- sprintf('  <trkpt lon="%0.5f" lat="%0.5f"></trkpt>', point[1], point[2])
    gpx <- append(gpx, trkpt)
  }
  gpx <- append(gpx, '</trkseg>')
  gpx <- append(gpx, '</trk>')
  gpx <- append(gpx, '</gpx>')
  write(gpx, dsn)
  return(invisible(gpx))
}