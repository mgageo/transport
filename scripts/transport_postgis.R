# <!-- coding: utf-8 -->
#
# utilisation des données d'OpenStreetMap
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
# auteur : Marc Gauthier
#
#
## fonctions de postgis
#
# https://osm2pgsql.org/doc/manual.html
#
# https://rtask.thinkr.fr/fr/interagir-avec-postgis-depuis-r-avec-dbi-et-sf/
#
# docker run --name "postgis" -e POSTGRES_USER=docker -e POSTGRES_PASS=docker -p 5432:5432 -e POSTGRES_DBNAME=docker -d -t kartoza/postgis
# ./osm2pgsql.exe relation_14194690_full.osm --database=docker --username=docker --password
#
# source("geo/scripts/transport.R");postgis_jour()
postgis_jour <- function(force = TRUE) {
  library(rpostgis)
  con <- dbConnect(
    RPostgres::Postgres(),
    host = "localhost",
    dbname = "docker",
    port = 5432,
    user = "docker",
    password = "docker"
  )
  roads.sf <- st_read(con, layer = "planet_osm_roads") %>%
    glimpse()
  plot(st_geometry(roads.sf))
  df1 <- roads.sf %>%
    dplyr::select(osm_id, name, ref)
  misc_print(df1)
}
# https://subscription.packtpub.com/book/programming/9781849518666/1/ch01lvl1sec15/importing-openstreetmap-data-with-the-osm2pgsql-command