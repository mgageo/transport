# <!-- coding: utf-8 -->
#
# réseau de bus utilisation des données GTFS
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
#
# https://towardsdatascience.com/r-for-gtfs-getting-the-number-of-trips-per-hour-for-each-line-4d22491e3944
#
# https://www.rdocumentation.org/packages/tidytransit/versions/0.5.1/topics/shapes_as_sfg
#
# source("geo/scripts/transport.R");config_xls('surf');gtfs_files_lire() %>% glimpse()
gtfs_files_lire <- function(force = FALSE) {
  carp()
  library(tidyverse)
  library(janitor)
  library(rio)
  if ( ! exists('gtfs_files.list')) {
    gtfs_files.list <<- list()
  }
  files <- c("agency", "routes", "shapes",  "stop_times",  "stops", "trips")
  for (file in files) {
    dsn <- sprintf("%s/%s.txt", gtfsDir, file)
    gtfs_files.list[[file]] <<- rio::import(dsn, encoding = "UTF-8")
  }
  return(invisible(gtfs_files.list))
}
gtfs_file_lire <- function(file = "routes") {
  carp()
  library(tidyverse)
  library(janitor)
  library(rio)
  dsn <- sprintf("%s/%s.txt", gtfsDir, file)
  df <- rio::import(dsn, encoding = "UTF-8")
  return(invisible(df))
}
#
# geocode pour la commune
# source("geo/scripts/transport.R");config_xls('bibus');gtfs_stops_geocode() %>% glimpse()
#
# version perso avec intersection sur le contour des communes
gtfs_stops_geocode <- function() {
  carp()
  ign_stops_commune()
}
#
# pb avec le service de datagouv
gtfs_stops_geocode_v1 <- function() {
  library(tidyverse)
  df <- gtfs_file_lire("stops")
  df1 <- df %>%
#    filter(location_type == 0) %>%
    dplyr::select(lat = stop_lat, lon = stop_lon, name = stop_id) %>%
    glimpse()
  df11 <- df1 %>%
    group_by(name) %>%
    summarize(nb = n()) %>%
    filter(nb > 1) %>%
    glimpse()
  dsn <- sprintf("%s/gtfs_stops_geocode_search.csv", varDir)
  rio::export(df1, dsn, sep = ",")
  carp("interrogation de datagouv")
  f_dest <- sprintf("%s/gtfs_stops_geocode_api.csv", varDir)
  geocode_reverse_csv_datagouv(dsn, f_dest, url = "https://api-adresse.data.gouv.fr")
  carp("f_dest: %s", f_dest)
  df2 <- rio::import(f_dest, encoding = "UTF-8") %>%
    dplyr::mutate(name = sprintf("%s", name)) %>%
    dplyr::select(name, city = result_city)
  df3 <- df %>%
    left_join(df2, by = c("stop_id" = "name")) %>%
    glimpse()
  dsn <- sprintf("%s/%s.txt", gtfsDir, "stops_geocode")
  rio::export(df3, dsn, sep = ",")
  carp("dsn: %s", dsn)
  df4 <- df3 %>%
    filter(city == "") %>%
    glimpse()
}

gtfs_stops_sf <- function(df) {
  library(tidyverse)
  library(janitor)
  library(sf)
  library(sp)
  library(rgdal)
  library(rgeos)
  glimpse(config)
  df <- df %>%
    mutate(lat = as.numeric(stop_lat)) %>%
    mutate(lon = as.numeric(stop_lon)) %>%
    filter(location_type == 0) %>%
    glimpse()
  sf <- st_as_sf(df, coords = c("lon", "lat"), crs = 4326, remove=FALSE)
  dsn <- sprintf("%s/%s.Rds", josmDir, "stops")
  carp("dsn: %s", dsn)
  saveRDS(sf, file=dsn)
  dsn <- sprintf("%s/%s.geojson", josmDir, "stops")
  carp("dsn: %s", dsn)
  st_write(sf, dsn, delete_dsn = TRUE)
  dsn <- sprintf("%s/%s.gpx", josmDir, "stops")
  carp("dsn: %s", dsn)
  spdf <- as_Spatial(sf)
  writeOGR(spdf, dsn, layer="stops", driver="GPX", dataset_options="GPX_USE_EXTENSIONS=yes", overwrite_layer=TRUE, delete_dsn = TRUE)
  return(invisible(sf))
}
#
# pour les trips : le lien route shape
gtfs_trips_tt <- function(tt) {
  carp()
#  plot(tt);  return()
  glimpse(tt$trips)
  ids <- tt$trips %>%
    dplyr::select(route_id, shape_id) %>%
    distinct() %>%
    arrange(route_id) %>%
    left_join(tt$routes) %>%
    View()
}
remove.BOM <- function(x) { colnames(x)[1] = substring(colnames(x)[1], 4) }
# lecture des routes, données GTFS
gtfs_routes <- function() {
  dsn <- sprintf("%s/routes.txt", gtfsDir)
  carp("dsn: %s", dsn)
  df <- read.table(dsn, header=TRUE, sep=",", blank.lines.skip = TRUE, stringsAsFactors=FALSE, quote='"', encoding="UTF-8")
#  df$route_id <- sprintf("%04d", df$route_id)
  print(head(df))
  carp("nrow : %d", nrow(df))
  return(invisible(df))
}
#
# lecture du tracé des lignes, données GTFS
gtfs_shapes <- function() {
  dsn <- sprintf("%s/shapes.txt", gtfsDir)
  carp('dsn: %s', dsn)
  df <- read.table(dsn, header=TRUE, sep=",", blank.lines.skip = TRUE, stringsAsFactors=FALSE, quote='"', encoding="UTF-8")
  carp("nrow : %d", nrow(df))
  return(invisible(df))
}
#
# lecture des stop_times, données GTFS
gtfs_stop_times <- function() {
  if ( ! exists("stop_times.df") ) {
    dsn <- sprintf("%s/stop_times.txt", gtfsDir)
    df <- read.table(dsn, header=TRUE, sep=",", blank.lines.skip = TRUE, stringsAsFactors=FALSE, quote='"', encoding="UTF-8")
    print(head(df))
    carp("nrow : %d", nrow(df))
    stop_times.df <<- df
  }
  return(invisible(stop_times.df))
}
#
# lecture des stops, données GTFS
gtfs_stops <- function() {
  library(tidyverse)
  dsn <- sprintf("%s/stops.txt", gtfsDir)
  carp('dsn: %s', dsn)
  df <- read.table(dsn, header=TRUE, sep=",", blank.lines.skip = TRUE, stringsAsFactors=FALSE, quote='"', encoding="UTF-8", colClasses=c("character"))
  colnames(df)[1] <- 'stop_id'
  return(invisible(df))
}
# vérification du format des données stops
gtfs_stops_verif <- function() {
  df <- gtfs_stops() %>%
    filter(stop_lat != '0') %>%
    mutate(lat = as.numeric(stop_lat)) %>%
    mutate(lon = as.numeric(stop_lon))
  return(invisible(df))
}
#
# lecture des trips, données GTFS
gtfs_trips <- function() {
  dsn <- sprintf("%s/trips.txt", gtfsDir)
  df <- read.table(dsn, header=TRUE, sep=",", blank.lines.skip = TRUE, stringsAsFactors=FALSE, quote='"', encoding="UTF-8")
  carp("nrow : %d dsn: %s", nrow(df), dsn)
  return(invisible(df))
}
# vérification du format des données shapes
gtfs_shapes_verif <- function(df) {
#  df <- gtfs_shapes()
  df$lat <- as.numeric(df$shape_pt_lat)
  df$lon <- as.numeric(df$shape_pt_lon)
  inconnu.df <- subset(df, is.na(df$lat))
  if ( nrow(inconnu.df) > 0 ) {
    carp("lat invalide nb: %d", nrow(inconnu.df))
    print(head(inconnu.df))
  }
  df <- subset(df, ! is.na(df$lat))
  inconnu.df <- subset(df, is.na(df$lon))
  if ( nrow(inconnu.df) > 0 ) {
    carp("lon invalide nb: %d", nrow(inconnu.df))
    print(head(inconnu.df))
  }
  df <- subset(df, ! is.na(df$lon))
  carp("nrow : %d", nrow(df))
  return(invisible(df))
}
#
# conversion des shapes d'un gtfs en un fichier par shape
#
gtfs_shapes_sf <- function(df) {
  library(tidyverse)
  library(sf)
  library(sp)
  library(rgdal)
  library(rgeos)
  glimpse(config)
  df <- df %>%
    mutate(lat = as.numeric(shape_pt_lat)) %>%
    mutate(lon = as.numeric(shape_pt_lon)) %>%
    glimpse()
  shapes <- sort(unique(df$shape_id))
  for (shape in shapes) {
    carp("shape: %s", shape)
    df1 <- df %>%
      filter(shape_id == shape)
    txt_gtfs_shape_sf(shape, df1)
  }
}
# conversion d'un shape d'un gtfs
gtfs_shape_sf <- function(shape, df) {
  m <- as.matrix(df[order(df$shape_pt_sequence), c("lon", "lat")])
  sfc <- sf::st_linestring(m) %>%
    st_sfc(crs = 4326)
  nc <- st_sf(shape_id = shape, geometry = sfc) %>%
    glimpse()
  dsn <- sprintf("%s/shape_%s.geojson", josmDir, shape)
  carp("dsn: %s", dsn)
  st_write(nc, dsn, delete_dsn = TRUE)
  dsn <- sprintf("%s/shape_%s.gpx", josmDir, shape)
  carp("dsn: %s", dsn)
  spdf <- as_Spatial(nc)
  writeOGR(spdf, dsn, layer="shape", driver="GPX", dataset_options="GPX_USE_EXTENSIONS=yes", overwrite_layer=TRUE, delete_dsn = TRUE)
}
#
# conversion des tracés shape en kml
# source("geo/scripts/keolis.R");gtfs_shapes2ogr()
gtfs_shapes2ogr <- function(df, reseau_dir) {
  library(sp)
  library(rgdal)
  df$lat <- as.numeric(df$shape_pt_lat)
  df$lon <- as.numeric(df$shape_pt_lon)
  inconnu.df <- subset(df, is.na(df$lat))
  if ( nrow(inconnu.df) > 0 ) {
    carp("lat invalide nb: %d", nrow(inconnu.df))
    print(head(inconnu.df))
  }
  df <- subset(df, ! is.na(df$lat))
  inconnu.df <- subset(df, is.na(df$lon))
  if ( nrow(inconnu.df) > 0 ) {
    carp("lon invalide nb: %d", nrow(inconnu.df))
    print(head(inconnu.df))
  }
  df <- subset(df, ! is.na(df$lon))
  carp("nrow: %d", nrow(df))
  coordinates(df) = ~ lon + lat
  ids <- unique(df$shape_id)
  liste <- list()
  for ( i in 1:length(ids) ) {
    id <- ids[i]
#    print(sprintf("shapes() i : %d, id : %s", i, id))
    df1 <- df[df$shape_id == id,]
    df1 <- df1[with(df1, order(df1$shape_pt_sequence)), ]
#    print(head(df1))
#    plot(df1)
    l <- Lines(Line(coordinates(df1)), ID=id)
    liste[[i]] <- l
  }
  sl <- SpatialLines(liste)
  spdf <- SpatialLinesDataFrame(sl, data.frame(name=ids), match.ID = FALSE)
  proj4string(spdf) <- CRS("+init=epsg:4326")
#  plot(spdf)
  dsn <- sprintf("%s/shapes.kml", reseau_dir)
  writeOGR(spdf, dsn, layer="ligne", driver="KML", overwrite_layer=TRUE)
  carp("dsn: %s", dsn)
  dsn <- sprintf("%s/shapes.geojson", reseau_dir)
  writeOGR(spdf, dsn, layer="ligne", driver="GeoJSON", overwrite_layer=TRUE)
  carp("dsn: %s", dsn)
  dsn <- sprintf("%s/shapes.gpx", reseau_dir)
  writeOGR(spdf, dsn, layer="shape", driver="GPX", dataset_options="GPX_USE_EXTENSIONS=yes", overwrite_layer=TRUE, delete_dsn = TRUE)
  carp("dsn: %s", dsn)
}
#
# détermination pour les voyages (trips) du parcours (shape) et des arrêts (stops)
gtfs_trips_stops_v1 <- function(agency='ILLENOO') {
  library(tidyverse)
  df <- gtfs_stop_times()
  df1 <- df %>%
    group_by(trip_id) %>%
    arrange(stop_sequence) %>%
    summarise(nb=n(), depart=first(stop_id), arrivee=last(stop_id), arrets=paste(stop_id, collapse = ";"))
  trips.df <- gtfs_trips()
  trips.df <- trips.df[, c("trip_id", "shape_id")]
  df1 <- merge(df1, trips.df, by.x="trip_id", by.y="trip_id", all.x=TRUE, all.y=FALSE)
  print(head(df1))
  df2 <- df1 %>%
    group_by(shape_id, arrets) %>%
    summarise(nb=n())
  print(head(df2))
  carp("trips_stops() df2 nrow : %d", nrow(df2))
  df3 <- df2 %>%
    group_by(shape_id) %>%
    summarise(nb=n())
  df3 <- df3[df3$nb > 1, ]
  options(tibble.width = Inf)
  print(head(df3))
  dsn <- sprintf("%s/trips_stops.csv", odDir)
  write.csv(df2, file=dsn, row.names=FALSE, na="", quote = FALSE, fileEncoding = "UTF-8")
#
# les voyages avec le même parcours
  carp("df3 nrow : %d", nrow(df3))
  print(df2[df2$shape_id %in% df3$shape_id, ])
}
#
# détermination pour les voyages (trips) du parcours (shape) et des arrêts (stops)
gtfs_trips_stops <- function() {
  library(tidyverse)
  df <- gtfs_file_lire('stop_times')
  carp('ajout nom des stops')
  stops.df <- gtfs_file_lire('stops')
  df1 <- df %>%
    left_join(stops.df, c("stop_id"="stop_id")) %>%
    glimpse()
  carp('determination depart, arrivee, et arrets')
  df2 <- df1 %>%
    group_by(trip_id) %>%
    arrange(stop_sequence) %>%
    summarise(nb=n(), depart=first(stop_name), arrivee=last(stop_name), arrets=paste(stop_name, collapse = ";")) %>%
    glimpse()
  carp('ajout voyage')
  trips.df <- gtfs_file_lire('trips')
  df3 <- df2 %>%
    left_join(trips.df, c("trip_id"="trip_id")) %>%
    glimpse() %>%
    print(10)
  carp('ajout route')
  routes.df <- gtfs_file_lire('routes')
  df4 <- df3 %>%
    left_join(routes.df, c("route_id"="route_id")) %>%
#    filter(agency_id==agency) %>%
    glimpse() %>%
    print(n=10)
  carp('parcours differents')
  df5 <- df4 %>%
    group_by(route_short_name, route_long_name, depart, arrivee, arrets, shape_id) %>%
    summarise(nb=n()) %>%
    arrange(route_short_name, route_long_name, depart, arrivee, arrets) %>%
    select(nb, route_short_name, route_long_name, depart, arrivee, arrets, shape_id) %>%
    glimpse() %>%
    print(n=20)
  dsn <- sprintf("%s/trips_stops.csv", reseau_dir)
  write.csv(df5, file=dsn, row.names=FALSE, na="", quote = FALSE, fileEncoding = "UTF-8")
  carp("dsn: %s", dsn)
  gtfs_wiki_trips_stops(df5)
}
gtfs_wiki_trips_stops <- function(df) {
  carp()
  wiki <- '
==Les trips stops==
{|class="wikitable sortable"
|-
!scope="col"| nb
!scope="col"| route_short_name
!scope="col"| route_long_name
!scope="col"| depart
!scope="col"| arrivee
!scope="col"| arrets
'
  template <- "|-
!scope='row' style='text-align:left' | {{nb}}
| {{route_short_name}}
| {{route_long_name}}
| {{depart}}
| {{arrivee}}
| {{arrets}}
"
  for ( i in 1:nrow(df) ) {
    tpl <- template
    tpl <- gtfs_template(tpl, df[i,])
    wiki <- sprintf("%s%s", wiki, tpl)
  }
  wiki <- sprintf("%s%s", wiki, '|}')
  Encoding(wiki) <- "UTF-8"
  dsn <- sprintf("%s/trips_stops_wiki.txt", reseauDir)
  f <- file(dsn, encoding="UTF-8")
  write(wiki, file = f, append = FALSE)
  carp("dsn: %s", dsn);
  return(invisible(wiki))
}
gtfs_wiki_routes <- function(df) {
  carp()
  wiki <- '
==Les routes==
{|class="wikitable sortable"
|-
!scope="col"| agency_id
!scope="col"| route_short_name
!scope="col"| route_long_name
!scope="col" class="unsortable"| route_color
!scope="col" class="unsortable"| route_text_color
'
  template <- "|-
!scope='row' style='text-align:left' | {{agency_id}}
| {{route_short_name}}
| {{route_long_name}}
| {{route_color}}
| {{route_text_color}}
"
  for ( i in 1:nrow(df) ) {
    tpl <- template
    tpl <- gtfs_template(tpl, df[i,])
    wiki <- sprintf("%s%s", wiki, tpl)
  }
  wiki <- sprintf("%s%s", wiki, '|}')
  Encoding(wiki) <- "UTF-8"
  dsn <- sprintf("%s/routes_wiki.txt", reseauDir)
  f <- file(dsn, encoding="UTF-8")
  write(wiki, file = f, append = FALSE)
  carp("dsn: %s", dsn);
  return(invisible(wiki))
}
gtfs_template <- function(tpl, df) {
  attributs <- colnames(df)
  glimpse(attributs)
  for (attribut in attributs) {
    pattern <- sprintf('@\\$%s@', attribut)
    pattern <- paste0("\\{\\{", attribut, "\\}\\}")
    v <- df[1, attribut]
#    carp("pattern: %s v: %s", pattern, v)
    tpl <- gsub(pattern, v, tpl)
  }
#  carp("tpl :%s", tpl)
  return(tpl)
}
#
# conversion en format gpx compatble osm
gtfs_gpx <- function(mtx, name) {
  library(lubridate)
  carp("début name: %s", name)
  df <- as.data.frame(mtx)
  glimpse(df)
  gpx <- '<?xml version="1.0"?>
<gpx version="1.1" creator="GDAL 3.4.1" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:ogr="http://osgeo.org/gdal" xmlns="http://www.topografix.com/GPX/1/1" xsi:schemaLocation="http://www.topografix.com/GPX/1/1 http://www.topografix.com/GPX/1/1/gpx.xsd">
'
  gpx <- append(gpx, '<trk>')
  name <- sprintf('<name>%s</name>', name)
  gpx <- append(gpx, name)

  gpx <- append(gpx, '<trkseg>')
  datetime <- ymd("2021/11/01")
  for(i in 1:nrow(df)) {
    x <- datetime + minutes(i)
    trkpt <- sprintf('  <trkpt lat="%s" lon="%s">
    <ele>115.976196</ele>
    <time>%s</time>
  </trkpt>', df[i, "lat"], df[i, "lon"], x)
    gpx <- append(gpx, trkpt)
  }
  gpx <- append(gpx, '</trkseg>')
  gpx <- append(gpx, '</trk>')
  gpx <- append(gpx, '</gpx>')
  return(invisible(gpx))
}