# <!-- coding: utf-8 -->
#
# le réseau de bus
#
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
## script pour les comparaisons
#
# distance max entre osm et le gtfs
stop_loin <- 250
# le réseau est configuré en variable globale dans transport.R
#
# source("geo/scripts/transport.R");diff_jour()
diff_jour <- function(force = TRUE) {
  library(tidyverse)
  library(rio)
  library(sf)
  carp()
#  tidytransit_jour(force = force)
  tidytransit_routes_stops(force = FALSE)
  diff_routemasters(force = force)
  gtfs2osm_routes_stops(force = force)
  diff_routes_shapes_tags(force = force)
}
#
# source("geo/scripts/transport.R");diff_routes()
# reseau=lorient
# perl scripts/keolis.pl --DEBUG 1 --DEBUG_GET 1 reseau $reseau gtfsosm_osm_stops
# perl scripts/keolis.pl --DEBUG 1 --DEBUG_GET 1 reseau $reseau gtfsosm_osm_routes
# perl scripts/keolis.pl --DEBUG 1 --DEBUG_GET 1 reseau $reseau gtfs2osm_routes_create
# perl scripts/keolis.pl --DEBUG 1 --DEBUG_GET 1 reseau $reseau gtfsosm_diff_routes
#
# source("geo/scripts/transport.R");diff_routes()
diff_routes <- function(force = TRUE, OsmChange = FALSE) {
  library(tidyverse)
  library(rio)
  library(sf)
  carp()
  Wiki <<- FALSE
#  gtfs2osm_jour(force = force)
  diff_relations_route_bus(force = force, OsmChange = OsmChange)
#  diff_relations_route_tags(force = force)
  if( Config[1, "shapes"] == "TRUE" ) {
#    gtfs2osm_routes_shapes_stops(force = force)
  }
}
#
# source("geo/scripts/transport.R");diff_routemasters()
diff_routemasters <- function(force = TRUE, OsmChange = FALSE) {
  library(tidyverse)
  library(rio)
  library(sf)
  carp()
  Wiki <<- FALSE
  gtfs2osm_jour_routemasters(force = force)
  diff_relations_routemaster_bus(force = force, OsmChange = OsmChange)
}
#
# l'enchainement pour un réseau avec shapes
# source("geo/scripts/transport.R");diff_toto(force = FALSE)
diff_toto <- function(force = TRUE) {
  library(tidyverse)
  library(rio)
  library(sf)
  carp()
  tidytransit_shapes_stops()
  tidytransit_routes_shapes_stops()
  gtfs2osm_relations_route_stops(force_osm = force)
  if( Config[1, "shapes"] == "TRUE" ) {
    gtfs2osm_relations_route_shape_stops(force = force)
    diff_routes_shapes_tags(force = force)
  } else {
    diff_routes_tags(force = force)
  }
  return(invisible())
}
#
# l'enchainement pour les stops
# source("geo/scripts/transport.R");diff_toto(force = FALSE)
diff_toto_stops <- function(force = TRUE) {
  library(tidyverse)
  library(rio)
  library(sf)
  carp()
  tt <- tidytransit_zip_lire()
  glimpse(tt$stops)
  tidytransit_stops_sf()
  diff_stops(force = force)
  return(invisible())
}
#
# pour les stops
# source("geo/scripts/transport.R");diff_stops(force_osm = TRUE, OsmChange = FALSE)
diff_stops <- function(force_osm = TRUE, OsmChange = FALSE) {
  carp()
  library(sf)
  library(tidyverse)
  carp("début")
  OsmChange <<- OsmChange
#
# les arrêts osm avec le tag k_ref
  osm.df <- overpass_get(query = "bus_stop_kref", format = "csv", force = force_osm) %>%
    rename(k_ref = Config[1, 'k_ref']) %>%
    mutate(k_ref = sprintf("%s", k_ref)) %>%
    glimpse()
  osm.sf <- st_as_sf(osm.df, coords = c("@lon", "@lat"), crs = 4326, remove = FALSE)
#
  carp("les arrêts avec référence en double")
  df11 <- osm.df %>%
    group_by(k_ref) %>%
    summarize(nb = n()) %>%
    filter(nb > 1) %>%
    glimpse()
  if (nrow(df11) > 0) {
    df12 <- osm.df %>%
      filter(k_ref %in% df11$k_ref) %>%
      arrange(k_ref, desc(`@timestamp`)) %>%
      clean_names() %>%
      dplyr::select(type, id, version, timestamp, user, name, highway, public_transport,k_ref)

    transport_df2html(df12, titre = "les arrêts avec référence en double")
    confess("****** références en double: %s", nrow(df11))
    df12 <- osm.df %>%
      filter(k_ref %in% df11$k_ref) %>%
      group_by(k_ref) %>%
      arrange(desc(`@timestamp`)) %>%
      filter(row_number() > 1)
    misc.ecrire(df12, "diff_stops_absents_gtfs", dir = transportDir)
    diff_stops_absents_gtfs(force = force)
  }
  if (nrow(osm.sf) > 0) {
    osm.sf <- osm.sf %>%
      st_transform(2154)
    Encoding(osm.sf$name) <- "UTF-8"
    cc <- sf::st_coordinates(osm.sf)
    osm.sf <- bind_cols(osm.sf, cc) %>%
      glimpse()
  }
#
  carp("les arrêts du gtfs")
  gtfs.sf <- tidytransit_stops_sf() %>%
    mutate(gtfs = "gtfs")
  gtfs.sf <- gtfs.sf %>%
    st_transform(2154)
  cc <- sf::st_coordinates(gtfs.sf)
  gtfs.sf <- bind_cols(gtfs.sf, cc) %>%
    glimpse()
  if (Reseau == "aleop44_") {
    gtfs.sf %>%
      filter(stop_id == "44HAUTalleA") %>%
      glimpse()
    stop("µµµµµµµµµµµµµµµµµµµµµµ")
  }
  if (Reseau == "vannes") {
    gtfs.sf$stop_id <- gtfs.sf$stop_code
  }
# modif juin 2024
#  if (Reseau == "rennes") {
#    gtfs.sf$stop_id <- gtfs.sf$stop_code
#  }
  carp("jointure stop_id # %s", Config[1, 'k_ref'])
  df <- dplyr::full_join(gtfs.sf %>%  st_drop_geometry(), osm.sf %>%  st_drop_geometry(), by=c('stop_id' = 'k_ref')) %>%
    arrange(stop_id) %>%
    glimpse()
  carp("les stops absents du gtfs")
  df1 <- df %>%
    filter(! is.na(name)) %>%
    filter(is.na(stop_name))
  if (nrow(df1) > 0) {
    carp("##### les stops absents du gtfs: %s/%s", nrow(df1), nrow(osm.sf))
#    return(invisible())
#    stop("****")
    misc_print(df1[, c("stop_id", "name", "@id")])
    misc.ecrire(df1, "diff_stops_absents_gtfs", dir = transportDir)
    diff_stops_absents_gtfs(force = force)
    confess("***** les stops absents du gtfs")
  }
  df3 <- df %>%
    filter(! is.na(stop_name)) %>%
    filter(! is.na(name)) %>%
    glimpse()
  carp("différence de nom")
  df11 <- df3 %>%
    filter(stop_name != name) %>%
    glimpse()
#  misc_print(df11[, c("stop_id", "stop_name", "name")])
  if (nrow(osm.sf) > 0) {
    carp("***** les stops trop loin: %s", stop_loin)
    df4 <- df3 %>%
      mutate(distance = round(sqrt((X.x - X.y)^2 + (Y.x - Y.y)^2), 0))
    df4 <- df4 %>%
      filter(distance > stop_loin)
    if (nrow(df4) > 0) {
      carp("différence de position")
      misc_print(df4[, c("@id", "stop_id", "stop_name", "name", "distance")])
      misc.ecrire(df4, "diff_stops_absents_gtfs", dir = transportDir)
      diff_stops_absents_gtfs(force = force)
      confess("***** les stops trop loin: %s", stop_loin)
    }
  }
    df14 <- df %>%
      filter(grepl("Maison du Département", stop_name)) %>%
      glimpse()
#    stop("****")
#
# on a enfin les stops du gtfs qui ne sont pas configurés dans osm
  df2 <- df %>%
    filter(! is.na(stop_id)) %>%
    filter(is.na(name))
  if (nrow(df2) > 0) {
    carp("les stops absents d'osm nb: %s", nrow(df2))
#    stop("$$$$$$$$$$$")
    misc_print(df2[, c("stop_id", "stop_name")])
    nc2 <- gtfs.sf %>%
      filter(stop_id %in% df2$stop_id)
    transport_ecrire(nc2, "diff_stops_absents_osm")
    dsn <- sprintf("%s/%s.geojson", transportDir, "diff_stops_absents_osm")
    st_write(st_transform(nc2, 4326), dsn, delete_dsn = TRUE, driver = "GeoJSON")
    carp("dsn: %s", dsn)
    rc <- diff_stops_absents_osm(force_osm = force_osm, OsmChange = OsmChange)
    if (rc == FALSE) {
      diff_stops_creation_gtfs(rds = "diff_stops_absents_osm")
      confess("crée les stops avec level0: %s", nrow(df2))
    }
  }
  carp("c'est tout bon")
#
# modification du name si tout en majuscule
  df12 <- df11 %>%
    dplyr::select(id  = "@id", type = "@type", name, stop_name) %>%
    mutate(NAME = toupper(name)) %>%
    mutate(STOP_NAME = toupper(stop_name)) %>%
    filter (STOP_NAME != stop_name) %>%
    filter(NAME == name) %>%
    glimpse()
  if (nrow(df12) == 0) {
    carp("pas de majuscule")
    return(invisible())
  }
  osm <- ""
  osm.list <- list()
  for (i12 in 1:nrow(df12)) {
    tags.df <- tribble(
      ~name, ~value,
      "name", df12[[i12, "stop_name"]]
    )
    o <- osmchange_object_modify_tags(id = df12[[i12, "id"]] , type = df12[[i12, "type"]], tags = tags.df, Change = FALSE)
    osm <- c(osm, o)
#    break
#    stop("*****")
  }
  osm <- paste(osm, "\n", collapse = "")
#  writeLines(osm);  stop("****")
  changeset_id <- osmapi_put("modify", text = osm)
  confess("######## %s", nrow(df12))
}
#
# pour ajouter les tags gtfs
# source("geo/scripts/transport.R");diff_stops_tags_gtfs(force_osm = FALSE, OsmChange = FALSE)
diff_stops_tags_gtfs <- function(force_osm = TRUE, OsmChange = FALSE) {
  carp()
  library(sf)
  library(tidyverse)
  library(janitor)
  carp("début")
  OsmChange <<- OsmChange
#
# les arrêts osm avec le tag k_ref
  gtfs_name <- sprintf("gtfs:stop_name:%s", Config[1, 'network'])
  osm.df <- overpass_get(query = "bus_stop_kref_gtfs", format = "csv", force = force_osm) %>%
    rename(k_ref = Config[1, 'k_ref']) %>%
    mutate(k_ref = sprintf("%s", k_ref)) %>%
    glimpse() %>%
    rename(gtfs_name = !!gtfs_name) %>%
#    clean_names() %>%
    glimpse()
  stops.df <- tidytransit_lire()$stops %>%
    dplyr::select(stop_id, stop_name) %>%
    glimpse()
  df1 <- osm.df %>%
    left_join(stops.df, by = c("k_ref" = "stop_id")) %>%
    filter(! is.na(stop_name)) %>%
    filter(gtfs_name != stop_name) %>%
#    filter(name == "") %>%
    glimpse()
  osm <- ""
  osm.list <- list()
  for (i1 in 1:nrow(df1)) {
    tags.df <- tribble(
      ~name, ~value,
#      "name", df1[[i1, "stop_name"]],
      gtfs_name, df1[[i1, "stop_name"]]
    )
    o <- osmchange_object_modify_tags(id = df1[[i1, "@id"]] , type = df1[[i1, "@type"]], tags = tags.df, Change = FALSE)
    osm <- c(osm, o)
#    break
#    stop("*****")
  }
  osm <- paste(osm, "\n", collapse = "")
#  writeLines(osm);  stop("****")
  changeset_id <- osmapi_put("modify", text = osm, comment = "absence de name, ajout du name gtfs")
  carp("######## %s changeset: %s", nrow(df1), changeset_id)
}
#
# source("geo/scripts/transport.R");diff_stops_distance()
diff_stops_distance <- function(force_osm = TRUE, OsmChange = FALSE) {
  library(stringr)
  library(sf)
  OsmChange <<- OsmChange
#
# les arrêts osm avec le tag k_ref
  osm.df <- overpass_get(query = "bus_stop_kref", format = "csv", force = force_osm) %>%
    rename(k_ref = Config[1, 'k_ref']) %>%
    mutate(k_ref = sprintf("%s", k_ref)) %>%
    glimpse()
  osm.sf <- st_as_sf(osm.df, coords = c("@lon", "@lat"), crs = 4326, remove = FALSE)
  osm.sf <- osm.sf %>%
    st_transform(2154)
  cc <- sf::st_coordinates(osm.sf)
  osm.sf <- bind_cols(osm.sf, cc) %>%
    glimpse()
#
  carp("les arrêts du gtfs")
  gtfs.sf <- tidytransit_stops_sf() %>%
    mutate(gtfs = "gtfs")
  gtfs.sf <- gtfs.sf %>%
    st_transform(2154)
  cc <- sf::st_coordinates(gtfs.sf)
  gtfs.sf <- bind_cols(gtfs.sf, cc) %>%
    glimpse()
  carp("jointure")
  df1 <- dplyr::full_join(gtfs.sf %>%  st_drop_geometry(), osm.sf %>%  st_drop_geometry(), by=c('stop_id' = 'k_ref')) %>%
    filter(! is.na(stop_id)) %>%
    filter(! is.na(name)) %>%
    arrange(stop_id) %>%
    glimpse()
  carp("filtrage")
  df2 <- df1 %>%
    mutate(distance = round(sqrt((X.x - X.y)^2 + (Y.x - Y.y)^2), 0)) %>%
    filter(distance > 5) %>%
    dplyr::select(id = `@id`, type = `@type`, name, `@timestamp`, stop_lat, stop_lon, distance) %>%
    glimpse()
  stop("*******")
  if ( 1 == 2 ) {
# les issues osmose
    issues_rds <- sprintf("d:/web.var/TRANSPORT/osmose_area_issues_bretagne_1260_12.rds")
    issues.df <- readRDS(issues_rds) %>%
      dplyr::select(id = elems.2.id, type = elems.2.type) %>%
      glimpse()
    df2 <- df2 %>%
      filter(id %in% issues.df$id) %>%
      glimpse()
  }
  if (1 == 2) {
# "mes" nodes
    version_rds <- sprintf("%s/diff_stops_version.rds", varDir)
    version.df <- readRDS(version_rds) %>%
      filter(user == "mga_geo") %>%
      glimpse()
    df2 <- df2 %>%
      filter(id %in% version.df$id) %>%
      glimpse()
  }
#  stop("****")
  if (nrow(df2) == 0) {
    carp("pas de modif")
    return()
  }
  osm <- ""
  for (i2 in 1:nrow(df2)) {
    o <- osmchange_object_modify_latlon(
      id = df2[[i2, "id"]],
      type = df2[[i2, "type"]],
      lat = df2[[i2, "stop_lat"]],
      lon = df2[[i2, "stop_lon"]],
    )
    osm <- c(osm, o)
#    break
#    stop("*****")
  }
  osm <- paste(osm, "\n", collapse = "")
#  writeLines(osm);stop("****")
  changeset_id <- osmapi_put("modify", text = osm)
  confess("######## %s", nrow(df2))
}
#
# source("geo/scripts/transport.R");diff_stops_version(force_osm = FALSE)
diff_stops_version <- function(force_osm = TRUE) {
  library(stringr)
  library(xml2)
#
# les arrêts osm avec le tag k_ref
  df1 <- overpass_get(query = "bus_stop_kref", format = "csv", force = force_osm) %>%
    rename(k_ref = Config[1, 'k_ref']) %>%
    mutate(k_ref = sprintf("%s", k_ref)) %>%
    glimpse()
  attributs.df <- data.frame()
  for (i1 in 1:nrow(df1)) {
    txt <- osmapi_get_object_version_xml(
      id = df1[[i1, "@id"]],
      type = df1[[i1, "@type"]]
    )
    xml <<- xml2::read_xml(txt)
    df2 <- xml_attrs(xml) %>%
      as.list() %>%
      as_tibble()
    attributs.df <- bind_rows(attributs.df, df2)
#    break
  }
  glimpse(attributs.df)
  version_rds <- sprintf("%s/diff_stops_version.rds", varDir)
  saveRDS(attributs.df, version_rds)
}
#
# pour trouver d'éventuels stops osm pour les stops solitaires du gtfs
#
# source("geo/scripts/transport.R");diff_stops_absents_osm(force_osm = FALSE)
diff_stops_absents_osm <- function(force_osm = TRUE, OsmChange = FALSE) {
  library(sf)
  library(tidyverse)
  library(janitor)
  carp()
  OsmChange <<- OsmChange
  Debug <<- FALSE
  osm_name <- "Maison du Département"
  osm_id <- "6680528465"
  carp("les arrêts du gtfs")
#  gtfs.sf <- tidytransit_stops_sf() %>%
  gtfs.sf <- transport_lire("diff_stops_absents_osm") %>%
#    filter(grepl("Village Collec", stop_name)) %>%
    st_transform(2154) %>%
    glimpse()
  bbox <- gtfs.sf %>%
    st_buffer(5000) %>%
    st_transform(4326) %>%
    st_bbox() %>%
    as.vector() %>%
    glimpse()
  Config_bbox <<- sprintf("%0.5f,%0.5f,%0.5f,%0.5f", bbox[2], bbox[1], bbox[4], bbox[3])
#  stop("****")
  carp("interrogation overpass")
  osm.df <- overpass_get(query = "bus_stop_bbox", format = "csv", force = force_osm) %>%
    rename(k_ref = Config[1, 'k_ref']) %>%
    mutate(k_ref = sprintf("%s", k_ref)) %>%
    filter(highway == "bus_stop") %>%
    filter(grepl("platform", public_transport)) %>%
    filter(k_ref == "") %>%
    glimpse()
  df11 <- osm.df %>%
#    filter(grepl(osm_name, name)) %>%
    filter(osm_id == `@id`) %>%
    glimpse()
  if (Debug == TRUE & nrow(df11) == 0) {
    stop("llllllllll")
  }
  df1 <- osm.df %>%
    group_by(k_ref) %>%
    summarize(nb = n()) %>%
    adorn_totals()
  misc_print(df1)
#  stop("jjjjjjj")
  osm.sf <- st_as_sf(osm.df, coords = c("@lon", "@lat"), crs = 4326, remove = FALSE) %>%
    st_transform(2154)
  df1 <- st_proches(gtfs.sf, osm.sf) %>%
    st_drop_geometry() %>%
    arrange(dist)
  df1 <- df1 %>%
    filter(dist < 150)  %>%
    glimpse()
#  stop("$$$$$$$$$$$")
  df2 <- df1 %>%
    mutate(dist = sprintf("%0.1f", dist)) %>%
    filter(k_ref == "") %>%
    dplyr::select(stop_id, stop_name, id = X.id, name, k_ref, dist) %>%
    arrange(stop_name) %>%
    glimpse()
  misc_print(df2)
  dsn <- sprintf("%s/diff_stops_absents_osm.csv", gtfsDir)
  df_ecrire(df2, dsn)
  carp("dsn: %s", dsn)
#  stop("******")
  dsn <- sprintf("%s/gtfs.zip", gtfsDir)
  carp("dsn: %s", dsn)
  gtfs <- strftime(file.info(dsn)$mtime, format="%d.%m.%Y")
  wiki <- sprintf('==Les stops gtfs absents %s==
{|class="wikitable sortable"
|-', gtfs)
  page <- sprintf("User:Mga_geo/Transports_publics/%s/%s", Config["wiki"], "diff_stops_absents_osm") %>%
    glimpse()
  wiki <- wiki_df2table(df2)
#  page <- append(page, wiki)
#  Wiki <<- TRUE
  wiki_page_init(page = page, article = wiki, force = TRUE)
  df1 <- df1 %>%
    filter(dist < 150) %>%
    arrange(dist) %>%
    glimpse()
#  stop("****")
  if (nrow(df1) == 0) {
    misc_ecrire(gtfs.sf, "diff_stops_creation_gtfs", dir = transportDir)
    diff_stops_creation_gtfs(rds = "diff_stops_creation_gtfs")
    carp("***** pas de rapprochement distance")
    return(FALSE)
  }
  osm <- ""
  osm.list <- list()
  for (i1 in 1:nrow(df1)) {
#  for (i1 in 1:3) {
    kref <- df1[[i1, "stop_id"]]
    id <- sprintf("%s", df1[[i1, "X.id"]])
    carp("i1: %s/%s %s %s", i1, nrow(df1), kref, id)
    if (id %in% names(osm.list)) {
      carp("id: %s déjà", id)
      next
    }
    osm.list[[id]] <- i1
    tags.df <- tribble(
      ~name, ~value,
      Config_k_ref, kref
    )
    o <- osmchange_object_modify_tags(id = df1[[i1, "X.id"]] , type = df1[[i1, "X.type"]], tags = tags.df, Change = FALSE)
    osm <- c(osm, o)
#    stop("*****")
  }
#  writeLines(paste(names(osm.list), "\n", collapse = ""));  stop("****")
  osm <- paste(osm, "\n", collapse = "")
  changeset_id <- osmapi_put("modify", text = osm)
  confess("######## %s", nrow(df1))
}
#
# pour trouver d'éventuels stops osm pour les stops solitaires du gtfs
# en rapprochement par commune et sur nom
# source("geo/scripts/transport.R");diff_stops_absents_osm_city(force_osm = FALSE)
diff_stops_absents_osm_city <- function(force_osm = TRUE, OsmChange = FALSE) {
  library(sf)
  library(tidyverse)
  library(janitor)
  library(geosphere)
  carp()
  OsmChange <<- OsmChange
  carp("les arrêts du gtfs")
  gtfs.sf <- transport_lire("diff_stops_absents_osm") %>%
    st_transform(2154) %>%
    glimpse()
  gtfs1.sf <- gtfs.sf %>%
    filter(dept == "44") %>%
#    glimpse() %>%
    dplyr::select(stop_id, stop_name, stop_lat, stop_lon, city, dept, insee) %>%
    glimpse()
  gtfs.df <- gtfs1.sf %>%
    st_drop_geometry() %>%
    glimpse()
  df11 <- transport_stops_conflate(gtfs.df, fic = "diff_stops_absents_osm")
  df11 <- st_stop2gpx(gtfs.df, fic = "diff_stops_absents_osm")
#  stop("*****")
#
# les platforms osm et le tag k_ref
  osm.df <- overpass_get(query = "nodes_bus_platform_area_kref", format = "csv", force = force_osm) %>%
    rename(k_ref = Config[1, 'k_ref']) %>%
    mutate(k_ref = sprintf("%s", k_ref))
  osm.sf <- st_as_sf(osm.df, coords = c("@lon", "@lat"), crs = 4326, remove = FALSE)
  osm1.sf <- osm.sf %>%
    filter(k_ref == "")
  osm2.df <- ign_osm_geocode(osm1.sf) %>%
    filter(dept == "44") %>%
    clean_names() %>%
    glimpse()
  carp("les communes du gtfs")
  communes.df <- gtfs.df %>%
    group_by(insee, city) %>%
    summarize(nb = n())
  titre <- "diff_stops_absents_osm_city"
  html <- html_entete(titre)
  for (ic in 1:nrow(communes.df)) {
    html <- html_append(html, sprintf("<h2>%s %s</h2>", communes.df[ic, "insee"], communes.df[ic, "city"]))
    html <- html_append(html, sprintf("<h3>osm</h3>"))
    df3 <- osm2.df %>%
      filter(insee == !!communes.df[[ic, "insee"]]) %>%
      arrange(name) %>%
      mutate(josm = sprintf("<a href='http://localhost:8111/load_object?objects=%s%s,relation_members=true,referrers=true' target='josm'>%s</a>", str_sub(type, 1, 1), id, name))
    names <- paste(df3$josm, ", ", collapse = "")
    html <- html_append(html, names)
    html <- html_append(html, sprintf("<h3>gtfs</h3>"))
    df4 <- gtfs.df %>%
      filter(insee == !!communes.df[[ic, "insee"]]) %>%
      arrange(stop_name) %>%
      mutate(name = sprintf("%s %s", stop_name, stop_id))
    names <- paste(df4$name, ", ", collapse = "")
    html <- html_append(html, names)
  }
  html <- html_append(html, html_pied())
#  transport_html_browse(html, titre, Exit = TRUE)
  df1 <- gtfs.df %>%
    left_join(
      osm2.df,
      by = c("insee" = "insee", "city" = "city", "stop_name" = "name"),
      relationship = "many-to-many"
    ) %>%
    filter(! is.na(id)) %>%
    mutate(distance = round(distHaversine(cbind(stop_lon, stop_lat), cbind(`lon`, `lat`)), 0)) %>%
    filter(distance < stop_loin) %>%
    group_by(stop_id) %>%
    arrange(distance) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    glimpse() %>%
    group_by(`id`) %>%
    arrange(distance) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    glimpse()
  osm <- ""
  for (i1 in 1:nrow(df1)) {
    kref <- df1[[i1, "stop_id"]]
    id <- sprintf("%s", df1[[i1, "id"]])
    tags.df <- tribble(
      ~name, ~value,
      Config_k_ref, kref
    )
    o <- osmchange_object_modify_tags(id = df1[[i1, "id"]] , type = df1[[i1, "type"]], tags = tags.df, Change = FALSE)
    osm <- c(osm, o)
#    stop("*****")
  }
  osm <- paste(osm, "\n", collapse = "")
  changeset_id <- osmapi_put("modify", text = osm)
  confess("######## %s", nrow(df1))
}
# source("geo/scripts/transport.R");diff_stops_absents_gtfs(force = FALSE)
diff_stops_absents_gtfs <- function(force = TRUE) {
  df1 <- misc.lire("diff_stops_absents_gtfs", dir = transportDir) %>%
    glimpse()
  champs.df <- tribble(
    ~osm,
    Config_k_ref
  )
  osm <- ""
  for (i1 in 1:nrow(df1)) {
    id <- df1[[i1, "@id"]]
    type <- df1[[i1, "@type"]]
    carp("i1: %s/%s id: %s", i1, nrow(df1), id)
    o <- osmchange_object_delete_tags(id = id, type = type, tags = champs.df, Change = FALSE)
    osm <- c(osm, o)
  }
  osm <- paste(osm, "\n", collapse = "")
  changeset_id <- osmapi_put("modify", text = osm)
}
#
# le fichier de création des nodes osm
#
# source("geo/scripts/transport.R");diff_stops_creation_gtfs()
diff_stops_creation_gtfs <- function(rds = "diff_stops_creation_gtfs", force = FALSE) {
  nc1 <- misc_lire(rds = "diff_stops_absents_osm", dir = transportDir)
  carp("nc1: %s", nrow(nc1))
  df1 <- nc1 %>%
    st_drop_geometry() %>%
    mutate(k_ref = Config_k_ref) %>%
    glimpse()
#
# pour essayer de trouver un nom en minuscules
  if ( 1 == 2) {
    df2 <- df1 %>%
      mutate(STOP_NAME = toupper(stop_name)) %>%
      glimpse()
    df3 <- df2 %>%
      filter(STOP_NAME != stop_name) %>%
      glimpse()
    df11 <- df2 %>%
      filter(STOP_NAME == stop_name) %>%
      glimpse()
    osm.df <- overpass_get(query = "name_area", format = "csv", force = force) %>%
      distinct(name) %>%
      mutate(NAME = toupper(name)) %>%
      filter(NAME != name) %>%
      glimpse()
    df12 <- df11 %>%
      left_join(osm.df, by = c("stop_name" = "NAME"))
    df13 <- df12 %>%
      filter(! is.na(name)) %>%
      mutate(stop_name = name) %>%
      glimpse()
    df14 <- bind_rows(df3, df13)
    df21 <- df12 %>%
      filter(is.na(name)) %>%
      mutate(stop_name = stringr::str_to_title(stop_name)) %>%
      dplyr::select(-name) %>%
      glimpse()
#  confess("df13: %s", nrow(df13))
  }
# le template level0
  dsn <- sprintf("%s/transport_level0_node.txt", cfgDir)
  carp("le template dsn: %s", dsn)
  template <- readLines(dsn)
  osm <- misc_df2tpl(df1, template)
  dsn <- sprintf("%s/transport_level0_node.txt", transportDir)
  carp("le fichier pour level0 dsn: %s", dsn)
  writeLines(osm, dsn)
  misc_scite(dsn)
}
#
# les différences de nom
#
# source("geo/scripts/transport.R");diff_stops_name_network()
diff_stops_name_network <- function(force = FALSE, force_osm = TRUE, OsmChange = TRUE) {
  OsmChange <<- OsmChange
#
# les arrêts osm avec le tag k_ref
  osm.df <- overpass_get(query = "bus_stop_kref", format = "csv", force = force_osm) %>%
    rename(k_ref = Config[1, 'k_ref']) %>%
    mutate(k_ref = sprintf("%s", k_ref)) %>%
    glimpse()
  osm.sf <- st_as_sf(osm.df, coords = c("@lon", "@lat"), crs = 4326, remove = FALSE) %>%
    glimpse()
#
  carp("les arrêts du gtfs")
  gtfs.sf <- tidytransit_stops_sf() %>%
    mutate(gtfs = "gtfs")
  gtfs.sf <- gtfs.sf %>%
    st_transform(2154)
  cc <- sf::st_coordinates(gtfs.sf)
  gtfs.sf <- bind_cols(gtfs.sf, cc) %>%
    glimpse()
  carp("jointure stop_id # %s", Config[1, 'k_ref'])
  df <- dplyr::full_join(gtfs.sf %>%  st_drop_geometry(), osm.sf %>%  st_drop_geometry(), by=c('stop_id' = 'k_ref')) %>%
    arrange(stop_id) %>%
    glimpse()
  carp("les arrêts osm sans tag name")
  df11 <- df %>%
    filter(!is.na(stop_name)) %>%
    filter(!is.na(`@id`))
  df12 <- df11 %>%
    filter(name == "") %>%
    dplyr::select(stop_id, stop_name, city, `@id`, "@type", name) %>%
    glimpse()
  if (nrow(df12) > 0) {
    osm <- ""
    for (i12 in 1:nrow(df12)) {
      tags.df <- tribble(
        ~name, ~value,
        "name", df12[[i12, "stop_name"]]
      )
      o <- osmchange_object_modify_tags(id = df12[[i12, "@id"]] , type = df12[[i12, "@type"]], tags = tags.df, Change = FALSE)
      osm <- c(osm, o)
    }
    osm <- paste(osm, "\n", collapse = "")
#     writeLines(osm);  stop("****")
    changeset_id <- osmapi_put("modify", text = osm, comment = "ajout de l'attribut name")
    confess("######## %s", nrow(df12))
  }
}

#
############################################
#
# les relations route_master
#
# comparaison à partir des routes gtfs
# source("geo/scripts/transport.R");diff_relations_routemaster_bus(force = FALSE)
diff_relations_routemaster_bus <- function(force = TRUE, OsmChange = FALSE) {
  library(tidyverse)
  library(janitor)
  carp()
  OsmChange <<- OsmChange
# osm
  carp("osm: les relations")
  doc <- overpass_get(query = "relations_routemaster_bus_network", format = "xml", force = force)
  objects <- xml2::xml_find_all(doc, "//relation")
  carp("les relations")
  osm.df <- osmapi_objects_tags(objects) %>%
    (function(.df){
      cls <- c("description", "website", "network:wikidata", "network:wikipedia")
      .df[cls[!(cls %in% colnames(.df))]] = NA
      return(.df)
    }) %>%
    glimpse()
  carp("osm: les doublons")
  df1 <- osm.df %>%
    group_by(ref) %>%
    summarize(nb = n()) %>%
    filter(nb > 1)
  if (nrow(df1) > 0) {
    glimpse(df1)
    df2 <- osm.df %>%
      filter(ref %in% df1$ref) %>%
      arrange(ref) %>%
      dplyr::select(ref, `@id`)
    misc_print(df2)
    carp("**** doublons nb: %s", nrow(df1))
  }
  carp("osm: tri par ref")
  if ("ref:network" %notin% names(osm.df)) {
    osm.df[, "ref:network"] <- osm.df[, "ref"]
  }
  osm.df <- osm.df %>%
    arrange(ref) %>%
    rename_with( ~ paste0(.x, ".osm"))
# le gtfs
  carp("gtfs: les routes")
  gtfs0.df <- misc.lire("gtfs2osm_routemasters", dir = transportDir)
  gtfs.df <- gtfs0.df %>%
    rename_with(~ paste0(.x, ".gtfs"))
  carp("osm et gtfs: fusion par ref_network")
  df1 <- osm.df %>%
    full_join(gtfs.df, by = c("ref:network.osm" = "ref:network.gtfs"))
  carp("osm et gtfs: absent du gtfs")
  df1 %>%
    filter(is.na(name.gtfs)) %>%
    glimpse()
  carp("osm et gtfs: absent d'osm")
  df2 <- df1 %>%
    filter(is.na(name.osm)) %>%
    filter(! is.na(ref.gtfs)) %>%
    glimpse()
  if (nrow(df2) > 0) {
    level0 <- ""
    for (i2 in 1:nrow(df2)) {
      dsn <- sprintf("%s/gtfs2osm_relations_routemaster_level0_%s.txt", osmDir, df2[[i2, "ref.gtfs"]])
      carp("dsn: %s", dsn)
      l <- readLines(dsn)
      level0 <- c(level0, l)
    }
    dsn <- sprintf("%s/diff_relations_routemaster_bus_level0.txt", osmDir)
    write(level0, dsn)
    carp("dsn: %s", dsn)
    carp("créer avec level0")
#    stop("****")
  }
  carp("osm et gtfs: les communs")
  df1 <- df1 %>%
    filter(!is.na(name.gtfs)) %>%
    filter(!is.na(name.osm)) %>%
    mutate(`ref:network.gtfs` = `ref:network.osm`) %>%
    select(order(colnames(.)))
  if (nrow(df1) > 0) {
    diff_objects_tags(df1, type = "master")
  }
}
#
############################################
#
# les relations route
#
#
# comparaison entre les routes osm et les routes gtfs
# source("geo/scripts/transport.R");diff_relations_route_bus(cmp = "members")
diff_relations_route_bus <- function(cmp = "tags", force = TRUE, OsmChange = FALSE) {
  library(tidyverse)
  library(janitor)
  OsmChange <<- OsmChange
  carp("osm: les relations")
  if ( Config_shapes == TRUE) {
    cle_gtfs <- "shape_id"
    cle_osm <- "gtfs:shape_id"
    dsn <- sprintf("%s/%s", transportDir, "gtfs2osm_routes_shapes_stops.csv")
    dsn <- sprintf("%s/%s", transportDir, "gtfs2osm_relations_route.csv")
    dsn <- sprintf("%s/%s", transportDir, "gtfs2osm_routes_stops.csv")
    dsn <- sprintf("%s/%s", transportDir, "gtfs2osm_relations_route_shape_stops.csv")
  } else {
    cle_gtfs <- "ref:network"
    cle_osm <- "ref:network"
    dsn <- sprintf("%s/%s", transportDir, "gtfs2osm_relations_route.csv")
    dsn <- sprintf("%s/%s", transportDir, "gtfs2osm_routes_stops.csv")
  }
  osm.df <- overpass_get(query = "relations_route_bus_network", format = "csv", force = force) %>%
    mutate(ref = sprintf("%s", ref)) %>%
    mutate(cle = .data[[cle_osm]]) %>%
    mutate(cle = sprintf("%s", cle))
  osm_names <- names(osm.df)
  df21 <- osm.df %>%
    filter(cle == "")
  if (nrow(df21) > 0) {
    df22 <- df21 %>%
      dplyr::select(cle, `@id`, `@type`, ref, "ref:network", name, from, to) %>%
      arrange(ref, "ref:network") %>%
      glimpse()
    carp("nb routes sans cle: %s", nrow(df21))
    transport_df2html(df22, titre = "diff_relations_route_bus")
    stop("*****")
    osm.df <- osm.df %>%
      filter(cle != "")
  }
  df1 <- osm.df %>%
    group_by(cle) %>%
    summarize(nb = n()) %>%
    filter(nb > 1)
  if (nrow(df1) > 0) {
    df2 <- osm.df %>%
      filter(cle %in% df1$cle) %>%
      arrange(cle) %>%
      dplyr::select(cle, `@id`, `@type`, ref, "ref:network", "gtfs:shape_id", name, from)
    carp("**** doublons nb: %s", nrow(df1))
    transport_df2html(df2, titre = "diff_relations_route_bus")
    stop("*****")
  }
  carp("osm: tri par ref")
#  if ("ref:network" %notin% names(osm.df)) {
#    osm.df[, "ref:network"] <- osm.df[, "ref"]
#  }
  if (1 == 2) {
    df0 <- osm.df %>%
      filter(! grepl("^\\d\\d\\d\\d", `gtfs:shape_id`)) %>%
      glimpse()
    if (nrow(df0) > 0) {
      confess("**** erreur osm star")
    }
  }
  osm.df <- osm.df %>%
    arrange(ref) %>%
    rename_with( ~ paste0(.x, ".osm"))
# le gtfs
  carp("gtfs: les routes avec les stops %s", dsn)
  gtfs.df <- fread(dsn, encoding = "UTF-8") %>%
    rename(`ref:network` = ref_network) %>%
    mutate(cle = .data[[cle_gtfs]]) %>%
    mutate(cle = sprintf("%s", cle)) %>%
    filter(cle != "") %>%
    filter(Ordre == 1) %>%
    dplyr::select(-`public_transport:version`, -route, -type)
  if (1 == 1) {
    df0 <- gtfs.df %>%
      filter(! grepl("^\\d\\d\\d\\d", `shape_id`)) %>%
      glimpse()
    if (nrow(df0) > 0) {
      carp("**** erreur gtfs star")
    }
  }
  df1 <- gtfs.df %>%
    group_by(cle) %>%
    summarize(nb = n()) %>%
    filter(nb > 1)
  if (nrow(df1) > 0) {
    df2 <- gtfs.df %>%
      filter(cle %in% df1$cle) %>%
      arrange(cle) %>%
      glimpse() %>%
      dplyr::select(cle, stops)
    carp("**** doublons nb: %s", nrow(df1))
#    misc_print(df2)
    transport_df2html(df2, titre = "diff_relations_route_bus")
    stop("*****")
  }
  gtfs.df <- gtfs.df %>%
    rename_with( ~ paste0(.x, ".gtfs")) %>%
    glimpse()
  gtfs_names <- names(gtfs.df)
  carp("osm et gtfs: fusion par cle")
  ko_fusion <- 0
  df1 <- osm.df %>%
    full_join(gtfs.df, by = c("cle.osm" = "cle.gtfs")) %>%
    glimpse()
  carp("osm et gtfs: absent du gtfs")
  df11 <- df1 %>%
    filter(is.na(name.gtfs))
  if (nrow(df11) > 0) {
    carp("osm et gtfs: absent du gtfs nb: %s", nrow(df11))
    df11 <- df11 %>%
      dplyr::select(ends_with(".osm")) %>%
      rename_with(~str_remove(., ".osm")) %>%
      dplyr::select("ref:network", `@id`, `@type`,  "gtfs:shape_id", name) %>%
      arrange("ref:network") %>%
      glimpse()
    transport_df2html(df11, titre = "diff_relations_route_bus_absent_gts")
    ko_fusion <- ko_fusion + 1
  }
  carp("osm et gtfs: absent d'osm")
  df12 <- df1 %>%
    filter(is.na(name.osm)) %>%
    filter(Ordre.gtfs == 1)

  if (nrow(df12) > 0) {
    df12 <- df12 %>%
      dplyr::select(ends_with(".gtfs")) %>%
      rename_with(~str_remove(., ".gtfs")) %>%
      dplyr::select("ref:network", "shape_id", name) %>%
      arrange("ref:network") %>%
      glimpse()
    carp("osm et gtfs: absent d'osm nb: %s", nrow(df12))
    html_df2fic(df12, titre = "diff_relations_route_bus_absent_osm")
    ko_fusion <- ko_fusion + 2
  }
  if (ko_fusion == 3) {
    df13 <- df11 %>%
      full_join(df12, by = c("ref:network")) %>%
#      filter(! is.na(shape_id)) %>%
#      filter(! is.na(`gtfs:shape_id`)) %>%
      arrange("ref:network") %>%
#      mutate(type = "relation") %>%
      glimpse()
    misc_print(df13)
  }
  if (ko_fusion != 0) {
#    confess("fusion osm gtfs %s", ko_fusion)
  }
  carp("osm et gtfs: les communs")
  df2 <- df1 %>%
    filter(!is.na(name.gtfs)) %>%
    filter(!is.na(name.osm)) %>%
    select(order(colnames(.)))
  carp("osm et gtfs: tri par ref")
  df2 <- df2 %>%
    arrange(`ref:network.osm`) %>%
    glimpse()
  df13 <- df2 %>%
    clean_names()
  df14 <- df13 %>%
    group_by(id_osm) %>%
    summarize(nb = n()) %>%
    filter(nb > 1) %>%
    glimpse()
  if (nrow(df14) > 0) {
    df14 <- df13 %>%
      filter(id_osm %in% df14$id_osm) %>%
      arrange(id_osm) %>%
      dplyr::select(id_osm, shape_id_gtfs)
    misc_print(df14)
    confess("***** id.osm en double %s", nrow(df14))
  }
  if (nrow(df2) > 0) {
    if (cmp == "tags") {
      diff_objects_tags(df2, type = "route")
    }
    if (cmp == "members") {
      diff_objects_members(df2, force = force)
    }
  }
  carp("fin")
  return(invisible())
#  misc_print(df2)
  carp("les tags communs")
  glimpse(gtfs_names)
  glimpse(osm_names)
  names_commun <- intersect(gtfs_names, osm_names) %>%
    glimpse()
  for (col in names_commun) {
    carp("col: %s", col)
    if (col == "ref:network") {
      next
    }
    gtfs <- sprintf("%s.gtfs", col)
    osm <- sprintf("%s.osm", col)
    df3 <- df2 %>%
      dplyr::select(`ref:network.osm`, Ref, `@id.osm`, gtfs = !!gtfs, osm = !!osm) %>%
      filter(gtfs != osm) %>%
      arrange(`ref:network.osm`)
    if (nrow(df3) > 0) {
      misc_print(df3)
    }
  }
}
#
# comparaison à partir des routes osm
# source("geo/scripts/transport.R");diff_osm_routes_tag_shape(force = TRUE)
diff_osm_routes_tag_shape <- function(force = TRUE) {
  library(tidyverse)
  library(data.table)
  library(sf)
  library(janitor)
  carp()
  carp("le gtfs et les shapes")
  dsn <- sprintf("%s/%s", texDir, "tidytransit_routes_shapes_stops.csv")
  shapes.df <- fread(dsn, encoding = "UTF-8") %>%
    glimpse()
#
# le fichier est bien à jour ?
  tt <- transport_lire("gtfs")
  s1.df <- shapes.df  %>%
    dplyr::select(shape_id) %>%
    mutate(source = "csv")
  s2.df <- tt$shapes %>%
    distinct(shape_id) %>%
    mutate(source = "tt") %>%
    glimpse()
  s3 <- s1.df %>%
    full_join(s2.df, by = c("shape_id"))  %>%
    filter(is.na(source.x) | is.na(source.y)) %>%
    glimpse()
#  stop("****")


  df <- overpass_get(query = "relations_route_bus_network", format = "csv", force = force) %>%
    clean_names() %>%
    glimpse()
  carp("les shapes en double")
  df1 <- df %>%
    group_by(gtfs_shape_id) %>%
    summarize(nb = n()) %>%
    filter(nb > 1) %>%
    glimpse()
  df2 <- df %>%
    filter(gtfs_shape_id %in% df1$gtfs_shape_id) %>%
    dplyr::select(ref_network, id, name, from, to, ref, gtfs_shape_id) %>%
    arrange(gtfs_shape_id) %>%
    glimpse()
  tex_df2kable(df2, suffixe = "doublon", longtable = TRUE)

  s.df <- shapes.df %>%
    distinct(shape_id) %>%
    mutate(source = "gtfs") %>%
    arrange(shape_id) %>%
    glimpse()
  df11 <- df %>%
    filter(gtfs_shape_id != "") %>%
    left_join(s.df, by = c("gtfs_shape_id" = "shape_id"), relationship = "many-to-many") %>%
    arrange(gtfs_shape_id) %>%
    glimpse()
  df111 <- df11 %>%
    filter(! is.na(source)) %>%
    dplyr::select(gtfs_shape_id, ref_network, id, name, from, to, ref) %>%
    arrange(gtfs_shape_id) %>%
    mutate(INDEX = row_number()) %>%
    glimpse()
  df112 <- df111 %>%
    mutate(id = as.character(id)) %>%
    dplyr::select(INDEX, gtfs_shape_id, ref_network, id, name)
  df113 <- df111 %>%
    mutate(name = sprintf("%s => %s", from, to)) %>%
    dplyr::select(INDEX, gtfs_shape_id, ref_network, id, name) %>%
    mutate(gtfs_shape_id = "", ref_network = "", id = "") %>%
    glimpse()
  df114 <- df112 %>%
    bind_rows(df113) %>%
    arrange(INDEX) %>%
    glimpse()
  tex_df2kable(df114, suffixe = "connu", longtable = TRUE)
#
# le fichier pour perl
  df31 <- shapes.df %>%
    filter(shape_id %in% df$gtfs_shape_id)
  dsn <- sprintf("%s/%s", texDir, "reseau_osm_routes_tag_shape.csv")
  readr::write_tsv(df31, file = dsn)
  carp("dsn: %s", dsn)
#
# on enlève les connus
  df12 <- df11 %>%
    filter(ref_network %notin% df111$ref_network) %>%
    filter(is.na(source)) %>%
    dplyr::select(gtfs_shape_id, ref_network, id, name, from, to, ref) %>%
    arrange(gtfs_shape_id) %>%
    mutate(INDEX = row_number()) %>%
    glimpse()
  df121 <- df12 %>%
    mutate(id = as.character(id)) %>%
    dplyr::select(INDEX, gtfs_shape_id, ref_network, id, name)
  df122 <- df12 %>%
    mutate(name = sprintf("%s => %s", from, to)) %>%
    dplyr::select(INDEX, gtfs_shape_id, ref_network, id, name) %>%
    mutate(gtfs_shape_id = "", ref_network = "", id = "") %>%
    glimpse()
  df123 <- df121 %>%
    bind_rows(df122) %>%
    arrange(INDEX) %>%
    glimpse()
#  stop("****")
  tex_df2kable(df123, suffixe = "inconnu", longtable = TRUE)
#
# pour trouver le shape potentiel
  df31 <- df %>%
    filter(gtfs_shape_id == "") %>%
    dplyr::select(id, ref_network, from, to) %>%
#    filter(! grepl("^2\\d\\d", ref)) %>%
#    filter(! grepl("^08", gtfs_shape_id)) %>%
    glimpse()
  df32 <- df31 %>%
    left_join(shapes.df, by = c("ref_network" = "ref_network")) %>%
    filter(! is.na(shape_id)) %>%
    group_by(ref_network) %>%
    arrange(desc(nb), desc(nb_stops)) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    arrange(ref_network) %>%
    dplyr::select(id, ref_network, from, to, first_name, last_name, shape_id, nb) %>%
    glimpse()
  tex_df2kable(df32, suffixe = "potentiel", longtable = TRUE)
  df33 <- df32 %>%
    mutate(id = sprintf("<a href='http://level0.osmz.ru/?url=relation/%s' target='_blank'>%s</a>", id, id))
  titre <- "reseau_osm_routes_tag_shape"
  misc_html_df2fic(df33, titre)
  misc_print(df32)
  tidytransit_sauve(df32, "reseau_osm_routes_tag_shape")
}

#
# comparaison à partir des routes gtfs
# source("geo/scripts/transport.R");diff_relations_route_tags(force = FALSE)
diff_relations_route_tags <- function(force = TRUE, OsmChange = FALSE) {
  library(tidyverse)
  library(data.table)
  library(sf)
  library(janitor)
  carp()
  OsmChange <<- OsmChange
  titre <- sprintf("diff_relations_route_tags_%s", Reseau)
  html <- misc_html_titre(titre)
  html <- misc_html_append(html, sprintf("<h1>%s</h1>", titre))
# osm
  carp("osm: les relations")
  osm.df <- overpass_get(query = "relations_route_bus_network", format = "csv", force = force)
  if (! is.na(Config_route_id)) {
    osm.df <- osm.df %>%
      filter(grepl(Config_route_id, ref)) %>%
      glimpse()
  }
  carp("osm: les doublons")
  df1 <- osm.df %>%
    group_by(`ref:network`) %>%
    summarize(nb = n()) %>%
    filter(nb > 1)
  if (nrow(df1) > 0) {
    glimpse(df1)
    df2 <- osm.df %>%
      filter(`ref:network` %in% df1$`ref:network`) %>%
      arrange(`ref:network`) %>%
      dplyr::select(`ref:network`, `@id`)
    misc_print(df2)
    carp("**** doublons nb: %s", nrow(df1))
  }
  carp("osm: tri par ref_network")
  osm.df <- osm.df %>%
    arrange(`ref:network`) %>%
    rename_with( ~ paste0(.x, ".osm")) %>%
    glimpse()
# le gtfs
  carp("gtfs: les routes avec les stops")
  dsn <- sprintf("%s/%s", transportDir, "gtfs2osm_relations_route_stops.csv")
  routes.df <- fread(dsn, encoding = "UTF-8") %>%
    rename(`ref:network` = ref_network) %>%
    arrange(`ref:network`) %>%
    rename_with(~ paste0(.x, ".gtfs")) %>%
    glimpse()
  carp("osm et gtfs: fusion par ref_network")
  df1 <- osm.df %>%
    full_join(routes.df, by = c("ref:network.osm" = "ref:network.gtfs")) %>%
    rename("ref:network" = "ref:network.osm") %>%
    glimpse()
  carp("osm et gtfs: absent du gtfs")
  df2 <- df1 %>%
    filter(is.na(name.gtfs))
  if (nrow(df2) > 0) {
    df3 <- df2 %>%
      dplyr::select(-matches(".gtfs$")) %>%
      rename_all(gsub, pattern = '\\.osm', replacement = '') %>%
      dplyr::select("ref:network", "@id", "name", "from", "to") %>%
      mutate(level0 = sprintf("<a href='http://level0.osmz.ru/?url=relation/%s' target='diff'>%s</a>", `@id`, `@id`))
    html <- misc_html_append_df(html, "<h2>osm et gtfs: absent du gtfs</h2>")
    html <- misc_html_append_df(html, df3)

#    stop("******")
  }
  carp("osm et gtfs: absent d'osm")
  df2 <- df1 %>%
    filter(is.na(name.osm))
  if (nrow(df2) > 0) {
    df3 <- df2 %>%
      dplyr::select(-matches(".osm$")) %>%
      rename_all(gsub, pattern = '\\.gtfs', replacement = '') %>%
      dplyr::select("ref:network", "name", "from", "to")
    html <- misc_html_append_df(html, "<h2>osm et gtfs: absent d'osm</h2>")
    html <- misc_html_append_df(html, df3)
    level0 <- ""
    for (i2 in 1:nrow(df2)) {
      dsn <- sprintf("%s/gtfs2osm_relations_route_level0_%s.txt", osmDir, df2[[i2, "ref:network"]])
      l <- readLines(dsn)
      level0 <- c(level0, l)
    }
    dsn <- sprintf("%s/diff_relations_route_tags_level0.txt", osmDir)
    write(level0, dsn)
    carp("dsn: %s", dsn)
    carp("créer avec level0")
    stop("****")
  }
  carp("osm et gtfs: les communs")
  df1 <- df1 %>%
    filter(!is.na(name.gtfs)) %>%
    filter(! is.na(name.osm)) %>%
    select(order(colnames(.))) %>%
    glimpse()
  carp("osm et gtfs: tri par ref_network")
#  df2 <- df1 %>%
#    dplyr::select(kref = `ref:network.osm`, from.gtfs, from.osm, to.gtfs, to.osm) %>%
#    arrange(kref)
#  misc_print(df2)
#  df2 <- df1 %>%
#    dplyr::select(kref = `ref:network.osm`, name.gtfs, name.osm) %>%
#    arrange(kref)
#  misc_print(df2)
  df3 <- df1 %>%
    dplyr::select(`@id.osm`, `ref:network`, `gtfs:shape_id.osm`, shape_id.gtfs) %>%
    glimpse() %>%
    filter(`gtfs:shape_id.osm` != shape_id.gtfs) %>%
    glimpse()
  if (nrow(df3) > 0) {
    type <- "relation"
    osm <- ""
    for (i3 in 1:nrow(df3)) {
      carp("i3: %s", i3)
      tags.df <- tribble(
        ~name, ~value,
        "gtfs:shape_id", df3[[i3, "shape_id.gtfs"]],
      )
      o <- osmchange_object_modify_tags(id = df3[[i3, "@id.osm"]], type = type, tags = tags.df)
      osm <- sprintf("%s\n%s", osm, o)
#    break
    }
    if (str_length(osm) > 10) {
      changeset_id <- osmapi_put("modify", text = osm)
      confess("osm: %s---", changeset_id)
    }
  }
#
# le fichier de sortie
  transport_html_browse(html, titre, Exit = FALSE)
}
#
# comparaison à partir des routes gtfs
# pour les relations route=bus avec tag gtfs:shape_id
#
# source("geo/scripts/transport.R");diff_relations_route_tag_shape_id(force = FALSE)
diff_relations_route_tag_shape_id <- function(rds = "diff_relations_route_tag_shape_id", force = TRUE) {
  library(tidyverse)
  library(data.table)
  library(sf)
  library(janitor)
  carp()
  cle_osm <- "gtfs:shape_id"
  titre <- sprintf("%s_%s", rds, Config[1, "reseau"])
  html <- misc_html_titre(titre)
  html <- misc_html_append(html, sprintf("<h1>%s</h1>", titre))
# osm
  carp("osm: les relations")
  osm.df <- overpass_get(query = "relations_route_bus_network", format = "csv", force = force) %>%
    as.data.frame() %>%
    glimpse()
  osm.df[, "kref"] <- osm.df[, cle_osm]
  df11 <- osm.df %>%
    glimpse() %>%
    filter(is.na(kref) | kref == "") %>%
    dplyr::select(`@id`, `ref:network`, name, from, to) %>%
    mutate(level0 = sprintf("<a href=http://level0.osmz.ru/?url=relation/%s>level0</a>", `@id`)) %>%
    glimpse()
  if (nrow(df11) > 0) {
    transport_ecrire(df11, rds)
    html <- html_append(html, "<h2>osm sans tag gtfs:shape_id</h2>")
    html <- html_append_df(html, df11)
    transport_html_browse(html, titre)
#    diff_relations_route_tag_shape_id_change(force = force)
    carp("osm sans tag gtfs:shape_id")
#    stop("ùùùùùùùùùùùùù")
  }
  osm.df <- osm.df %>%
    filter(kref != "") %>%
    filter(! is.na(kref))
  carp("test doublons")
  df12 <- osm.df %>%
    group_by(kref) %>%
    summarize(nb = n()) %>%
    filter(nb > 1)
  if (nrow(df12) > 0) {
    df13 <- osm.df %>%
      filter(kref %in% df12$kref) %>%
      arrange(kref, `ref:network`) %>%
      mutate(josm = sprintf("<a href='http://localhost:8111/import?url=https://api.openstreetmap.org/api/0.6/relation/%s/full'>josm</a>", `@id`)) %>%
      dplyr::select(kref, `@id`, josm, `ref:network`, from, to, name) %>%
      mutate(`@id` = sprintf("<a href=http://level0.osmz.ru/?url=relation/%s>%s</a>", `@id`, `@id`))
    html <- html_append(html, "<h2>Les doublons</h2>")
    html <- html_append_df(html, df13)
    transport_html_browse(html, titre)
    confess("Les doublons")
  }
  carp("osm: tri par kref")
  osm.df <- osm.df %>%
    arrange(kref) %>%
    rename_with( ~ paste0(.x, ".osm")) %>%
    glimpse()
# le gtfs
  carp("gtfs: les routes avec les stops")
  dsn <- sprintf("%s/%s", transportDir, "gtfs2osm_routes_shapes_stops.csv")
  gtfs.df <- fread(dsn, encoding = "UTF-8") %>%
    mutate(kref = `shape_id`) %>%
    arrange(kref) %>%
    rename_with( ~ paste0(.x, ".gtfs")) %>%
    glimpse()
  carp("osm et gtfs: fusion par kref")
  df1 <- osm.df %>%
    full_join(gtfs.df, by = c("kref.osm" = "kref.gtfs")) %>%
    rename(kref = kref.osm) %>%
    glimpse()
  carp("osm et gtfs: absent du gtfs")
  df11 <- df1 %>%
    filter(is.na(name.gtfs)) %>%
    arrange(kref)
  if (nrow(df11) > 0) {
    carp("****absent du gtfs")
    df11 <- df11 %>%
      select(-ends_with(".gtfs")) %>%
      rename_at(vars(matches(".osm")), ~str_remove(., ".osm")) %>%
      dplyr::select("ref:network", `gtfs:shape_id`, `@id`, version = `@version`, from, to)
    df12 <- df11 %>%
      mutate(id = sprintf("<a href='http://level0.osmz.ru/?url=%s%s'>%s</a>", "r", `@id`, `@id`)) %>%
      arrange("ref:network") %>%
      glimpse()
    html <- misc_html_append_df(html, "<h2>osm et gtfs: absent du gtfs</h2>")
    html <- misc_html_append_df(html, df12)
#    transport_html_browse(html, titre)
    transport_ecrire(df11, rds)
    diff_relations_route_tag_shape_id_change(force = force)
    confess("absent du gtfs")
  }
  html <- misc_html_append_df(html, "<h2>osm et gtfs: les communs</h2>")
  df11 <- df1 %>%
    filter(! is.na(name.gtfs)) %>%
    filter(! is.na(name.osm)) %>%
    select(order(colnames(.))) %>%
    glimpse()
  tidytransit_sauve(df11, rds)
  carp("osm et gtfs: tri par ref_network")
  df12 <- df11 %>%
    dplyr::select(kref, from.gtfs, from.osm, to.gtfs, to.osm) %>%
    arrange(kref)
  html <- misc_html_append_df(html, df12)
  df12 <- df11 %>%
    dplyr::select(kref, name.gtfs, name.osm) %>%
    arrange(kref)
#
# le fichier de sortie
  fic <- sprintf("%s.html", titre)
  transport_html_browse(html, titre, Exit = FALSE)
}
#
#####
# pour ajouter/mettre à jour en automatique le gtfs:shape_id
#
# source("geo/scripts/transport.R");diff_relations_route_tag_shape_id_change(force = FALSE)
diff_relations_route_tag_shape_id_change <- function(rds = "diff_relations_route_tag_shape_id", force = FALSE, OsmChange = FALSE) {
  OsmChange <<- OsmChange
  carp("les routes osm")
  df1 <- transport_lire(rds) %>%
    filter("ref:network" != "") %>%
    rename(ref_network = "ref:network") %>%
    rename(id = "@id") %>%
    glimpse()
  carp("les routes gtfs par ref:network")
  df2 <- transport_lire(rds = "tidytransit_routes_stops") %>%
    group_by(ref_network) %>%
    arrange(desc(nb), desc(nb_stops)) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    arrange(ref_network) %>%
#    filter(grepl("^5", ref_network)) %>%
    glimpse()
  carp("la fusion")
  df3 <- df1 %>%
    left_join(df2, by = c("ref_network" = "ref_network")) %>%
    filter(!is.na(shape_id)) %>%
    arrange(ref_network) %>%
    dplyr::select(id, ref_network, from, to, first_name, last_name, shape_id, nb, nb_stops) %>%
    glimpse()
  if (nrow(df3) == 0) {
    carp("c'est tout bon")
    return(invisible())
  }
  tex_df2kable(df3, suffixe = "potentiel", longtable = TRUE)
  df4 <- df3 %>%
    mutate(id = sprintf("<a href='http://level0.osmz.ru/?url=relation/%s' target='_blank'>%s</a>", id, id)) %>%
    glimpse()
#  stop("****")
  titre <- "diff_relations_route_tag_shape_id_change"
  html <- html_titre(titre)
  html <- html_append_df(html, df4)
  transport_html_browse(html, titre, Exit = FALSE)
#  stop("****")
  type <- "relation"
  osm <- ""
  for (i3 in 1:nrow(df3)) {
    carp("i3: %s/%s", i3, nrow(df3))
    tags.df <- tribble(
      ~name, ~value,
      "gtfs:shape_id", df3[[i3, "shape_id"]],
    )
    o <- osmchange_object_modify_tags(id = df3[[i3, "id"]], type = type, tags = tags.df)
    osm <- sprintf("%s\n%s", osm, o)
  }
  if (str_length(osm) > 10) {
    changeset_id <- osmapi_put("modify", text = osm)
#    cat(osm)
    confess("osm: %s---", changeset_id)
  }
}
#
#
# comparaison entre les routes osm et les routes gtfs pour les stops
# source("geo/scripts/transport.R");diff_relations_route_bus_stops(force = FALSE)
diff_relations_route_bus_stops <- function(force = TRUE) {
  library(tidyverse)
  library(janitor)
  carp("osm: les relations")
  df1 <- overpass_get(query = "relations_route_bus_network", format = "csv", force = force) %>%
    as.data.frame() %>%
    filter(`gtfs:shape_id` != "")
  carp("gtfs: les shapes avec les stops")
  gtfs.df <- tidytransit_lire("gtfs_shapes_stops_id")
  gtfs.sf <<- st_as_sf(gtfs.df, coords = c("stop_lon", "stop_lat"), crs = 4326, remove = TRUE) %>%
    st_transform(2154)
  stops.df <- data.frame()
  for(i1 in 1:nrow(df1)) {
    carp("i1: %s/%s", i1, nrow(df1))
    df2 <- diff_relation_route_bus_stops(df1[i1, ], force = FALSE) %>%
      st_drop_geometry()
    stops.df <- bind_rows(stops.df, df2)
#    break
  }
  transport_sauve(stops.df, "diff_relations_route_bus_stops")
}
diff_relation_route_bus_stops <- function(df1, force = TRUE) {
  library(tidyverse)
  library(janitor)
  ref <- df1[1, "@id"]
  type <- df1[1, "@type"]
  carp("relation: %s", ref)
  doc <- osmapi_get_object_full_xml(ref = ref, type = type, force = TRUE, force_osm = TRUE)
  xpath <- sprintf(".//%s[@id='%s']", type, ref)
  relation <- xml_find_all(doc, xpath)
  tag <- xml_find_all(relation, ".//tag[@k='gtfs:shape_id']")
  if (length(tag) == 0) {
    Carp("*****pas de tag gtfs:shape_id")
    return(invisible())
  }
  shape_id <<- xml_attr(tag, "v")
  stops.sf <- gtfs.sf %>%
    filter(shape_id == !!shape_id) %>%
    dplyr::select(starts_with("stop_"))
  members <- xml_find_all(relation, ".//member[@role='platform']")
  if (length(members) == 0) {
    Carp("*****pas de platform")
    return(invisible())
  }
  carp("members nb: %s", length(members))
  members.df <<- osmapi_objects_tags(members) %>%
    mutate(xpath = sprintf(".//%s[@id='%s']", `@type`, `@ref`))
  objects.df <- data.frame()
  for (i in 1:nrow(members.df)) {
    member <- xml_find_all(doc, members.df[i, "xpath"])
    object.df <- osmapi_objects_tags(member)
    objects.df <- bind_rows(objects.df, object.df)
  }
  objects.df <- objects.df %>%
    filter(grepl("platform", public_transport)) %>%
    filter(osm_type == "node") %>%
    mutate(lon = as.numeric(`@lon`)) %>%
    mutate(lat = as.numeric(`@lat`))
  objects.sf <- st_as_sf(objects.df, coords = c("lon", "lat"), crs = 4326, remove = TRUE) %>%
    st_transform(2154)
  objects.sf <- objects.sf %>%
    dplyr::select(`@id`, name, starts_with("ref") | starts_with("gtfs"))
  df1 <- st_proches(objects.sf, stops.sf)
  misc_print(df1)
  return(invisible(df1))
}
#
# pour mettre en place le tag gtfs:shape_id
# source("geo/scripts/transport.R");diff_relations_route_bus_stops_verif(force = FALSE)
diff_relations_route_bus_stops_verif <- function(force = TRUE) {
  library(tidyverse)
  library(janitor)
  stop("*****")
  df1 <- transport_lire("diff_relations_route_bus_stops") %>%
    glimpse() %>%
    filter(dist < 25) %>%
    group_by(X.id) %>%
    arrange(dist) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    glimpse()
#  misc_print(df1)
# les stops du gtfs utilisés plusieurs fois
  df2 <- df1 %>%
    group_by(stop_id) %>%
    summarize(nb = n()) %>%
    filter(nb > 1) %>%
    glimpse()
# pour ne pas les avoir
  df3 <- df1 %>%
    filter(stop_id %notin% df2$stop_id)
  osm <- ""
  for (i3 in 1:nrow(df3)) {
    carp("i3: %s", i3)
    tags.df <- tribble(
      ~name, ~value,
      Config_k_ref, df3[[i3, "stop_id"]],
    )
    o <- osmchange_object_modify_tags(id = df3[[i3, "X.id"]], type = "node", tags = tags.df)
    osm <- sprintf("%s\n%s", osm, o)
#    break
  }
#  writeLines(osm)
  OsmChange <<- TRUE
  if (str_length(osm) > 10) {
    changeset_id <- osmapi_put("modify", text = osm, comment = "tentative de mise en place de la ref gtfs stop_id")
    confess("osm: %s---", changeset_id)
  }
}
#
## les ways des relations route
#
#
# source("geo/scripts/transport.R");diff_relations_route_ways(force = FALSE)
diff_relations_route_ways <- function(force = TRUE, OsmChange = FALSE) {
  library(janitor)
  OsmChange <<- OsmChange
  carp("osm: les relations %s", Config_operator)
#  stop("$$$$$$$$$$$$$$$$$")
  osm.df <- overpass_get(query = "relations_route_bus_network", format = "csv", force = force) %>%
    clean_names()
  if (! is.na(Config_route_id)) {
    osm.df <- osm.df %>%
      filter(grepl(Config_route_id, ref)) %>%
      glimpse()
  }
  osm.df <- osm.df %>%
    filter(!is.na(gtfs_shape_id)) %>%
    glimpse()
  josm.df <- tidytransit_lire("valhalla_shapes") %>%
    glimpse()
  df1 <- osm.df %>%
    left_join(josm.df, by = c("gtfs_shape_id" = "shape_id")) %>%
    filter(! is.na(osm)) %>%
    glimpse()
  for (i in 1:nrow(df1)) {
    carp("i: %s/%s", i, nrow(df1))
    shape_id <- df1[[i, "gtfs_shape_id"]]
    diff_relation_route_ways(df1[[i, "id"]], df1[[i, "osm"]], OsmChange = OsmChange)
  }
  carp("fin")
}
#
#
diff_relation_route_ways <- function(id = id, osm = osm, force = TRUE, OsmChange = FALSE) {
  OsmChange <<- OsmChange
  txt <- osmapi_object_txt(ref = id, type = "relation", force = TRUE)
  lignes <- unlist(str_split(txt, "\n"))
  relation1 <- grep(" <relation id=", lignes, value = TRUE)
  tags <- grep("  <tag ", lignes, value = TRUE)
  members <- grep("  <member ", lignes, value = TRUE)
  stops <- grep(' role="\\S+"', members, value = TRUE)
  ways <- grep(' role=""', members, value = TRUE)
  members_lg <- length(members)
  stops_lg <- length(stops)
  ways_lg <- length(ways)
  if ( members_lg != (stops_lg + ways_lg)) {
    osm <- paste(osm, "\n", collapse = "")
    writeLines(osm)
    carp("members_lg %s != (stops_lg %s + ways_lg %s", members_lg, stops_lg, ways_lg)
    carp("vérifier les forward/backward")
#    return(invisible(""))
    stop("****")
  }
  if (ways_lg != 0) {
    return(invisible(""))
  }
  osm <- paste(c(relation1, stops, osm, tags, " </relation>"), "\n", collapse = "")
#  writeLines(osm);stop("llllllllllllllllllll")
  changeset_id <- osmapi_put("modify", text = osm)
  carp("osm: %s---", changeset_id)
  return(invisible(osm))
}
#
## les relations route_master et route
#
# les différences pour les relations route et route_master
# source("geo/scripts/transport.R");diff_relations_bus_tags(force = FALSE)
diff_relations_bus_tags <- function(force = TRUE, OsmChange = FALSE) {
  OsmChange <<- OsmChange
#  gtfs2osm_routemasters()
  carp("osm: les relations %s", Config_operator)
#  stop("$$$$$$$$$$$$$$$$$")
  osm.df <- overpass_get(query = "relations_bus_network", format = "csv", force = force) %>%
    glimpse() %>%
    mutate(ref = sprintf("%s", ref)) %>%
#    (function(.df){
#      cls <- Config_tags
#      .df[cls[!(cls %in% colnames(.df))]] = NA
#      return(.df)
#    }) %>%
    rename_with( ~ paste0(.x, ".osm")) %>%
    glimpse()
  gtfs.df <- misc.lire("gtfs2osm_routemasters", dir = transportDir) %>%
    glimpse() %>%
    dplyr::select(-`public_transport:version`, -route_master, -type, -`ref:network`) %>%
    rename_with( ~ paste0(.x, ".gtfs")) %>%
    glimpse()
  df1 <- osm.df %>%
    full_join(gtfs.df, by = c("ref.osm" = "ref.gtfs")) %>%
    glimpse()
  df2 <- df1 %>%
    filter(is.na(name.osm)) %>%
    glimpse()
  df3 <- df1 %>%
    filter(is.na(name.gtfs)) %>%
    glimpse()
  df4 <- df1 %>%
    filter(! is.na(name.osm)) %>%
    filter(! is.na(name.gtfs)) %>%
    glimpse()
  if (nrow(df4) > 0) {
    carp("prise en compte des différences")
    diff_objects_tags(df4, type = "routes")
  }
  carp("fin")
}
#
# comparaison des members entre deux objets
# puis modif avec osmchange
diff_objects_members <- function(df1, type = "route", force = TRUE) {
  carp("df1")
  glimpse(df1)
  osm <- ""
  for (i1 in 1:nrow(df1)) {

#    glimpse(df1[i1,])
    id <- df1[[i1, "@id.osm"]]
    carp("i1: %s/%s id: %s", i1, nrow(df1), id)
    # les données osm
    o <- diff_object_members(df1[i1, ], force = force)
    if (o != "") {
      osm <- sprintf("%s\n%s", osm, o)
#      break
    }
  }
  if (str_length(osm) > 10) {
    osm <- paste(osm, "\n", collapse = "")
#    writeLines(osm);stop("****")
    changeset_id <- osmapi_put("modify", text = osm)
    confess("osm: %s---", changeset_id)
  }
}
diff_object_members <- function(df1, force = TRUE) {
#  glimpse(df1[1, ])
  osm <- ""
  id <- df1[[1, "@id.osm"]]
# les données osm
  txt <- osmapi_object_txt(ref = id, type = "relation", force = TRUE)
#  writeLines(txt);stop("mmmmmm")
#  osm <- txt
#  osm <- stringr::str_replace(osm, regex(".*<(node|way|relation)", dotall = TRUE), "<\\1");
#  osm <- stringr::str_replace(osm, regex("</(node|way|relation)>.*", dotall = TRUE), "</\\1>")
  lignes <- unlist(str_split(txt, "\n"))
  relation1 <- grep(" <relation id=", lignes, value = TRUE)
  tags <- grep("  <tag ", lignes, value = TRUE)
  members <- grep("  <member ", lignes, value = TRUE)
  platforms <- grep(' role="platform', members, value = TRUE)
  stops <- grep(' role="stop', members, value = TRUE)
  ways <- grep(' role=""', members, value = TRUE)
  members_lg <- length(members)
  platforms_lg <- length(platforms)
  stops_lg <- length(stops)
  ways_lg <- length(ways)
  if ( members_lg != (platforms_lg + stops_lg + ways_lg)) {
    osm <- paste(osm, "\n", collapse = "")
    writeLines(osm)
    carp("members_lg %s != (platforms_lg %s + stops_lg %s + ways_lg %s", members_lg, platforms_lg, stops_lg, ways_lg)
    carp("vérifier les forward/backward")
#    return(invisible(""))
    stop("****")
  }
  df1 <- tibble(gtfs = unlist(str_split(df1[[1, "stops_osm_id.gtfs"]], ","))) %>%
    separate_wider_delim(gtfs, delim = " ", names = c("type", "id")) %>%
    mutate(type = dplyr::recode(type,
      "wy" = "way",
      "nd" = "node",
      "rl" = "relation"
    )) %>%
    mutate(osm = sprintf("  <member type=\"%s\" ref=\"%s\" role=\"platform\"/>", type, id))
  if (identical(platforms, df1$osm)) {
    return(invisible(""))
  }
  lg <- max(length(platforms), length(df1$osm))
  list1 <- c(platforms, rep("", 200))
  list2 <- c(df1$osm, rep("", 200))
  df2 <- tibble(osm = list1[1:lg], gtfs = list2[1:lg]) %>%
    mutate(osm = gsub("_(entry|exit)_only","", osm)) %>%
    mutate(egal = ifelse(osm == gtfs, TRUE, FALSE)) %>%
    filter(egal == FALSE) %>%
    glimpse()
  if (nrow(df2) == 0) {
    return(invisible(""))
  }
  misc_print(df2)
  osm <- paste(c(relation1, df1$osm, stops, ways, tags, " </relation>"), "\n", collapse = "")
#  writeLines(osm);stop("llllllllllllllllllll")
  return(invisible(osm))
}
#
# comparaison des tags entre deux objets
# puis modif avec osmchange
diff_objects_tags <- function(df1, type = "route", OsmChange = TRUE) {
  champs.df <- tribble(
    ~attribut,
      "name",
      "description",
      "ref:network",
      "from",
      "to",
      "network",
#      "operator",
      "colour",
      "text_colour",
#      "network:wikidata",
#      "network:wikipedia",
    )
  if (type == "master") {
    champs.df <- tribble(
      ~attribut,
      "name",
      "description",
      "ref:network",
      "network",
#      "operator",
      "colour",
      "text_colour",
#       "network:wikidata",
    )
  }
  if (type == "routes") {
    champs.df <- tribble(
      ~attribut,
      "network",
      "operator",
      "colour",
      "text_colour",
#       "network:wikidata",
    )
  }
  champs.df <- champs.df %>%
    mutate(gtfs = sprintf("%s.gtfs", attribut)) %>%
    mutate(osm = sprintf("%s.osm", attribut))
  osm <- ""
  nb_changes <- 0
  for (i1 in 1:nrow(df1)) {
    id <- df1[[i1, "@id.osm"]]
# pour l'encodage <>
    if (id != "324474") {
#      next
    }
#    glimpse(df1[i1, ]); stop("*****")
#    carp("i1: %s %s %s %s %s", i1, df1[[i1, "@id.osm"]], df1[[i1, "type.osm"]], df1[[i1, "ref.osm"]], df1[[i1, "name.osm"]])
    carp("i1: %s %s %s", i1, df1[[i1, "@id.osm"]], df1[[i1, "cle.osm"]])
    tags.osm <- list()
    tags.gtfs <- list()
    tags.df <- champs.df
    tags.df$OSM <- NA
    tags.df$GTFS <- NA
    tags.df$egal <- TRUE
    nb_diff <- 0
    for (ic in 1:nrow(champs.df)) {
      tags.df[ic, "OSM"] <- ""
      tags.df[ic, "GTFS"] <- ""
      if (tags.df[[ic, "osm"]] %in% names(df1)) {
        tags.df[ic, "OSM"] <- df1[[i1, tags.df[[ic, "osm"]]]]
      }
      if (tags.df[[ic, "gtfs"]] %in% names(df1)) {
#        confess("tag non configuré dans gtfs2osm: %s", tags.df[[ic, "gtfs"]])
        tags.df[ic, "GTFS"] <- df1[[i1, tags.df[[ic, "gtfs"]]]]
      }
      if (is.na(tags.df[[ic, "OSM"]])) {
        tags.df[[ic, "OSM"]] <- "NA"
      }
      Carp("ic: %s %s#%s", ic, tags.df[[ic, "OSM"]],tags.df[[ic, "GTFS"]])
      if (tags.df[[ic, "OSM"]] != tags.df[[ic, "GTFS"]]) {
        tags.df[ic, "egal"] <- FALSE
        nb_diff <- nb_diff + 1
      }
    }
#    carp("nb_diff: %s", nb_diff)
    if (identical(tags.df$OSM, tags.df$GTFS)) {
      next
    }
    if (nb_diff == 0) {
      next
    }
    setdiff(tags.df$OSM, tags.df$GTFS) %>%
      glimpse()
    t.df <- tags.df %>%
      mutate(OSM = substr(OSM, 1, 50)) %>%
      mutate(GTFS = substr(GTFS, 1, 50)) %>%
    misc_print(t.df)
    if (OsmChange == TRUE) {
      tags.df <- tags.df %>%
        mutate(osm = gsub('\\.osm$', '', osm)) %>%
        dplyr::select(name = osm, value = GTFS)
#      misc_print(tags.df); exit()
      type <- "relation"
      o <- osmchange_object_modify_tags(id = id, type = type, tags = tags.df, Change = FALSE)
      nb_changes <- nb_changes + 1
      writeLines(o)
      if (nb_changes > 1) {
#        stop("*****")
      }
      osm <- sprintf("%s\n%s", osm, o)
    }
#    break
  }
  if (str_length(osm) > 10) {
    osm <- paste(osm, "\n", collapse = "")
#    writeLines(osm);stop("****")
    changeset_id <- osmapi_put("modify", text = osm)
    confess("osm: %s---", changeset_id)
  }
}

