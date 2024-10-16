# <!-- coding: utf-8 -->
#
# le réseau de bus
#
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
## la commune des arrêts
#
# source("geo/scripts/transport.R");ign_stops_commune()
ign_stops_commune <- function(territoire = FALSE) {
  library(readr)
  library(sf)
  carp()
  communes.sf <- ign_adminexpress_lire_sf() %>%
    dplyr::select("city" = NOM, INSEE_DEP)
  if (territoire == TRUE) {
    communes.sf <- ign_adminexpress_lire_sf() %>%
      filter(INSEE_DEP == Config[1, "territoire"]) %>%
      glimpse()
  }
  stops.df <- gtfs_file_lire("stops") %>%
    glimpse()
  carp("rapprochement")
  stops.sf <- st_as_sf(stops.df, coords = c("stop_lon", "stop_lat"), crs = 4326, remove = FALSE) %>%
    st_transform(2154) %>%
    glimpse()
  df1 <- stops.sf %>%
    st_join(communes.sf) %>%
    st_drop_geometry() %>%
    glimpse()
  dsn <- sprintf("%s/%s.txt", gtfsDir, "stops_geocode")
  rio::export(df1, dsn, sep = ",")
  carp("dsn: %s n: %s", dsn, nrow(df1))
  df2 <- df1 %>%
    filter(city == "") %>%
    glimpse()
  df3 <- df1 %>%
    group_by(INSEE_DEP) %>%
    summarize(nb = n())
  misc_print(df3)
}
#
# source("geo/scripts/transport.R");ign_tidytransit_geocode(tt)
ign_tidytransit_geocode <- function(tt) {
  library(readr)
  library(sf)
  territoire <- Config[1, "territoire"]
  carp("territoire: %s", territoire)
  territoire <- unlist(strsplit(territoire, ",")) %>%
    glimpse()
  communes.sf <- ign_adminexpress_lire_sf() %>%
#    filter(INSEE_DEP %in% territoire) %>%
    dplyr::select(city = NOM, dept = INSEE_DEP, insee = INSEE_COM)
  stops.df <- tt$stops
  stops.sf <- st_as_sf(stops.df, coords = c("stop_lon", "stop_lat"), crs = 4326, remove = FALSE) %>%
    st_transform(2154)
  df1 <- stops.sf %>%
    st_join(communes.sf) %>%
    st_drop_geometry()
  df2 <- df1 %>%
    filter(is.na(city))
# trop d'échecs
  if (nrow(df2) > 5) {
    misc_print(df2)
    confess("échec geocode nb: %s", nrow(df2))
  }
# on est moins restrictif pour le rapprochement
  if (nrow(df2) > 0) {
    misc_print(df2)
    df1 <- stops.sf %>%
      st_join(communes.sf, join = st_nearest_feature) %>%
      st_drop_geometry() %>%
      glimpse()
    df2 <- df1 %>%
      filter(is.na(city))
    if (nrow(df2) > 10) {
      misc_print(df2)
      confess("échec geocode nb: %s", nrow(df2))
    }
  }
  tt$stops <- df1
  return(invisible(tt))
}
#
# source("geo/scripts/transport.R");ign_osm_geocode(stops.df)
ign_osm_geocode <- function(stops.sf) {
  territoire <- Config[1, "territoire"]
  carp("territoire: %s", territoire)
  territoire <- unlist(strsplit(territoire, ",")) %>%
    glimpse()
  communes.sf <- ign_adminexpress_lire_sf() %>%
    filter(INSEE_DEP %in% territoire) %>%
    dplyr::select(city = NOM, dept = INSEE_DEP, insee = INSEE_COM)
  stops.sf <- stops.sf %>%
    st_transform(2154)
  carp("intersection avec les communes")
  df1 <- stops.sf %>%
    st_join(communes.sf) %>%
    st_drop_geometry()
  df2 <- df1 %>%
    filter(is.na(city))
# trop d'échecs
  if (nrow(df2) > 5) {
    misc_print(df2)
    confess("échec geocode nb: %s", nrow(df2))
  }
  return(invisible(df1))
}