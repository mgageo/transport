# <!-- coding: utf-8 -->
#
# le réseau de bus de Saint-Malo
#
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
#
#
# pour les stops
# source("geo/scripts/transport.R");rmat_stops()
rmat_stops <- function(force_osm = FALSE) {
  carp()
#
# les arrêts osm avec le tag k_ref
  osm.df <- overpass_get(query = "bus_stop_kref", format = "csv", force = force_osm) %>%
    rename(k_ref = Config[1, 'k_ref']) %>%
    mutate(k_ref = sprintf("%s", k_ref)) %>%
    glimpse()
  osm.sf <- st_as_sf(osm.df, coords = c("@lon", "@lat"), crs = 4326, remove = FALSE) %>%
    dplyr::select(name, k_ref) %>%
    st_transform(2154) %>%
    glimpse()
  carp("les arrêts du gtfs")
  gtfs.sf <- tidytransit_lire("tidytransit_stops_sf") %>%
    dplyr::select(stop_id, stop_name, parent_station) %>%
    st_transform(2154) %>%
    glimpse()
  df1 <- st_proches(osm.sf, gtfs.sf, k = 1) %>%
    arrange(dist) %>%
    filter(dist < 5) %>%
    glimpse()
  misc_print(df1)
}
