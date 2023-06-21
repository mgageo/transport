# <!-- coding: utf-8 -->
#
# la partie package sf
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ==========================================================================
#
#
# https://www.r-bloggers.com/2023/04/progress-on-r-spatial-evolution-apr-2023/
#
# https://github.com/r-spatial/sf/issues/371
st_drop_geometry <- function(x) {
  if(inherits(x,"sf")) {
    x <- st_set_geometry(x, NULL)
    class(x) <- 'data.frame'
  }
  return(x)
}
st_drop_geom <- function(x) {
  if(inherits(x,"sf"))
    ret <- x[,setdiff(names(x),attr(x,'sf_column')),drop=T]
  else
    ret <- x
  class(ret) <- 'data.frame'
  return(ret)
}
st_version <- function(x) {
  library(sf)
  print(sprintf('sf_extSoftVersion()["GDAL"]: %s', sf_extSoftVersion()["GDAL"]))
  dsn <- "D:/bvi35/CouchesCBNBrest/CGTV_35.shp"
  gdal_utils("info", dsn, options = c("-mm", "-proj4"))
  st_read(dsn)
}
st_proches <- function(nc1, nc2) {
  carp()
  library(tidyverse)
  library(sf)
  nc2$no_ <- 1:nrow(nc2)
  nc1$proche_ <- st_nearest_feature(nc1, nc2)
  df <- dplyr::left_join(nc1 %>% as.data.frame(), nc2 %>% as.data.frame(), by = c('proche_' = 'no_'))
  df$d <- st_distance(df$geometry.x, df$geometry.y, by_element = TRUE)
  df <- df %>%
    dplyr::select(-proche_) %>%
    mutate(d = as.integer(d))
  return(invisible(df))
}
#
## analyse d'un fichier osm avec sf/gdal
#
#
st_query <- function(requete, fic, force = FALSE, layer = "points") {
  library(tidyverse)
  library(stringr)
  library(sf)
  dsn2 <- sprintf("%s/%s.Rds", transportDir, fic)
  if (file.exists(dsn2) && force == FALSE) {
    nc <- readRDS(dsn2)
    return(invisible(nc))
  }
  dsn1 <- overpass_query(requete, fic, force = force)
  layers <- sf::st_layers(dsn1) %>%
    glimpse()
# https://github.com/r-spatial/sf/issues/1157
  ini_new <- "#configuration osm import
attribute_name_laundering=no
[points]
osm_id=yes
osm_version=yes
osm_timestamp=yes
osm_uid=yes
osm_user=yes
osm_changeset=yes
all_tags=yes
[lines]
osm_id=yes
osm_version=yes
osm_timestamp=yes
osm_uid=yes
osm_user=yes
osm_changeset=yes
all_tags=yes
[multipolygons]
osm_id=yes
osm_version=yes
osm_timestamp=yes
osm_uid=yes
osm_user=yes
osm_changeset=yes
all_tags=yes
[multilinestrings]
osm_id=yes
osm_version=yes
osm_timestamp=yes
osm_uid=yes
osm_user=yes
osm_changeset=yes
all_tags=yes
[other_relations]
osm_id=yes
osm_version=yes
osm_timestamp=yes
osm_uid=yes
osm_user=yes
osm_changeset=yes
all_tags=yes
"
  writeLines(ini_new, "ini_new.ini")
  nc <- data.frame()
  for (layer in layers$name) {
    carp("layer: %s", layer)
    nc1 <- st_read(dsn1, layer = layer, options = "CONFIG_FILE=ini_new.ini") %>%
      mutate(LAYER = !!layer)
    if (nrow(nc) == 0) {
      nc <- nc1
    } else {
      nc <- bind_rows(nc, nc1)
    }
  }
  glimpse(nc)
  if(nrow(nc) > 0) {

# comment faire sale
    for (i in 1:nrow(nc)) {
      all_tags <- nc[[i, "all_tags"]]
      pattern <- '"([^"]+)"=>"([^"]+)"'
      kv.list <<- all_tags %>%
        str_match_all(pattern)
      kv.df <- kv.list %>%
        data.frame()
      for (j in 1:nrow(kv.df)) {
        k <- kv.df[j, "X2"]
        v <- kv.df[j, "X3"]
        nc[i, k] <- v
      }
    }
  }
  saveRDS(nc, file = dsn2)
  return(invisible(nc))
}