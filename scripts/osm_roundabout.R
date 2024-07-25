# <!-- coding: utf-8 -->
#
# quelques fonctions pour OpenStreetMap
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#

#
# lecture des fichiers
# source("geo/scripts/osm.R"); roundabout_parquet_lire(force = FALSE)
roundabout_parquet_lire  <- function(departement = 35, force = TRUE) {
  library(arrow)
  dsn <- sprintf("%s/roundabout_ways.parquet", duckdbDir)
  df1 <- arrow::read_parquet(dsn) %>%
    mutate(id = sprintf("w%s", id)) %>%
    glimpse()
  df1 <- df1 %>%
    filter(pt1 != pt9)
  rp.df <- tibble()
  for (nb_ways in 1:nrow(df1)) {
    carp("les ronds-points %s", nrow(df1))
    df2 <- df1 %>%
      filter(pt1 == pt9)
    if (nrow(df2) > 0) {
#      carp("les ronds-points")
      rp.df <- bind_rows(rp.df, df2)
    }
    df1 <- df1 %>%
      filter(pt1 != pt9)
    if (nrow(df1) == 0 ) {
      break
    }
# on recherche pour le 1er
    pt <- df1[[1, "pt9"]]
    df2 <- df1 %>%
      filter(pt1 == !!pt)
    if (nrow(df2) == 1) {
#      carp("ajout d'une way")
      df1[[1, "id"]] <- sprintf("%s,%s", df1[[1, "id"]], df2[[1, "id"]])
      df1[[1, "pt9"]] <- df2[[1, "pt9"]]
      df1 <- df1 %>%
        filter(id != df2[[1, "id"]])
      next
    }
#    glimpse(df1[1,])
    if (nrow(df2) == 0) {
      rp.df <- bind_rows(rp.df, df1[1, ])
      df1 <- df1[-1, ]
      next
    }
    confess("****** %s", nrow(df2))
  }
  df1 <- rp.df %>%
      filter(pt1 != pt9)
  misc_print(df1)
}
