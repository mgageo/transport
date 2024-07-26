# <!-- coding: utf-8 -->
#
# quelques fonctions pour piloter les scripts via un fichier excel
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
#
#
#
# initialisation des actions
actions_init <- function(dsn, prefixe = "misc_", onglet = "actions") {
  library(rio)
  actions_dsn <<- dsn
  actions.df <- import(dsn, which=onglet) %>%
    glimpse()
  for(i in 1:nrow(actions.df)) {
    action <- actions.df[i, "action"]
    onglet <- actions.df[i, "onglet"]
    if (is.na(action)) {
      break
    }
    fonction <- sprintf("%s%s", prefixe, actions.df[i, "action"])
    a <- list(onglet)
    p <- do.call(fonction, a)
  }
}
actions_onglet_lire <- function(onglet) {
  library(rio)
  df <- rio::import(actions_dsn, which = onglet) %>%
    glimpse()
  return(invisible(df))
}
