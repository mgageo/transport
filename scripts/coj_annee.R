# <!-- coding: utf-8 -->
#
# quelques fonctions pour le comptage oiseaux des jardins
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d"Utilisation Commerciale - Partage des Conditions Initiales à l"Identique 2.0 France
#
# !! modifier dans les templates stat_tpl.tex 2022 -> 2023
# !! modifier dans sources_actions.xlsx, onglet stat Annee=2023
# !! modifier dans sources_actions.xlsx, onglet dates samedi 2024
# !! modifier dans le source coj_actions.R 2023 -> 2024
#
anneeRef <- "2023"    # l'année en cours
anneePrec <- "2022"   # l'année précédente
anneeRef <- "2024"    # l'année en cours
anneePrec <- "2023"   # l'année précédente
deptRef <- "35"
libelleRef <- "35"
anneesSqlv2 <- c("2019", "2020", "2021", "2022", "2023", "2024")
anneesFaune <- c("2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024")
debug <- FALSE
#
##  enchainement des traitements
# source("geo/scripts/coj.R");annee_jour(force = FALSE)
# source("geo/scripts/coj.R");actions_config("config_bzh")
annee_jour <- function(annee = anneeRef, force = TRUE) {
  carp("début annee: %s", annee)
  sqlv2_jour(force = force)
  faune_coj_get(anneeRef, force = force)
  faune_donnees_jardin_annee_lire(anneeRef, force = TRUE)
#  return()
  donnees_fusion_sqlv2()
  donnees_fusion_faune()
  donnees_fusion_sources()
#  actions_config("config_bzha")
  actions_config("config_bzh")
#  actions_config("config_35")
#  actions_config("config_44")
}
# pour une année
annee_debut_fin <- function(annee = "2019") {
  library(lubridate)
  carp("début")
  df <- read.table(text = "aaaa,debut,fin
2009,07.02.2009,08.02.2009
2010,08.02.2010,09.02.2010
2011,29.01.2011,30.01.2011
2012,28.01.2012,29.01.2012
2013,26.01.2013,27.01.2013
2014,25.01.2014,26.01.2014
2015,24.01.2015,25.01.2015
2016,30.01.2016,31.01.2016
2017,28.01.2017,29.01.2017
2018,27.01.2018,28.01.2018
2019,26.01.2019,27.01.2019
2020,25.01.2020,26.01.2020
2021,30.01.2021,31.01.2021
2022,29.01.2022,30.01.2022
2023,28.01.2023,29.01.2023
2024,27.01.2024,28.01.2024
", header = TRUE, sep = ",", blank.lines.skip = TRUE, stringsAsFactors = FALSE, quote = "")
  df <- df %>%
    filter(aaaa == annee)
  d <- df[1, "debut"]
  df[1, "Debut"] <- dmy(d) - days(5)
  d <- df[1, "fin"]
  df[1, "Fin"] <- dmy(d) + days(5)
  return(invisible(df))
}
