# <!-- coding: utf-8 -->
#
# quelques fonctions pour la comptage oiseaux des jardins
# auteur: Marc Gauthier
# licence: Creative Commons Paternité - Pas d"Utilisation Commerciale - Partage des Conditions Initiales à l"Identique 2.0 France
# ===============================================================
#
# les traitements pour la LPO
#
# Je préfère attendre de tout avoir d"un seul coup.
# Pour l"instant les données seront simplement intégrées au bilan
# (le module d"import ne fonctionne pas pour l"Observatoire des oiseaux des jardins).
# Idéalement j"ai besoin d"une base avec, pour chaque jardin, le nombre d"individus observés par espèce.
# Pour chaque jardin il me faut au minimum
# une commune, un code postal (ou mieux un code Insee), le département,
# la date d"observation, un identifiant unique par observateur,
# la présence ou non de mangeoires,
# l"heure de début de comptage (si vous avez sinon je fais sans).
# Enfin, si vous avez une description de l"environnement autour du jardin d"observation (milieu urbain, campagne, péri-urbain),
# je suis preneuse, (mais je peux faire sans aussi J).
#
## enchainement des traitements --------------------
#
# source("geo/scripts/coj.R");lpo_jour()
lpo_jour <- function(debug = TRUE) {
  carp()
  lpo_get(debug)
  lpo_export()
}
#
# récupération des fichiers et controle minimum
#
lpo_get <- function(debug = TRUE) {
  carp()
  if (debug == TRUE) {
    sqlv2_tables_get()
  }
  sqlv2_tables_lire(a = "2020", debug = debug)
  df1 <- entete.df %>%
    mutate(saisiedate = as.Date(cojsaisiedate, format = "%d/%m/%Y")) %>%
    filter(saisiedate < as.Date("2020-01-19") | saisiedate > as.Date("2020-02-05")) %>%
    dplyr::select(cojid, statut = cojcreationstatut, cojcreationdate, cojcreationheure, cojsaisiedate)
  if (nrow(df1) > 0) {
    carp("**** date")
    print(df1, n = 80)
  }
  debut <- as.POSIXct(strptime("07:00", "%H:%M"))
  fin <- as.POSIXct(strptime("19:00", "%H:%M"))
  df1 <- entete.df %>%
    glimpse() %>%
    mutate(saisieheure = as.POSIXct(strptime(cojsaisieheure, "%H:%M"))) %>%
    filter(saisieheure < debut | saisieheure > fin) %>%
    dplyr::select(cojid, statut = cojcreationstatut, cojcreationdate, cojsaisieheure, saisieheure)
  if (nrow(df1) > 0) {
    carp("**** heure: %s", nrow(df1))
    print(df1, n = 80)
  }
}
#
# mise en format LPO
#
# source("geo/scripts/coj.R");lpo_export()
lpo_export <- function() {
  carp()
  library(tidyverse)
  library(writexl)

  df1 <- entete.df %>%
    glimpse() %>%
    dplyr::select(
      id = cojid
      , date = cojsaisiedate
      , heure = cojsaisieheure
      , commune = cojjardincommune
      , departement = cojjardindepartement
      , insee = cojjardininsee
      , typelieu = cojjardintypelieu
      , mangeoire = cojnourouinon
    ) %>%
    replace_na(list(mangeoire = "non")) %>%
    mutate(mangeoire = ifelse(mangeoire == "on", "oui", "non")) %>%
    glimpse()
  df2 <- details.df %>%
    glimpse() %>%
    dplyr::select(
      id = cojdetailnumidentete
      , espece = cojdetailespece
      , nombre = cojdetailnombre
    ) %>%
    filter(id %in% df1$id) %>%
    glimpse()
  if (1 == 2) {
    library(xlsx)
    dsn <- sprintf("%s/lpo.xlsx", varDir)
    carp("dsn: %s", dsn)
    write.xlsx(df1, dsn, sheetName = "entete",
      col.names = TRUE, row.names = FALSE, append = FALSE)
    write.xlsx(df2, dsn, sheetName = "details",
      col.names = TRUE, row.names = FALSE, append = TRUE)
  }
  dsn <- sprintf("%s/lpo_entete.xlsx", varDir)
  carp("dsn: %s", dsn)
  write_xlsx(df1, dsn)
  dsn <- sprintf("%s/lpo_details.xlsx", varDir)
  write_xlsx(df2, dsn)
}
