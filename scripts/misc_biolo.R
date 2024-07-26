# <!-- coding: utf-8 -->
#
# la partie données biolovision faune-bretagne
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
# lecture des données, export de faune-bretagne
#
# https://cran.r-project.org/web/packages/curl/vignettes/intro.html#on_reusing_handles
fbDir <- sprintf("%s/bvi35/CouchesFauneBretagne", Drive);
tplBioloDir <- sprintf("%s/geo/BIOLO", baseDir)
#
## avec des templates
#
#
# url pour les nicheurs
#
# source("geo/scripts/apivn.R"); url <- biolo_url_fb(espece = "Alouette haussecol") %>% glimpse()
biolo_url_fb <- function(maille = "E015N686", espece = "Alouette haussecol",  depuis = "01.01.2019", odf = "nicheur") {
#  carp("espece: %s maille: %s", espece, maille)
  tpl <- sprintf("%s/biolo_%s_tpl.txt", tplBioloDir, odf)
#  carp("tpl: %s", tpl)
  df <- misc_lire("biolo63_especes") %>%
    filter(espece == !!espece)
  if (nrow(df) == 0) {
    carp("espece: %s", espece)
    re <- sprintf("^%s", espece)
    df <- misc_lire("biolo63_especes") %>%
      filter(grepl(re, espece))
    if (nrow(df) == 0) {
      carp("espece: %s", espece)
      return(invisible("???"))
    }
  }
  Species <- df[1, "id"]
  df <- misc_lire("biolo63_grids") %>%
    filter(maille == !! maille)
  Grid <- df[1, "id"]
  if (nrow(df) == 0) {
    confess("maille: %s", maille)
  }
  parametres <- readLines(tpl)
  date_jour <- Sys.Date()
  to <- strftime(date_jour, "%d.%m.%Y")
  variables <- list(
    espece = espece,
    maille = maille,
    Species = Species,
    Grid = Grid,
    From = depuis,
    To = to
    )
  parametres <- misc_list2tpl(variables, parametres)
  url <- paste(parametres, collapse = '&')
#  Carp("url: %s", url)
  url <- sprintf("https://www.faune-bretagne.org/index.php?%s", url)
  return(invisible(url))
}
#
# source("geo/scripts/apivn.R"); url <- biolo_url_ff() %>% glimpse()
biolo_url_ff <- function(maille = "E015N686", espece = "Barge à queue noire",  depuis = "01.01.2019", odf = "hiver") {
#  carp("espece: %s maille: %s", espece, maille)
  tpl <- sprintf("%s/biolo_%s_maille_espece_ff_tpl.txt", tplBioloDir, odf)
#  carp("tpl: %s", tpl)
  df <- misc_lire("biolo63_especes_ff") %>%
    filter(espece == !! espece)
  Species <- df[1, "id"]
  df <- misc_lire("biolo63_grids_ff") %>%
    filter(maille == !! maille)
  Grid <- df[1, "id"]
  parametres <- readLines(tpl)
  date_jour <- Sys.Date()
  to <- strftime(date_jour, "%d.%m.%Y")
  variables <- list(
    espece = espece,
    maille = maille,
    Species = Species,
    Grid = Grid,
    From = depuis,
    To = to
  )
  parametres <- misc_list2tpl(variables, parametres)
  url <- paste(parametres, collapse = '&')
#  Carp("url: %s", url)
  url <- sprintf("https://www.faune-france.org/index.php?%s", url)
  return(invisible(url))
}
#
# source("geo/scripts/apivn.R"); url <- biolo_url_fb_espece() %>% glimpse()
biolo_url_fb_espece <- function(espece = "Barge à queue noire",  depuis = "01.01.2019", odf = "hiver") {
#  carp("espece: %s", espece)
  tpl <- sprintf("%s/biolo_%s_espece_fb_tpl.txt", tplBioloDir, odf)
  if (! file.exists(tpl)) {
    carp("tpl: %s", tpl)
    stop("******")
  }
  df <- misc_lire("biolo63_especes_fb", apivnDir) %>%
    filter(espece == !! espece)
  Species <- df[1, "id"]
  parametres <- readLines(tpl)
  date_jour <- Sys.Date()
  to <- strftime(date_jour, "%d.%m.%Y")
  variables <- list(
    espece = espece,
    Species = Species,
    From = depuis,
    To = to
    )
  parametres <- misc_list2tpl(variables, parametres)
  url <- paste(parametres, collapse = '&')
#  Carp("url: %s", url)
  url <- sprintf("https://www.faune-bretagne.org/index.php?%s", url)
  firefox <- "C:\\Program Files\\Mozilla Firefox\\firefox.exe"
#  cmde <- sprintf(firefox, url)
#  system(firefox, invisible = FALSE)
  return(url)
}

#
# source("geo/scripts/apivn.R"); url <- biolo_url_ff() %>% glimpse()
biolo_url_ff_espece <- function(espece = "Barge à queue noire",  depuis = "01.01.2019", odf = "hiver") {
#  carp("espece: %s", espece)
  tpl <- sprintf("%s/biolo_%s_espece_ff_tpl.txt", tplBioloDir, odf)
  if (! file.exists(tpl)) {
    carp("tpl: %s", tpl)
    stop("******")
  }
  df <- misc_lire("biolo63_especes_ff", apivnDir) %>%
    filter(espece == !! espece)
  Species <- df[1, "id"]
  parametres <- readLines(tpl)
  date_jour <- Sys.Date()
  to <- strftime(date_jour, "%d.%m.%Y")
  variables <- list(
    espece = espece,
    Species = Species,
    From = depuis,
    To = to
    )
  parametres <- misc_list2tpl(variables, parametres)
  url <- paste(parametres, collapse = '&')
#  Carp("url: %s", url)
  url <- sprintf("https://www.faune-france.org/index.php?%s", url)
  firefox <- "C:\\Program Files\\Mozilla Firefox\\firefox.exe"
#  cmde <- sprintf(firefox, url)
#  system(firefox, invisible = FALSE)
  return(url)
}
#
# source("geo/scripts/apivn.R"); url <- biolo_url_fb_lieudit() %>% glimpse()
biolo_url_fb_lieudit <- function(lieudit = "307040") {
  url <- sprintf("https://www.faune-bretagne.org/index.php?m_id=104&%s", lieudit)
  return(url)
}
# sp_DCa=1 pour date de saisie
#
#
# export par auteur/observateur
#
biolo_export_auteur <- function(auteur_id, auteur_name, dsn='d:/test.xlsx', depuis = '01.01.2021', fin = '01.01.2050', Format='XLSX', tg='1', force=TRUE) {
  carp("auteur: %s %s", auteur_id, auteur_name)
  tpl <- sprintf("%s/biolo_%s_tpl.txt", tplBioloDir, "auteur")
  carp("tpl: %s", tpl)
  parametres <- readLines(tpl)
  date_jour <- Sys.Date()
  to <- strftime(date_jour, "%d.%m.%Y")
  variables <- list(
    ObserverName = auteur_id,
    ObserverNameString = auteur_name,
    From = depuis,
    To = fin,
    DateSynth = to
    ) %>%
    glimpse()
  parametres <- misc_list2tpl(variables, parametres)
  tpl <- sprintf("%s/biolo_%s.txt", tplBioloDir, "auteur")
  writeLines(tpl, parametres)
  url <- paste(parametres, collapse = '&')
  Carp("url: %s", url)
  url <- sprintf("%s?%s", biolo_url, url)
#  stop('***')
  biolo_export_xls(url, dsn, force = force)
  biolo_export_get(dsn, force = force)
  carp("auteur: %s dsn: %s", auteur_id, dsn)
}
#
# sur une emprise
biolo_export_bbox <- function(dsn='d:/test.xlsx', From = '01.01.1900', Nord, Ouest, Sud, Est, Format='XLSX', tg='1', force=TRUE) {
  tpl <- sprintf("%s/biolo_%s_tpl.txt", tplBioloDir, "bbox")
  carp("tpl: %s", tpl)
  parametres <- readLines(tpl)
  date_jour <- Sys.Date()
  to <- strftime(date_jour, "%d.%m.%Y")
  variables <- list(
    DFrom = From,
    DTo = to,
    CoordW = Ouest,
    CoordS = Sud,
    CoordE = Est,
    CoordN = Nord,
    DateSynth = to
    ) %>%
    glimpse()
  parametres <- misc_list2tpl(variables, parametres)
#  print(parametres); stop("*****")
  url <- paste(parametres, collapse = '&')
  carp("url: %s", url)
  url <- sprintf("https://www.faune-bretagne.org/index.php?%s", url)
#  stop('***')
  biolo_export_xls(url, dsn, force=force)
  biolo_export_get(dsn, force=TRUE)
  carp("dsn: %s", dsn)
}
#
# export pour coj, oiseaux des jardins
biolo_export_coj <- function(debut='15.01.2020', fin='15.02.2020', dsn='d:/test.xlsx', Format='XLSX', force = TRUE) {
  carp()
  tpl <- sprintf("%s/biolo_%s_tpl.txt", tplBioloDir, "coj")
  carp("tpl: %s", tpl)
  parametres <- readLines(tpl)
  date_jour <- Sys.Date()
  to <- strftime(date_jour, "%d.%m.%Y")
  variables <- list(
    DFrom = debut,
    DTo = fin,
    DateSynth = to
    ) %>%
    glimpse()
  parametres <- misc_list2tpl(variables, parametres)
  url <- paste(parametres, collapse = '&')
  carp("url: %s", url)
  url <- sprintf("https://www.faune-bretagne.org/index.php?%s", url)
  biolo_export_xls(url, dsn, force=force)
  biolo_export_get(dsn, force=TRUE)
  carp("dsn: %s", dsn)
}
#
# export avec un templacte
biolo_export_tpl <- function(
    debut = "15.01.2020",
    fin = "15.02.2050",
    dsn = "d:/test.xlsx",
    Format = "XLSX",
    tg = "1",
    DCa = 0,
    tpl = "date",
    espece = "111",
    filtre = "g+c+i",
    groupe = "16",
    force = TRUE
  ) {
  carp()
  tpl <- sprintf("%s/biolo%s_%s_tpl.txt", tplBioloDir, biolo_tpl, tpl)
  carp("tpl: %s", tpl)
  parametres <- readLines(tpl)
  date_jour <- Sys.Date()
  to <- strftime(date_jour, "%d.%m.%Y")
  variables <- list(
    tg = tg,
    FExportFormat = Format,
    DFrom = debut,
    DTo = fin,
    From = debut,
    To = fin,
    DCa = DCa,
    DateSynth = to,
    Espece = espece,
    Filtre = filtre,
    Family = groupe
    ) %>%
    glimpse()
  parametres <- misc_list2tpl(variables, parametres)
  url <- paste(parametres, collapse = '&')
  carp("url: %s", url)
#  mga <<- url;stop('***')
  url <- sprintf("%s/index.php?%s", biolo_url, url)
  biolo_export_xls(url, dsn, force = force)
  biolo_export_get(dsn, force = TRUE)
}
#
# export par departement pour le wetlands
# https://www.faune-bretagne.org/index.php?m_id=97&p_c=5&p_cc=-1&sp_tg=1
# &sp_DChoice=range&sp_DFrom=13.12.2018&sp_DTo=14.12.2018&sp_DSeasonFromDay=1&sp_DSeasonFromMonth=1&sp_DSeasonToDay=31&sp_DSeasonToMonth=12
# &sp_DOffset=5&speciesFilter=&sp_S=462&sp_SChoice=category&sp_Cat%5Bnever%5D=1&sp_Cat%5Bveryrare%5D=1&sp_Cat%5Brare%5D=1&sp_Cat%5Bunusual%5D=1&sp_Cat%5Bescaped%5D=1&sp_Cat%5Bcommon%5D=1&sp_Cat%5Bverycommon%5D=1&sp_Family=1
# &sp_PChoice=canton&sp_cC=0010&sp_cCO=0&sp_CommuneCounty=36&sp_Commune=13859&sp_Info=&sp_P=0&sp_Coord%5BW%5D=-1.606268576562&sp_Coord%5BS%5D=48.111339307451&sp_Coord%5BE%5D=-1.5882991246303&sp_Coord%5BN%5D=48.129308759383&sp_Grid_Info=
# &sp_Grid=0&sp_AltitudeFrom=-5&sp_AltitudeTo=387&sp_CommentValue=&sp_ObserverNameString=&sp_ObserverName=0&sp_OnlyAH=0&sp_Ats=-00000&sp_F=0&sp_project=&sp_I=0&sp_FDisplay=DATE_PLACE_SPECIES&sp_DFormat=DESC&sp_FChoice=export
# &sp_FExportFormat=XLSX&sp_ExportModel=36&sp_FOrderListSpecies=ALPHA&sp_FListSpeciesChoice=DATA&sp_DateSynth=15.12.2018&sp_FOrderSynth=ALPHA&sp_FGraphChoice=DATA&sp_FGraphFormat=auto&sp_FAltScale=250&sp_FAltChoice=DATA&sp_FMapFormat=none&submit=Chercher
biolo_export_departement <- function(departement, debut='13.12.2018', fin='14.12.2018', dsn='d:/test.xlsx', Format='XLSX', tg='1', force=T) {
  carp("departement: %s", departement)
  sp_cC <- switch(departement,
    '22' = '1000',
    '29' = '0100',
    '35' = '0010',
    '56' = '0001'
  )
  url <- c("https://www.faune-bretagne.org/index.php?m_id=97&p_c=5&p_cc=-1");
  url <- append(url, sprintf("&sp_tg=%s&sp_DateSynth=01.01.2021", tg));
  url <- append(url, sprintf('&sp_DChoice=range&sp_DFrom=%s&sp_DTo=%s', debut, fin))
# sp_DCa=0 pour date d'observation
  url <- append(url, sprintf('&sp_DCa=%s&sp_SChoice=all', 0))
# que le département 35
  url <- append(url, sprintf('&sp_PChoice=canton&sp_cC=%s&sp_cCO=0&sp_CommuneCounty=36', sp_cC));
#  url <- append(url, '&sp_PChoice=all');
  url <- append(url, sprintf('&sp_FChoice=export&sp_FExportFormat=%s&sp_ExportModel=36', Format));
  url <- paste(url, collapse = '')
#  print(sprintf("%s", url))
  biolo_export_xls(url, dsn, force=force)
  carp("departement: %s dsn: %s", departement, dsn)
}
#
# export par espèce
# source("geo/scripts/fb.R"); biolo_export_espece(espece='111')
biolo_export_espece <- function(espece='111', filtre = "g+c+i", dsn='d:/test.xlsx', depuis = '01.01.1900', Format='XLSX', tg='1', force=TRUE) {
  carp("espece: %s", espece)
  tpl <- sprintf("%s/biolo_%s_tpl.txt", tplBioloDir, "espece")
  carp("tpl: %s", tpl)
  parametres <- readLines(tpl)
  date_jour <- Sys.Date()
  to <- strftime(date_jour, "%d.%m.%Y")
  variables <- list(
    FExportFormat = Format,
    Espece = espece,
    Filtre = filtre,
    From = depuis,
    To = to,
    DateSynth = to
    ) %>%
    glimpse()
  parametres <- misc_list2tpl(variables, parametres)
  url <- paste(parametres, collapse = '&')
  carp("url: %s", url)
  url <- sprintf("https://www.faune-bretagne.org/index.php?%s", url)
#  stop('***')
  biolo_export_xls(url, dsn, force = force)
  biolo_export_get(dsn, force = force)
  carp("espece: %s dsn: %s", espece, dsn)
}
#
# export par famille
# doubloun avec groupe ...
# source("geo/scripts/fb.R"); biolo_export_famille(famille='16')
biolo_export_famille <- function(famille='16', dsn='d:/test.xlsx', depuis = '01.01.1900', Format='XLSX', tg='1', force=TRUE) {
  carp("famille: %s", famille)
  tpl <- sprintf("%s/biolo_%s_tpl.txt", tplBioloDir, "famille")
  carp("tpl: %s", tpl)
  parametres <- readLines(tpl)
  date_jour <- Sys.Date()
  to <- strftime(date_jour, "%d.%m.%Y")
  variables <- list(
    FExportFormat = Format,
    Famille = famille,
    From = depuis,
    To = to,
    DateSynth = to
    ) %>%
    glimpse()
  parametres <- misc_list2tpl(variables, parametres)
  url <- paste(parametres, collapse = '&')
  carp("url: %s", url)
  url <- sprintf("https://www.faune-bretagne.org/index.php?%s", url)
#  stop('***')
  biolo_export_xls(url, dsn, force = force)
  biolo_export_get(dsn, force = force)
  carp("espece: %s dsn: %s", famille, dsn)
}
biolo_export_formulaire <- function(dsn='d:/test.xlsx', depuis = '01.01.2021', fin = '31.12.2041', Format='XLSX', tg='1', DCa = 0, force=TRUE) {
  tpl <- sprintf("%s/biolo_formulaire_tpl.txt", tplBioloDir)
  carp("tpl: %s", tpl)
  parametres <- readLines(tpl)
  date_jour <- Sys.Date()
  to <- strftime(date_jour, "%d.%m.%Y")
  variables <- list(
    FExportFormat = Format,
    DFrom = depuis,
    DTo = fin,
    DateSynth = to
    ) %>%
    glimpse()
  parametres <- misc_list2tpl(variables, parametres)
  url <- paste(parametres, collapse = '&')
  url <- sprintf("%s/index.php?%s", biolo_url, url)
  carp("url: %s", url)
  stop("*****")
  biolo_export_xls(url, dsn, force = force)
  biolo_export_get(dsn, force = force)
  carp("dsn: %s", dsn)
}
biolo_export_formulaire_observateur <- function(observateur = "40", dsn='d:/test.xlsx', depuis = '01.01.2021', Format='XLSX', tg='1', force=TRUE) {
  tpl <- sprintf("%s/biolo_formulaire_observateur_tpl.txt", tplBioloDir)
  carp("tpl: %s", tpl)
  parametres <- readLines(tpl)
  date_jour <- Sys.Date()
  to <- strftime(date_jour, "%d.%m.%Y")
  variables <- list(
    ObserverName = observateur,
    FExportFormat = Format,
    From = depuis,
    To = to,
    DateSynth = to
    ) %>%
    glimpse()
  parametres <- misc_list2tpl(variables, parametres)
  url <- paste(parametres, collapse = '&')
  carp("url: %s", url)
  url <- sprintf("https://www.faune-bretagne.org/index.php?%s", url)
  biolo_export_xls(url, dsn, force = force)
  biolo_export_get(dsn, force = force)
  carp("dsn: %s", dsn)
}
#
# export par groupe
#
biolo_export_groupe <- function(groupe = "13", depuis='01.01.2021', fin='31.12.2021', dsn='d:/test.xlsx', Format='XLSX', tg='1', force=TRUE) {
  carp("groupe: %s", groupe)
  tpl <- sprintf("%s/biolo_%s_tpl.txt", tplBioloDir, "groupe")
  carp("tpl: %s", tpl)
  parametres <- readLines(tpl)
  date_jour <- Sys.Date()
  to <- strftime(date_jour, "%d.%m.%Y")
  variables <- list(
    FExportFormat = Format,
    Family = groupe,
    From = depuis,
    To = to,
    DateSynth = to
  ) %>%
    glimpse()
  parametres <- misc_list2tpl(variables, parametres)
  url <- paste(parametres, collapse = '&')
  carp("url: %s", url)
  url <- sprintf("https://www.faune-bretagne.org/index.php?%s", url)
  biolo_export_xls(url, dsn, force=force)
  biolo_export_get(dsn, force=TRUE)
  carp("groupe: %s dsn: %s", groupe, dsn)
}
#
# export par observateur
#
biolo_export_observateur <- function(observateur, dsn='d:/test.xlsx', depuis = '01.01.1900', fin = '26.02.2022', Format='XLSX', tg='1', force=TRUE) {
  carp("observateur: %s", observateur)
  tpl <- sprintf("%s/biolo_%s_tpl.txt", tplBioloDir, "observateur")
  carp("tpl: %s", tpl)
  parametres <- readLines(tpl)
  date_jour <- Sys.Date()
  to <- strftime(date_jour, "%d.%m.%Y")
  variables <- list(
    ObserverName = observateur,
    From = depuis,
    To = fin,
    DateSynth = to
    ) %>%
    glimpse()
  parametres <- misc_list2tpl(variables, parametres)
  tpl <- sprintf("%s/biolo_%s.txt", tplBioloDir, "observateur")
  writeLines(parametres, tpl)

  url <- paste(parametres, collapse = '&')
  carp("url: %s", url)
  url <- sprintf("https://www.faune-bretagne.org/index.php?%s", url)
#  stop('***')
  biolo_export_xls(url, dsn, force=force)
  biolo_export_get(dsn, force=TRUE)
  carp("observateur: %s dsn: %s", observateur, dsn)
}
#
# source("geo/scripts/cheveche35.R"); biolo_ff(); biolo_export_ff_observateur()
biolo_export_ff_observateur <- function(observateur = 14025, dsn = "d:/test.xlsx", depuis = "6.02.2022", fin = "26.02.2022", Format = "JSON", tg = "1", force = TRUE) {
  carp("observateur: %s", observateur)
  if ( file.exists(dsn) & force == FALSE ) {
    carp("*** fichier present: %s", dsn)
    return()
  }
  biolo_ff(force = TRUE)
  tpl <- sprintf("%s/biolo_%s_tpl.txt", tplBioloDir, "ff_observateur")
  carp("tpl: %s", tpl)
  parametres <- readLines(tpl)
  date_jour <- Sys.Date()
  to <- strftime(date_jour, "%d.%m.%Y")
  variables <- list(
    ObserverName = observateur,
    FExportFormat = Format,
    From = depuis,
    To = fin,
    DateSynth = to
    ) %>%
    glimpse()
  parametres <- misc_list2tpl(variables, parametres)
  url <- paste(parametres, collapse = '&')
#  writeLines(url)
  url <- sprintf("%s/index.php?%s", biolo_url, url)
  carp("url: %s", url)
  biolo_export_xls(url, dsn, force = force)
  biolo_export_get(dsn, force = TRUE)
  carp("observateur: %s dsn: %s", observateur, dsn)
}
#
# pour les projets
biolo_export_projet <- function(
    dsn = "d:/test.xlsx",
    PROJET = "EPOC-ODF",
    projet = "projet_epoc_odf",
    depuis = "01.01.2021",
    fin = "31.12.2041",
    Format = "XLSX",
    tg = "1",
    DCa = 0,
    force=TRUE
  ) {
  tpl <- sprintf("%s/biolo_%s_%s_tpl.txt", tplBioloDir, projet, biolo_suffixe)
  carp("tpl: %s", tpl)
  parametres <- readLines(tpl)
  date_jour <- Sys.Date()
  to <- strftime(date_jour, "%d.%m.%Y")
  variables <- list(
    P = PROJET,
    FExportFormat = Format,
    DFrom = depuis,
    DTo = fin,
    DateSynth = to
    ) %>%
    glimpse()
  parametres <- misc_list2tpl(variables, parametres)
  url <- paste(parametres, collapse = '&')
  url <- sprintf("%s/index.php?%s", biolo_url, url)
  carp("url: %s", url)
#  stop("*****")
  biolo_export_xls(url, dsn, force = force)
  biolo_export_get(dsn, force = force)
  carp("dsn: %s", dsn)
}
#
# export par protocole
# https://www.faune-bretagne.org/index.php?m_id=97&p_c=2&p_cc=-1&sp_tg=1&sp_DateSynth=30.10.2019&sp_DChoice=range&sp_DFrom=08.01.2019&sp_DTo=30.01.2019&sp_DCa=0&sp_SChoice=all&sp_PChoice=canton&sp_cC=0010&sp_OnlyStoc=WATERBIRD&backlink=skip&sp_FChoice=export&sp_FExportFormat=XLSX&sp_ExportModel=1
biolo_export_protocole <- function(dsn='d:/test.xlsx', Format='XLSX', tg='1', force=T) {
  carp()
  url <- 'https://www.faune-bretagne.org/index.php?m_id=97&p_c=2&p_cc=-1&sp_tg=1&sp_DateSynth=30.10.2019&sp_DChoice=range&sp_DFrom=08.01.2019&sp_DTo=30.01.2019&sp_DCa=0&sp_SChoice=all&sp_PChoice=canton&sp_cC=0010&sp_OnlyStoc=WATERBIRD&backlink=skip&sp_FChoice=export&sp_FExportFormat=XLSX&sp_ExportModel=36'
  biolo_export_xls(url, dsn, force=force)
  carp("dsn: %s", dsn)
}
#
# export protocole shoc
# !! ne marche pas, je n'ai pas les droits
biolo_export_proto_shoc <- function(dsn = 'd:/test.xlsx', depuis = "01.01.1900", Format = 'XLSX', tg = '1', force = TRUE) {
  carp()
  tpl <- sprintf("%s/biolo_%s_tpl.txt", tplBioloDir, "proto_shoc")
  carp("tpl: %s", tpl)
  parametres <- readLines(tpl)
  date_jour <- Sys.Date()
  to <- strftime(date_jour, "%d.%m.%Y")
  variables <- list(
    FExportFormat = Format,
    From = depuis,
    To = to,
    DateSynth = to
    ) %>%
    glimpse()
  parametres <- misc_list2tpl(variables, parametres)
  url <- paste(parametres, collapse = '&')
  carp("url: %s", url)
  url <- sprintf("https://www.faune-bretagne.org/index.php?%s", url)
#  stop('***')
  biolo_export_xls(url, dsn, force=force)
  biolo_export_get(dsn, force=TRUE)
}
#
# export protocole stoc
biolo_export_proto_stoc <- function(dsn = 'd:/test.xlsx', depuis = "01.01.1900", Format = 'XLSX', tg = '1', force = TRUE) {
  carp()
  tpl <- sprintf("%s/biolo_%s_tpl.txt", tplBioloDir, "proto_stoc")
  carp("tpl: %s", tpl)
  parametres <- readLines(tpl)
  date_jour <- Sys.Date()
  to <- strftime(date_jour, "%d.%m.%Y")
  variables <- list(
    FExportFormat = Format,
    From = depuis,
    To = to,
    DateSynth = to
    ) %>%
    glimpse()
  parametres <- misc_list2tpl(variables, parametres)
  url <- paste(parametres, collapse = '&')
  carp("url: %s", url)
  url <- sprintf("https://www.faune-bretagne.org/index.php?%s", url)
#  stop('***')
  biolo_export_xls(url, dsn, force=force)
  biolo_export_get(dsn, force=TRUE)
}
#
# export protocole wetland waterbird
biolo_export_proto_waterbird <- function(dsn = 'd:/test.xlsx', debut = "01.01.1900", fin = "01.01.2030",Format = 'XLSX', force = TRUE) {
  carp()
  tpl <- sprintf("%s/biolo_%s_tpl.txt", tplBioloDir, "proto_waterbird")
  carp("tpl: %s", tpl)
  parametres <- readLines(tpl)
  date_jour <- Sys.Date()
  to <- strftime(date_jour, "%d.%m.%Y")
  variables <- list(
    FExportFormat = Format,
    From = debut,
    To = fin,
    DateSynth = to
    ) %>%
    glimpse()
  parametres <- misc_list2tpl(variables, parametres)
  url <- paste(parametres, collapse = '&')
  carp("url: %s", url)
  url <- sprintf("https://www.faune-bretagne.org/index.php?%s", url)
#  stop('***')
  biolo_export_xls(url, dsn, force=force)
  biolo_export_get(dsn, force=TRUE)
}
biolo_export_region <- function(
    debut='13.12.2018',
    fin = "01.01.2030",
    dsn = 'd:/test.xlsx',
    Format = 'XLSX',
    tg = '1',
    dca = '1',
    force = T
  ) {
  carp()
  tpl <- sprintf("%s/biolo_%s_tpl.txt", tplBioloDir, "coj")
  carp("tpl: %s", tpl)
  parametres <- readLines(tpl)
  date_jour <- Sys.Date()
  to <- strftime(date_jour, "%d.%m.%Y")
  variables <- list(
    DFrom = debut,
    DTo = fin,
    DateSynth = to
    ) %>%
    glimpse()
  parametres <- misc_list2tpl(variables, parametres)
  url <- paste(parametres, collapse = '&')
  carp("url: %s", url)
  url <- sprintf("https://www.faune-bretagne.org/index.php?%s", url)
  biolo_export_xls(url, dsn, force = force)
  biolo_export_get(dsn, force = TRUE)
  carp("dsn: %s", dsn)
}
# https://www.faune-france.org/index.php?m_id=1381&id_form=2181298&ExportFormat=JSON
# source("geo/scripts/fb.R"); biolo_ff();biolo_export_trace()
# gci35 secteur 4 le 14/06/2024
# source("geo/scripts/fb.R"); biolo_ff();biolo_export_trace(id_form = 3232443)
biolo_export_trace <- function(id_form=2181298, dsn = 'd:/test.json') {
  carp()
  biolo_handle()
  url <- sprintf("m_id=1381&id_form=%s&ExportFormat=JSON", id_form)
  carp("url: %s", url)
  url <- sprintf("%s?%s", biolo_url, url)
  carp("dsn: %s", dsn)
  res <- curl_fetch_disk(url, dsn, handle = biolo_h)
  Sys.sleep(3)
  print(res$content)
}
#
#
biolo_export_xls <- function(url, dsn, force = FALSE) {
  Carp("dsn: %s", dsn)
  library(curl)
  if ( force == FALSE & file.exists(dsn)) {
    return()
  }
  if ( file.exists(dsn)) {
    file.remove(dsn)
  }
  dsn <- sub('\\.[^.]+$', '.jsonb', dsn)
  if ( file.exists(dsn)) {
    file.remove(dsn)
  }
#  library(XML)
  h <- biolo_handle()
#  carp("dsn: %s url: %s", dsn, url)
  dest <- sprintf("%s/biolo_export_xls.txt", bioloDir)
  params <- str_split_1(url, "&")
  writeLines(params, dest)
  carp("dest: %s", dest)
#  stop("*****")
  res <- curl_fetch_disk(url, dsn, handle = h)
  Sys.sleep(3)
  print(res$content)
#  stop("****")
#  lignes <- readLines(res$content)
#  print(lignes);
#  stop('*****')
#  biolo_tous_les_exports()
}
#
# la version d'août 2018
# source("geo/scripts/fb.R"); biolo_ff(); biolo_tous_les_exports()
biolo_tous_les_exports <- function(dest = FALSE) {
  carp()
  require(stringr)
  library(jsonlite)
  lignes <- biolo_tous_les_exports_get()
#  writeLines(Lignes)
  js_lignes <- subset(lignes, grepl('AsyncDownloadList\\(\\[', lignes))
  print(js_lignes[1])
  js <- js_lignes[1]
#  return();
#  lignes <- paste(lignes, collapse="|")
  pattern <- '\\{.*?\\}'
  xls_lignes <- str_match_all(js, pattern)
#  print(xls_lignes)
  pattern <- '\\((.*)\\)'
  json_lignes <- str_match_all(js, pattern)
  json_lignes <- unlist(json_lignes[[1]])
#  print(json_lignes)
  json <- json_lignes[1, 2]
  df <- fromJSON(json)
#  View(df)
  for ( i in 1:nrow(df) ) {
    carp("file_name: %s", df[i, 'file_name'])
    if ( df[i, 'available'] != 1 ) {
      carp("unavailable %s", df[i, 'file_name'])
      next
    }
#    next
    dsn <- sprintf("%s/%s", bioloDir, df[i, 'file_name'])
    url <- sprintf('%s/index.php?m_id=1472&content=export&id_file=%s', biolo_url, df[i, 'id'])
    if ( ! file.exists(dsn) ) {
      curl_download(url, dsn, handle = biolo_h)
    }
    carp(" dsn: %s", dsn)
    if ( i == nrow(df) && dest != FALSE ) {
      carp("copy dest: %s", dest)
      file.copy(dsn, dest, overwrite = TRUE, copy.mode = TRUE, copy.date = FALSE)
    }
  }
}
#
# récupération d'un export
biolo_export_get <- function(dest = FALSE, force = FALSE) {
  require(tidyverse)
  library(jsonlite)
  Carp('dest: %s', dest)
  if ( file.exists(dest) & force == FALSE ) {
    carp("*** fichier present: %s", dest)
    return()
  }
  if ( file.exists(dest)) {
    file.remove(dest)
  }
  dsn <- sub('\\.[^.]+$', '.jsonb', dest)
  if ( ! file.exists(dsn) ) {
    carp("*** fichier absent: %s", dsn)
    return()
  }
  df <- read_json(dsn)
  glimpse(df)
  file_name <- df['file_name']
  carp("file_name: %s", file_name)
#  biolo_fb()
  h <- biolo_handle()
  url <- sprintf('%s/index.php?m_id=1472&content=export&id_file=%s', biolo_url, df["id"])
  carp("url: %s", url)
  for (i in seq(1, 1000)) {
    req <- curl_fetch_disk(url, dest, handle = h)
    carp("status: %s", req$status_code)
    if ( req$status_code == 200) {
      break
    }
    if ( file.exists(dest) & file.info(dest)$size > 0) {
      carp("*** fichier present: %s", dest)
      break
    }
    carp("attente: %d id_file: %s", i, df["id"])
    Sys.sleep(10)
  }
#  stop('***')
}
#
# test
# source("geo/scripts/geoca.R");biolo_export_test() %>% glimpse()
biolo_export_test <- function() {
  require(tidyverse)
  library(jsonlite)
  biolo_ff()
  h <- biolo_handle()
  dest <- "d:/biolo_export_test.xlsx"
  url <- sprintf('%s/index.php?m_id=1472&content=export&id_file=%s', biolo_url, "74465")
  carp("url: %s", url)
  req <- curl_fetch_disk(url, dest, handle = h)
  carp("status: %s", req$status_code)
}
#
# la liste des fichiers d'export
# source("geo/scripts/geoca.R"); biolo_ff();biolo_tous_les_exports_get() %>% glimpse()
biolo_tous_les_exports_get <- function() {
  carp()
  library(curl)
#  h <- biolo_handle()
  url <- "m_id=1635"
  url <- sprintf("%s/index.php?%s", biolo_url, url)
  carp("url: %s", url)
  dsn <- sprintf("%s/biolo_tous_les_exports.html", bioloDir)
  res <- curl_fetch_disk(url, dsn, handle = biolo_h)
#  res$content
#  print(res$content)
  lignes <- readLines(res$content)
  carp("dsn: %s", dsn)
  return(invisible(lignes))
}
#
# lecture des fichiers json
# source("geo/scripts/fb.R");biolo_export_json()
biolo_export_json <- function(Format='XLSX') {
  carp("")
  biolo_fbdir();
  library(jsonlite)
#  library(tidyverse)
  files <- list.files(exportDir, pattern = '\\.json$', full.names = TRUE, ignore.case = TRUE)
  carp("nb files: %s", length(files))
  if ( exists("json.df") ) {
    remove(json.df)
  }
  for (i in 1:length(files) ) {
    file <- files[i]
    if ( ! grepl("/20\\d\\d_[01]\\d", file) ) {
#      next;
    }
    df <- fromJSON(file, flatten = TRUE)
#    print(class(df))
#    print(str(df))
#    View(df); stop('****');
    file_name <- df$file_name
    carp("%s %s", file_name, file)
    file_name <- sprintf("%s/%s", bioloDir, file_name)
    if ( ! file.exists(file_name) ) {
      carp("***absent")
#      file.remove(file)
      next;
    }
    fn <- sub('.*/', '', file)
    fn <- sub('\\.json$', '', fn)
    fn <- sprintf("%s/%s.%s", exportDir, fn, tolower(Format))
    carp(fn)
    file.copy(file_name, fn, copy.date=TRUE)
  }
}
#
# import des fichiers de données
# oiseaux, date d'observation
# source("geo/scripts/fb.R");biolo_import_donnees(Format='XLSX', dca=0, tg=1)
# oiseaux, date de saisie
# source("geo/scripts/fb.R");biolo_import_donnees(Format='XLSX', dca=1, tg=1)
biolo_import_donnees <- function(Format='XLSX') {
  carp("")
  files <- list.files(exportDir, pattern = sprintf('\\.%s$', tolower(Format)) , full.names = TRUE, ignore.case = TRUE)
  carp("nb files: %s", length(files))
  if ( exists("rda.df") ) {
    remove(rda.df)
  }
  for (i in 1:length(files) ) {
#    print(sprintf("sql_import_donnees() %s", files[i]))
    if ( ! grepl("/20", files[i]) ) {
#      next;
    }
    file <- files[i]
    dsn <- sprintf("%s.Rda", file);
    if ( ! file.exists(dsn) ) {
      biolo_import_donnees_xl2rda(file, dsn)
    }
#    next;
    load(file=dsn)
    carp("nrow:%s dsn:%s", nrow(df), dsn)
    if ( exists("rda.df") ) {
      rda.df <- rbind(rda.df, df)
    } else {
      rda.df <- df
    }
    remove(df)
  }
  dsn <- sprintf("%s/donnees.Rda", exportDir);
  file.remove(dsn)
  save(rda.df, file=dsn)
  carp("nrow: %s dsn: %s", nrow(rda.df), dsn)
}

# extraction des données avec un filtre
biolo_rda_donnees <- function() {
  carp("")
  files <- list.files(exportDir, pattern = '^\\d.*\\.Rda$', full.names = TRUE, ignore.case = TRUE)
  carp("nb files: %s", length(files))
  if ( exists("rda.df") ) {
    remove(rda.df)
  }
  for (i in 1:length(files) ) {
    dsn <- files[i]
    load(file=dsn)
    carp("nrow:%s dsn:%s", nrow(df), dsn)
    df <- subset(df, NAME_SPECIES %in% filtre.df$NAME_SPECIES)
    carp("nrow:%s dsn:%s", nrow(df), dsn)

#    print(colnames(df))
#    View(df);stop('***')
    if ( exists("rda.df") ) {
      rda.df <- rbind(rda.df, df)
    } else {
      rda.df <- df
    }
    remove(df)
  }
  return(invisible(rda.df))
}
# lecture d'un fichier Rda
biolo_rda_donnees_lire <- function(id) {
  carp("")
  dsn <- sprintf("%s/%s.xlsx.Rda", exportDir, id);
  load(file=dsn)
  return(invisible(df))
}
#
#
# lecture du fichier rda
# source("geo/scripts/fb.R");biolo_oiseaux_rda_lire()
biolo_oiseaux_rda_lire <- function(tg=1, dca=1, force=FALSE) {
  if ( exists("oiseaux.df") & force==FALSE) {
    return(oiseaux.df)
  }
  dsn <- sprintf("%s/web.var/geo/fb2018/mois_%s_%s/donnees.Rda", Drive, tg, dca);
  load(file=dsn)
  df <- rda.df
  carp("nrow:%s dsn:%s", nrow(df), dsn)
  df$Date <- sprintf("%s.%s.%s", df$DATE_DAY, df$DATE_MONTH, df$DATE_YEAR)
  df$d <- as.Date(df$Date, "%d.%m.%Y")
  df$departement <- df$COUNTY
  inconnuDF <- subset(df, is.na(df$d))
  if ( length(inconnuDF$d) > 0 ) {
    carp(" date invalide")
    print(head(inconnuDF))
    stop("***")
  }
#  colnames(df) <- tolower(colnames(df))
#  colnames(df)[colnames(df)=="name_species"] <- "espece"
#  colnames(df)[colnames(df)=="municipality"] <- "lib_commune"
  df$lib_commune <- sprintf("%s/%s", df$MUNICIPALITY, df$COUNTY)
  df$lib_auteur <- sprintf("%s %s", df$SURNAME, df$NAME)
  oiseaux.df <<- df
  return(invisible(df))
}

biolo_export_lire_sf <- function(dsn) {
  library(readxl)
  library(janitor)
  carp("dsn: %s", dsn)
  df <- read_excel(dsn, col_names = TRUE)
  df <- df[-1,]
  df <- df %>%
    clean_names() %>%
    remove_empty(c("rows")) %>%
    glimpse()
  for ( a in c("coord_lon", "coord_lat", "atlas_code", "date_year") ) {
    df[, a] <- sapply(df[, a], as.numeric)
  }
  nc <- st_as_sf(df, coords = c("coord_lon", "coord_lat"), crs = 4326, remove=FALSE) %>%
    glimpse()
  return(invisible(nc))
}

#
# lecture des espèces, source faune-bretagne
# à partir du formulaire de saisie
biolo_espece_lire <- function() {
  f_utf8 <- sprintf("%s/web/geo/BIOLOVISION/espece.csv", Drive)
  carp("f_utf8: %s", f_utf8)
  df <- read.csv(file = f_utf8, head=TRUE, sep = ';', quote = '')
  carp("f_utf8:%s nb:%d", f_utf8, length(df$id_species))
  return(df)
}
#
## sauvegarde des extractions ---------------------------------
#
# export version journalière avec la date de saisie
biolo_export_donnees_jour <- function(dsnExport = "d:/fb_export_donnees.xlsx", depuis="01.05.2020", tg=1, dca=1, Format="XLSX", force=FALSE) {
  carp()
  library(readxl)
  library(writexl)
  exportDir <<- sprintf("%s/jour_%s_%s", fbDir, tg, dca);
  dir.create(exportDir, showWarnings = FALSE, recursive = TRUE)
  format <- "%d.%m.%Y"
  date_jour <- Sys.Date() - 1
  jours <- seq(as.Date(depuis, format = format), date_jour, by = "days")
  jours <- format(jours, format)
  df1 <- data.frame()
  for (i in 1:length(jours)) {
    jour <- jours[i]
    aaaammjj <- format(as.Date(jour, format = format), "%Y_%m_%d")
    dsn <- sprintf("%s/%s.%s", exportDir, aaaammjj, tolower(Format))
    if (! file.exists(dsn)) {
      carp("%s %s %s", jour, aaaammjj, dsn)
      biolo_export_region(debut = jour, fin = jour, dsn, dca = dca, tg = tg, Format = Format, force = TRUE)
      biolo_export_get(dsn, force = TRUE)
    }
    df <- readxl::read_excel(dsn, col_names = TRUE)
# la première ligne en moins
    df <- df[-1, ]
    carp("nrow: %s dsn: %s", nrow(df), dsn)
    if (nrow(df) < 1) {
      next
    }
    df1 <- rbind(df1, df)
  }
  writexl::write_xlsx(df1, path = dsnExport)
  carp("nrow: %s dsnExport: %s", nrow(df1), dsnExport)
}
#
# export version mensuelle, avec la date de saisie ou la date de l"observation
biolo_export_donnees_mois <- function(dsnExport = "d:/fb_export_donnees.xlsx", depuis="01.01.2020", fin="01.05.2020", tg = 1, dca = 1, Format = "XLSX", force = FALSE) {
  carp("début")
  library(readxl)
  library(writexl)
  exportDir <<- sprintf("%s/mois_%s_%s", fbDir, tg, dca);
  dir.create(exportDir, showWarnings = FALSE, recursive = TRUE)
  format <- "%d.%m.%Y"
  date_jour <- as.Date("21.07.2017", format = format)
  date_jour <- Sys.Date()
  if (dca == 0) {
    mois <- seq(as.Date(depuis, format = format), as.Date(fin, format = format), by = "1 month")
  } else {
#  mois <- seq(as.Date("01.01.2017", format = format), date_jour, by = "1 month")
    mois <- seq(as.Date("01.04.2013", format = format), date_jour, by = "1 month")
  }
  df1 <- data.frame()
# pour le mois en cours
  mois[length(mois) + 1] <- date_jour
  for (i in 1:(length(mois) - 2)) {
    from <- mois[i]
    to <- mois[i + 1] - 1
    from <- format(from, format)
    to <- format(to, format)
    aaaamm <- format(mois[i], "%Y_%m")
    dsn <- sprintf("%s/%s.%s", exportDir, aaaamm, tolower(Format))
    json <- sub("\\.[^.]+$", ".json", dsn)
    if (! file.exists(dsn)) {
      carp("%s %s %s %s", from, to, aaaamm, dsn)
      biolo_export_region(debut = from, fin = to, dsn, dca = dca, tg = tg, Format = Format, force = TRUE)
      biolo_export_get(dsn, force = TRUE)
    }
#    next
    df <- readxl::read_excel(dsn, col_names = TRUE)
# la première ligne en moins
    df <- df[-1, ]
    carp("nrow: %s dsn: %s", nrow(df), dsn)
    if (nrow(df) < 1) {
      next
    }
    df1 <- rbind(df1, df)
  }
  glimpse(df1)
  dsn <- gsub(".xlsx$", ".Rds", dsnExport)
  saveRDS(df1, file=dsn)
  carp("dsn: %s", dsn)
  writexl::write_xlsx(df1, path = dsnExport)
  carp("nrow: %s dsnExport: %s", nrow(df1), dsnExport)
}
#
## lecture des fichiers de données
#
# lecture des données, export de faune-bretagne
biolo_lire_xls <- function(fic="donnees.xls") {
  library(readxl)
  library("raster")
# mga 22/05/2017
#  dsn <- sprintf("%s/%s", fbDir, fic)
# mga 09/02/2018
  dsn <- fic
  carp("dsn: %s", dsn)
  df <- readxl::read_excel(dsn, col_names = TRUE)
  spdf <- biolo_lire_clean(df)
  return(invisible(spdf))
}
#
# normalisation du fichier Excel
biolo_lire_clean <- function(df) {
#  print(head(df[,c('COORD_LAT', 'COORD_LON')]))
#  print(colnames(df))
#  df <- df[, c('ID_SIGHTING', 'ID_SPECIES', 'NAME_SPECIES', 'FAMILY_NAME', 'DATE', 'PLACE', 'MUNICIPALITY', 'INSEE', 'COORD_LAT', 'COORD_LON', 'COMMENT', 'ESTIMATION_CODE', 'TOTAL_COUNT', 'PRECISION', 'NAME', 'SURNAME')]
# la première ligne en moins
  df <- df[-1,]
# la date format R
  df$Date <- sprintf("%s.%s.%s", df$DATE_DAY, df$DATE_MONTH, df$DATE_YEAR)
  df$d <- as.Date(df$Date, "%d.%m.%Y")
  inconnuDF <- subset(df, is.na(df$d))
  if ( length(inconnuDF$d) > 0 ) {
    carp("date invalide")
    print(head(inconnuDF))
    stop("***")
  }
  df$departement <- df$COUNTY
# transformation en spatial
  df [,"COORD_LAT"] <- sapply(df[,"COORD_LAT"], as.character)
  df [,"COORD_LAT"] <- sapply(df[,"COORD_LAT"], as.numeric)
  df [,"COORD_LON"] <- sapply(df[,"COORD_LON"], as.character)
  df [,"COORD_LON"] <- sapply(df[,"COORD_LON"], as.numeric)
  bug.df <- subset(df, COORD_LAT < 48)
  if ( nrow(bug.df) > 0 ) {
    print(head(bug.df))
    stop("***")
  }
  coordinates(df) = ~ COORD_LON + COORD_LAT
#  print(sapply(df, class))
  spdf <- SpatialPointsDataFrame(df,data.frame(df[,]))
  proj4string(spdf) <- CRS("+init=epsg:4326")
  spdf <- spTransform(spdf, CRS("+init=epsg:2154"))
  carp("nrow: %d", nrow(spdf@data))
#  stop("====")
  return(spdf)
}
#
# que les données atlas
biolo_lire_atlas <- function(spdf) {
  library("raster")
  zone.spdf <- fonds_grille_lire()
#  print(head(zone.spdf@data))
  spdf@data$NUMERO <- over(spdf, zone.spdf)$NUMERO
  df <- spdf@data
  inconnuDF <- subset(df, is.na(df$NUMERO))
  if ( length(inconnuDF$NUMERO) > 0 ) {
    carp(" hors mailles %s", nrow(inconnuDF))
#    print(head(inconnuDF))
#    stop("faune_lire_atlas()")
  }
  spdf <- spdf[!(is.na(spdf@data$NUMERO)),]
  carp("nrow: %d", nrow(spdf@data))
#  stop("***")
  return(spdf)
}
#
# lecture d'un fichier d'export VisioNature
biolo_export_lire_sf <- function(dsn, force=FALSE) {
  if ( exists('export.sf') & force==FALSE) {
    return(invisible(export.sf))
  }
  require(sf)
  require(janitor)
  require(tidyverse)
  library(rio)
  carp('dsn: %s', dsn)
  df <- rio::import(dsn) %>%
    glimpse()
# la première ligne en moins
  df <- df[-1,] %>%
    mutate(lat=as.numeric(COORD_LAT)) %>%
    mutate(lon=as.numeric(COORD_LON)) %>%
    mutate(date = as.Date(as.numeric(as.character(DATE)), origin="1899-12-30")) %>%
    glimpse()
  export.sf <<- st_as_sf(df, coords = c("lon", "lat"), crs = 4326)
  return(invisible(export.sf))
}
#
## les handle de connexion aux sites
#
# https://cran.r-project.org/web/packages/curl/vignettes/intro.html#on_reusing_handles
#
biolo_fb <- function(force = FALSE) {
  carp()
  biolo_url <<- "https://www.faune-bretagne.org"
  biolo_SUFFIXE <<- "FB"
  biolo_tpl <<- ""
  USERNAME <<- mes_options("biolo_username")
  PASSWORD <<- mes_options("biolo_password")
  biolo_site(force = force)
}
biolo_ff <- function(force = FALSE) {
  carp()
  biolo_url <<- "https://www.faune-france.org"
  biolo_SUFFIXE <<- "FF"
  biolo_tpl <<- "_ff"
  USERNAME <<- mes_options("biolo_ff_username")
  PASSWORD <<- mes_options("biolo_ff_password")
  departements_id <<- c("23", "36", "30", "57", "51", "54")
  biolo_site(force = force)
}
biolo_fbzh <- function(force = FALSE) {
  carp()
  biolo_url <<- "https://bzh.biolovision.net"
  biolo_SUFFIXE <<- "FBZH"
  biolo_tpl <<- "_fbzh"
  USERNAME <<- mes_options("biolo_username")
  PASSWORD <<- mes_options("biolo_password")
  departements_id <<- c("23", "36", "30", "57", "51", "54")
  biolo_site(force = force)
}
biolo_site <- function(force = FALSE) {
  biolo_suffixe <<- tolower(biolo_SUFFIXE)
  bioloDir <<- sprintf("%s/bvi35/Couches%s", Drive, biolo_SUFFIXE)
  biolo6Dir <<- sprintf("%s/biolo6", bioloDir)
  biolo8Dir <<- sprintf("%s/biolo8", bioloDir)
  biolo10Dir <<- sprintf("%s/biolo10", bioloDir)
  biolo53Dir <<- sprintf("%s/biolo53", bioloDir)
  biolo63Dir <<- sprintf("%s/biolo63", bioloDir)
  biolo98Dir <<- sprintf("%s/biolo98", bioloDir)
  biolo104Dir <<- sprintf("%s/biolo104", bioloDir)
  biolo1380Dir <<- sprintf("%s/biolo1380", bioloDir)
  biolo1389Dir <<- sprintf("%s/biolo1389", bioloDir)
  biolo1390Dir <<- sprintf("%s/biolo1390", bioloDir)
  biolo1441Dir <<- sprintf("%s/biolo1441", bioloDir)
  biolo60008Dir <<- sprintf("%s/biolo60008", bioloDir)
  dir.create(biolo6Dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(biolo8Dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(biolo10Dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(biolo53Dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(biolo63Dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(biolo98Dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(biolo104Dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(biolo1380Dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(biolo1389Dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(biolo1390Dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(biolo1441Dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(biolo60008Dir, showWarnings = FALSE, recursive = TRUE)
  if (force == TRUE ) {
    biolo_handle(force)
  }
}
biolo_handle <- function(force = FALSE) {
  if ( exists('biolo_h') && force == FALSE ) {
    return(biolo_h)
  }
  carp()
  if ( ! exists('biolo_url') ) {
    biolo_fb()
  }
  library(curl)
  url_login <- sprintf("%s/index.php?m_id=1&logout=1", biolo_url);
  h <- new_handle()
  handle_setheaders(h,
    "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; WOW64; rv:51.0) Gecko/20100101 Firefox/51.0",
    "Accept-Encoding" = "identity"
  )
  handle_setform(h,
    login = "1",
    USERNAME = USERNAME,
    PASSWORD = PASSWORD,
    login_button= "Me connecter"
  )
  req <- curl_fetch_memory(url_login, handle = h)
  biolo_h <<- h
  return(h)
#  print(handle_cookies(h))
}
# la version faune-france
# source("geo/scripts/epoc.R");biolo_handle_ff(force = TRUE)
biolo_handle_ff <- function(force = FALSE) {
  if ( exists('biolo_h') && force == FALSE ) {
    return(biolo_h)
  }
  carp()
  biolo_ff()
  library(httr)
  return(h)
}
#
# la version avec rvest
# https://riptutorial.com/r/example/23955/using-rvest-when-login-is-required
#
# source("geo/scripts/cheveche35.R"); biolo_ff(); biolo_session()
biolo_session <- function(force = FALSE) {
  if ( exists('biolo_s') && force == FALSE ) {
    return(biolo_s)
  }
  carp()
  if ( ! exists('biolo_url') ) {
    biolo_ff()
  }
  library(rvest)
  url_login <- sprintf("%s/index.php?m_id=1&logout=1", biolo_url);
  session <- session(url_login)
  form <- html_form(session)[[1]]
  filled_form <- html_form_set(form, USERNAME = USERNAME, PASSWORD = PASSWORD)
  page <- session_submit(session, filled_form)
  statut <- page %>%
    html_node(".profile-card-container") %>%
    html_text()
  carp("statut: %s", statut)
  biolo_s <<- session
  return(invisible(biolo_s))
}
#
#
biolo_session_xls <- function(url, dsn, force = FALSE) {
  Carp("dsn: %s", dsn)
  if ( force == FALSE & file.exists(dsn)) {
    return()
  }
  if ( file.exists(dsn)) {
    file.remove(dsn)
  }
  dsn <- sub('\\.[^.]+$', '.jsonb', dsn)
  if ( file.exists(dsn)) {
    file.remove(dsn)
  }
#  library(XML)
  h <- biolo_handle(force)
#  carp("dsn: %s url: %s", dsn, url)
  res <- curl_fetch_disk(url, dsn, handle = h)
  Sys.sleep(3)
  print(res$content)
  stop("****")
#  lignes <- readLines(res$content)
#  print(lignes);
#  stop('*****')
#  biolo_tous_les_exports()
}

#
# requete avec réponse immédiate
biolo_get <- function(url, dsn, force=FALSE) {
  carp("dsn: %s url: %s", dsn, url)
  library(curl)
  h <- biolo_handle()
  res <- curl_fetch_disk(url, dsn, handle = h)
  Sys.sleep(3)
  print(res$content)
}
biolo_xml_parse_files <- function(files) {
  carp()
  library(xml2)
  library(tidyverse)
  for (i in 1:length(files) ) {
    dsn <- files[[i]]
    df2 <- biolo_xml_parse_file(dsn)
    if( class(df2) != "data.frame") {
      carp("**** dsn: %s", dsn)
      next;
    }
    if(exists('df1')) {
#      df1[setdiff(names(df2), names(df1))] <- NA
#      df2[setdiff(names(df1), names(df2))] <- NA
      df1 <- rbind(df1, df2)
#      df1 <- bind_rows(df1, df2)
    } else {
      df1 <- df2
    }
  }
  glimpse(df1)
  carp("df1: %s", nrow(df1))
  return(invisible(df1))
}
biolo_xml_parse_file <- function(dsn) {
  library(xml2)
  library(tidyverse)
  doc <- read_xml(dsn, asText = TRUE, useInternal = TRUE, getDTD = FALSE)
  rows <- xml2::xml_find_all(doc, "//name")
  if(length(rows) < 1) {
    return()
  }
  if(length(rows) > 499) {
    carp('*** 499')
  }
  rows.df <- rows %>%
    map(xml_attrs) %>%
    map_df(~as.list(.))
  textes <- rows %>%
    map(xml_text) %>%
    unlist()
  rows.df <- cbind(rows.df, textes)
  rows.df$dsn <- dsn
  rows.df <- rows.df %>%
    mutate(i = 1:nrow(.))
  carp("dsn: %s nrow: %s", dsn, nrow(rows.df))
  return(invisible(rows.df))
}
# source("geo/scripts/visionature.R");biolo_fb();biolo_lieudits_cmp(force = TRUE)
biolo_lieudits_cmp <- function(force = FALSE) {
  library(tidyverse)
  df1 <- biolo98_communes_extract() %>%
    dplyr::select(id = id_place, commune, lieudit) %>%
    glimpse()
  df2 <- biolo63_bbox_lire() %>%
    filter(place_type != "garden") %>%
    dplyr::select(id, commune, lieudit = textes) %>%
    glimpse()
  df3 <- df1 %>%
    filter(! id %in% df2$id)
  misc_print(df3)
  df4 <- df2 %>%
    filter(! id %in% df1$id)
  misc_print(df4)
}
#
## fonctions utilitaires
#
# sauvegarde / restauration
biolo.list <<- list()
biolo_lire <- function(rds = 'lire', rds_dir = FALSE) {
#  carp("rds: %s", rds)
  if ( rds_dir == FALSE) {
    rds_dir = bioloDir
  }
  if ( ! exists(rds, where = biolo.list)) {
    dsn <- sprintf("%s/%s.Rds", rds_dir, rds)
#    carp("dsn: %s", dsn)
    biolo.list[[rds]] <<- readRDS(file = dsn)
  }
  return(invisible(biolo.list[[rds]]))
}
biolo_ecrire <- function(obj, rds = "sauve", rds_dir = FALSE) {
  biolo.list[[rds]] <<- obj
  if ( rds_dir == FALSE) {
    rds_dir = bioloDir
  }
  dsn <- sprintf("%s/%s.Rds", rds_dir, rds)
  carp("dsn: %s", dsn)
  saveRDS(obj, file = dsn)
  return(invisible(dsn))
}