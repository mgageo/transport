# <!-- coding: utf-8 -->
#
# le réseau de bus
#
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
# avec gtfs-to-html
# https://www.npmjs.com/package/gtfs-to-html/v/0.20.0
# en powershell
# cd d:/web.heb/transport/FR-BRE-TIBUS
# gtfs-to-html
#
# le script geo/scripts/transport.ps1 lance gtfs-to-html dans chaque dossier
#
# la configuration pour le site web
#
# source("geo/scripts/transport.R");web_jour()
web_jour <- function() {
  web_config_path()
}
#
# source("geo/scripts/transport.R");web_config_url()
web_config_url <- function() {
  carp()
  dsn <- sprintf("%s/config_tpl.json", cfgDir)
  config_json <-  readLines(dsn)
  df1 <- config_agency_lire() %>%
    filter(! is.na(PTNA)) %>%
    filter(grepl("^FR", PTNA)) %>%
    glimpse()
  cols <- colnames(df1)
  for(i1 in 1:nrow(df1)) {
    for (i2 in 1:ncol(df1)) {
      col <- sprintf("Config_%s", cols[i2])
      assign(col, df1[[i1, i2]], envir = .GlobalEnv)
    }
    agency <- gsub(".*\\-", "", Config_PTNA)
    variables <- list(
      agency = agency,
      PTNA = Config_PTNA,
      gtfs_source = Config_gtfs_source
    )
    dir <- sprintf("%s/%s", webDir, Config_PTNA)
    dir.create(dir, showWarnings = FALSE, recursive = TRUE)
    dsn <- sprintf("%s/config.json", dir)
    config <- misc_list2tpl(variables, config_json)
    writeLines(config, dsn)
  }
}
#
# source("geo/scripts/transport.R");web_config_path()
web_config_path <- function() {
  carp()
  dsn <- sprintf("%s/config_path_tpl.json", cfgDir)
  config_json <-  readLines(dsn)
  df1 <- config_agency_lire() %>%
    filter(! is.na(PTNA)) %>%
    filter(grepl("^FR", PTNA)) %>%
    glimpse()
  cols <- colnames(df1)
  for(i1 in 1:nrow(df1)) {
    for (i2 in 1:ncol(df1)) {
      col <- sprintf("Config_%s", cols[i2])
      assign(col, df1[[i1, i2]], envir = .GlobalEnv)
    }
    agency <- gsub(".*\\-", "", Config_PTNA)
    variables <- list(
      agency = agency,
      PTNA = Config_PTNA
    )
    dir <- sprintf("%s/%s", webDir, Config_PTNA)
    dir.create(dir, showWarnings = FALSE, recursive = TRUE)
    dsn <- sprintf("%s/config.json", dir)
    config <- misc_list2tpl(variables, config_json)
    writeLines(config, dsn)
    gtfsDir <- sprintf("%s/%s", varDir, Config_gtfs_dir)
    dsn_source <- sprintf("%s/gtfs.zip", gtfsDir)
    carp("dsn_source: %s", dsn_source)
    dsn <- sprintf("%s/gtfs.zip", dir)
    file.copy(dsn_source, dsn, overwrite = TRUE)
    carp("dsn: %s", dsn)
  }
}