# <!-- coding: utf-8 -->
#
# quelques fonctions pour la comptage oiseaux des jardins
# auteur: Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
# le traitement des exports sql en format xlsx
#
# les traitements à enchainer suite à une mise à jour
# source("geo/scripts/coj.R");sql_jour()
sql_jour <- function() {
  library(tidyverse)
#  sql_fusion_ecrire()
#  df <- sql_rows2donnees()
#  print(df[df$admid == "24381",])
  dev.new()
  txDir <- texDir
  texDir <<- sprintf("%s/%s", txDir, "bzh")
  donnees_lire(force=TRUE, filtre=TRUE)
  sql_jour_stat()
  donnees_lire(force=TRUE, filtre=TRUE)
  dept <- "35"
  texDir <<- sprintf("%s/%s", txDir, dept)
  donnees.df <<- filter(donnees.df, saidep == dept)
  sql_jour_stat()
  sql_commune_dept()
  sql_commune_annee()
}
sql_jour_stat <- function() {
  sql_stat_annee_saidep()
  sql_stat_annee_saidep_juxta("m_especes")
  sql_stat_annee_saidep_juxta("m_oiseaux")
  sql_stat_annee_critere("nb_oiseaux")
  sql_stat_annee_critere("nb_especes")
  sql_stat_annee_saidep_tex()
  sql_stat_annee_saidep_espece_tex()
  sql_stat_espece_tex()
  sql_presence_annees()
  sql_presence_annees_histo()
}
#
# récupération des tables sur le serveur local en format xlsx
# source("geo/scripts/coj.R");sql_export_get()
# ! ne marche pas : pb mémoire ...
# à faire en ligne de commandes
sql_export_get <- function() {
  carp()
  sql_table_get('bvcoj_sauve_2013')
  sql_table_get('bvcoj_sauve_2014')
  sql_table_get('bvcoj_sauve_2015')
  sql_table_get('bvcoj_sauve_2016')
  sql_table_get('bvcoj_sauve_2017')
  sql_table_get('bvcoj_sauve_2018')
}
sql_table_get <- function(table) {
  library(httr)
  url <- sprintf('http://bv/outils/coj_sauve.php?action=xlsx&export=%s', table)
#  url <- sprintf('http://bretagne-vivante-dev.org//outils/coj_sauve.php?action=xlsx&export=%s', table)
  dsn <- sprintf('%s/%s.xlsx', varDir, table)
  GET(url, authenticate(mes_options("coj_username"), mes_options("coj_password")), write_disk(dsn, overwrite=TRUE))
  carp('dsn: %s', dsn)
}
sql_table_lire <- function(annee) {
  library(readxl)
  dsn <- sprintf("%s/bvcoj_sauve_%s.xlsx", varDir, annee)
  df <- read_excel(dsn)
  carp("nrow : %d dsn:%s", nrow(df), dsn)
  df$annee <- annee
  return(invisible(df))
}
#
# lecture des fichiers excel et concaténation
# source("geo/scripts/coj.R");sql_export_lire()
sql_export_lire <- function() {
  library("readxl")
  if (exists("sql_export.df")) {
    return(invisible(sql_export.df))
  }
  annees <- c("2013", "2014", "2015", "2016", "2017", "2018")
  nb_annees <- 0
  for (annee in annees ) {
    dsn <- sprintf("%s/bvcoj_sauve_%s.xlsx", varDir, annee)
    df <- read_excel(dsn)
    carp("nrow : %d dsn:%s", nrow(df), dsn)
    df$a <- annee
    if ( nb_annees == 0 ) {
      fusion.df <- df;
    } else {
      fusion.df <- rbind(fusion.df, df)
    }
    nb_annees <- nb_annees + 1
  }
  df <- fusion.df
  sql_export.df <<- df
  carp("nrow : %d", nrow(df))
  return(invisible(df))
}

#
# production du fichier de l'ensemble des années
# source("geo/scripts/coj.R");sql_export_ecrire()
sql_export_ecrire <- function() {
  carp()
  annees <- c("2013", "2014", "2015", "2016", "2017", "2018")
  df <- sql_export_lire()
  df <- sql_fusion_clean(df, annees)
# pour les NA
  df <- as.data.frame(df)
  carp("df nrow : %s", nrow(df))
  dsn <- sprintf("geo/COJ/sql_export.Rda")
  carp("dsn : %s", dsn)
  save(df, file=dsn)
  return(invisible(df))
}
#
# lecture des fichiers excel et concaténation
sql_fusion <- function() {
  library("readxl")
  annees <- c("2013", "2014", "2015", "2016", "2017", "2018")
#  annees <- c("2018")
  nb_annees <- 0
  for (annee in annees ) {
    dsn <- sprintf("geo/COJ/2018/bvcoj_sauve_%s.xlsx", annee)
    dsn <- sprintf("geo/COJ/bvcoj_sauve_%s.xlsx", annee)
    dsn <- sprintf("%s/bvcoj_sauve_%s.xlsx", varDir, annee)
    df <- read_excel(dsn)
    carp("nrow : %d dsn:%s", nrow(df), dsn)
    df$a <- annee
    if ( nb_annees == 0 ) {
      fusion.df <- df;
    } else {
      fusion.df <- rbind(fusion.df, df)
    }
    nb_annees <- nb_annees + 1
  }
  df <- fusion.df
  carp("nrow : %d", nrow(df))
  df <- sql_fusion_clean(df, annees)
  return(invisible(df))
}
#
# nettoyage rapide des données
sql_fusion_clean <- function(df, annees) {
# que les départements "bretons"
  les_departements <- c("22", "29", "35", "44", "56")
  df <- df[df$saidep %in% les_departements, ]
  carp("nrow : %d saidep", nrow(df))
  df <- subset(df, ! is.na(df$saicom))
  carp("nrow : %d saicom", nrow(df))
  df$saidat <- gsub("/(\\d\\d)$", "/20\\1", df$saidat)
  df$d <- as.Date(df$saidat, "%d/%m/%Y")
  df$annee <- format(df$d, "%Y")
  df <- subset(df, ! is.na(df$annee))
  carp("nrow : %d saidat", nrow(df))
  df <- df[df$annee %in% annees, ]
  carp("nrow : %d annee", nrow(df))
  df <- subset(df, ! is.na(df$saicom))
  carp("nrow : %d saicom", nrow(df))
#  View(df)
  return(invisible(df))
}
#
# production du fichier de l'ensemble des années
# source("geo/scripts/coj.R");sql_fusion_ecrire()
sql_fusion_ecrire <- function() {
  carp()
  df <- sql_fusion()
# pour les NA
  df <- as.data.frame(df)
  carp("df nrow : %s", nrow(df))
  dsn <- sprintf("geo/COJ/sql_fusion.Rda")
  carp("dsn : %s", dsn)
  save(df, file=dsn)
  return(invisible(df))
}
sql_fusion_lire <- function(source='export') {
  carp()
  dsn <- sprintf("geo/COJ/sql_%s.Rda", source)
  carp("dsn : %s", dsn)
  load(dsn)
  carp("df nrow : %s", nrow(df))
  return(invisible(df))
}
#
# la liste des emails du 35
# source("geo/scripts/coj.R");sql_emails()
sql_emails <- function() {
  carp()
  library(tidyverse)
  df <- sql_fusion_lire() %>%
    glimpse()
#  liste <- unique(df[df$annee %in% c('2017', '2018') & df$saidep == '50' & df$saicom == '50 019', c("saimai", "saicom")])
#  liste <- unique(df[df$annee %in% c('2017', '2018') & df$saidep == '35'])
  liste <- df %>%
    drop_na(saimai) %>%
    filter(saidep == '35') %>%
    filter(annee %in% c('2016', '2017', '2018')) %>%
    distinct(saimai) %>%
    glimpse()
#  liste <- sort(liste)
#  View(liste)
  carp("%s", nrow(liste))
  print(paste(liste[1:400, 'saimai'], collapse = ', '))
  print(paste(liste[401:800, 'saimai'], collapse = ', '))
}

#
# détermination du nourissage
sql_fusion_sainou <- function() {
  carp()
  df <- sql_fusion_lire()
  df$id <- sprintf("%s/%s", df$admid, df$annee)
  df$nb_na <- rowSums(is.na(df[, c("sainoub", "sainoum", "sainoug", "sainoup", "sainoua", "sainoudes")]))
  df$sainou <- 'non'
  df$sainou[df$nb_na < 6] <- 'oui'
  df <- df[, c("id", "sainou")]
  print(head(df))
  return(invisible(df))
}
#
# préparation du dataframe des stat par année et par département
sql_stat_df <- function() {
  library(dplyr)
  df <- donnees_lire()
  df1 <- df %>%
    group_by(annee, saidep, admid) %>%
    summarise(nb_especes = n(), nb_oiseaux = sum(nb) )
  print(sprintf("sql_stat_df() nrow:%s df1", nrow(df1)))
  stat.df <- df1 %>%
    group_by(annee, saidep) %>%
#    summarise(nb = n(), nb_oiseaux = sum(nb_oiseaux), nb_especes=sum(nb_especes), m_oiseaux=sprintf("%.1f", nb_oiseaux/nb), m_especes=sprintf("%.1f", nb_especes/nb))
    summarise(nb_jardins = n(), nb_oiseaux = sum(nb_oiseaux), nb_especes=sum(nb_especes), m_oiseaux=nb_oiseaux/nb_jardins, m_especes=nb_especes/nb_jardins)
#  View(stat.df)
  return(invisible(stat.df))
}
#
# la table de correspondance avec les identifiants du formulaire html et les noms d'espèce
sql_id2nom <-function() {
  dsn <- sprintf("%s/id2nom.csv", cfgDir)
  df <- read.table(dsn, header=TRUE, sep=";", blank.lines.skip = TRUE, stringsAsFactors=FALSE, quote="")
  carp(" %s nrow: %d", dsn, nrow(df))
#  df$espece <- iconv(df$espece, "UTF-8")
  return(invisible(df))
}

#
# v1 : très très lent
sql_rows2donnees_v1 <-function() {
  id2nom.df <- sql_id2nom()
  df <- sql_fusion_lire()
  attributs <- c("admid", "annee", "espece", "nb")
  Encoding(attributs) <- "UTF-8"
  df1 <- as.data.frame(matrix(ncol=length(attributs), nrow=200000))
  colnames(df1) <- attributs
  ligne <- 1
  print(sprintf("sql_rows2donnees()"))
  start.time <- Sys.time()
  flush.console()
  for (i in 1:nrow(df)) {
    if ( i%%100 == 0 ) {
      end.time <- Sys.time()
      print(sprintf("sql_rows2donnees() i:%d temps:%s", i, end.time - start.time))
      flush.console()
      start.time <- Sys.time()
    }
    for(j in 1:nrow(id2nom.df) ) {
      id <- id2nom.df[j, "id"]
      v <- df[i, id]
      if ( is.na(v) ) {
        next;
      }
      df1[ligne, "espece"] <- id2nom.df[j, "nom"]
      df1[ligne, "nb"] <-  v
      df1[ligne, "admid"] <- df[i, "admid"]
      df1[ligne, "annee"] <- df[i, "annee"]
      ligne <- ligne + 1
    }
  }
  View(df1)
}
#
# v2 : avec dplyr
# source("geo/scripts/coj.R");sql_rows2donnees()
sql_rows2donnees <- function() {
  library(dplyr)
  library(tidyr)
  carp()
  id2nom.df <- sql_id2nom() %>%
    glimpse()
  df <- sql_fusion_lire('export') %>%
    glimpse()
  entete <- colnames(df)[1:27]
  entete <- c(entete, 'annee')
  attributs <- c(entete, "espece", "nb") %>%
    glimpse()
#  Encoding(attributs) <- "UTF-8"
  df1 <- as.data.frame(matrix(ncol=length(attributs), nrow=200000))
  colnames(df1) <- attributs
  keycol <- "id"
  valuecol <- "nb"
  gathercols <- id2nom.df$id
  df1 <- gather_(df, keycol, valuecol, gathercols) %>%
    glimpse()
#  stop('***')

#  print(df1[df1$admid == "24381",])
  attrs <- c(entete, "id", "nb") %>%
    glimpse()
  df1 <- df1[, c(attrs)]
  carp("df1 nrow:%s", nrow(df1))
  df1 <- subset(df1, ! is.na(df1$nb))
#  df1 <- subset(df1, ! is.numeric(df1$nb))
#  df1 <- subset(df1, df1$nb > 0)
  df1 <- left_join(df1, id2nom.df) %>%
    glimpse()
  df2 <- df1 %>%
    filter(is.na(espece))
  if ( nrow(df2) > 0) {
    carp('espece invalide')
    glimpse(df2)
    stop('***')
  }
  carp("df1 nrow:%s", nrow(df1))
  df1 <- df1[, attributs]
  for (i in 1:8) {
    nb <- sprintf("nb%s", i)
    esp <- sprintf("esp%s", i)
    attrs <- c(entete, esp, nb)
    df2 <- df[, attrs]
    colnames(df2) <-  attributs
    df2 <- subset(df2, ! is.na(df2$espece))
    df2 <- subset(df2, ! is.na(df2$nb))
#    df2 <- subset(df2, ! is.numeric(df2$nb))
#    df2 <- subset(df2, df2$nb > 0)
    carp("df2 nrow:%s i:%s", nrow(df2), i)
    df1 <- rbind(df1, df2)
#    View(df2); stop("***")
  }
  df <- df1
  df <- subset(df, df$espece != "aucun")
  df$espece <- gsub("\\\\+", "", df$espece, perl = TRUE)
#  df <- subset(df, ! is.numeric(df$nb))
#  df <- subset(df, grepl("^\\d+$", df$nb))
  df1 <- df %>%
    group_by(espece) %>%
    summarize(nb=n()) %>%
    arrange(-nb) %>%
  glimpse()
#  View(df)
  carp("df nrow : %s", nrow(df))
  dsn <- sprintf("%s/sql_donnees.Rda", cfgDir)
  carp("dsn : %s", dsn)
  save(df, file=dsn)
  return(invisible(df))
}
# source("geo/scripts/coj.R");df <- sql_donnees_lire()
sql_donnees_lire <- function() {
  if(exists('sql_donnees.df')) {
    return(invisible(sql_donnees.df))
  }
  library(tidyverse)
  dsn <- sprintf("%s/sql_donnees.Rda", cfgDir)
  carp('dsn: %s', dsn)
  load(dsn)
  glimpse(df)
  sql_donnees.df <<- df
  return(invisible(df))
}
