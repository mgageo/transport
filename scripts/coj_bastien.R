# <!-- coding: utf-8 -->
#
# quelques fonctions pour la comptage oiseaux des jardins
# auteur: Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
# le traitement des fichiers de Bastien Blanc (service civique BV Séné)
# fichiers transmis par Yann Février le 26/04/2019
# source("geo/scripts/coj.R");bastien_lire()
bastienDir <- sprintf('%s/BastienBlanc/Analyse_ODJ', varDir)
bastien_lire <- function(force = FALSE) {
  library(rio)
  library(tidyverse)
  library(stringr)
  dsn <- sprintf('%s/BDD_ODJ_entete_2009_2019.csv', bastienDir)
  carp('dsn: %s', dsn)
  if ( ! exists('entete.df') | force == TRUE) {
    entete.df <<- import(dsn) %>%
      glimpse()
  }
  dsn <- sprintf('%s/BDD_ODJ_details_2009_2019.csv', bastienDir)
  carp('dsn: %s', dsn)
  if ( ! exists('details.df') | force == TRUE) {
    details.df <<- import(dsn) %>%
      glimpse()
  }
}
# source("geo/scripts/coj.R");bastien_stat()
bastien_stat <- function() {
  library(rio)
  library(tidyverse)
  library(stringr)
  bastien_lire()
  df <- entete.df %>%
    dplyr::select(date=cojsaisiedate) %>%
    mutate(annee=str_extract(date, "\\d{4}")) %>%
    group_by(annee) %>%
    summarize(nb=n()) %>%
    glimpse()
}
# source("geo/scripts/coj.R");bastien_cmp_annees()
bastien_cmp_annees <- function() {
  carp()
  library("readxl")
  annees <- c("2013", "2014", "2015", "2016", "2017", "2018")
  for (annee in annees ) {
    bastien_cmp_annee(annee)
  }
}
# source("geo/scripts/coj.R");bastien_cmp_annee()
bastien_cmp_annee <- function(annee='2018') {
  library(rio)
  library(tidyverse)
  library(stringr)
  carp()
  bastien_lire(force = TRUE)
  filtre <- sprintf('_%s$', annee)
  filtre2 <- sprintf('%s$', annee)
  carp("les données de Bastien")
  df <- entete.df %>%
    glimpse() %>%
    filter(grepl(filtre, cojid)) %>%
#    print(n=20, na.print='') %>%
    glimpse()
  if (nrow(df) == 0) {
    return()
  }
  dsn <- sprintf("%s/bvcoj_sauve_%s.xlsx", varDir, annee)
  sql.df <- import(dsn) %>%
    mutate(cojid=sprintf('%d_%s', admid, annee)) %>%
    filter(grepl(filtre2, saidat)) %>%
    glimpse()
  carp('absent de sql')
  anti_join(df, sql.df, by='cojid') %>%
    glimpse()
  carp('absent de damien')
  df1 <- anti_join(sql.df, df, by='cojid') %>%
    glimpse()
  dsn <- sprintf('%s/%s_absent_damien.xlsx', bastienDir, annee)
  export(df1,dsn)
  carp('dsn: %s', dsn)
#
# comparaison de différents champs
  carp('nourrissage')
  df1 <- df %>%
    dplyr::select(cojid, sainoub=cojnouron)
  df2 <- sql.df %>%
    dplyr::select(cojid, sainoub)
  df3 <- left_join(df1, df2, by='cojid') %>%
    filter(sainoub.x != sainoub.y) %>%
    glimpse()
  carp('nourrissage pain')
  df1 <- df %>%
    dplyr::select(cojid, sainoup=cojnourpain)
  df2 <- sql.df %>%
    dplyr::select(cojid, sainoup)
  df3 <- left_join(df1, df2, by='cojid') %>%
    filter(sainoup.x != sainoup.y) %>%
    glimpse()
}
#
# pour la v2 de la base sql
# source("geo/scripts/coj.R");bastien_cmp2_annee()
bastien_cmp2_annee <- function(annee='2019') {
  library(rio)
  library(tidyverse)
  library(stringr)
  bastien_lire()
  filtre <- sprintf('/%s$', annee)
  filtre2 <- sprintf('%s$', annee)
  df <- entete.df %>%
    filter(grepl(filtre, cojsaisiedate)) %>%
#    print(n=20, na.print='') %>%
    glimpse()
  dsn <- sprintf("%s/bvcoj_v2_entete.xlsx", varDir)
  sql.df <- import(dsn) %>%
    glimpse() %>%
    mutate(cojid=sprintf('%d', cojid)) %>%
    glimpse()
  carp('absent de sql')
  anti_join(df, sql.df, by='cojid') %>%
    glimpse()
  carp('absent de damien')
  df1 <- anti_join(sql.df, df, by='cojid') %>%
    glimpse()
  dsn <- sprintf('%s/%s_absent_damien.xlsx', bastienDir, annee)
  export(df1,dsn)
  carp('dsn: %s', dsn)
  carp('nourrissage pain')
  df1 <- df %>%
    dplyr::select(cojid, cojnourpain)
  df2 <- sql.df %>%
    dplyr::select(cojid,  cojnourpain)
  df3 <- left_join(df1, df2, by='cojid') %>%
    filter(cojnourpain.x !=  cojnourpain.y) %>%
    glimpse()
}
#
# pour la v1 de la base sql
# source("geo/scripts/coj.R");bastien_cmp_details_annee()
bastien_cmp_details_annee <- function(annee='2018') {
  library(rio)
  library(tidyverse)
  library(stringr)
  bastien_lire()
  filtre <- sprintf('/%s$', annee)
  especes.df  <- read.table(text="from|to
Bernache a ventre clair *|Bernache a ventre claire
Epervier d'Europe|Epervier d Europe
Gallinule poule-d'eau|Gallinule poule-d eau
Heron garde-boufs|Heron garde-boeufs
Loriot d'Europe|Loriot d Europe
Martin-pecheur d'Europe|Martin-pecheur d Europe
Rouge-gorge familier|Rougegorge familier
Verdier d'Europe|Verdier d Europe
", header=TRUE, sep="|", blank.lines.skip = TRUE, stringsAsFactors=FALSE, quote="")
  df <- details.df %>%
    filter(grepl(filtre, cojdetailcontroledate)) %>%
    mutate(admid=str_extract(cojdetailnumidentete, '^\\d+')) %>%
    rename(espece=cojdetailespece) %>%
    glimpse()
  sql_details.df <- sql_rows2donnees_lire() %>%
    filter(annee=='2018') %>%
    mutate(admid=as.character(admid)) %>%
#    mutate(espece=iconv(espece, to="ASCII//TRANSLIT")) %>%
    glimpse()
  Encoding(sql_details.df$espece) <- 'UTF-8'
  sql_details.df$espece <- iconv(sql_details.df$espece, from='UTF-8', to="ASCII//TRANSLIT")
  for(i in 1:nrow(especes.df)) {
     sql_details.df[grepl(especes.df[i, 'from'], sql_details.df$espece), 'espece'] <- especes.df[i, 'to']
  }
  carp('absent de sql')
  df1 <- anti_join(df, sql_details.df, by=c('admid', 'espece')) %>%
    glimpse()
  carp('absent de damien')
  df2 <- anti_join(sql_details.df, df, by=c('admid', 'espece')) %>%
    glimpse()
  carp('liste des especes damien')
  df1 <- df %>%
    group_by(espece) %>%
    summarize(nb=n()) %>%
    glimpse() %>%
    print(n=10)
  carp('liste des especes sql')
  df2 <- sql_details.df %>%
    group_by(espece) %>%
    summarize(nb=n()) %>%
    glimpse() %>%
    print(n=10)
  df3 <- full_join(df1, df2, by='espece') %>%
    glimpse() %>%
    arrange(espece) %>%
    print(n=150)
  df %>%
    filter(espece=='Busard des roseaux') %>%
    glimpse()
}