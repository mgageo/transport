# <!-- coding: utf-8 -->
#
# quelques fonctions sur les fichiers insee
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
inseeDir <- sprintf("%s/bvi35/CouchesINSEE", Drive);
# https://www.insee.fr/fr/information/4652957
# les autres échelons administratifs avec les tables d'appartenance : https://www.insee.fr/fr/information/2028028
# lecture du répertoire des communes
# https://www.insee.fr/fr/information/2114819#titre-bloc-3
# Code officiel géographique au 1er janvier 2018
# https://www.insee.fr/fr/information/3363419#titre-bloc-3
# rm(list=ls());  source("geo/scripts/coj.R");insee_cog_lire()
insee_cog_lire <- function() {
  library(tidyverse)
  carp()
  dsn <- sprintf("%s/france2016.txt", inseeDir)
  dsn <- sprintf("%s/France2018.txt", inseeDir)
  carp("dsn : %s", dsn)
  meta.df <- read_delim("Longueur;Nom;Désignation en clair
1;ACTUAL;Code actualité de la commune
1;CHEFLIEU;Chef-lieu d'arrondissement, de département, de région ou bureau centralisateur de canton
1;CDC;Découpage de la commune en cantons
2;RANG;Nombre de fractions cantonales + 1 de la commune lorsqu'elle est multicantonale
2;REG;Code région
3;DEP;Code département
3;COM;Code commune
1;AR;Code arrondissement
2;CT;Code canton
1;MODIF;Indicateur de modification subie par la commune
5;POLE;Code de la commune pôle de la commune fusionnée
1;TNCC;Type de nom en clair
5;ARTMAJ;Article (majuscules)
70;NCC;Nom en clair (majuscules)
5;ARTMIN;Article (typographie riche)
70;NCCENR;Nom en clair (typographie riche)
5;ARTICLCT;Article (canton)
70;NCCCT;Nom en clair du canton (typographie riche)", ";", col_names=TRUE)
  print(head(meta.df))
#  df <- read_fwf(dsn, fwf_widths(meta.df$Longueur, col_names=meta.df$Nom))
#  df <- read_delim(dsn, "\t", col_names=TRUE, quote="", locale = locale(encoding = 'LATIN1'))
  df <- read.table(dsn, header=TRUE, sep="\t", quote="", colClasses = "character")
  df <- df %>%
    filter(DEP %in% c('22', '29', '35', '44', '56')) %>%
    select(DEP, COM, NCC, NCCENR) %>%
    mutate(saicom=sprintf("%s %s", DEP, COM)) %>%
    select(saicom, NCCENR) %>%
    glimpse()
#  df <- distinct(df, saicom, .keep_all = TRUE)
#  print(head(df))
  carp("df nrow : %s", nrow(df))
  return(invisible(df))
}
insee_cog_xlsx_lire <- function() {
  library(tidyverse)
  library(readxl)
  library(janitor)
  carp()
  dsn <- sprintf("%s/France2018.xlsx", inseeDir)
  carp("dsn: %s", dsn)
  df <- read_excel(dsn) %>%
#    clean_names() %>%
    mutate(insee=sprintf("%s%s", DEP, COM)) %>%
    glimpse()
  return(invisible(df))
}
#
# les communes de 2019
# https://fr.wikipedia.org/wiki/Liste_des_communes_nouvelles_cr%C3%A9%C3%A9es_en_2019
#  rm(list=ls()); source("geo/scripts/coj.R");insee_cog2019_lire()
insee_cog2019_lire <- function() {
  library(tidyverse)
  dsn <- sprintf("%s/Wiki2019.txt", inseeDir)
  carp("dsn : %s", dsn)
  df <- read.table(dsn, header=TRUE, sep=";", quote="", colClasses = "character")
  df %>%
    select(saicom, NCCENR) %>%
    arrange(NCCENR)
  return(invisible(df))
}
#
# génération du javascript
# rm(list=ls()); source("geo/scripts/coj.R");insee_option()
insee_option <- function() {
  library(tidyverse)
  library(rio)
  carp()
  df <- insee_cog_lire()
  df2019 <- insee_cog2019_lire()
  df <- rbind(df, df2019) %>%
    mutate(saicom=gsub(' ', '', saicom), dpt=str_sub(saicom, 1, 2)) %>%
    glimpse()
  bzh <- ''
  for ( d in c('22', '29', '35', '44', '56')) {
    carp('d: %s', d)
    df1 <- df %>%
      filter(grepl(d, dpt)) %>%
      distinct(NCCENR, .keep_all = TRUE) %>%
      arrange(NCCENR) %>%
      mutate(opt=sprintf('wch=wch+\'<option value="%s#%s">%s</option>\';', saicom, NCCENR, NCCENR)) %>%
      glimpse()
    t <- paste(df1$opt, sep="\n", collapse="\n")
    bzh <- paste0(bzh, t)
#    cat(t, sep = '\n')
    dsn <- sprintf("%s/dpt_%s.txt", inseeDir, d)
    cat(file=dsn, t, sep = '\n')
  }

  dsn <- sprintf("%s/dpt_%s.txt", inseeDir, 'bzh')
  cat(file=dsn, bzh, sep = '\n')
  carp('dsn: %s', dsn)
}
#
# les communautés de communes
# rm(list=ls()); source("geo/scripts/rpg.R");insee_epci()
insee_epci <- function() {
  library(tidyverse)
  library(readxl)
  dsn <- sprintf("%s/EPCI_au_01-01-2024.xlsx", inseeDir)
  carp("dsn : %s", dsn)
  df <- read_excel(dsn, sheet = "Composition_communale", skip = 5) %>%
    glimpse()
  return(invisible(df))
}