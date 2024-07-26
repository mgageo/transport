# <!-- coding: utf-8 -->
#
# quelques fonctions pour la comptage oiseaux des jardins
# auteur: Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
# le traitement du fichier excel de l'année 2012
# fichier Odj_2012.xlsx transmis par Guillaume Gélinaud le 04/01/2018
#
# !!! attention, certaines feuilles sont masquées
guillaumeDir <- sprintf('%s/GuillaumeGelinaud/', varDir)
#
# source("geo/scripts/coj.R");xl2012_jour()
xl2012_jour <- function(force = FALSE) {
  library(rio)
  library(tidyverse)
  library(stringr)
  carp()
  xl2012_stat(force = force)
}
# source("geo/scripts/coj.R");xl2012_lire(force = TRUE)
xl2012_lire <- function(force = FALSE) {
  carp()
  options(dplyr.print_max = 1e9, tibble.width = Inf)
  library("readxl")
  if ( exists("xl2012.df") && force == FALSE) {
    return(invisible(xl2012.df))
  }
  dsn <- sprintf("%s/OdJ_2012_mga.xlsx", guillaumeDir)
  df <- readxl::excel_sheets(dsn) %>%
    glimpse()
  df <- readxl::read_excel(dsn, col_names = FALSE, skip = 4)
  carp("dsn: %s nrow : %s", dsn, nrow(df))
#  misc_print(head(df));stop("*****")
  df <- df[, 1:25]
  champs <- "Support Identifiant DateObs HeureObs LieuObs TypeLieu Nourrissage Nourriture AutresNourritures Observateur Adresse Telephone Email Departement CodeDept Commune Lieudit X Y NumUTM NomUTM Famille NomFrancais NomScientifique Nombre"
  champs <- unlist(strsplit(champs, " "))
  colnames(df) <- champs
  df$id <- 1:nrow(df)
#  View(df)
  na.df <- subset(df, is.na(df$Identifiant)) %>%
    dplyr::select(id, Support, Identifiant)
  if ( nrow(na.df) > 0 ) {
    carp("Identifiant nb: %d", nrow(na.df))
    misc_print(na.df)
#    stop("***")
  }
  df <- df[!(is.na(df$Identifiant)),]
  na.df <- subset(df, is.na(df$Support))
  if ( nrow(na.df) > 0 ) {
    print(sprintf("xl2012_lire() Support nb: %d", nrow(na.df)))
#    stop("***")
  }
#  df <- subset(df, ! is.na(df$Support))
  df$DateObs[is.na(df$DateObs)] <- as.Date("29/01/2012", "%d/%m/%Y")
  df$HeureObs[is.na(df$HeureObs)] <- as.Date(.5, origin="1899-12-30")
  df$nombre <- gsub(".*(\\d+).*", "\\1", df$Nombre)
  df$nombre <- as.numeric(df$Nombre)
  na.df <- subset(df, is.na(df$nombre))
  if ( nrow(na.df) > 0 ) {
    carp("nombre # numeric nb: %d", nrow(na.df))
    print(head(na.df[,c("Nombre", "id")]))
    stop("***")
  }
  df$insee <- gsub(".*(\\d\\d \\d\\d\\d).*", "\\1", df$Commune)
  na.df <- subset(df, is.na(df$insee))
  if ( nrow(na.df) > 0 ) {
    carp("insee nb: %d", nrow(na.df))
    print(na.df[, c("id", "Support", "Identifiant", "Commune", "insee")])
#    stop("***")
  }
  df <- subset(df, ! is.na(df$insee))
  df$dpt <- gsub("(\\d+) \\d+", "\\1", df$insee)
  na.df <- subset(df, is.na(df$dpt))
  if ( nrow(na.df) > 0 ) {
    print(sprintf("xl2012_lire() dpt nb: %d", nrow(na.df)))
#    print(na.df[,c("Commune", "insee", "id")])
    stop("***")
  }
  df <- subset(df, ! is.na(df$insee))
  print(sprintf("xl2012_lire() nrow : %s", nrow(df)))
  xl2012.df <<- df
  return(invisible(df))
}
xl2012_lire_v2 <- function() {
  print(sprintf("xl2012_lire()"))
  options( java.parameters = "-Xmx2500m")
  library("openxlsx")
  if ( exists("xl2012.df") ) {
#    rm(xl2012.spdf", pos = ".GlobalEnv")
#    return(invisible(xl2012.df))
  }
  dsn <- sprintf("geo/COJ/OdJ_2012_mga.xlsx", "Feuil2")
  df <- read.xlsx(dsn, sheet="Feuil2", colNames = FALSE, startRow=5)
  print(sprintf("xl2012_lire() nrow : %s", nrow(df)))
  df <- df[, 1:25]
  champs <- "Support Identifiant DateObs HeureObs LieuObs TypeLieu Nourrissage Nourriture AutresNourritures Observateur Adresse Telephone Email Departement CodeDept Commune Lieudit X Y NumUTM NomUTM Famille NomFrancais NomScientifique Nombre"
  champs <- unlist(strsplit(champs, " "))
  colnames(df) <- champs
  df$id <- 1:nrow(df)
#  View(df)
  df <- df[!(is.na(df$Identifiant)),]
  df <- subset(df, ! is.na(df$Support))
  df$DateObs[is.na(df$DateObs)] <- as.Date("29/01/2012", "%d/%m/%Y")
  df$HeureObs[is.na(df$HeureObs)] <- as.Date(.5, origin="1899-12-30")
  df$nombre <- gsub(".*(\\d+).*", "\\1", df$Nombre)
  df$nombre <- as.numeric(df$nombre)
  View(df[is.na(df$nombre), c("Nombre", "nombre")])
  xl2012.df <<- df
  return(invisible(df))
}
#
# stat
xl2012_stat <- function(force = FALSE) {
  carp()
  library(tidyverse)
  df <- xl2012_lire(force = force)
  print(sprintf("xl2012_stat() df nrow : %s", nrow(df)))
  df1 <- df %>%
    group_by(Identifiant, dpt)  %>%
    summarise(nb_donnees = n(), nb_oiseaux = sum(Nombre))
  df2 <- df1 %>%
    ungroup() %>%
    group_by(dpt)  %>%
    summarise(nb_jardins = n(), nb_oiseaux = sum(nb_oiseaux))
  misc_print(df2)
}
#
# regroupement par comptage
# pb avec ald27, bb123, LF900, Odj 1
xl2012_comptage <- function() {
  print(sprintf("xl2012_comptage()"))
  library("dplyr")
  df <- xl2012_lire()
  print(sprintf("xl2012_comptage() df nrow : %s", nrow(df)))
  df1 <- df %>% group_by(Identifiant)  %>%
    summarise(NbEspeces = n(), NbOiseaux = sum(nombre))
  print(sprintf("xl2012_comptage() df1 nrow : %s", nrow(df1)))
  df1 <- df %>%
    group_by(Support, Identifiant, DateObs, HeureObs, LieuObs, TypeLieu, Nourrissage, Nourriture, AutresNourritures, Observateur, Adresse, Telephone, Email, Departement, CodeDept, Commune, Lieudit, insee, dpt)  %>%
    summarise(NbEspeces = n(), NbOiseaux = sum(Nombre))
  print(sprintf("xl2012_comptage() df1 nrow : %s", nrow(df1)))
  df2 <- df1 %>% group_by(Identifiant)  %>%
    summarise(nb = n())
  df3 <- df2[df2$nb > 1,]
  if ( nrow(df3) > 0 ) {
    df4 <- df1[df1$Identifiant %in% df3$Identifiant, ]
    df4 <- df4[order(df4$Identifiant), ]
    i <- 1
    while ( i < nrow(df4) ) {
      v1 <- df4[i, ]
      print(v1)
      i <- i+1
      v2 <- df4[i, ]
      i <- i+1
      print(which(v1 != v2))
    }
    View(df4)
  }
  return(invisible(df1))
}
xl2012_comptage_ecrire <- function() {
  print(sprintf("xl2012_comptage_ecrire()"))
  library("xlsx")
  df <- xl2012_comptage()
# pour les NA
  df <- as.data.frame(df)
  print(sprintf("xl2012_comptage_ecrire() df nrow : %s", nrow(df)))
  dsn <- sprintf("geo/COJ/OdJ_2012_comptage.xlsx")
  print(sprintf("xl2012_comptage_ecrire() dsn : %s", dsn))
  write.xlsx(df, dsn, row.names=FALSE)
  dsn <- sprintf("geo/COJ/OdJ_2012_comptage.Rda")
  print(sprintf("xl2012_comptage_ecrire() dsn : %s", dsn))
  save(df, file=dsn)
  return(invisible(df))
}
#
# source("geo/scripts/coj.R");xl2012_comptage_lire()
xl2012_comptage_lire <- function() {
  carp()
  dsn <- sprintf("geo/COJ/OdJ_2012_comptage.Rda")
  carp(" dsn : %s", dsn)
  load(dsn)
  carp(" df nrow : %s", nrow(df))
  View(df)
}
#
# nettoyage du champ espèce
# source("geo/scripts/coj.R");xl2012_especes()
xl2012_especes <- function() {
  carp()
  library("dplyr")
  dsn <- sprintf("%s\\sql_donnees.Rda", cfgDir)
  load(file=dsn)
  df1 <- df %>% group_by(espece) %>%
    summarise(NbJardins= n())
  print(df1)
#  View(especes.df); stop("***")
  df <- xl2012_lire()
#  View(df)
  carp(" df nrow : %s", nrow(df))
  df$NomFrancais <- xl2012_especes_clean(df$NomFrancais)
  df1 <- df %>% group_by(NomFrancais)  %>%
    summarise(NbJardins= n()) %>%
    filter(! NomFrancais %in% df1$espece)
  print(df1)
  return(invisible(df1))
}
xl2012_especes_clean <- function(v) {
  dsn <- sprintf("geo/COJ/xl2012_especes_clean.csv")
  carp(" dsn : %s", dsn)
  df <-  read.table(dsn, header=TRUE, sep="|", blank.lines.skip = TRUE, stringsAsFactors=FALSE, quote="", encoding="UTF-8")
  carp(" df nrow : %s", nrow(df))
  for(i in 1:nrow(df) ) {
    regexp <- df$regexp[i]
    espece <- df$espece[i]
#    carp( " %s => %s", regexp, espece)
    v <- gsub(regexp, espece, v, perl=TRUE, ignore.case=TRUE)
  }
  return(v)
}
#
# passage en format sql
# source("geo/scripts/coj.R");xl2012_especes_sql()
xl2012_especes_sql <- function() {
  carp()
  df <- xl2012_lire()
  df$NomFrancais <- xl2012_especes_clean(df$NomFrancais)
  df$annee <- "2012"
  df <- df[, c("Identifiant", "annee", "dpt", "insee", "NomFrancais", "nombre")]
  colnames(df) <- c("admid", "annee", "saidep", "saicom", "espece", "nb")
  dsn <- sprintf("%s/xl2012_donnees.Rda", cfgDir)
  carp(" dsn : %s", dsn)
  save(df, file=dsn)
  donnees_lire(rda="xl2012_donnees.Rda", force=TRUE)
  return(invisible(df))
}