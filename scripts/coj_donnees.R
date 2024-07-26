# <!-- coding: utf-8 -->
#
# quelques fonctions pour la comptage oiseaux des jardins
# auteur: Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
# le traitement des données
#
# les traitements à enchainer suite à une mise à jour
# source("geo/scripts/coj.R");donnees_jour()
donnees_jour <- function() {
  library(tidyverse)
  carp()
# xl2012
  donnees_fusion_xl()
# sql
  donnees_fusion_sql()
# sqlv2
  donnees_fusion_sqlv2()
# faune
  donnees_fusion_faune()
# lpo44
  donnees_fusion_lpo44()
# odj
  donnees_fusion_odj()
# on concaténe tout
  donnees_fusion_sources()
}

# lecture des données fusionnées
# source("geo/scripts/coj.R");donnees_fusion_lire()
donnees_fusion_lire <- function(rda="donnees_fusionv2.Rda") {
  carp()
  library(tidyverse)
  dsn <- sprintf("%s/%s", cfgDir, rda)
  carp("dsn: %s", dsn)
  load(dsn)
  glimpse(df)
  carp("df nrow: %s", nrow(df))
  return(invisible(df))
}
#
## fusion des différentes sources de données --------------------------------------------
#
# pour chaque source de données on doit disposer d'un fichier au format "source"
#
# source("geo/scripts/coj.R"); df <- donnees_fusion_sources()
donnees_fusion_sources <- function() {
  carp()
  library(tidyverse)
  df <- data.frame()
  for (source in c("xl", "sql", "sqlv2", "faune", "lpo44", "odj")) {
    df1 <- lire_rds(sprintf("donnees_fusion_%s_sources", source))  %>%
      glimpse()
    df <- rbind(df, df1)
  }
  sauve_rds(df)
  donnees_fusion_stat(df)
  return(invisible(df))
}
#
# fusion des données faune
# source("geo/scripts/coj.R"); df <- donnees_fusion_faune() %>% glimpse()
donnees_fusion_faune <- function() {
  carp("début")
  df <- data.frame()
  for (annee in anneesFaune) {
    df1 <- faune_donnees_jardin_annee_lire(annee) %>%
      mutate(date = excel_numeric_to_date(as.numeric(date))) %>%
      mutate(jjmmaaaa = sprintf("%s.%s.%s", date_day, date_month, date_year)) %>%
      dplyr::select(date_year, jjmmaaaa, id_form, county, insee, name_species, total_count)
    df <- rbind(df, df1)
  }
  sauve_rds(df)
  df <- df %>%
    dplyr::select(annee = date_year, jjmmaaaa, admid = id_form, dept = county, insee, espece = name_species, nb = total_count) %>%
    mutate(nb = as.numeric(nb)) %>%
    mutate(nb = ifelse(is.na(nb), 1, nb)) %>%
    mutate(source = "faune") %>%
    glimpse()
  sauve_rds(df, suffixe = "sources")
  return(invisible(df))
}
#
# fusion des données odj, 2013-2014
# source("geo/scripts/coj.R");donnees_fusion_odj()
donnees_fusion_odj <- function() {
  carp("début")
  df <- odj_lire() %>%
    mutate(jjmmaaaa = sprintf("%s.%s.%s", date_day, date_month, date_year)) %>%
    dplyr::select(date_year, jjmmaaaa, id_place, county, insee, name_species, total_count)
  sauve_rds(df)
  df <- df %>%
    dplyr::select(annee = date_year, jjmmaaaa, admid = id_place, dept = county, insee, espece = name_species, nb = total_count) %>%
    mutate(nb = as.numeric(nb)) %>%
    mutate(nb = ifelse(is.na(nb), 1, nb)) %>%
    mutate(source = "odj") %>%
    glimpse()
  sauve_rds(df, suffixe = "sources")
}
#
# fusion des données lpo44
# source("geo/scripts/coj.R");donnees_fusion_lpo44()
donnees_fusion_lpo44 <- function(force = FALSE) {
  carp("début")
  df <- lpo44_lire() %>%
    mutate(jjmmaaaa =  strftime(date, format="%d.%m.%Y")) %>%
    mutate(date_year =  strftime(date, format="%Y")) %>%
    mutate(insee = "44200") %>%
    dplyr::select(date_year, jjmmaaaa, id_form, county, insee, name_species, total_count)
  sauve_rds(df)
  df <- df %>%
    dplyr::select(annee = date_year, jjmmaaaa, admid = id_form, dept = county, insee, espece = name_species, nb = total_count) %>%
    mutate(nb = as.numeric(nb)) %>%
    mutate(nb = ifelse(is.na(nb), 1, nb)) %>%
    mutate(source = "lpo44") %>%
    glimpse()
  sauve_rds(df, suffixe = "sources")
}
#
# fusion des données en format xls
# source("geo/scripts/coj.R");donnees_fusion_xl()
donnees_fusion_xl <- function() {
  carp()
  df <- xl2012_lire()
  sauve_rds(df)
  df1 <- df %>%
    mutate(insee = gsub(" ", "", insee), annee = "2012", jjmmaaaa = format(DateObs, "%d.%m.%Y")) %>%
    glimpse() %>%
    dplyr::select(annee, jjmmaaaa, admid = Identifiant, dept = dpt, insee, espece = NomFrancais, nb = nombre) %>%
    mutate(nb = as.numeric(nb)) %>%
    mutate(nb = ifelse(is.na(nb), 1, nb)) %>%
    mutate(source = "xl2012")
  sauve_rds(df1, suffixe = "sources")
  donnees_fusion_stat(df1)
  return(invisible(df))
}
#
# fusion des données en format sql
# source("geo/scripts/coj.R");donnees_fusion_sql()
donnees_fusion_sql <- function() {
  carp()
  df <- sql_donnees_lire()
  sauve_rds(df)
  df1 <- df %>%
    mutate(insee = gsub(" ", "", saicom)) %>%
    dplyr::select(annee, jjmmaaaa = credat, admid, dept = saidep, insee, espece, nb) %>%
    mutate(nb = as.numeric(nb)) %>%
    mutate(nb = ifelse(is.na(nb), 1, nb)) %>%
    mutate(source = "sql") %>%
    glimpse()
  sauve_rds(df1, suffixe = "sources")
  donnees_fusion_stat(df1)
  return(invisible(df))
}
#
# fusion des données en format sqlv2
# source("geo/scripts/coj.R");donnees_fusion_sqlv2()
donnees_fusion_sqlv2 <- function() {
  carp()
  df <- data.frame()
  for (a in anneesSqlv2) {
    df2 <- sqlv2_tables_lire(a = a) %>%
      glimpse()
      df <- rbind(df, df2) %>%
        glimpse()
  }
  sauve_rds(df)
  df1 <- df %>%
    mutate(insee = gsub(" ", "", saicom)) %>%
    dplyr::select(annee, jjmmaaaa, admid, dept = saidep, insee, espece, nb) %>%
    mutate(source = "sqlv2")
  sauve_rds(df1, suffixe = "sources")
  donnees_fusion_stat(df1)
  return(invisible(df))
}
donnees_fusion_stat <- function(df) {
  carp()
  library(janitor)
  library(knitr)
  df1 <- df %>%
    group_by(annee, dept, admid) %>%
    summarise(nb_especes = n(), nb_oiseaux = sum(nb))
  df2 <- df1 %>%
    group_by(dept, annee) %>%
    summarise(nb = n()) %>%
    ungroup() %>%
    pivot_wider(names_from = annee, values_from = nb, values_fill = list(nb = 0)) %>%
    adorn_totals()
  print(knitr::kable(df2, format = "pipe"))
  df1 <- df %>%
    group_by(annee, source, admid) %>%
    summarise(nb_especes = n())
  df2 <- df1 %>%
    group_by(source, annee) %>%
    summarise(nb = n()) %>%
    ungroup() %>%
    pivot_wider(names_from = annee, values_from = nb, values_fill = list(nb = 0)) %>%
    select(sort(names(.))) %>%
    select(source, everything()) %>%
    adorn_totals()
  print(knitr::kable(df2, format = "pipe"))
  return(invisible(df))
}
#
# version avec annulle et remplace une année
donnees_fusion_sqlv2_v0 <- function(a="2019") {
  carp("a: %s", a)
  library(tidyverse)
  df1 <- lire_rds("donnees_fusion_sqlv2") %>%
    glimpse()
  if (a != "") {
    df2 <- sqlv2_tables_lire(a = a) %>%
      glimpse()
    df1 <- df1 %>%
      filter(annee != a) %>%
      glimpse()
      df <- rbind(df1, df2) %>%
        glimpse()
    sauve_rds(df)
    df1 <- df %>%
      mutate(insee = gsub(" ", "", saicom)) %>%
      dplyr::select(annee, admid, dept = saidep, insee, espece, nb) %>%
      mutate(source = "sqlv2")
    sauve_rds(df1, suffixe = "sources")
  } else {
    df <- df1
  }
  df1 <- df %>%
    group_by(annee, dept = saidep, admid) %>%
    summarise(nb_especes = n())
  df2 <- df1 %>%
    group_by(dept, annee) %>%
    summarise(nb = n()) %>%
    ungroup() %>%
    pivot_wider(names_from = annee, values_from = nb, values_fill = list(nb = 0)) %>%
    glimpse()
  print(kable(df2))
  return(invisible(df))
}
#
# les stat pour comparaison avec les bilans
# source("geo/scripts/coj.R");donnees_sql_stat()
donnees_sql_stat <- function() {
  carp()
  library(tidyselect)
  df <- lire_rds("donnees_fusion_sqlv2") %>%
    glimpse()
  annees <- c("2013", "2014", "2015", "2016", "2017", "2018", "2019")
  df1 <- df %>%
    filter(annee %in% annees) %>%
    group_by(annee, saidep, admid) %>%
    summarize(nb = n()) %>%
    group_by(annee, departement = saidep) %>%
    summarize(nb = n()) %>%
    pivot_wider(names_from = departement, values_from = nb, values_fill = list(nb = 0)) %>%
    ungroup() %>%
    mutate("bzh" = dplyr::select(., matches("\\d")) %>% rowSums(na.rm = TRUE)) %>%
    print(n = 20)
  stat.df <- emmanuelle_stat() %>%
    mutate(annee = sprintf("%s", annee)) %>%
    glimpse()
  df2 <- df1 %>%
    left_join(stat.df, by = c("annee"), suffix = c(".sql", ".xls")) %>%
    dplyr::select(sort(peek_vars())) %>%
    print(n = 20)
  return(invisible(df1))
}

# lecture des données
# et filtrage des jardins hors normes
# source("geo/scripts/coj.R");donnees_lire(force=TRUE, filtre=FALSE)
donnees_lire <- function(rda = "donnees_fusion_sources", force = FALSE, filtre = TRUE) {
  carp()
  if (exists("donnees.df") & force == FALSE) {
    return(invisible(donnees.df))
  }
#  df <- donnees_fusion_lire(rda)
  df <- lire_rds("donnees_fusion_sources") %>%
    mutate(nb = as.numeric(nb)) %>%
    glimpse()
  df1 <- df %>%
    mutate(ligne = 1:nrow(.)) %>%
    filter(is.na(espece))
  if (nrow(df1) > 0) {
    carp("************* espece na: %s", nrow(df1))
    glimpse(df1)
#    stop("*************")
  }
  df <- df %>%
    filter(! is.na(espece))
  df1 <- df %>%
    group_by(espece) %>%
    summarize(nb1 = n())
  cause.df <<- data.frame(id = character(), espece = character(), cause = character(), nb = integer(), stringsAsFactors = FALSE)
  df <- df %>%
    mutate(nb = as.numeric(as.character(nb))) %>%
    mutate(admid = as.numeric(admid)) %>%
    mutate(id = sprintf("%s/%05d", annee, admid))
  df <- donnees_lire_clean(df)
#  df <- df %>%
#    filter(saidep == "35" & annee == "2018")
  if (filtre == TRUE) {
    df <- donnees_lire_hors(df)
    df <- donnees_lire_jardin(df)
    df <- donnees_lire_ko(df)
    df <- donnees_lire_exclu(df)
    df <- donnees_lire_marginales(df)
    cause.df %>%
      glimpse() %>%
      filter(grepl("2019$", id)) %>%
#      arrange(id) %>%
      print(n = 100, na.print = "")
    stop("***")
  }
  df1 <- df %>%
    mutate(ligne = 1:nrow(.)) %>%
    filter(is.na(espece))
  if (nrow(df1) > 0) {
    carp("************* espece na: %s", nrow(df1))
    glimpse(df1)
#    stop("*************")
  }
  df <- df %>%
    filter(! is.na(espece))
  donnees.df <<- df %>%
    glimpse()
  df2 <- df %>%
    group_by(espece) %>%
    summarize(nb2 = n()) %>%
    print(n = 250, na.print = "")
  carp("nrow: %s", nrow(df))
  return(invisible(df))
  carp("espece apres conversion")
  df3 <- df1 %>%
    full_join(df2) %>%
    filter(is.na(nb1)) %>%
    print(n = 150, na.print = "")
  carp("espece avant conversion")
  df3 <- df1 %>%
    full_join(df2) %>%
    filter(is.na(nb2)) %>%
    print(n = 150, na.print = "")
}
#
# nettoyage du nom des espèces
donnees_lire_clean <- function(donnees.df) {
  dsn <- sprintf("%s/espece_clean.csv", cfgDir)
  df <- read.table(dsn, header = TRUE, sep = ";", blank.lines.skip = TRUE, stringsAsFactors = FALSE, quote = "")
  df <- especes_lire("espece_clean")
  carp(" %s nrow: %d", dsn, nrow(df))
  print(df)
#  df$espece <- iconv(df$espece, "UTF-8")
  for (i in 1:nrow(df)) {
    regexp <- df$regex[i]
    df1 <- subset(donnees.df, grepl(regexp, espece, ignore.case = TRUE, perl = TRUE))
    if (nrow(df1) > 0) {
      df1 %>%
        group_by(espece) %>%
        summarize(nb = n())
      donnees.df$espece[grepl(regexp, donnees.df$espece, ignore.case = TRUE, perl = TRUE)] <- df$espece[i]
    }
#    df1$espece[grepl(regexp, df1$espece, ignore.case=TRUE, perl=TRUE)] <- df$espece[i]
#    print(df1)
  }
#  stop('***')
  return(invisible(donnees.df))
}
#
# pour exclure des espèces
donnees_lire_exclu <- function(df) {
  carp()
  dsn <- sprintf("%s/espece_exclu.csv", cfgDir)
  exclu.df <- read.table(dsn, header = TRUE, sep = ";", blank.lines.skip = TRUE, stringsAsFactors = FALSE, quote = "", encoding = "UTF-8")
  carp("%s nrow: %d", dsn, nrow(exclu.df))
#  print(Encoding(ko.df$espece))
  df1 <- df %>%
    filter(espece %in% exclu.df$espece)
  df1$cause <- "exclu"
#  cause.df <<- rbind(cause.df, df1[, c("id", "espece", "cause", "nb")])
  df3 <- df1 %>%
    group_by(espece) %>%
    summarize(nb = n())
  tex_df2table(df3)
  carp("df1 nrow : %s", nrow(df1))
  df2 <- df %>%
    filter(! espece %in% exclu.df$espece)
  carp("df2 nrow : %s", nrow(df2))
  return(invisible(df2))
}
#
# filtrage des jardins avec une espèce > 100
donnees_lire_hors <- function(df) {
  carp("df nrow : %s", nrow(df))
  df1 <- df %>%
    filter(nb > 100)
  df1$cause <- "nb > 100"
  cause.df <<- rbind(cause.df, df1[, c("id", "espece", "cause", "nb")])
  df2 <- df %>%
    filter(! id %in% df1$id)
  carp(" df2 nrow : %s", nrow(df2))
  return(invisible(df2))
}
#
# filtrage des jardins avec plus de 200 oiseaux ou moins de 4
donnees_lire_jardin <- function(df) {
  carp(" df nrow : %s", nrow(df))
  df1 <- df %>%
    group_by(id) %>%
    summarise(nb = sum(nb))
  df1 <- df1 %>%
    filter(nb > 200 | nb < 4)
  df1$cause <- "nb > 200 | nb < 4"
  df1$espece <- "*"
  cause.df <<- rbind(cause.df, df1[, c("id", "espece", "cause", "nb")])
  df2 <- df %>%
    filter(! id %in% df1$id)
  carp(" df2 nrow : %s", nrow(df2))
  return(invisible(df2))
}
#
# pour supprimer des comptages avec erreur espèce
donnees_lire_ko <- function(df) {
  carp()
  dsn <- sprintf("%s/espece_ko.csv", cfgDir)
  ko.df <- read.table(dsn, header = TRUE, sep = ";", blank.lines.skip = TRUE, stringsAsFactors = FALSE, quote = "", encoding = "UTF-8")
  carp("%s nrow: %d", dsn, nrow(ko.df))
#  print(Encoding(ko.df$espece))
  df1 <- df %>%
    filter(espece %in% ko.df$espece)
  carp("df1 nrow : %s", nrow(df1))
  df1$cause <- "ko"
  cause.df <<- rbind(cause.df, df1[, c("id", "espece", "cause", "nb")])
  df3 <- df1 %>%
    group_by(espece) %>%
    summarize(nb = n())
  tex_df2table(df3)
  df3 <- left_join(ko.df, df3, by = "espece")
  print(df3)
  df2 <- df %>%
    filter(! id %in% df1$id)
  carp("df2 nrow : %s", nrow(df2))
  return(invisible(df2))
}
#
# filtrage des jardins avec des espèces marginales
donnees_lire_marginales <- function(df) {
  carp("df nrow : %s", nrow(df))
  df1 <- df %>%
    group_by(espece) %>%
    summarise(nb = sum(nb))
  df1 <- df1 %>%
    filter(nb < 5)
  print(df1)
  df3 <- df %>%
    filter(espece %in% df1$espece) %>%
    group_by(id)
  carp("df3 nrow : %s", nrow(df3))
  if (nrow(df3) == 0) {
    return(invisible(df))
  }
  df4 <- df %>%
    filter(id %in% df3$id & espece %in% df1$espece)
  df4$cause <- "marginales"
  cause.df <<- rbind(cause.df, df4[, c("id", "espece", "cause", "nb")])
  df2 <- df %>%
    filter(! id %in% df3$id)
  carp("df2 nrow : %s", nrow(df2))
  return(invisible(df2))
}
#
# passage en format xlsx pour Matthieu
# source("geo/scripts/coj.R");donnees_xlsx()
donnees_xlsx <- function() {
  library(tidyverse)
  library(xlsx)
  carp()
  df <- donnees_lire(force = TRUE, filtre = FALSE)
  df2 <- df %>%
    filter(saidep == "35")
#  print(head(df2));stop("***")
  dsn <- sprintf("%s/sql_donnees.xlsx", cfgDir)
  write.xlsx(df2, dsn, sheetName = "coj",  col.names = TRUE, row.names = FALSE, append = FALSE, showNA = TRUE)
  carp("dsn:%s, nrow:%s", dsn, nrow(df2))
}
