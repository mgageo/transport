# <!-- coding: utf-8 -->
#
# quelques fonctions pour la comptage oiseaux des jardins
# auteur: Marc Gauthier
# licence: Creative Commons Paternité - Pas d"Utilisation Commerciale - Partage des Conditions Initiales à l"Identique 2.0 France
# ===============================================================
#
# le traitement des exports sql v2 en format xlsx
#
# les traitements à enchainer suite à une mise à jour
# source("geo/scripts/coj.R");sqlv2_jour()
sqlv2_jour <- function(force = TRUE) {
  carp()
  library(tidyverse)
  sqlv2_tables_get(a = anneeRef, force = force)
  sqlv2_tables_lire(a = anneeRef, force = force)
}
#
# récupération des tables sur le serveur en format xlsx
# source("geo/scripts/coj.R");sqlv2_tables_get(a = "2024", force = FALSE)
sqlv2_tables_get <- function(a = "2022", force = FALSE) {
  carp("a: %s force: %s", a, force)
  sqlv2_table_get(a = a, "bvcoj_v2_entete", force = force)
  sqlv2_table_get(a = a, "bvcoj_v2_details", force = force)
}
sqlv2_table_get <- function(a, table, force = FALSE) {
  library(httr)
  carp("a: %s table: %s force: %s", a, table, force)
  dsn <- sprintf("%s/%s_%s.xlsx", varDir, table, a)
  if (file.exists(dsn) && force == FALSE) {
    return(invisible(dsn))
  }
  url <- sprintf("http://bv/outils/coj_sauve.php?action=xlsx&export=%s", table)
  url <- sprintf("http://bretagne-vivante-dev.org/outils/coj_sauve.php?action=xlsx&export=%s_mga_%s", table, a)
  carp("url: %s", url)
  httr::GET(url, authenticate(mes_options("coj_username"), mes_options("coj_password")), write_disk(dsn, overwrite = TRUE))
  carp("dsn: %s", dsn)
  return(invisible(dsn))
}
#
# lecture des tables en format excel et validation
# source("geo/scripts/coj.R"); df <- sqlv2_tables_lire("2021", TRUE)
sqlv2_tables_lire <- function(a = "2021", force = FALSE) {
  library(tidyverse)
  library(rio)
  library(janitor)
  carp()
  suffixe <- sprintf("_%s", a)
  entete.df <<- sqlv2_table_lire(sprintf("bvcoj_v2_entete%s", suffixe)) %>%
    glimpse()
  details.df <<- sqlv2_table_lire(sprintf("bvcoj_v2_details%s", suffixe)) %>%
    glimpse()
  df <- sqlv2_table_entete_valide(entete.df) %>%
    mutate(annee = a) %>%
    filter(grepl(a, cojcreationdate)) %>%
    dplyr::select(cojid, annee, saidep = cojjardindepartement, saicom = cojjardininsee) %>%
    glimpse()
#  stop("***")
  df1 <- sqlv2_table_detail_valide(details.df) %>%
    left_join(df, by = c("cojdetailnumidentete" = "cojid"))
  carp("details sans entete")
  df1 %>%
    filter(is.na(annee)) %>%
    glimpse()
  df1 <- df1 %>%
    filter(! is.na(annee)) %>%
    glimpse()
  carp("details especes_ko")
  especes_ko <- c("M", "Geai des ch", "Go")
  df1 %>%
    dplyr::select(admid = cojdetailnumidentete, annee, saidep, saicom, espece = cojdetailespece, nb) %>%
    filter(espece %in% especes_ko) %>%
    glimpse()
  df1 <- df1 %>%
    glimpse() %>%
    dplyr::select(admid = cojdetailnumidentete, jjmmaaaa = cojdetailcontroledate, annee, saidep, saicom, espece = cojdetailespece, nb)
  carp("liste des especes")
  df2 <- df1 %>%
    group_by(espece) %>%
    summarize(nb = n()) %>%
    arrange(-nb)
  if (debug) {
    View(df2)
  }
  carp("nrow: %s", nrow(df1))
  return(invisible(df1))
}
#
# lecture des tables en format excel et validation
# source("geo/scripts/coj.R");sqlv2_tables_matthieu()
sqlv2_tables_matthieu <- function(debug = TRUE) {
  library(tidyverse)
  library(rio)
  library(janitor)
  library(writexl)
  sqlv2_tables_lire(a = "2020", debug = debug)
  df <- sqlv2_table_entete_valide(entete.df) %>%
    filter(cojjardindepartement == "35") %>%
    glimpse()
  dsn <- sprintf("%s/bvcoj_v2_entete_35.xlsx", varDir)
  carp("dsn: %s", dsn)
  write_xlsx(df, dsn)
  df1 <- details.df %>%
    filter(cojdetailnumidentete %in% df$cojid) %>%
    glimpse()
  dsn <- sprintf("%s/bvcoj_v2_details_35.xlsx", varDir)
  carp("dsn: %s", dsn)
  write_xlsx(df1, dsn)
}
#
# on valide
sqlv2_table_detail_valide <- function(df) {
  carp("nombre non numérique")
  df1 <- df %>%
    mutate(nb = as.numeric(cojdetailnombre)) %>%
    filter(is.na(nb))
  if (nrow(df1) > 0) {
    carp("****")
    glimpse(df1)
#    stop("***")
  }
  df <- df %>%
    mutate(jardinlongitude = as.numeric(cojdetailcontrolelongitude)) %>%
    mutate(jardinlatitude = as.numeric(cojdetailcontrolelatitude))
  df2 <- df %>%
    filter(is.na(jardinlongitude) | is.na(jardinlatitude))
  if (nrow(df2) > 0) {
    carp("longitude/latitude invalide")
    glimpse(df2)
  }
  df <- df %>%
    mutate(nb = as.numeric(cojdetailnombre)) %>%
    filter(! is.na(nb)) %>%
    filter(nb > 0) %>%
    glimpse()
  carp("nrow: %s", nrow(df))
  return(invisible(df))
# bug : manque des espèces
  dsn <- sprintf("%s/espece_saisie.csv", cfgDir)
  saisie.df <- read.table(dsn, header = TRUE, sep = ";", blank.lines.skip = TRUE, stringsAsFactors = FALSE, quote = "")
  carp("%s nrow: %d", dsn, nrow(saisie.df))
#  saisie.df$espece <- iconv(saisie.df$espece, "UTF-8")
  saisie.df$saisie <- 1:nrow(saisie.df)
  df2 <- df1 %>%
    left_join(saisie.df, by = c("cojdetailespece" = "espece")) %>%
    filter(is.na(saisie)) %>%
    select(cojdetailespece) %>%
    print(n = 150)
  df <- df %>%
    left_join(saisie.df, by = c("cojdetailespece" = "espece")) %>%
    filter(! is.na(saisie)) %>%
    select(-saisie)
  carp("nrow: %s", nrow(df))
  return(invisible(df))
}
#
# on valide
sqlv2_table_entete_valide <- function(df) {
  carp()
  df <- df %>%
    mutate(creationdate = as.Date(cojcreationdate, format = "%d/%m/%Y")) %>%
    mutate(saisiedate = as.Date(cojsaisiedate, format = "%d/%m/%Y"))
  carp("date invalide")
  df1 <- df %>%
    filter(is.na(saisiedate))
  if (nrow(df1) > 0) {
    glimpse(df1)
  }
  df <- df %>%
    filter(! is.na(saisiedate)) %>%
    mutate(jardinlongitude = as.numeric(cojjardinlongitude)) %>%
    mutate(jardinlatitude = as.numeric(cojjardinlatitude))
  df2 <- df %>%
    filter(is.na(jardinlongitude) | is.na(jardinlatitude))
  if (nrow(df2) > 0) {
    carp("longitude/latitude invalide")
    glimpse(df2)
  }
  df <- df %>%
    filter(! is.na(jardinlongitude)) %>%
    filter(! is.na(jardinlatitude))
# que les départements "bretons"
  carp("departement invalide")
  les_departements <- c("22", "29", "35", "44", "56")
  df3 <- df %>%
    filter(! cojjardindepartement %in% les_departements)
  if (nrow(df3) > 0) {
    glimpse(df3)
  }
  df <- df %>%
    filter(cojjardindepartement %in% les_departements)
  carp("code insee invalide")
  df4 <- df %>%
    filter(is.na(cojjardininsee))
  if (nrow(df4) > 0) {
    glimpse(df4)
#    sqlv2_table_entete_geocode(df4)
  }
  carp("nrow: %s", nrow(df))
#  stop("***")
  return(invisible(df))
}
#
# on geocode inverse
sqlv2_table_entete_geocode <- function(df) {
  carp()
  library(rio)
  df <- df %>%
    dplyr::select(lat = jardinlatitude, lon = jardinlongitude, cojid) %>%
    glimpse()
  f_orig <- sprintf("%s/sqlv2_table_entete.csv", cfgDir);
  rio::export(df, f_orig)
  f_reverse <- sprintf("%s/sqlv2_table_entete_reverse.csv", cfgDir);
  geocode_reverse_csv_datagouv(f_orig, f_reverse)
}
#
# on fait des stat
# source("geo/scripts/coj.R");sqlv2_table_entete_stat()

sqlv2_table_entete_stat <- function() {
  carp()
  options(stringsAsFactors = FALSE)
  entete.df <- sqlv2_table_lire("bvcoj_v2_entete")
  df <- sqlv2_table_entete_valide(entete.df) %>%
    glimpse()
  df1 <- df %>%
    filter(saisiedate >= as.Date("2019-01-25") & saisiedate <= as.Date("2019-02-05")) %>%
    group_by(saisiedate, saidep = cojjardindepartement) %>%
    summarize(nb_jardins = n()) %>%
    mutate(saidep = sprintf("%02d", saidep)) %>%
    print(n = 20)
  gg <- ggplot(data = df1, aes(x = saisiedate, y = nb_jardins, fill = saidep, label = nb_jardins)) +
    geom_bar(stat = "identity") +
    geom_text(size = 3, position = position_stack(vjust = 0.5))
  print(gg)
}

#
# lecture d"un fichier excel
sqlv2_table_lire <- function(table) {
#  library(rio)
  library(readxl)
  dsn <- sprintf("%s/%s.xlsx", varDir, table)
  carp("dsn: %s", dsn)
#  df <- import(dsn)
#  df <- read_excel(dsn, col_types = "text")
  df <- read_excel(dsn)
  return(invisible(df))
}
#
# lecture des tables en format excel et validation
# source("geo/scripts/coj.R"); df <- sqlv2_admid(admid = "2024/sqlv2/23011")
# source("geo/scripts/coj.R"); df <- sqlv2_admid(admid = "2024/sqlv2/23972")
sqlv2_admid <- function(admid = "2024/sqlv2/23011", force = FALSE) {
  library(tidyverse)
  library(rio)
  library(janitor)
  carp()
  asi <- str_split_1(admid, "/")
  a <- asi[1]
  cojid <- asi[3]
  suffixe <- sprintf("_%s", a)
  entete.df <- sqlv2_table_lire(sprintf("bvcoj_v2_entete%s", suffixe)) %>%
    filter(cojid == !!cojid) %>%
    glimpse()
  details.df <- sqlv2_table_lire(sprintf("bvcoj_v2_details%s", suffixe)) %>%
    filter(cojdetailnumidentete == !!cojid) %>%
    mutate(nb = as.integer(cojdetailnombre)) %>%
    dplyr::select(espèce = cojdetailespece, nb) %>%
    arrange(espèce) %>%
    adorn_totals()
  misc_print(details.df)
}