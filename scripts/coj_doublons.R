# <!-- coding: utf-8 -->
#
# quelques fonctions pour la comptage oiseaux des jardins
# auteur: Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
# la recherche des doublons
#
# les données proviennent :
# 1/ de la base sql de Bretagne Vivante (coj)
# 2/ de la base faune-bretagne via un export (faune)
# la recherche ne va porter que sur les données d'une année
#
# base coj contient des données du département 44
# les critères disponibles sont :
# 1/ l'adresse email
#
# les traitements à enchainer suite à une mise à jour
# source("geo/scripts/coj.R");doublons_jour()
doublons_jour <- function() {
  library(tidyverse)
  carp()
  annee <<- '2020'
  doublons_donnees_lire()
  doublons_email()
  doublons_spatial()
  tex_pdflatex('faune_doublons.tex')
}
#
# doublons sur l'adresse email
doublons_email <- function() {
  library(tidyverse)
  carp()
  carp('extraction email coj')
  email_coj.df <- coj.df %>%
    rename(email=cojobservateurmail) %>%
    group_by(cojid, email) %>%
    summarize(nb_especes=n(), nb_oiseaux=sum(as.integer(cojdetailnombre))) %>%
    glimpse()
  carp('extraction email faune')
  email_faune.df <- faune.df %>%
    group_by(id_form, email) %>%
    summarize(nb_especes=n(), nb_oiseaux=sum(as.integer(total_count))) %>%
    glimpse()
  carp('jointure')
  df1 <- email_coj.df %>%
    inner_join(email_faune.df, by=c('email')) %>%
    glimpse() %>%
    print(n=30)
   tex_df2table(df1, dossier="faune", num=TRUE, nb_lignes=65)
}
#
# doublons spatial
doublons_spatial <- function() {
  library(tidyverse)
  library(sf)
  library(nngeo)
  carp()
  carp('spatial coj')
  lonlat_coj.df <- coj.df %>%
    rename(lon=cojdetailcontrolelongitude) %>%
    rename(lat=cojdetailcontrolelatitude) %>%
    group_by(cojid, lon, lat) %>%
    summarize(nb_especes=n(), nb_oiseaux=sum(as.integer(cojdetailnombre))) %>%
    filter(! is.na(lat)) %>%
    glimpse()
  carp('extraction email faune')
  lonlat_faune.df <- faune.df %>%
    rename(lon=coord_lon) %>%
    rename(lat=coord_lat) %>%
    mutate(lon=as.numeric(lon)) %>%
    mutate(lat=as.numeric(lat)) %>%
    group_by(id_form, lon, lat) %>%
    summarize(nb_especes=n(), nb_oiseaux=sum(as.integer(total_count))) %>%
    glimpse()
  carp('conversion en spatial')
  coj.sf <- st_as_sf(lonlat_coj.df, coords=c("lon", "lat"), crs = 4326) %>%
    st_transform(2154)
  faune.sf <- st_as_sf(lonlat_faune.df, coords=c("lon", "lat"), crs = 4326) %>%
    st_transform(2154)
  carp('jointure')
  df1 <- st_join(coj.sf, faune.sf, st_nn, k = 1, maxdist = 100) %>%
    filter(! is.na(nb_especes.y)) %>%
    st_drop_geometry() %>%
    glimpse() %>%
    print(n=30)
  sauve_rds(df1)
  tex_df2table(df1, dossier="faune", num=TRUE, nb_lignes=65)
  return(invisible(df1))
}
#
# doublons spatial, la liste
# source("geo/scripts/coj.R");doublons_spatial_liste()
doublons_spatial_liste <- function() {
  library(tidyverse)
  library(rio)
  carp()
  coj.df[is.na(coj.df)] <- ""
  df <- lire_rds('doublons_spatial')
  for(i in 1:nrow(df)) {
    df1 <- doublons_coj_liste(df[i, 'cojid'])
    for(c in colnames(df1) ) {
      df[i, c] <- df1[1, c]
    }
    df1 <- doublons_faune_liste(df[i, 'id_form'])
    for(c in colnames(df1) ) {
      df[i, c] <- df1[1, c]
    }
  }
  glimpse(df)
  print(df, n=20)
  dsn <- sprintf("%s/doublons_spatial.xlsx", cfgDir);
  rio::export(df, dsn)
  tex_dftpl2fic(df, 'doublons_liste')
  tex_pdflatex('faune_doublons.tex')
}
#
# détermination des jardins avec le même observateur
# source("geo/scripts/coj.R");doublons_spatial_filtre()
doublons_spatial_filtre <- function() {
  library(tidyverse)
  library(rio)
  carp()
  dsn <- sprintf("%s/doublons_spatial.xlsx", cfgDir);
  df <- rio::import(dsn)
  df$doublon <- ''
  View(df)
  for(i in 1:nrow(df)) {
#
# on ne compare que si les nombres d'especes et d'oiseaux sont proches
    nbx <- df[i, 'nb_especes.x']
    nby <- df[i, 'nb_especes.y']
    delta <- as.integer(abs(nbx-nby)*10/(nbx+nby))
    if(delta > 3) {
      carp('%s delta especes: %s', i, delta)
#      glimpse(df[i,])
      next
    }
    nbx <- df[i, 'nb_oiseaux.x']
    nby <- df[i, 'nb_oiseaux.y']
    delta <- as.integer(abs(nbx-nby)*10/(nbx+nby))
    if(delta > 3) {
      carp('%s delta oiseaux: %s %s %s', i, delta, nbx, nby)
#      glimpse(df[i,]);      stop('***')
      next;
    }
# l'adresse email coj doit être valide
#    carp('i: %s email: %s', i, df[i, 'coj_email'])
    if( ! is.na(df[i, 'coj_email'])) {
      if (df[i, 'coj_email'] == df[i, 'faune_email'] ) {
        df[i, 'doublon'] <- 'email'
        next;
      }
    }
#
# comparaison sur le nom
# risque avec des observateurs de la même famille
    if( ! is.na(df[i, 'coj_nom'])) {
      if (grepl(df[i, 'faune_nom'], df[i, 'coj_nom'], ignore.case = TRUE) ) {
        df[i, 'doublon'] <- 'nom'
#        glimpse(df[i,])
        next;
      }
    }
  }
  df1 <- df %>%
    filter(doublon != '') %>%
    View(.)
}
#
# lecture des données
doublons_donnees_lire <- function() {
  library(tidyverse)
  carp()
  if ( ! exists('coj.df')) {
    entete.df <- sqlv2_table_lire('bvcoj_v2_entete') %>%
      glimpse()
    details.df <- sqlv2_table_lire('bvcoj_v2_details') %>%
      glimpse()
    coj.df <<- left_join(entete.df, details.df, by=c('cojid'='cojdetailnumidentete')) %>%
      glimpse()
  }
  if ( ! exists('faune.df')) {
    faune.df <<- faune_donnees_jardin_annee_lire(annee=annee) %>%
     glimpse()
  }
}
# source("geo/scripts/coj.R");doublons_coj_liste()
doublons_coj_liste <- function(id=48) {
#  carp('cojid: %s', id)
  id <- sprintf('%s', id)
  df <- coj.df %>%
    filter(cojid==get("id")) %>%
    mutate(coj_horo=sprintf('%s %s', cojcreationdate, cojcreationheure)) %>%
    mutate(coj_adresse=sprintf('%s/%s/%s', cojjardininsee, cojjardincommune, cojjardinadresse)) %>%
#    glimpse() %>%
    dplyr::select(coj_nom=cojobservateurnom, coj_email=cojobservateurmail, coj_adresse, coj_horo)
#  tpl <- '{{cojobservateurnom}}, {{cojobservateurmail}}, {{cojjardinadresse}}'
#  txt <- tex_df2tpl(df, 1, tpl)
#  carp('txt: %s', txt)
#  stop('***')
  return(invisible(df))
}
# source("geo/scripts/coj.R");doublons_faune_liste()
doublons_faune_liste <- function(id="18663") {
  id <- sprintf('%s', id)
#  carp('id_form: %s', id)
  df <- faune.df %>%
    filter(id_form==get("id")) %>%
    mutate(faune_nom=sprintf('%s %s', name, surname)) %>%
    mutate(faune_horo=sprintf('%s/%s/%s %sh%s', date_day, date_month, date_year, time_start_hour, time_start_min)) %>%
    mutate(faune_adresse=sprintf('%s/%s/%s', insee, municipality, place)) %>%
#    glimpse() %>%
    dplyr::select(faune_nom, faune_email=email, faune_horo, faune_adresse)
#  tpl <- '{{tra_name}}, {{email}}'
#  txt <- tex_df2tpl(df, 1, tpl)
#  carp('txt: %s', txt)
#  stop('***')
  return(invisible(df))
}
#
# géocadage des données faune
# source("geo/scripts/coj.R");doublons_faune_geocode()
doublons_faune_geocode <- function() {
  library(rio)
  library(tidyverse)
  carp()
  df <- faune.df %>%
#    glimpse() %>%
    group_by(id_sighting, coord_lon, coord_lat) %>%
    summarize(nb=n()) %>%
    dplyr::select(lat=coord_lat, lon=coord_lon, id_sighting) %>%
    glimpse()
  f_orig <- sprintf("%s/doublons_faune.csv", cfgDir);
  rio::export(df, f_orig)
  f_reverse <- sprintf("%s/doublons_faune_geo.csv", cfgDir);
  geocode_reverse_csv_datagouv(f_orig, f_reverse)
}
# source("geo/scripts/coj.R");doublons_faune_geocode_lire()
doublons_faune_geocode_lire <- function() {
  library(rio)
  library(tidyverse)
  carp()
  f_reverse <- sprintf("%s/doublons_faune_geo.csv", cfgDir);
  df <- rio::import(f_reverse, encoding = "UTF-8") %>%
    glimpse() %>%
    filter(is.na(result_citycode)) %>%
    glimpse()

}