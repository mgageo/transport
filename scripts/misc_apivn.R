# <!-- coding: utf-8 -->
#
# quelques fonctions pour les données apivn
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
#
# https://www.faune-bretagne.org/index.php?m_id=43
#
# l'instance apivn
user1 <- "apivn45"; # 10/03/2023
user2 <- "apivn187"; # 03/04/2023
user3 <- "apivn184"; # 15/11/2023
user2 <- "VMName"; # 02/07/2024
#
# source("geo/scripts/apivn.R"); df <- apivn_oiseaux_lire()
apivn_oiseaux_lire <- function(force = FALSE) {
  library(tidyverse)
  dsn <- sprintf("%s/postgresql_src_vn_species.xlsx", apivnDir)
  carp("dsn: %s", dsn)
  df <- rio::import(dsn)
  df1 <- df %>%
    filter(id_taxo_group == 1) %>%
# pour les oiseaux
    filter(is_used == TRUE)
  biolo_ecrire(df1, "apivn_oiseaux")
  df2 <- df1 %>%
# pour les nicheurs "bvo"
#    filter(atlas_start > 0) %>%
    rename(espece = french_name) %>%
    dplyr::select(espece, latin_name, atlas_start, atlas_end) %>%
    mutate(debut = as.Date(atlas_start - 1, origin = "2021-12-31")) %>%
    mutate(debut = format(debut, "%d %B")) %>%
    mutate(fin = as.Date(atlas_end - 1, origin = "2021-12-31")) %>%
    mutate(fin = format(fin, "%d %B")) %>%
    mutate(periode = sprintf("%s - %s", debut, fin)) %>%
    dplyr::select(-debut, -fin) %>%
    arrange(espece)
  return(invisible(df2))
}
#
# la requête sql par défaut
apivn_extract_psql <- function() {
  carp()
  psql <- "
SELECT src_vn.observations.id_form_universal, id_sighting, id_species, french_name, date, admin_hidden
  , precision, estimation_code, count
  , id_place, place
  , src_vn.observations.project_code
  , src_vn.forms.coord_lat, src_vn.forms.coord_lon
  , src_vn.forms.date_start, src_vn.forms.date_stop
  , src_vn.forms.time_start, src_vn.forms.time_stop
  , src_vn.forms.full_form
  , src_vn.forms.protocol_name
  , src_vn.forms.observer_uid
  FROM src_vn.observations
  LEFT JOIN src_vn.species ON
    src_vn.observations.id_species = src_vn.species.id
    AND src_vn.observations.taxonomy = src_vn.species.id_taxo_group
  LEFT JOIN src_vn.forms ON
    src_vn.observations.id_form_universal = src_vn.forms.id_form_universal
  WHERE src_vn.observations.taxonomy = 1
    AND %s
"
  return(invisible(psql))
}
#
# source("geo/scripts/apivn.R"); extract_psql()
apivn_extract_psql <- function() {
  carp()
  psql <- "
SELECT src_vn.observations.id_form_universal, id_sighting, id_species, french_name, date, admin_hidden
  , precision, estimation_code, count
  , src_vn.observations.project_code
  , src_vn.forms.coord_lat, src_vn.forms.coord_lon
  , src_vn.forms.date_start, src_vn.forms.date_stop
  , src_vn.forms.time_start, src_vn.forms.time_stop
  , src_vn.forms.full_form
  , src_vn.forms.protocol_name
  FROM src_vn.observations
  LEFT JOIN src_vn.species ON
    src_vn.observations.id_species = src_vn.species.id
    AND src_vn.observations.taxonomy = src_vn.species.id_taxo_group
  LEFT JOIN src_vn.forms ON
    src_vn.observations.id_form_universal = src_vn.forms.id_form_universal
  WHERE src_vn.observations.taxonomy = 1
    AND %s
"
  return(invisible(psql))
}
#
# source("geo/scripts/apivn.R"); extract_psql()
apivn_extract_psql2 <- function() {
  carp()
  psql <- "
SELECT src_vn.observations.id_form_universal, id_sighting, id_species, french_name, date, admin_hidden
  , insert_date, update_date
  , src_vn.forms.protocol_name
  FROM src_vn.observations
  LEFT JOIN src_vn.species ON
    src_vn.observations.id_species = src_vn.species.id
    AND src_vn.observations.taxonomy = src_vn.species.id_taxo_group
  LEFT JOIN src_vn.forms ON
    src_vn.observations.id_form_universal = src_vn.forms.id_form_universal
  WHERE src_vn.observations.taxonomy = 1
    AND %s
"
  return(invisible(psql))
}
#
# source("geo/scripts/apivn.R"); extract_psql()
apivn_extract_psql3 <- function() {
  carp()
  psql <- "
SELECT src_vn.observations.id_form_universal, id_sighting, id_species, french_name, date, admin_hidden
  , insert_date, update_date
  , src_vn.forms.id as id_form, src_vn.forms.protocol_name, src_vn.forms.date_start, src_vn.forms.time_start, src_vn.forms.date_stop, src_vn.forms.time_stop
  FROM src_vn.observations
  LEFT JOIN src_vn.species ON
    src_vn.observations.id_species = src_vn.species.id
    AND src_vn.observations.taxonomy = src_vn.species.id_taxo_group
  LEFT JOIN src_vn.forms ON
    src_vn.observations.id_form_universal = src_vn.forms.id_form_universal
  WHERE src_vn.observations.taxonomy = 1
    AND %s
"
  return(invisible(psql))
}
#
# pour les especes
apivn_extract_psql_especes <- function(especes) {
  library(stringr)
  carp()
  psql <- "
SELECT src_vn.observations.id_form_universal, id_sighting, id_species, french_name, date, admin_hidden
  , precision, estimation_code, count, behaviours, src_vn.observations.observer_uid
  , src_vn.places.id_commune, src_vn.places.name, src_vn.local_admin_units.insee, src_vn.local_admin_units.name as commune
  FROM src_vn.observations
  LEFT JOIN src_vn.species ON
    src_vn.observations.id_species = src_vn.species.id
    AND src_vn.observations.taxonomy = src_vn.species.id_taxo_group
  LEFT JOIN src_vn.places ON
    src_vn.observations.id_place = src_vn.places.id
  LEFT JOIN src_vn.local_admin_units ON
    src_vn.local_admin_units.id = src_vn.places.id_commune
  WHERE
    src_vn.local_admin_units.insee ~ '^35'
    AND species.french_name ~ '({especes})'
"
  especes <- paste(especes, collapse = "|")
  psql <- str_glue(psql)
  carp("psql: %s", psql)
  return(invisible(psql))
}
#
# pour les famillles
apivn_extract_psql_familles <- function(familles) {
  library(stringr)
  carp()
  psql <- "
SELECT src_vn.observations.id_form_universal, id_sighting, id_species, french_name, date, admin_hidden
  , precision, estimation_code, count
  FROM src_vn.observations
  LEFT JOIN src_vn.species ON
    src_vn.observations.id_species = src_vn.species.id
    AND src_vn.observations.taxonomy = src_vn.species.id_taxo_group
  LEFT JOIN src_vn.families ON
    src_vn.species.id_taxo_group = src_vn.families.id_taxo_group
  WHERE src_vn.families.latin_name IN {familles}
"
  familles <- paste(familles, collapse = "','")
  familles <- sprintf("('%s')", familles)
  psql <- str_glue(psql)
  carp("psql: %s", psql)
  return(invisible(psql))
}
#
# source("geo/scripts/apivn.R"); extract_psql_shoc()
apivn_extract_psql_shoc <- function() {
  carp()
  psql <- "
  SELECT id_sighting, id_species, french_name, date, timing
  ,  id_place, place, precision, src_vn.observations.coord_lat, src_vn.observations.coord_lon
  , insert_date, update_date, src_vn.observations.observer_uid
  , src_vn.forms.id_form_universal, src_vn.forms.id as id_form, src_vn.forms.protocol_name, src_vn.forms.date_start, src_vn.forms.time_start, src_vn.forms.date_stop, src_vn.forms.time_stop
  , src_vn.forms.observer_uid, src_vn.forms.protocol
  FROM src_vn.observations
  LEFT JOIN src_vn.species ON
    src_vn.observations.id_species = src_vn.species.id
    AND src_vn.observations.taxonomy = src_vn.species.id_taxo_group
  LEFT JOIN src_vn.forms ON
    src_vn.observations.id_form_universal = src_vn.forms.id_form_universal
  WHERE src_vn.observations.taxonomy = 1
    AND %s
"
  return(invisible(psql))
}
#
# pour les oncb
apivn_extract_psql_tm <- function() {
  carp()
  psql <- "
  SELECT src_vn.observations.*
  , src_vn.forms.id as id_form, src_vn.forms.protocol_name, src_vn.forms.protocol
  , src_vn.forms.date_start, src_vn.forms.time_start, src_vn.forms.date_stop, src_vn.forms.time_stop
  , src_vn.forms.comments
  , src_vn.forms.coord_lat as form_lat, src_vn.forms.coord_lon as form_lon
  , src_vn.places.id_commune, src_vn.places.name, src_vn.places.coord_lat as place_lat, src_vn.places.coord_lon as place_lon
  , src_vn.local_admin_units.name as municipality, src_vn.local_admin_units.insee
--  , src_vn.species.french_name as espece
  , src_vn.families.latin_name as famille
  , mga.species_json.french_name as espece
  FROM src_vn.observations
  LEFT JOIN src_vn.species ON
    src_vn.observations.id_species = src_vn.species.id
    AND src_vn.observations.taxonomy = src_vn.species.id_taxo_group
  LEFT JOIN mga.species_json ON
    src_vn.observations.id_species = mga.species_json.id
    AND src_vn.observations.taxonomy = mga.species_json.id_taxo_group
  LEFT JOIN src_vn.families ON
    mga.species_json.id_taxo_group = src_vn.families.id_taxo_group
    AND mga.species_json.sempach_id_family = src_vn.families.id
  LEFT JOIN src_vn.forms ON
    src_vn.observations.id_form_universal = src_vn.forms.id_form_universal
  LEFT JOIN src_vn.places ON
    src_vn.observations.id_place = src_vn.places.id
  LEFT JOIN src_vn.local_admin_units ON
    src_vn.local_admin_units.id = src_vn.places.id_commune
  WHERE src_vn.observations.taxonomy = 1
    AND %s
"
  return(invisible(psql))
}
#
# pour les oncb
apivn_extract_psql_obs <- function() {
  carp()
  psql <- "
  SELECT src_vn.observations.*
  , src_vn.places.id_commune, src_vn.places.name
  , src_vn.local_admin_units.name as municipality, src_vn.local_admin_units.insee
  , src_vn.species.french_name as espece
  FROM src_vn.observations
  LEFT JOIN src_vn.species ON
    src_vn.observations.id_species = src_vn.species.id
    AND src_vn.observations.taxonomy = src_vn.species.id_taxo_group
  LEFT JOIN src_vn.places ON
    src_vn.observations.id_place = src_vn.places.id
  LEFT JOIN src_vn.local_admin_units ON
    src_vn.local_admin_units.id = src_vn.places.id_commune
  WHERE src_vn.observations.taxonomy = 1
    AND %s
"
  return(invisible(psql))
}
#
# pour les places
apivn_extract_psql_places <- function() {
  carp()
  psql <- "
  SELECT src_vn.places.*
  , src_vn.local_admin_units.name as municipality, src_vn.local_admin_units.insee
  FROM src_vn.places
  LEFT JOIN src_vn.local_admin_units ON
    src_vn.local_admin_units.id = src_vn.places.id_commune
  WHERE %s
"
  return(invisible(psql))
}
#
# pour les abc
apivn_extract_psql_abc_1 <- function() {
  carp()
  psql <- "
DROP TABLE IF EXISTS ign.observations;
"
  return(invisible(psql))
}
apivn_extract_psql_abc_2 <- function() {
  carp()
  psql <- "
CREATE TABLE ign.observations AS
SELECT point.*
  FROM src_vn.observations point, ign.admin_express_commune poly
  WHERE %s;
"
  return(invisible(psql))
}
apivn_extract_psql_abc_3 <- function() {
  carp()
  psql <- "
  SELECT ign.observations.*, french_name
  , src_vn.places.name
  , src_vn.local_admin_units.name as municipality, src_vn.local_admin_units.insee
  FROM ign.observations
  LEFT JOIN src_vn.species ON
    ign.observations.id_species = src_vn.species.id
    AND ign.observations.taxonomy = src_vn.species.id_taxo_group
  LEFT JOIN src_vn.places ON
    ign.observations.id_place = src_vn.places.id
  LEFT JOIN src_vn.local_admin_units ON
    src_vn.local_admin_units.id = src_vn.places.id_commune
;
"
  return(invisible(psql))
}
#
# source("geo/scripts/apivn.R"); apivn_extract_abc()
apivn_extract_abc <- function(insee = "35051", user = user1, force = FALSE) {
  carp()
  rds <- sprintf("apivn_extract_coj_%s", insee)
  res <- misc_lire(rds, force = force)
  if ( class(res) != "logical") {
    return(invisible(res))
  }
  psql_connect(user = user)
  psql <- apivn_extract_psql_abc_1()
  res <- db_SendQuery(db_con, psql) %>%
    glimpse()
  psql <- apivn_extract_psql_abc_2()
  inter <- sprintf("st_intersects(poly.wkb_geometry, point.geom) AND poly.insee_com like '%s'", insee)
  psql <- sprintf(psql, inter)
  res <- db_SendQuery(db_con, psql) %>%
    glimpse()
  psql <- apivn_extract_psql_abc_3()
  res <- db_SendQuery(db_con, psql) %>%
    glimpse()
  misc_ecrire(res, rds)
  rio::export(res, "apivn_extract_abc.xlsx")
  return(invisible(res))
}
#
# source("geo/scripts/apivn.R"); apivn_extract_coj()
apivn_extract_coj <- function(user = "apivn133", force = TRUE) {
  carp()
  res <- misc_lire("apivn_extract_coj", force = force)
  if ( class(res) != "logical") {
#    return(invisible(res))
  }
  psql_connect(user = user)
  psql <- apivn_extract_psql_tm()
  psql <- sprintf(psql, "src_vn.places.place_type = 'garden'")
  res <- db_SendQuery(db_con, psql) %>%
    glimpse()
  misc_ecrire(res, "apivn_extract_coj")
  return(invisible(res))
}
#
# source("geo/scripts/apivn.R"); apivn_extract_epoc()
apivn_extract_epoc <- function(psql, user = "transfer23") {
  carp()
  psql_connect(user = user)
  psql <- sprintf(psql, "src_vn.observations.project_code = 'EPOC'")
  res <- db_SendQuery(db_con, psql) %>%
    glimpse()
  misc_ecrire(res, "apivn_extract_epoc")
}
#
# source("geo/scripts/apivn.R"); apivn_extract_ObsRap_test(force = TRUE) %>% glimpse()
apivn_extract_ObsRap_test <- function(user = user1, force = FALSE) {
  carp()
  df <- misc_lire("apivn_extract_ObsRap_test")
  if (!is.logical(df) && force == FALSE) {
    return(invisible(df))
  }
  psql_connect(user = user)
  psql <- apivn_extract_psql_tm()
  psql <- sprintf(psql, "src_vn.observations.project_code = 'ObsRap_test'")
  res <- db_SendQuery(db_con, psql)
  misc_ecrire(res, "apivn_extract_ObsRap_test")
  return(invisible(res))
}
#
# source("geo/scripts/apivn.R"); apivn_extract_epoc_odf()
apivn_extract_epoc_odf <- function(user = user1, force = FALSE) {
  carp()
  df <- misc_lire("apivn_extract_epoc_odf")
  if (!is.logical(df) && force == FALSE) {
    return(invisible(df))
  }
  psql_connect(user = user)
  psql <- apivn_extract_psql_tm()
#  psql <- sprintf(psql, "src_vn.observations.project_code = 'EPOC-ODF' LIMIT 250")
  psql <- sprintf(psql, "src_vn.observations.project_code = 'EPOC-ODF'")
  res <- db_SendQuery(db_con, psql)
  misc_ecrire(res, "apivn_extract_epoc_odf")
}
#
# source("geo/scripts/apivn.R"); apivn_extract_epoc_odf_places() %>% glimpse()
apivn_extract_epoc_odf_places <- function(user = user1, force = FALSE) {
  carp()
  df <- misc_lire("apivn_extract_epoc_odf_places")
  if (!is.logical(df) && force == FALSE) {
    return(invisible(df))
  }
  psql_connect(user = user)
  psql <- apivn_extract_psql_places()
  psql <- sprintf(psql, "src_vn.places.name LIKE 'EPOC-ODF%'")
  res <- db_SendQuery(db_con, psql)
  misc_ecrire(res, "apivn_extract_epoc_odf_places")
  return(invisible(res))
}
#
# source("geo/scripts/apivn.R"); apivn_extract_espece()
apivn_extract_espece <- function(user = user1, espece) {
  carp()
  psql_connect(user = user)
  psql <- apivn_extract_psql_obs()
  espece <- sprintf("src_vn.observations.id_species = %s", espece)
  psql <- sprintf(psql, espece)
  res <- db_SendQuery(db_con, psql) %>%
    glimpse()
  misc_ecrire(res, "apivn_extract_espece")
}
#
# source("geo/scripts/apivn.R"); apivn_extract_especes()
apivn_extract_especes <- function(user = user1, especes = c("Goéland"), force = FALSE) {
  carp()
  dsn <- "apivn_extract_especes"
  df <- misc.lire(dsn)
  if (!is.logical(df) && force == FALSE) {
    return(invisible(df))
  }
  psql_connect(user = user)
  psql <- apivn_extract_psql_especes(especes)
  res <- db_SendQuery(db_con, psql) %>%
    glimpse()
  misc.ecrire(res, dsn)
}
#
# source("geo/scripts/apivn.R"); apivn_extract_familles()
apivn_extract_familles <- function(user = user1, familles) {
  carp()
  psql_connect(user = user)
  psql <- apivn_extract_psql_familles(familles)
  res <- db_SendQuery(db_con, psql) %>%
    glimpse()
  misc_ecrire(res, "apivn_extract_familles")
}
#
# source("geo/scripts/apivn.R"); apivn_extract_oncb()
apivn_extract_oncb <- function(psql, user = "apivn33") {
  carp()
  psql_connect(user = user)
  psql <- apivn_extract_psql()
  psql <- sprintf(psql, "src_vn.forms.protocol_name = 'TERRITORY_MAPPING'")
  res <- db_SendQuery(db_con, psql) %>%
    glimpse()
  misc_ecrire(res, "apivn_extract_oncb")
}
#
# source("geo/scripts/apivn.R"); apivn_extract_project() %>% glimpse()
apivn_extract_project <- function(project = "ABC Domloup", user = user1, force = FALSE) {
  carp()
  rds <- sprintf("apivn_extract_project_%s", project)
  res <- misc_lire(rds, force = force)
  if ( class(res) != "logical") {
    return(invisible(res))
  }
  psql_connect(user = user)
  psql <- apivn_extract_psql_tm()
#  psql <- sprintf(psql, "src_vn.observations.project_code = 'EPOC-ODF' LIMIT 250")
  project <- sprintf("src_vn.observations.project_code = '%s'", project)
  psql <- sprintf(psql, project)
  res <- db_SendQuery(db_con, psql)
  misc_ecrire(res, rds)
  return(invisible(res))
}
#
# pour les espèces avec leur famille
# source("geo/scripts/apivn.R"); df <- apivn_extract_species_families() %>% glimpse()
apivn_extract_species_families <- function(familles = "('Accipitridae')", user = user1, force = FALSE) {
  library(glue)
  carp()
  psql <- glue("
  SELECT mga.species_json.french_name as espece
  , mga.species_json.rarity as rarity
  , mga.species_json.is_used as is_used
  , src_vn.families.latin_name as famille
  FROM mga.species_json
  LEFT JOIN src_vn.families ON
    mga.species_json.sempach_id_family = src_vn.families.id
    AND mga.species_json.id_taxo_group = src_vn.families.id_taxo_group
  WHERE src_vn.families.latin_name IN {familles}
    AND mga.species_json.is_used = 1
")
  psql_connect(user = user)
  res <- db_SendQuery(db_con, psql) %>%
    glimpse()
  misc_ecrire(res, "apivn_extract_species_families")
  return(invisible(res))
}
#
# source("geo/scripts/apivn.R"); apivn_extract_stoc()
apivn_extract_stoc <- function(user = "apivn33", force = TRUE) {
  carp()
  res <- misc_lire("apivn_extract_stoc", force = force)
  if ( class(res) != "logical") {
    return(invisible(res))
  }
  psql_connect(user = user)
  psql <- apivn_extract_psql_tm()
  psql <- sprintf(psql, "src_vn.forms.protocol_name = 'STOC_EPS'")
  res <- db_SendQuery(db_con, psql) %>%
    glimpse()
  misc_ecrire(res, "apivn_extract_stoc")
  return(invisible(res))
}
#
# source("geo/scripts/wetlands.R"); apivn_extract_wetlands()
apivn_extract_wetlands <- function() {
  carp()
  psql_connect(user = user1)
  psql <- apivn_extract_psql_tm()
  psql <- sprintf(psql, "src_vn.forms.protocol_name = 'WATERBIRD'")

  res <- db_SendQuery(db_con, psql) %>%
    glimpse()
  misc_ecrire(res, "apivn_extract_wetlands")
  return(invisible(res))
}
#
# source("geo/scripts/apivn.R"); apivn_extract_lire()
apivn_extract_lire_ <- function(source = "apivn_extract_epoc_odf") {
#  carp()
  library(lubridate)
  df <- misc_lire(source) %>%
    mutate(aaaa = strftime(date, format="%Y-%m")) %>%
    mutate(aaaamm = strftime(date, format="%Y-%m")) %>%
    mutate(mm = strftime(date, format="%m")) %>%
    mutate(heure_debut = hms(time_start)) %>%
    mutate(heure_fin = hms(time_stop)) %>%
    mutate(duree = as.numeric(as.duration(heure_fin - heure_debut)))
  return(invisible(df))
}
#
# source("geo/scripts/epoc.R"); apivn_extract_table_forms_json_pretty()
apivn_extract_table_forms_json_pretty <- function(user = user1, id = 104115 ) {
  library(jsonlite)
  library(sf)
  carp()
  psql_connect(user = user)
  psql <- sprintf("
  SELECT *
  FROM import.forms_json
  WHERE id = %s
", id)
  df <- db_SendQuery(db_con, psql) %>%
    glimpse()
  txt <- df[1, "item"]
  print(prettify(txt, indent = 2))
  return(invisible(df))
}
# source("geo/scripts/epoc.R"); apivn_extract_table(table = "observations")
apivn_extract_table <- function(user = user1, table = "observations", schema = "src_vn") {
  psql_connect(user = user)
  psql <-"
  SELECT *
  FROM {schema}.{table}
"
  psql <- stringr::str_glue(psql)
  df <- db_SendQuery(db_con, psql)
  carp("schema: %s table: %s nrow: %s", schema, table, nrow(df))
  return(invisible(df))
}
#
# source("geo/scripts/epoc.R"); apivn_extract_table_forms_json(id = 122563)
apivn_extract_table_forms_json <- function(user = user1, id = 5509077) {
  library(jsonlite)
  library(sf)
  carp()
  psql_connect(user = user)
  psql <- sprintf("
  SELECT *
  FROM import.forms_json
  WHERE id = %s
;
", id)
  df <- db_SendQuery(db_con, psql) %>%
    glimpse()
  carp("pretty du premier")
  txt <- df[1, "item"]
  print(prettify(txt, indent = 2))
  return(invisible(df))
}
#
# "source": "WEB"
# source("geo/scripts/epoc.R"); apivn_extract_table_observations_json(id = 5507825)
# source("geo/scripts/epoc.R"); apivn_extract_table_observations_json(id = 6272812)
# hirondelle avec colonie
# source("geo/scripts/epoc.R"); apivn_extract_table_observations_json(id = 7615285)
apivn_extract_table_observations_json <- function(user = user1, id = 7615285) {
  library(jsonlite)
  library(sf)
  carp()
  psql_connect(user = user)
  psql <- sprintf("
  SELECT *
    , item::jsonb->>'observers' as observers
  FROM import.observations_json
  WHERE id = %s
;
", id)
  df <- db_SendQuery(db_con, psql) %>%
    glimpse()
  carp("pretty du premier")
  txt <- df[1, "item"]
  print(prettify(txt, indent = 2))
  return(invisible(df))
}
#
# source("geo/scripts/epoc.R"); apivn_extract_table_observations_json_epoc_odf()
apivn_extract_table_observations_json_epoc_odf <- function(user = user1) {
  library(jsonlite)
  library(sf)
  carp()
  psql_connect(user = user)
  psql <- sprintf("
  SELECT *
  FROM import.observations_json
  WHERE item::jsonb->'observers' @> '[{\"project_code\":\"EPOC-ODF\"}]'
;
")
  df <- db_SendQuery(db_con, psql) %>%
    glimpse()
  txt <- df[1, "item"]
  print(prettify(txt, indent = 2))
  misc_ecrire(df, "apivn_extract_table_observations_json_epoc_odf")
  return(invisible(df))
}
#
# source("geo/scripts/epoc.R"); df <- apivn_extract_table_observations_json_form()
apivn_extract_table_observations_json_form <- function(user = user1, id = 122563) {
  library(jsonlite)
  library(sf)
  carp()
  psql_connect(user = user)
  psql <- sprintf("
  SELECT *
  FROM import.observations_json
  WHERE item::jsonb->'observers' @> '[{\"id_form\":\"%s\"}]'
;
", id)
  df <- db_SendQuery(db_con, psql) %>%
    glimpse()
  txt <- df[1, "item"]
  print(prettify(txt, indent = 2))
  misc_ecrire(df, "apivn_extract_table_observations_json_form")
  return(invisible(df))
}
#
# source("geo/scripts/epoc.R"); apivn_extract_table_observations_json_place()
apivn_extract_table_observations_json_place <- function(user = user1) {
  library(jsonlite)
  library(sf)
  carp()
  psql_connect(user = user)
  psql <- sprintf("
  SELECT *
  FROM import.observations_json
  WHERE item::jsonb->'place'->>'name' ='EPOC-ODF_83010_Officiel'
;
")
  df <- db_SendQuery(db_con, psql) %>%
    glimpse()
  txt <- df[1, "item"]
  print(prettify(txt, indent = 2))
  misc_ecrire(df, "apivn_extract_table_observations_json_epoc_odf")
  return(invisible(df))
}
#
# source("geo/scripts/geoca.R"); apivn_extract_table_places_json() %>% glimpse()
apivn_extract_table_places_json <- function() {
  library(jsonlite)
  library(sf)
  carp()
  psql_connect(user = user1)
  psql <- "
  SELECT (item::jsonb->>'id')::INTEGER as id
    , item::jsonb->>'coord_lat' as coord_lat
    , item::jsonb->>'coord_lon' as coord_lon
    , item::jsonb->>'name' as name
    , item::jsonb->>'created_by' as created_by
    , item::jsonb->'created_date'->>'@ISO8601' as created_date
    , item::jsonb->'last_updated_date'->>'@ISO8601' as updated_date
    , item::jsonb->>'last_updated_by' as last_updated_by
    , src_vn.places.id_commune
    , src_vn.local_admin_units.insee, src_vn.local_admin_units.name as commune
  FROM import.places_json
  LEFT JOIN src_vn.places ON
    (item::jsonb->>'id')::INTEGER = src_vn.places.id
  LEFT JOIN src_vn.local_admin_units ON
    src_vn.places.id_commune = src_vn.local_admin_units.id
;
"
  df <- db_SendQuery(db_con, psql)
  return(invisible(df))
}
#
# source("geo/scripts/epoc.R"); apivn_extract_table_places_json_epoc()
apivn_extract_table_places_json_epoc <- function(user = user1) {
  library(jsonlite)
  library(sf)
  carp()
  psql_connect(user = user)
  psql <- "
  SELECT (item::jsonb->>'id')::INTEGER as id
    , item::jsonb->>'coord_lat' as coord_lat
    , item::jsonb->>'coord_lon' as coord_lon
    , item::jsonb->>'name' as name
    , item::jsonb->>'created_by' as created_by
    , item::jsonb->'created_date'->>'@ISO8601' as created_date
    , item::jsonb->'last_updated_date'->>'@ISO8601' as updated_date
    , item::jsonb->>'last_updated_by' as last_updated_by
    , src_vn.places.id_commune
    , src_vn.local_admin_units.insee, src_vn.local_admin_units.name as commune
  FROM import.places_json
  LEFT JOIN src_vn.places ON
    (item::jsonb->>'id')::INTEGER = src_vn.places.id
  LEFT JOIN src_vn.local_admin_units ON
    src_vn.places.id_commune = src_vn.local_admin_units.id
;
"
  df <- db_SendQuery(db_con, psql) %>%
    filter(grepl("^EPOC", name)) %>%
    glimpse()
  return(invisible(df))
}
#
# source("geo/scripts/apivn.R");apivn_extract_table_places_json_pretty()
apivn_extract_table_places_json_pretty <- function(id = "305019", user = "apivn45") {
  library(jsonlite)
  psql_connect(user)
  psql <- "SELECT jsonb_pretty(item)
  FROM import.places_json
  WHERE id = %s
;
"
  psql <- sprintf(psql, id)
  res <- db_SendQuery(db_con, psql) %>%
    glimpse()
  print(prettify(res$jsonb_pretty))
}
#
# pour convertir un json en requête sql
# source("geo/scripts/apivn.R");apivn_item2table()
apivn_item2table <- function(table = "import.species_json", user = user1) {
  library(jsonlite)
  library(glue)
  Table <- gsub(".*\\.", "", table)
#  confess("Table: %s", Table)
  psql_connect(user)
  psql <- glue(r"[
DROP TABLE IF EXISTS mga.{Table};
]")
  res <- db_SendQuery(db_con, psql) %>%
    glimpse()
  psql <- glue(r"[
SELECT item
FROM {table}
LIMIT 1
;
]")
  res <- db_SendQuery(db_con, psql) %>%
    glimpse()
#  print(prettify(res$item))
  liste <- jsonlite::fromJSON(res$item, simplifyVector = FALSE)
  select <- glue(r"[
CREATE TABLE mga.{Table} AS
SELECT
]")
  for (var in names(liste)) {
    carp("var: %s val: %s", var, liste[[var]])
    type  <- "::text"
    if (! is.na(as.numeric(liste[[var]]))) {
      type <- "::integer"
    }
    select <- glue(r"[{select}
  (item->>'{var}'){type} {var},
]")
  }
  select <- gsub(",$", "", select)
  select <- glue(r"[{select}
FROM {table}
;
]")
  writeLines(select)
  res <- db_SendQuery(db_con, select) %>%
    glimpse()
 psql <- glue(r"[
SELECT * FROM mga.{Table};
]")
  res <- db_SendQuery(db_con, psql) %>%
    glimpse()
}
# extraction des observations d'un formulaire
# source("geo/scripts/apivn.R"); apivn_lire_observations_form()
apivn_lire_observations_form <- function(user = user2, id = "65_2461372") {
  carp()
  psql_connect(user = user)
  psql <- sprintf("
SELECT src_vn.forms.*
  FROM src_vn.forms
  WHERE src_vn.forms.id_form_universal = '%s'
", id)
  res <- db_SendQuery(db_con, psql)
  misc_print(res)
  observer_uid <- res[1, "observer_uid"]

  psql <- sprintf("
SELECT src_vn.observations.*
  FROM src_vn.observations
  WHERE src_vn.observations.id_form_universal = '%s'
", id)
  res <- db_SendQuery(db_con, psql)
  misc_print(res)
    psql <- sprintf("
SELECT src_vn.observations.*
  FROM src_vn.observations
  WHERE src_vn.observations.observer_uid = '%s'
", observer_uid)
  res <- db_SendQuery(db_con, psql)
#  misc_print(res)
}
#
## conversion d'un champ en json
# https://rstudio-pubs-static.s3.amazonaws.com/31702_9c22e3d1a0c44968a4a1f9656f1800ab.html
#
# source("geo/scripts/apivn.R"); apivn_recode_details_test()
apivn_recode_details_test <- function() {
  carp()
  df <- apivn_extract_epoc_odf(force = FALSE)
  apivn_recode_details(df, mode = "separate") %>%
    glimpse()
}
apivn_recode_details <- function(df, mode = "separate") {
  library(jsonlite)
  carp()
  df1 <- df %>%
    filter(! is.na(details)) %>%
    dplyr::select(id_sighting, details) %>%
    glimpse()
  if (nrow(df1) == 0) {
#    df[, "Details"] <- NA
    return(invisible(df))
  }
  df1$Details <- NA
  if (is(df1, "sf")) {
    df1 <- st_drop_geometry(df1)
  }
  details.df <- tibble()
  carp("nrow: %s", nrow(df1))
  for (i1 in 1:nrow(df1)) {
    details <- df1[[i1, "details"]]
#    carp("i1: %s details: %s", i1, df1[i1, "details"])
# list of lists
    lol <- jsonlite::fromJSON(details, simplifyVector = FALSE)
#    glimpse(lol)
    df4 <- tibble()
    for (l in lol) {
      df2 <- l %>% as_tibble()
      df2$id_sighting <- df1[[i1, "id_sighting"]]
      tryCatch({
        if("distance" %in% colnames(df2)) {
          df3 <- df2 %>%
            mutate(Detail = sprintf("[%s%s,%s]", count, sex, distance))
        } else {
          df3 <- df2 %>%
            mutate(Detail = sprintf("%s%s", count, sex))
        }
        },
        error = function(cond) {
          print("error")
          glimpse(df1[i1, ])
          glimpse(lol)
          glimpse(l)
          glimpse(df2)
        }
      )
      df4 <- bind_rows(df4, df3)
      details.df <- bind_rows(details.df, df2)
    }
    Details <- paste(df4$Detail, collapse = ";")
    df1[[i1, "Details"]] <- Details
#    carp("i1: %s Details: %s", i1, df1[[i1, "Details"]])
#    df1[i1, "json"][[1]] <- json
  }
  if ( 1 == 2) {
    glimpse(details.df)
    df2 <- details.df %>%
      group_by(age, sex, distance, condition) %>%
      summarize(nb = n())
    misc_print(df2)
  }
  if ( mode == "separate") {
    details.df <- details.df %>%
      mutate(distance = dplyr::recode(distance,
       "LESS25" = "<025m",
       "LESS100" = "<100m",
       "LESS200" = "<200m",
       "MORE200" = ">200m"
    ))
    df <- df %>%
      left_join(details.df, by = c("id_sighting")) %>%
      mutate(count.y = as.numeric(count.y))
  } else {
    df <- df %>%
      left_join(df1, by = c("id_sighting", "details"))
  }
  return(invisible(df))
}

#
#  rm(list=ls()); source("geo/scripts/apivn.R"); apivn_recode_estimation_code_test()
apivn_recode_estimation_code_test <- function() {
  carp()
  df <- apivn_extract_table(table = "observations")
  apivn_recode_estimation_code(df)
}
apivn_recode_estimation_code <- function(df) {
  carp()
  tr <- tribble(
    ~estimation_code, ~Estimation_Code,
    "ESTIMATION", "~",
    "EXACT_VALUE", "",
    "MINIMUM", ">=",
    "NO_VALUE", "?"
  )
  df <- df %>%
    left_join(tr, by = c("estimation_code"))
  df1 <- df %>%
    filter(is.na(Estimation_Code))
  if (nrow(df1) > 0) {
    glimpse(df1)
    confess("code non supporté")
  }
}
#
## les données en localisation précise
# calcul de la distance
# rm(list=ls()); source("geo/scripts/epoc.R"); df <- apivn_recode_precise_test() %>% glimpse()
apivn_recode_precise_test <- function(annee = "2022", force = FALSE) {
  carp()
  df <- apivn_extract_lire(annee, force = force)
  df <- apivn_recode_precise(df)
  return(invisible(df))
}
apivn_recode_precise <- function(df) {
  library(sf)
  carp()
  df1 <- df %>%
    filter(precision == "precise") %>%
    dplyr::select(id_sighting, coord_lat, coord_lon, form_lat, form_lon)
  nc1 <- st_as_sf(df1, coords = c("coord_lon", "coord_lat"), crs = 4326) %>%
    st_transform(2154)
  nc2 <- st_as_sf(df1, coords = c("form_lon", "form_lat"), crs = 4326) %>%
    st_transform(2154)
  df2 <- left_join(
    nc1 %>% as.data.frame(),
    nc2 %>% as.data.frame(),
    by = c("id_sighting")
  )
  df2$distance <- as.integer(st_distance(df2$geometry.x, df2$geometry.y, by_element = TRUE))
  df2 <- df2 %>%
    mutate(Distance = cut(distance,
      breaks = c(-1, 25, 100, 200, 10000),
      labels = c("<025m", "<100m", "<200m", ">200m")
    )) %>%
    mutate(Distance = as.character(Distance)) %>%
    dplyr::select(id_sighting, distance, Distance) %>%
    glimpse()
  df <- df %>%
    left_join(df2, by = c("id_sighting"))
  return(invisible(df))
}
#
# source("geo/scripts/apivn.R"); apivn_stat_forms_protocol()
apivn_stat_forms_protocol <- function(user = user1) {
  carp()
  psql_connect(user = user)
  psql <- "
SELECT src_vn.forms.protocol_name, COUNT(*)
  FROM src_vn.forms
  GROUP BY src_vn.forms.protocol_name
"
  res <- db_SendQuery(db_con, psql)
  misc_print(res)
  misc_ecrire(res, "apivn_extract_forms_protocol")
}
#
# source("geo/scripts/apivn.R"); apivn_stat_observations_project()
apivn_stat_observations_project <- function(user = user1) {
  carp()
  psql_connect(user = user)
  psql <- "
SELECT src_vn.observations.project_code, COUNT(*)
  FROM src_vn.observations
  GROUP BY src_vn.observations.project_code
"
  res <- db_SendQuery(db_con, psql)
  misc_print(res)
  misc_ecrire(res, "apivn_extract_observations_project")
}
#
# source("geo/scripts/apivn.R"); apivn_stat_observations_champ(champ = "estimation_code")
apivn_stat_observations_champ <- function(user = user1, champ = "estimation_code") {
  carp()
  psql_connect(user = user)
  psql <- "
SELECT src_vn.observations.{champ}, COUNT(*)
  FROM src_vn.observations
  GROUP BY src_vn.observations.{champ}
"
  psql <- stringr::str_glue(psql)
  res <- db_SendQuery(db_con, psql)
  misc_print(res)
}
#
# source("geo/scripts/apivn.R"); apivn_mga_schema_create()
apivn_mga_schema_create <- function(user = user1) {
  carp()
  psql_connect(user = user)
  psql <- "
CREATE SCHEMA IF NOT EXISTS mga;
SELECT id, item
FROM import.species_json
"

  res <- db_SendQuery(db_con, psql)
  misc_print(res)
}
