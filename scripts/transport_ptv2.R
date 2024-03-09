# <!-- coding: utf-8 -->
#
# les réseaux de bus
#
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
# migration en PTv2
#
# source("geo/scripts/transport.R");ptv2_jour()
ptv2_jour <- function(force = FALSE) {
  library(tidyverse)
  library(data.table)
  library(janitor)
  library(rio)
  library(tidyverse)
}
#
# vérification pour les arrêts des relations route=bus
# source("geo/scripts/transport.R");ptv2_platforms_route_bus()
ptv2_platforms_route_bus <- function(fic = "ptv2_platforms_route_bus", force = FALSE, force_osm = TRUE) {
  library(tidyverse)
  library(data.table)
  library(janitor)
  library(rio)
  library(tidyverse)
  od <- overpass_get(query = "ptv2_platforms_route_bus", format = "od", force = force_osm) %>%
    glimpse()
  transport_ecrire(od, fic)
}

#
# source("geo/scripts/transport.R")ptv2_platforms_route_bus_maj()
ptv2_platforms_route_bus_maj <- function(fic = "ptv2_platforms_route_bus", OsmChange = FALSE) {
  library(sf)
  library(janitor)
  OsmChange <<- OsmChange
  od <- transport_lire(fic)
  df1 <- od$osm_points %>%
    st_drop_geometry() %>%
    glimpse()
  osm <- ""
  tags.df <- tribble(
    ~name, ~value,
    "public_transport", "platform"
  )
  for (i1 in 2:nrow(df1)) {
    carp("i1: %s/%s %s", i1, nrow(df1), df1[[i1, "osm_id"]])
    o <- osmchange_object_modify_tags(id = df1[[i1, "osm_id"]] , type = "node", tags = tags.df, Change = FALSE)
    osm <- c(osm, o)
#    break
  }
  osm <- paste(osm, "\n", collapse = "")
#  writeLines(osm);  stop("****")
  changeset_id <- osmapi_put("modify", text = osm, comment = "migration en PTv2")
  carp("changeset_id: %s", changeset_id)
}
#
# vérification pour les relations route=bus
# source("geo/scripts/transport.R");ptv2_relations_route_bus()
ptv2_relations_route_bus <- function(fic = "ptv2_relations_route_bus", force = FALSE, force_osm = TRUE) {
  library(tidyverse)
  library(data.table)
  library(janitor)
  library(rio)
  library(tidyverse)
  od <- overpass_get(query = "ptv2_relations_route_bus", format = "csv", force = force_osm) %>%
    glimpse()
  transport_ecrire(od, fic)
}

#
# source("geo/scripts/transport.R")ptv2_relations_route_bus_maj()
ptv2_relations_route_bus_maj <- function(fic = "ptv2_relations_route_bus", OsmChange = FALSE) {
  library(sf)
  library(janitor)
  OsmChange <<- OsmChange
  df1 <- transport_lire(fic) %>%
    glimpse()
  for (i1 in 1:nrow(df1)) {
    carp("i1: %s/%s %s", i1, nrow(df1), df1[[i1, "@id"]])
    ptv2_relation_route_bus_maj(df1[[i1, "@id"]])
  }
}
ptv2_relation_route_bus_maj <- function(ref, force = TRUE) {
  library(sf)
  library(janitor)
  carp("ref: %s", ref)
  doc <- ptv2_relation_route_bus_full(ref, force = TRUE)
# les données osm
  txt <- osmapi_object_txt(ref = ref, type = "relation")
  osm <- txt
  osm <- stringr::str_replace(osm, regex(".*<(node|way|relation)", dotall = TRUE), "<\\1");
  osm_avant <- stringr::str_replace(osm, regex("</(node|way|relation)>.*", dotall = TRUE), "</\\1>")
  lignes <- unlist(str_split(txt, "\n"))
  relation1 <- grep(" <relation id=", lignes, value = TRUE)
  tags <- grep("  <tag ", lignes, value = TRUE)
  members <- grep("  <member ", lignes, value = TRUE)

  df1 <- tibble(osm = members) %>%
    separate_wider_regex(osm, patterns = c(
      "  <member type=\"",
      type = ".*",
      "\" ref=\"",
      ref = ".*",
      "\" role=\"",
      role = ".*",
      "\"/>")
    ) %>%
    mutate(role = gsub("(backward|forward)", "", role)) %>%
    mutate(xpath = sprintf(".//%s[@id='%s']", type, ref)) %>%
    glimpse()
  platforms <- c("platform", "platform_entry_only", "platform_exit_only")
  for (i1 in 1:nrow(df1)) {
#    carp("i1: %s/%s %s %s %s", i1, nrow(df1), df1[[i1, "type"]], df1[[i1, "ref"]], df1[[i1, "role"]])
    object <<- xml_find_all(doc, df1[[i1, "xpath"]])
# pas de tag => pas de role
    if (length(object) == 0) {
      df1[[i1, "role"]] <- ""
      next
    }
    tag <<- xml_find_all(object, './tag[@k="public_transport"]')
# pas de tag public_transport => pas de role
    if (length(tag) == 0) {
      df1[[i1, "role"]] <- ""
      next
    }
    val <<- xml_attr(tag, "v")
# tag public_transport => platform
    if (val == "platform") {
      if (df1[[i1, "role"]] %notin% platforms) {
        df1[[i1, "role"]] <- "platform"
      }
      next
    }
#    break
  }
  df1 <- df1 %>%
    mutate(osm = sprintf('  <member type="%s" ref="%s" role="%s"/>', type, ref, role))
  arrets.df <- df1 %>%
    filter(role != "")
  ways.df <- df1 %>%
    filter(role == "")
  if (length(grep('public_transport:version', tags)) == 0) {
    tags <- c(tags, '  <tag k="public_transport:version" v="2"/>')
  }
  osm <- paste(c(relation1, arrets.df$osm, ways.df$osm, tags, " </relation>"), "\n", collapse = "")
  if (osm != osm_avant) {
    writeLines(osm);
    changeset_id <- osmapi_put("modify", text = osm, comment = "mise en conformité PTv2")
    carp("osm: %s---", changeset_id)
#    stop("llllllllllllllllllll")
  }
#  misc_print(df1)
#  stop("ùùùùùùùùùùùùùùùùù")
}
ptv2_relation_route_bus_full <- function(ref, force = FALSE, force_osm = TRUE) {
  dsn_rds <- sprintf("%s/transport_ptv2_%s.rds", osmDir, ref)
  if (file.exists(dsn_rds) & force == FALSE) {
    doc <- readRDS(dsn_rds)
  } else {
    dsn <- osmapi_get_object_full(ref = ref, type = "relation", force = force_osm)
    doc <- read_xml(dsn)
    saveRDS(doc, dsn_rds)
  }
  return(invisible(doc))
}