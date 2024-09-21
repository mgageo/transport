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
  od <- overpass_get(query = "relations_route_bus_area", format = "csv", force = force_osm) %>%
    glimpse()
  transport_ecrire(od, fic)
}

#
# source("geo/scripts/transport.R");ptv2_relations_route_bus_maj()
ptv2_relations_route_bus_maj <- function(fic = "ptv2_relations_route_bus", OsmChange = FALSE) {
  library(sf)
  library(janitor)
  library(xml2)

  OsmChange <<- OsmChange
  df1 <- transport_lire(fic) %>%
    glimpse()
  for (i1 in 1:nrow(df1)) {
    carp("i1: %s/%s %s", i1, nrow(df1), df1[[i1, "@id"]])
    ptv2_relation_route_bus_maj(df1[[i1, "@id"]])
#    break
  }
}
ptv2_relation_route_bus_maj <- function(ref, force = TRUE) {
  library(sf)
  library(janitor)
  carp("ref: %s", ref)
  doc <- ptv2_relation_route_bus_full(ref, force = TRUE)
# les données osm
  txt <- osmapi_object_txt(ref = ref, type = "relation", force = TRUE)
  osm <- txt
  osm <- stringr::str_replace(osm, regex(".*<(node|way|relation)", dotall = TRUE), "<\\1");
  osm_avant <- stringr::str_replace(osm, regex("</(node|way|relation)>.*", dotall = TRUE), "</\\1>")
  lignes <- unlist(str_split(txt, "\n"))
#  writeLines(lignes)
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
      Carp("***absent: %s", df1[[i1, "xpath"]])
    }
    tag <<- xml_find_all(object, './tag[@k="public_transport"]')
# pas de tag public_transport => pas de role
    if (length(tag) == 0) {
#      Carp("***pas de tag public_transport: %s", df1[[i1, "xpath"]])
      df1[[i1, "role"]] <- ""
      if (grepl("node", df1[[i1, "xpath"]])) {
        stop("*******", ref, df1[[i1, "xpath"]])
      }
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
    if (val == "stop_position") {
      if (df1[[i1, "role"]] %notin% platforms) {
        df1[[i1, "role"]] <- "stop"
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
    lignes_avant <- unlist(str_split(osm_avant, "\n"))
    lignes_apres <- unlist(str_split(osm, "\n"))
    max_ln <- max(length(lignes_avant), length(lignes_apres))
    lignes_avant <- c(lignes_avant, rep(NA, max_ln - length(lignes_avant)))
    lignes_apres <- c(lignes_apres, rep(NA, max_ln - length(lignes_apres)))
    df0 <- tibble(avant = str_trim(lignes_avant), apres = str_trim(lignes_apres)) %>%
      mutate(diff = ifelse(avant == apres, "===", "****")) %>%
      filter(diff != "===") %>%
      mutate(diff = sprintf("%-60s %-60s %s", substring(avant, 1, 60), substring(apres, 1, 60), diff)) %>%
      glimpse()
    writeLines(df0$diff)
    if (nrow(df0) != 0) {
#      writeLines(osm);
      changeset_id <- osmapi_put("modify", text = osm, comment = "mise en conformité PTv2")
      carp("osm: %s---", changeset_id)
    }
#https://www.openstreetmap.org/changeset/156800725    stop("llllllllllllllllllll")
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
#
## le nettoyage des arrêts
#
# https://nlehuby.5apps.com/bien-cartographier-les-bus.html
#
#
# source("geo/scripts/transport.R");config_xls('nomad61');ptv2_nodes_osmose_21411()
ptv2_nodes_osmose_21411 <- function(force_osm = FALSE, OsmChange = FALSE) {
  library(tidyverse)
  dsn_rds <- sprintf("%s/osmose_country_issues_%s.rds", varDir, Config[1, "reseau"])
  df1 <- readRDS(dsn_rds) %>%
    filter(item == "2140") %>%
    filter(class == "21411") %>%
    glimpse()
  osm <- ""
  for (i1 in 1:nrow(df1)) {
    carp("i1: %s", i1)
    tags.df <- tribble(
      ~name, ~value,
      "highway", "bus_stop",
      "public_transport", "platform",
      "-bus", "****"
    )
    o <- osmchange_object_modify_tags(id = df1[[i1, "elems.1.id"]] , type = df1[[i1, "elems.1.type"]], tags = tags.df, Change = FALSE)
    osm <- c(osm, o)
#    break
  }
}
#
#
# pour transformer en platform
# source("geo/scripts/transport.R");config_xls('nomad61');ptv2_nodes_2_platform(query = "ptv2_stop_sans_public")
# source("geo/scripts/transport.R");config_xls('nomad61');ptv2_nodes_2_platform(query = "ptv2_platform_bus_yes")
# source("geo/scripts/transport.R");config_xls('nomad61');ptv2_nodes_2_platform(query = "ptv2_stop2platform")
# source("geo/scripts/transport.R");config_xls('nomad61');ptv2_nodes_2_platform(query = "ptv2_node_pas_way")
ptv2_nodes_2_platform <- function(query = "ptv2_stop_sans_public", force_osm = TRUE, OsmChange = FALSE) {
  library(tidyverse)
  OsmChange <<- OsmChange
  od <- overpass_get(query = query, format = "csv", force = force_osm) %>%
    glimpse()
  if(nrow(od) == 0) {
    carp("c'est tout bon")
    return(invisible())
  }
  osm <- ""
  for (i in 1:nrow(od)) {
    carp("i: %s/%s", i, nrow(od))
    tags.df <- tribble(
      ~name, ~value,
      "highway", "bus_stop",
      "public_transport", "platform",
      "-bus", "yes"
    )
    o <- osmchange_object_modify_tags(id = od[[i, "@id"]] , type = od[[i, "@type"]], tags = tags.df, Change = FALSE)
    osm <- c(osm, o)
#    break
  }
  osm <- paste(osm, "\n", collapse = "")
  writeLines(osm);
  changeset_id <- osmapi_put("modify", text = osm, "signalements osmose pour schema PTv2") %>%
    glimpse()
}
#
# pour transformer en stop_position
# source("geo/scripts/transport.R");config_xls('nomad61');ptv2_nodes_2_stop_position()
# source("geo/scripts/transport.R");config_xls('nomad61');ptv2_nodes_2_stop_position(query = "ptv2_bus_stop_sur_way")
# source("geo/scripts/transport.R");config_xls('nomad61');ptv2_nodes_2_stop_position(query = "ptv2_node_sur_way")
ptv2_nodes_2_stop_position <- function(query = "ptv2_platform_sur_way", force_osm = TRUE, OsmChange = FALSE) {
  library(tidyverse)
  OsmChange <<- OsmChange
  od <- overpass_get(query = query, format = "csv", force = force_osm) %>%
    glimpse()
  if(nrow(od) == 0) {
    carp("c'est tout bon")
    return(invisible())
  }
  osm <- ""
  nb <- 0
  for (i in 1:nrow(od)) {
    carp("i: %s/%s", i, nrow(od))
    tags.df <- tribble(
      ~name, ~value,
      "-highway", "bus_stop",
      "public_transport", "stop_position",
      "bus", "yes"
    )
    o <- osmchange_object_modify_tags(id = od[[i, "@id"]] , type = od[[i, "@type"]], tags = tags.df, Change = FALSE)
    if (o != "") {
      osm <- c(osm, o)
      nb <- nb + 1
#      break
    }
  }
  carp("nb: %s", nb)
  osm <- paste(osm, "\n", collapse = "")
#  writeLines(osm);
  changeset_id <- osmapi_put("modify", text = osm, "signalements osmose pour schema PTv2") %>%
    glimpse()
}