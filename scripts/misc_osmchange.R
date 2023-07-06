# <!-- coding: utf-8 -->
#
# utilisation des données d'OpenStreetMap
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
# auteur : Marc Gauthier
#
# https://wiki.openstreetmap.org/wiki/API_v0.6
#
# https://gist.github.com/typebrook/d166d5e8d0a293c30f697b0f403b3c0e
#
# source("geo/scripts/transport.R"); osmchange_put()
osmchange_put <- function(osmchange = "create", text = "osm", comment = "test", force = FALSE) {
  library(tidyverse)
  xml <- sprintf('<osm>
  <changeset>
    <tag k="created_by" v="R 0.6"/>
    <tag k="comment" v="%s"/>
  </changeset>
</osm>', comment)
  changeset_id <- osmapi_api("changeset/create", xml, methode = "PUT")
  carp("changeset_id: %s", changeset_id)
  text <- osmchange_osmchange(changeset_id, change = osmchange, osm = text)
#  writeLines(text);stop("******")
  path <- sprintf("changeset/%s/upload", changeset_id)
  res <- osmapi_api(path, xml = text, methode = "POST")
  path <- sprintf("changeset/%s/close", changeset_id)
  res <- osmapi_api(path, methode = "PUT") %>%
    glimpse()
  changeset_url <- sprintf("%s/changeset/%s", api_url, changeset_id)
  return(invisible(changeset_url))
}
#
# pour modifier le changeset_id
# source("geo/scripts/transport.R"); osmchange_osmchange()
osmchange_osmchange <- function(changeset_id = 200, change = "create", osm = "<node>") {
  timestamp <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S.0+02:00")
  osm <- str_replace(osm, 'changeset="\\d+"', 'changeset="{changeset_id}"')
  text <- '<osmChange version="0.6" generator="R mga_geo">
<{change}>
{osm}
</{change}>
</osmChange>'
  text <- str_glue(text)
  text <- str_glue(text)
  return(text)
}
osmchange_object_modify_tags <- function(id = "13400195", type = "relation", tags = list()) {
  library(stringi)
#  library(textutils)
  osm <- osmapi_get_object_xml(id = id, type = type)
  if (! is_tibble(tags)) {
    tags.df <- tags %>%
      enframe() %>%
      arrange(name)
  } else {
    tags.df <- tags
  }
  tags.df$value <- html_replace(tags.df$value)
#  misc_print(tags.df); stop("*****")
  nb_modifs <- 0
  for (i in 1:nrow(tags.df)) {
    gtfs <- sprintf('  <tag k="%s" v="%s"/>', tags.df[[i, "name"]], tags.df[[i, "value"]])
#    print(gtfs);
    re <- regex(sprintf('^(.*)(  <tag k="%s" v="[^"]*"/>)(.*$)', tags.df[[i, "name"]]), , dotall = TRUE)
    matches <- str_match(osm, re)
# le tag est présent ?
    if (length(matches) != 3) {
# non, on l'insère à la fin
      re <- regex('^(.*)(</(node|way|relation)>.*$)', , dotall = TRUE)
      matches <- str_match(osm, re)[1, ] %>%
        glimpse()
      carp("id: %s\n  osm :\n  gtfs: %s",id, gtfs)
      osm <- sprintf("%s%s\n%s", matches[[2]], gtfs, matches[[3]])
      nb_modifs <- nb_modifs + 1
    } else {
# https://stackoverflow.com/questions/70486931/how-to-decode-strings-in-a-data-frame-using-r/70487386#70487386
# pas de modifs ?
      if (gtfs == matches[[3]]) {
        next
      }
      carp("id: %s\n  osm : %s\n  gtfs: %s",id, matches[[3]], gtfs)
      osm <- sprintf("%s%s%s", matches[[2]], gtfs, matches[[4]])
      nb_modifs <- nb_modifs + 1
    }
  }
  if (nb_modifs > 0) {
    writeLines(osm)
    changeset_url <- osmchange_put("modify", text = osm, comment = "mise a jour de gtfs shape")
    carp("id: %s, nb_modifs: %s",id, nb_modifs)
    return(invisible(changeset_url))
  }
  return(invisible())
}
#
## tests
#####################################################################
#
# test sur un node
# - création
# - modification
# - suppression
# source("geo/scripts/transport.R");osmchange_test()
osmchange_test <- function() {
  library(stringi)
# création d'un node
#  id <- osmchange_test_create_node()
  id <- "138017514"
#
  df <- osmapi_get_changeset(id = id)
  text <- osmapi_get_object_xml(id = df[[1, "id"]], type = df[[1, "xml_name"]])
  text <- stri_replace(text, 'v="mga_geo_2"', regex = 'v="mga_geo"')
  print(text)
  changeset_id <- osmchange_put("modify", text = text)
  text <- osmapi_get_object_xml(id = df[[1, "id"]], type = df[[1, "xml_name"]])
  changeset_id <- osmchange_put("delete", text = text)
  return(invisible())
}
#
# create_node : création d'un node
# source("geo/scripts/transport.R");osmchange_test_create_node()
osmchange_test_create_node <- function() {
  text <- '<node id="-1" timestamp="{timestamp}" lat="48.0875052" lon="-1.6445175" changeset="1" version="1">
  <tag k="name" v="mga_geo"/>
</node>
'
  changeset_id <- osmchange_put("create", text = text)
  txt <- osmapi_get_changeset(id = changeset_id)
  return(invisible(changeset_id))
}
#
# test de modifications de tags
# - création
# - modification
# - suppression
# source("geo/scripts/transport.R");osmchange_test_tags()
osmchange_test_tags <- function() {
  library(stringi)
  tags <- list(
    "gtfs:shape_id" = "15_0_150017_701C77E049894695A9D427BB66C96C20_LANTEL_3_ST_JJ_31"
  )
  id <- "13400195"
  type <- "relation"
  osmchange_object_modify_tags(id = id, type = type, tags = tags)
  return(invisible())
}

