# <!-- coding: utf-8 -->
# les réseaux de bus d'une zone
# auteur : Marc Gauthier
#
# source("geo/scripts/transport.R"); df <- zone_jour()
zone_jour <- function(force = FALSE) {
  carp()
  config_xls('breizhgo');
  dsn <- zone_relations_get(force = force)
  df <- zone_relations_parse(dsn = dsn)
  return(invisible(df))
}
#
# l'ensemble des relations de la zone
zone_relations_get <- function(force = TRUE) {
  carp()
  fic <- 'zone_relations'
  requete <- sprintf("area[name='%s']->.a;
relation(area.a)[type=route][route=bus];
(._;<<;);
out meta;", Config[1, "zone"])
  dsn <- overpass_query_json(query = requete, fic = fic, force = force)
  return(invisible(dsn))
}
zone_relations_parse <- function(dsn) {
  carp()
  json.list <- jsonlite::fromJSON(dsn, simplifyVector = FALSE, simplifyDataFrame = FALSE)
  elements.list <- json.list$elements
  relations.df <- data.frame(
    id = character(),
    user = character(),
    timestamp = character(),
    type = character(),
    network = character()
  )
  for (i in 1:length(elements.list)) {
    element.list <- elements.list[[i]]
    tags <- element.list$tags
    for ( t in c("type", "network") ) {
      if (! exists(t, where = tags)) {
        tags[[t]] <- "???"
      }
    }
#    glimpse(tags);
    relations.df[i, ] = c(
      element.list$id,
      element.list$user,
      element.list$timestamp,
      tags$type,
      tags$network
    )
  }
#  saveRDS(relations.df, file = dsn)
  df1 <<- relations.df %>%
    group_by(network, type) %>%
    summarize(nb = n()) %>%
    rename(tag_network = network) %>%
    glimpse()
  df2 <- df1 %>%
    pivot_wider(names_from = type, values_from = nb, values_fill = list(nb = 0), names_repair = "unique")
  print(knitr::kable(df2, format = "pipe"))
  return(invisible(relations.df))
}