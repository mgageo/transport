# <!-- coding: utf-8 -->
#
# le réseau de bus
#
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
## détection du vandalisme de mapbox
#
# analyse des relations route=bus cassées
# et préparation message sur le changeset
#
# source("geo/scripts/transport.R");mapbox_jour()
mapbox_jour <- function() {
  library(readr)
  carp()
  dsn <- sprintf("geo/TRANSPORT/mapbox.txt")
  df <- read_delim(dsn, delim = "|", col_names = FALSE) %>%
    glimpse()
  colnames(df) <- c("id", "date", "changeset", "user", "network", "name")
  df1 <- df %>%
    group_by(changeset) %>%
    summarize(relations = paste(id, collapse = ",  ")) %>%
    mutate(url =  sprintf("https://www.openstreetmap.org/changeset/%s", changeset)) %>%
    glimpse()
  for (i in 1:nrow(df1)) {
    mapbox_changeset(df1[[i, "url"]], df1[[i, "relations"]])
  }
  return(invisible(df))
}
mapbox_changeset <- function(url, relations) {
  library(rvest)
  library(clipr)
  carp("url: %s", url)
  session <- rvest::session(url)
  comments <- session %>%
    html_node("div.changeset-comments")
  if (length(comments) > 0) {
    return()
  }
  attributs <- session %>%
    html_nodes("td.browse-tag-v") %>%
    html_text2()
  if (length(attributs) == 0) {
    return()
  }
  carp("attributs: %s", paste(attributs, collapse = ";"))
  if (length(grep("mapbox", attributs)) == 0) {
    return()
  }
  browseURL(
    url,
    browser = "C:/Program Files/Mozilla Firefox/firefox.exe"
  )
  if (grepl(",", relations)) {
    comment <- sprintf("Bonsoir,
Suite à vos modifications, les trajets des relations route=bus %s sont cassées.
Marc
", relations)
  } else {
    comment <- sprintf("Bonsoir,
Suite à vos modifications, le trajetde la relation route=bus %s est cassée.
Marc
", relations)
  }
  write_clip(comment)
  stop("****")
}