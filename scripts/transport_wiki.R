# <!-- coding: utf-8 -->
#
# utilisation des données d'OpenStreetMap
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
# auteur : Marc Gauthier
#
#
# pour mettre à jour le wiki
#
# https://medium.com/@kyleake/wikipedia-data-scraping-with-r-rvest-in-action-3c419db9af2d
# https://awesomeopensource.com/project/yusuzech/r-web-scraping-cheat-sheet#rvest7.3
# https://github.com/yusuzech/r-web-scraping-cheat-sheet/blob/master/README.md#rvest3
#
# source("geo/scripts/transport.R");config_xls('tudbus');wiki_connect()
wiki_session <- FALSE
wiki_host <- 'https://wiki.openstreetmap.org';
wiki_connect <- function(force = TRUE) {
  library(tidyverse)
  library(rvest)
  if (Wiki == FALSE) {
    return(invisible(wiki_session))
  }
  carp()
  if ( wiki_session != FALSE & force == FALSE) {
    return(invisible(wiki_session))
  }
  url <- sprintf("%s/w/index.php?title=Special:UserLogin", wiki_host)
  session <- rvest::session(url)
  unfilled_forms <- html_form(session)
  login_form <- unfilled_forms[[1]]
  username <- mes_options("osm_wiki_username")
  password <- mes_options("osm_wiki_password")
#  carp("%s/%s", username, password)
  form <- html_form_set(login_form, wpName=username, wpPassword=password)
  wiki_session <<- session_submit(session, form)
  return(invisible(wiki_session))
}
wiki_page_init <- function(page = "User:Mga_geo/Transports_publics/toto", article = 'titi', force = FALSE) {
  library(tidyverse)
  library(rvest)
  if (Wiki == FALSE) {
    return(invisible(wiki_session))
  }
  url <- sprintf("%s/w/index.php?title=%s&action=edit", wiki_host, URLencode(page));
  carp("url: %s", url)
#  return()
  if (class(wiki_session) != "rvest_session") {
    wiki_connect()
  }
  edit <- wiki_session %>%
    session_jump_to(url) %>%
    glimpse()
# c'est bien la création d'une page ?
  new.rvest <- edit %>%
    html_nodes("div.mw-newarticletext")
  if (length(new.rvest) != 1 && force == FALSE) {
    carp("page déjà existante")
    stop("*****")
  }
  edit_forms <- edit %>%
    html_form()

  edit_form <- edit_forms[[1]]
  form <- html_form_set(edit_form, wpTextbox1 = article)
  session3 <- session_submit(edit, form)
  return()
  html <- content(session3$response,as = "text")
  dsn <- sprintf("%s/wiki.html", transportDir)
  write(html, file = dsn, append = FALSE)
  carp("dsn: %s", dsn)
}
# source("geo/scripts/transport.R");config_xls('arcachon');wiki_pages_init()
# source("geo/scripts/transport.R");config_xls('strasbourg');wiki_pages_init()
wiki_pages_init <- function() {
  if (is.na(Config[1, "wiki"])) {
     stop("*****")
  }
  Wiki <<- TRUE
  wiki_page <- Config[[1, "wiki"]]
  article <- '
==Liens==
((website))

http://tools.geofabrik.de/osmi/?view=pubtrans_routes&lon=-2.99101&lat=47.67459&zoom=14&opacity=0.86&overlays=ptv2_routes_,ptv2_routes_valid,ptv2_routes_invalid,ptv2_error_,ptv2_error_ways,ptv2_error_nodes

=Mes actions=
==Mes pages==
* [http://wiki.openstreetmap.org/w/index.php?title=Special%3APrefixIndex&prefix=Mga_geo/Transports_publics/((wiki))&namespace=2 mes pages sur le wiki]

==Outils==
<pre>
area[name="((zone))"]->.a;
(
node(area.a)[highway=bus_stop];
node(area.a)[public_transport];
);
out meta;
</pre>
les arrêts avec une erreur de tag public_transport v2
<pre>
area[name="((zone))"]->.a;
node["public_transport"="platform"](area.a)->.b;
way(bn.b);
node(w)->.c;
node.b.c;
out meta;
</pre>
* les stops qui n"appartiennent pas à une way
<pre>
area[name="((zone))"]->.a;
node["public_transport"~"stop"](area.a)->.b;
way(bn.b);
node(w)->.c;
(node.b; -node.c;);
out meta;
</pre>
<pre>
area[name="((zone))"]->.a;
(
relation(area.a)[route=bus];
);
out meta;
</pre>

=OpenStreetMap=
{{User:Mga_geo/Transports_publics/((wiki))/network}}
{{User:Mga_geo/Transports_publics/((wiki))/route_master}}
{{User:Mga_geo/Transports_publics/((wiki))/route}}
=GTFS=
{{User:Mga_geo/Transports_publics/((wiki))/gtfs_routes_shapes}}


{{User:Mga_geo/Transports_publics/((wiki))/routes}}

{{User:Mga_geo/Transports_publics/((wiki))/routes_stops}}

{{User:Mga_geo/Transports_publics/((wiki))/gtfs_shapes}}
'
  wiki_connect()
  article <- wiki_dfi2tpl(Config, 1, article)
  page <- sprintf("User:Mga_geo/Transports_publics/%s", wiki_page)
  wiki_page_init(page = page, article = article, force = TRUE)
  for (p in c("network", "route_master", "route", "routes", "routes_stops", "gtfs_shapes", "gtfs_routes_shapes")) {
    page <- sprintf("User:Mga_geo/Transports_publics/%s/%s", wiki_page, p) %>%
      glimpse()
    wiki_page_init(page = page, article = "roro", force = TRUE)
  }
}
#
# template du pauvre
wiki_dfi2tpl <- function(df, i, tpl) {
  for ( v in colnames(df) ) {
    if( is.na(df[i, v]) ) {
      val <- ''
    } else {
      val <- as.character(df[i, v])
    }
#    carp("v: %s val: %s", v, val)
    re <- paste0("\\(\\(", v, "\\)\\)")
#    carp("re: %s", re)
    tpl <-  gsub(re, val, tpl, perl = TRUE)
  }
  return(invisible(tpl))
}
#
# test de la création d'une table
# source("geo/scripts/transport.R");df <- wiki_df2table_test()
wiki_df2table_test <- function() {
  library(tidyverse)
  df <- tibble(x = 1, y = 2, z = c(3, 4, 5))
  wiki <- wiki_df2table(df)
  print(wiki)
  write(wiki, "d:/wiki.txt")
}
#
# création d'une table mediawiki
wiki_df2table <- function(df) {
  wiki <- '{| class="wikitable sortable"'
  wiki <- append(wiki, "|-")
  colonnes <- colnames(df)
  for (i in 1:length(colonnes)) {
    wiki <- append(wiki, sprintf('!scope="col"| %s', colonnes[i]))
  }
  for (i in 1:nrow(df)) {
    wiki <- append(wiki, "|-")
    wiki <- append(wiki, sprintf('!scope="row" | %s', df[i, 1]))
    for (j in 2:ncol(df)) {
      wiki <- append(wiki, sprintf('| %s', df[i, j]))
    }
  }
  wiki <- append(wiki, "|}")
  wiki <- paste(wiki,  collapse = "\n")
#  glimpse(wiki);stop("********")
  return(invisible(wiki))
}