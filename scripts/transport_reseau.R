# <!-- coding: utf-8 -->
#
# le réseau de bus
#
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
## script chapeau pour un réseau
#
# le réseau est configuré en variable globale dans transport.R
#
# source("geo/scripts/transport.R");reseau_jour()
reseau_jour <- function(reseau = Reseau, force = TRUE) {
  library(tidyverse)
  library(rio)
  library(sf)
  carp()
  config_xls(reseau)
  reseau_tpl_tex(reseau = reseau)
  if (grepl("MOBIBREIZH", Config[1, "gtfs_dir"])) {
    mobibreizh_gtfs_reseaux(reseau = reseau)
  }
  reseau_osm_jour(reseau = reseau, force = force)
  if (is.na(Config[1, "gtfs_dir"])) {
    return()
  }
  reseau_gtfs_jour(reseau = reseau, force = force)
  reseau_diff_jour(reseau = reseau, force = force)
}
#
# recopie des templates tex
# source("geo/scripts/transport.R");reseau_tpl_tex(reseau = Reseau)
reseau_tpl_tex <- function(reseau = "star", force = FALSE) {
  carp()
  config_xls(reseau)
  files <- list.files(tplDir, pattern = "reseau.*_tpl.tex$", full.names = TRUE, ignore.case = TRUE, recursive = FALSE)
  for (file in files) {
    tex <- readLines(file)
    dsn <- str_replace(file, "reseau", Reseau)
    dsn <- str_replace(dsn, "_tpl.tex", ".tex")
    dsn <- str_replace(dsn, ".*/", sprintf("%s/", texDir))
    carp("dsn: %s", dsn)
    write(tex, file = dsn, append = FALSE)
  }
  from <- sprintf("%s/misc.tex", tplDir)
  to <- sprintf("%s/misc.tex", texDir)
  file.copy(from = from, to = to, overwrite = TRUE)
}
# source("geo/scripts/transport.R");reseau_diff_jour(force = TRUE)
reseau_diff_jour <- function(reseau = Reseau, force = FALSE) {
  library(tidyverse)
  library(data.table)
  library(janitor)
  carp("reseau: %s", reseau)
  config_xls(reseau)
  if (Config[[1, "shapes"]] != "FALSE") {
    reseau_tpl_tex(reseau = reseau)
    reseau_osm_routes_tag_shape(reseau = reseau, force = force)
    reseau_osm_routes_shapes(reseau = reseau, force = force)
    tex_pdflatex(sprintf("%s_diff.tex", reseau))
  }
}
# source("geo/scripts/transport.R");reseau_gtfs_jour(reseau = Reseau)
reseau_gtfs_jour <- function(reseau = Reseau, force = TRUE) {
  library(tidyverse)
  carp("reseau: %s", reseau)
  config_xls(reseau)
  reseau_tpl_tex(reseau = reseau)
  tidytransit_jour(reseau = reseau, force = force)
  if (Config[[1, "shapes"]] != "FALSE") {
    tidytransit_gtfs_routes_shapes_stops_carto()
    tex_pdflatex(sprintf("%s_gtfs_routes_shapes_stops_carto.tex", reseau))
    tex_pdflatex(sprintf("%s_gtfs_shapes.tex", reseau))
  } else {
    reseau_gtfs_routes_stops()
    tex_pdflatex(sprintf("%s_gtfs.tex", reseau))
  }
}
# source("geo/scripts/transport.R");reseau_osm_jour(force = TRUE, force_members = TRUE)
reseau_osm_jour <- function(reseau = Reseau, force = TRUE, force_members = TRUE, force_osm = TRUE) {
  library(tidyverse)
  library(data.table)
  library(janitor)
  reseau <- Reseau
  reseau_tpl_tex(reseau = reseau)
  osm_relations_route_bus_verif(reseau = reseau, force = force)
  osm_relations_route_members(reseau = reseau, force = force_members, force_osm = force_osm, osrm = FALSE)
  osm_relations_route_members_valid(force = force_members, force_osm = force_osm)
  tex_pdflatex(sprintf("%s_osm.tex", reseau))
}
# source("geo/scripts/transport.R");reseau_osm_routes_jour(force = TRUE)
reseau_osm_routes_jour <- function(reseau = Reseau, force = TRUE) {
  library(tidyverse)
  library(data.table)
  library(janitor)
  carp("reseau: %s", reseau)
  config_xls(reseau)
  reseau_tpl_tex(reseau = reseau)
  reseau_osm_routes_refs(reseau = Reseau, force = force)
  tex_pdflatex(sprintf("%s_routes.tex", reseau))
}

#
# comparaison à partir des routes gtfs
# source("geo/scripts/transport.R");reseau_diff_routes(reseau = Reseau, force = FALSE)
reseau_diff_routes <- function(reseau = "star", force = FALSE) {
  library(tidyverse)
  library(data.table)
  library(sf)
  library(janitor)
  carp()
  config_xls(reseau)
  osm.df <- overpass_get(query = "relations_route_bus_network", format = "csv", force = force) %>%
    clean_names() %>%
    glimpse()
  tt <- tidytransit_lire()
  routes.df <- tt$routes %>%
    glimpse()
  df1 <- routes.df %>%
    filter(route_type %notin% c(4)) %>%              # le ferry de Bordeaux
    filter(route_desc %notin% c("Métro", "Transport scolaire")) %>%
    filter(route_short_name %notin% osm.df$ref) %>%
    glimpse()
}
#
# comparaison à partir des routes osm
# source("geo/scripts/transport.R");reseau_osm_routes_tag_shape(reseau = Reseau, force = TRUE)
reseau_osm_routes_tag_shape <- function(reseau = "star", force = FALSE) {
  library(tidyverse)
  library(data.table)
  library(sf)
  library(janitor)
  carp()
  config_xls(reseau)
  star.df <- star202210_supprime()
  dsn <- osm_relations_route_bus_csv(force = force)
  df <- fread(dsn, encoding = "UTF-8") %>%
    clean_names() %>%
    glimpse()
  carp("les shapes en double")
  df1 <- df %>%
    group_by(gtfs_shape_id) %>%
    summarize(nb = n()) %>%
    filter(nb > 1) %>%
    glimpse()
  df2 <- df %>%
    filter(gtfs_shape_id %in% df1$gtfs_shape_id) %>%
    dplyr::select(ref_network, id, name, from, to, ref, gtfs_shape_id) %>%
    arrange(gtfs_shape_id) %>%
    glimpse()
  tex_df2kable(df2, suffixe = "doublon", longtable = TRUE)
  carp("les shapes inconnus")
  tt <- tidytransit_lire()
  shapes.df <- tt$shapes %>%
    glimpse() %>%
    distinct(shape_id) %>%
    mutate(source = "gtfs") %>%
    glimpse()
  df11 <- df %>%
    left_join(shapes.df, by = c("gtfs_shape_id" = "shape_id"))
  df12 <- df11 %>%
    filter(is.na(source)) %>%
    dplyr::select(gtfs_shape_id, ref_network, id, name, from, to, ref) %>%
#    filter(! grepl("^2\\d\\d", ref)) %>%
#    filter(! grepl("^08", gtfs_shape_id)) %>%
#    filter(! grepl("^00", gtfs_shape_id)) %>%
    filter(ref %notin% star.df$ligne) %>%
    arrange(gtfs_shape_id) %>%
    mutate(INDEX = row_number()) %>%
    glimpse()
  df121 <- df12 %>%
    mutate(id = as.character(id)) %>%
    dplyr::select(INDEX, gtfs_shape_id, ref_network, id, name)
  df122 <- df12 %>%
    mutate(name = sprintf("%s => %s", from, to)) %>%
    dplyr::select(INDEX, gtfs_shape_id, ref_network, id, name) %>%
    mutate(gtfs_shape_id = "", ref_network = "", id = "") %>%
    glimpse()
  df123 <- df121 %>%
    bind_rows(df122) %>%
    arrange(INDEX) %>%
    glimpse()
#  stop("****")
  tex_df2kable(df123, suffixe = "inconnu", longtable = TRUE)
#
# pour trouver le shape potentiel
  df13 <- df12 %>%
    dplyr::select(id, ref_network, gtfs_shape_id) %>%
#    filter(! grepl("^2\\d\\d", ref)) %>%
#    filter(! grepl("^08", gtfs_shape_id)) %>%
    glimpse()
  dsn <- sprintf("%s/%s", texDir, "tidytransit_gtfs_routes_shapes_stops.csv")
  shapes.df <- fread(dsn, encoding = "UTF-8") %>%
    glimpse()
  df14 <- df13 %>%
    left_join(shapes.df, by = c("ref_network" = "ref_network")) %>%
#    group_by(gtfs_shape_id, id, ref_network) %>%
#    summarize(shapes = paste0(shape_id, collapse = ",")) %>%
    arrange(gtfs_shape_id, shape_id) %>%
    dplyr::select(gtfs_shape_id, id, shape_id) %>%
    glimpse()
  tex_df2kable(df14, suffixe = "potentiel", longtable = TRUE)
  return(invisible(df2))
}
#
# source("geo/scripts/transport.R");reseau_osm_routes_refs(reseau = Reseau, force = TRUE)
reseau_osm_routes_refs <- function(reseau = "star", force = TRUE) {
  library(tidyverse)
  library(data.table)
  library(sf)
  library(janitor)
  carp()
  config_xls(reseau)
  dsn <- osm_relations_route_bus_csv(force = force)
  df <- fread(dsn, encoding = "UTF-8") %>%
    as.data.table() %>%
    clean_names() %>%
    glimpse()
  df1 <- df %>%
    filter(is.na(ref_network)) %>%
    glimpse()
  tex_df2kable(df1, suffixe = "absent", longtable = TRUE)
  gtfs.df <- tidytransit_lire("gtfs_routes_stops") %>%
    glimpse()
  df2 <- df %>%
    filter(ref_network %notin% gtfs.df$ref_network) %>%
    glimpse()
  tex_df2kable(df2, suffixe = "inconnu", longtable = TRUE)
  texFic <- sprintf("%s/%s", imagesDir, "reseau_osm_routes_refs.tex")
  TEX <- file(texFic)
  tex <- sprintf("<!-- coding: utf-8 -->
%s ", format(Sys.time(), "%d/%m/%Y %H:%M:%S"))
#
# le template tex
  dsn <- sprintf("%s/routes_refs_tpl.tex", tplDir)
  template <- readLines(dsn) %>%
    glimpse()
#  stop("****")
  df1 <- df %>%
    dplyr::select(id, timestamp, user, ref_network) %>%
    arrange(ref_network)
  tex_df2kable(df1, suffixe = "lst", longtable = TRUE)
  for (i in 1:nrow(df)) {
    carp("i: %s/%s", i, nrow(df))
    id <-  df[i, "id"]
    ref <- df[[i, "ref_network"]]
    dsn <- sprintf("%s/reseau_osm_routes_refs_%s.pdf", imagesDir, id)
#    glimpse(df[i, ]);stop("****")
# les données osm
    rc <- osmapi_get_members_platform(ref = id, force = TRUE)
    route.df <- rc$relation %>%
      glimpse() %>%
      mutate(first_stop = sub(";.*$", "", stops_id)) %>%
      mutate(last_stop = sub("^.*;", "", stops_id))
    df1 <- rbind(colnames(route.df), route.df) %>%
      t() %>%
      as.data.frame()
    rownames(df1) <- NULL
    t <- tex_df2table(df1, suffixe = id, dossier = "images", entete = "lp{4cm}p{15cm}")
# les données gtfs
    df2 <- gtfs.df %>%
      filter(ref_network == !!ref)
    df3 <- df2 %>%
      dplyr::select(first_stop, last_stop)
    tex_df2table(df3, suffixe = id, dossier = "stop", entete = "lp{4cm}p{4cm}")
    if (1 == 2) {
      t2 <- ""
      for (i2 in 1:nrow(df2)) {
        df3 <- df2[i2, ]
        df3 <- rbind(colnames(df3), df3) %>%
          t() %>%
          as.data.frame()
        rownames(df3) <- NULL
        t <- tex_df2table(df3, suffixe = id, dossier = "gtfs", entete = "lp{4cm}p{15cm}")
        t2 <- append(t2, t)
      }
      dsn <- tex_dsn(suffixe = id, dossier = "gtfs")
      carp("dsn: %s", dsn)
      f <- file(dsn, open="w")
      writeLines(t2, f)
      close(f)
    }
    tpl <- tex_df2tpl(df, i, template)
    tpl <- escapeLatexSpecials(tpl)
    tex <- append(tex, tpl)
    next
    break
    osm.sf <- osmapi_get_ways(ref = id, force = force, force_osm = FALSE) %>%
      st_transform(2154)
    add <- FALSE
    if (! st_is_empty(osm.sf[1,]) ) {
      plot(st_geometry(osm.sf), col = "blue", lwd = 3)
      add <- TRUE;
    }
    dsn <- dev2pdf(suffixe = id, dossier = "images")
  }
  write(tex, file = TEX, append = FALSE)
  close(TEX)
  carp("texFic: %s", texFic)
}
#
# source("geo/scripts/transport.R");reseau_osm_routes_shapes(reseau = Reseau, force = FALSE);tex_pdflatex(sprintf("%s_diff.tex", Reseau))
reseau_osm_routes_shapes <- function(reseau = "star", force = TRUE) {
  library(tidyverse)
  library(data.table)
  library(sf)
  library(janitor)
  carp()
  config_xls(reseau)
  dsn <- osm_relations_route_bus_csv(force = force)
  df <- fread(dsn, encoding = "UTF-8") %>%
    as.data.table() %>%
    clean_names()
  texFic <- sprintf("%s/%s", imagesDir, "reseau_osm_routes_shapes.tex")
  TEX <- file(texFic)
  tex <- sprintf("<!-- coding: utf-8 -->
%s ", format(Sys.time(), "%d/%m/%Y %H:%M:%S"))
#
# le template tex
  dsn <- sprintf("%s/routes_shapes_tpl.tex", tplDir)
  template <- readLines(dsn) %>%
    glimpse()
  if (reseau == "star") {
    star.df <- star202210_supprime()
    df <- df %>%
#    filter(grepl("^0", gtfs_shape_id)) %>%
      filter(ref %notin% star.df$ligne) %>%
#      filter(grepl("^08", gtfs_shape_id)) %>%
      filter(grepl("^0[01]", gtfs_shape_id)) %>%
      arrange(gtfs_shape_id) %>%
      glimpse()
  }
#  stop("****")
  df1 <- df %>%
    dplyr::select(id, timestamp, user, ref_network, gtfs_shape_id) %>%
    arrange(gtfs_shape_id)
  tex_df2kable(df1, suffixe = "lst", longtable = TRUE)
  for (i in 1:nrow(df)) {
    carp("i: %s/%s", i, nrow(df))
    id <-  df[i, "id"]
    shape <- df[[i, "gtfs_shape_id"]]
    dsn <- sprintf("%s/reseau_osm_routes_shapes_%s.pdf", imagesDir, id)
#    glimpse(df[i, ]);stop("****")
#    osmapi_get_members_platform(ref = id, force = force)
    osm.sf <- osmapi_get_ways(ref = id, force = force, force_osm = force) %>%
      st_transform(2154)
    add <- FALSE
    if (! st_is_empty(osm.sf[1,]) ) {
      plot(st_geometry(osm.sf), col = "blue", lwd = 3)
      add <- TRUE;
    }

#    stop("****")
    dsn_shape <- sprintf("%s/shape_%s.gpx", josmDir, shape)
    if (! file.exists(dsn_shape)) {
      title(sub = "shape absent")
#      confess("*** dsn_shape: %s", dsn_shape)
    } else {
      shape.sf <- st_read(dsn_shape, layer = "tracks", quiet = TRUE) %>%
        st_transform(2154)
      plot(st_geometry(shape.sf), add = add, col = "green", lwd = 3)
      inters <- st_intersection(st_buffer(shape.sf, 10), osm.sf)
      plot(st_geometry(inters), add = TRUE, col = "grey", lwd=3)
    }
    titre <- sprintf("relation: %s shape: %s", id, shape)
    title(titre)
    legend("topleft",
      , legend=c("osm", "gtfs", "inters")
      , col=c("blue", "green", "grey")
      , lwd = 3, lty = 1, cex = 0.8, box.lty = 0
    )
    dsn <- dev2pdf(suffixe = id, dossier = "images")
# pour le latex
#    tex <- append(tex, sprintf("\\mongraphique{images/reseau_routes_shapes_%s.pdf}", id))
    df[i, "shape"] <- shape
    df[i, "dsn_shape"] <- dsn_shape
    tpl <- tex_df2tpl(df, i, template)
    tpl <- escapeLatexSpecials(tpl)
    tex <- append(tex, tpl)
#    break
    next
    diff.sfc <- st_difference(shape.sf, inters)
    if (length(diff.sfc) >= 1 ) {
      carp('shape_id: %s diff: %s', shape, length(diff.sfc))
      plot(st_geometry(diff.sfc), add = TRUE, col = 'red', lwd = 3)
 #     break
    }
    stop("****")
  }
  write(tex, file = TEX, append = FALSE)
  close(TEX)
  carp(" texFic: %s", texFic)
}

#
# la comparaison des deux sources
#
# pour les routes
# source("geo/scripts/transport.R");reseau_osm_routes_gtfs_shapes()
reseau_osm_routes_gtfs_shapes <- function() {
  carp()
  transport_osm_routes_gtfs_shapes_df()
  transport_osm_routes_gtfs_shapes_diff()
}

transport_osm_routes_gtfs_shapes_df <- function() {
  library(tidyverse)
  library(sf)
  carp()
  shapes.sf <- tidytransit_shapes_sf_lire() %>%
    mutate(source='shapes') %>%
    glimpse()
  routes.sf <- osm_routes_read() %>%
    dplyr::select(shape_id=ref.FR.STAR) %>%
    filter(! is.na(shape_id)) %>%
    glimpse()
  names(routes.sf$geometry)=NULL
  names(shapes.sf$geometry)=NULL

# avec la géométrie
  carp('jointure')
  df <- dplyr::left_join(routes.sf %>% as.data.frame(), shapes.sf %>% as.data.frame(), by=c('shape_id'='shape_id')) %>%
    glimpse()
#
# le tracé gtfs est inclus dans le buffer route osm
  carp('passage en Lambert 93 et buffer')
  nc1 <- st_transform(routes.sf, 2154) %>%
    st_buffer(50)
  nc2 <- st_transform(shapes.sf, 2154)
  df <- dplyr::left_join(nc1 %>% as.data.frame(), nc2 %>% as.data.frame(), by=c('shape_id'='shape_id')) %>%
    filter(! is.na(source)) %>%
    glimpse()
  transport_save(df, 'osm_routes_gtfs_shapes')
}
# source("geo/scripts/transport.R");transport_osm_routes_gtfs_shapes_diff()
transport_osm_routes_gtfs_shapes_diff <- function() {
  carp('contains')
  library(sf)
  library(tidyverse)
  df <- transport_read('osm_routes_gtfs_shapes') %>%
    arrange(shape_id)
  nc <- st_as_sf(df, geom_routes=st_sfc(df$geometry.x))
  nc$geom_shapes <- st_sfc(df$geometry.y)
  for (i in 1:nrow(df)) {
    nc1 <- nc[i, ]
    plot(st_geometry(nc1$geom_routes))
    plot(st_geometry(nc1$geom_shapes), add=TRUE)
    inters <- st_intersection(nc1$geom_shapes, nc1$geom_routes)
    plot(st_geometry(inters), add=TRUE, col='blue', lwd=3)
#    carp('inters')
#    glimpse(inters)
#    carp('shapes')
#    glimpse(nc1$geom_shapes)
    diff.sfc <<- st_difference(nc1$geom_shapes, inters)
    if (length(diff.sfc) >= 1 ) {
      carp('shape_id: %s', nc[[i, 'shape_id']])
      plot(st_geometry(diff.sfc), add=TRUE, col='red', lwd=3)
 #     break
    }
  }
}
transport_save <- function(obj, fic='objets_stop') {
  dsn <- sprintf("%s/%s.Rds", transportDir, fic)
  saveRDS(obj, dsn)
  carp("dsn: %s", dsn)
  return(invisible(obj))
}
transport_read <- function(fic='objets_stop') {
  dsn <- sprintf("%s/%s.Rds", transportDir, fic)
  obj <- readRDS(dsn)
  carp("dsn: %s", dsn)
  return(invisible(obj))
}
#
## pour les stops
#
# la partie osm
#
# source("geo/scripts/transport.R");reseau_osm_jour_stops(force = TRUE)
reseau_osm_jour_stops <- function(reseau = "star", force = FALSE) {
  library(tidyverse)
  library(data.table)
  library(janitor)
  carp()
  config_xls(reseau)
  reseau_osm_stops_valid(force_osm = TRUE)
}
#
# lecture en csv
# source("geo/scripts/transport.R");reseau_osm_stops_valid(force_osm = FALSE)
reseau_osm_stops_valid <- function(force_osm = TRUE) {
  library(tidyverse)
  library(data.table)
  library(janitor)
  k_ref <- Config[[1, 'k_ref']]
  carp("k_ref: %s", k_ref)
  titre <- sprintf("reseau_osm_stops_valid_%s", Config[1, 'reseau'])
  html <- misc_html_titre(titre)
  html <- misc_html_append(html, "<h1>OSM</h1>")
#
#
  html <- misc_html_append(html, "<h2>Les stops du réseau</h2>")
  html <- misc_html_append(html, "<h3>Répartition</h3>")
  network.df <- overpass_get(query = "bus_stop_network", format = "csv", force = force_osm) %>%
    filter(ferry == "" & tram == "" & rail == "" & train == "") %>%
    dplyr::select(-ferry, -tram, -rail,  -train) %>%
    glimpse()
  df1 <- network.df %>%
    group_by(`@type`, public_transport, highway, bus) %>%
    summarize(nb = n()) %>%
    glimpse()
  misc_print(df1)
  html <- misc_html_append_df(html, df1)
  html <- misc_html_append(html, "<h3>Non conforme PTv2</h3>")
  tags.df <- read.table(text="type,public_transport,highway,bus
node,platform,bus_stop,
node,stop_position,,yes
", header=TRUE, sep=",", blank.lines.skip = TRUE, stringsAsFactors = FALSE, quote="")
  misc_print(tags.df)
  zoom <- "left=%0.5f&right=%0.5f&top=%0.5f&bottom=%0.5f"
  select <- "%s%s"

  df1 <- network.df %>%
    filter(!(`@type` == "node" & public_transport == "platform" & highway == "bus_stop" & bus == "")) %>%
    filter(!(`@type` == "node" & public_transport == "stop_position" & highway == "" & bus == "yes")) %>%
    filter(!(`@type` == "relation" & public_transport == "stop_area" & highway == "" & bus == "")) %>%
    filter(!(railway != "")) %>%
    arrange(`@type`, public_transport, highway, bus) %>%
    mutate(select = sprintf(select, `@type`, `@id`)) %>%
    mutate(zoom = sprintf(zoom, `@lon` - .001, `@lon` + .001, `@lat` + .001, `@lat` - .001)) %>%
    mutate(josm = sprintf("<a href='http://127.0.0.1:8111/load_and_zoom?%s&select=%s'>josm</a>", zoom, select)) %>%
    dplyr::select(-select, -zoom)
  df2 <- df1  %>%
    group_by(`@type`, public_transport, highway, bus) %>%
    summarize(nb = n()) %>%
    glimpse()
  misc_print(df2)
  html <- misc_html_append_df(html, df2)
  html <- misc_html_append_df(html, df1)
#
#
  html <- misc_html_append(html, "<h2>Les stops de la zone</h2>")
  html <- misc_html_append(html, "<h3>Répartition</h3>")
  area.df <- overpass_get(query = "bus_stop_area", format = "csv", force = force_osm) %>%
    filter(ferry == "" & tram == "" & rail == "" & train == "") %>%
    dplyr::select(-ferry, -tram, -rail,  -train) %>%
    glimpse()
  df1 <- area.df %>%
    group_by(`@type`, public_transport, highway, bus) %>%
    summarize(nb = n()) %>%
    glimpse()
  misc_print(df1)
  html <- misc_html_append_df(html, df1)
  html <- misc_html_append(html, "<h3>Non conforme PTv2</h3>")
  tags.df <- read.table(text="type,public_transport,highway,bus
node,platform,bus_stop,
node,stop_position,,yes
", header=TRUE, sep=",", blank.lines.skip = TRUE, stringsAsFactors = FALSE, quote="")
  misc_print(tags.df)
  zoom <- "left=%0.5f&right=%0.5f&top=%0.5f&bottom=%0.5f"
  select <- "%s%s"

  df1 <- area.df %>%
    filter(!(`@type` == "node" & public_transport == "platform" & highway == "bus_stop" & bus == "")) %>%
    filter(!(`@type` == "node" & public_transport == "stop_position" & highway == "" & bus == "yes")) %>%
    filter(!(`@type` == "relation" & public_transport == "stop_area" & highway == "" & bus == "")) %>%
    filter(!(railway != "")) %>%
    arrange(`@type`, public_transport, highway, bus) %>%
    mutate(select = sprintf(select, `@type`, `@id`)) %>%
    mutate(zoom = sprintf(zoom, `@lon` - .001, `@lon` + .001, `@lat` + .001, `@lat` - .001)) %>%
    mutate(josm = sprintf("<a href='http://127.0.0.1:8111/load_and_zoom?%s&select=%s'>josm</a>", zoom, select)) %>%
    dplyr::select(-select, -zoom)
  df2 <- df1  %>%
    group_by(`@type`, public_transport, highway, bus) %>%
    summarize(nb = n()) %>%
    glimpse()
  misc_print(df2)
  html <- misc_html_append_df(html, df2)
  html <- misc_html_append_df(html, df1)
#
#
  dsn <- sprintf("%s/%s.html", webDir, titre)
  write(html, dsn)
  carp("dsn: %s", dsn)
  url <- sprintf("http://localhost/transport/%s.html", titre)
  browseURL(
    url,
    browser = "C:/Program Files/Mozilla Firefox/firefox.exe"
  )
}
reseau_toto <- function() {
# les stops à partir des relations route
  html <- misc_html_append(html, "<h2>Les stops des relations route du réseau</h2>")
  network.sf <- osm_bus_stop_network(force = force_osm) %>%
    glimpse()
  network.df <- network.sf %>%
    mutate(lon = st_coordinates(.)[, 'X']) %>%
    mutate(lat = st_coordinates(.)[, 'Y']) %>%
    st_drop_geometry() %>%
    rename(k_ref = !!k_ref) %>%
    clean_names()

#
  html <- misc_html_append(html, "<h3>platform way</h3>")
  df1 <- network.df %>%
    filter(type == "way") %>%
    glimpse()
  html <- misc_html_append_df(html, df1)

#
  html <- misc_html_append(html, "<h3>pb de tags platform stop</h3>")
  df1 <- network.df %>%
    filter(highway != "bus_stop" | public_transport != "platform") %>%
    glimpse()
  df1 <- df1 %>%
    filter(bus != "yes" | public_transport != "stop_position") %>%
    glimpse()
  html <- misc_html_append_df(html, df1)
#
  html <- misc_html_append(html, "<h2>absence de k_ref sur les platform</h2>")
  df1 <- network.df %>%
    filter(highway == "bus_stop" & public_transport == "platform") %>%
    filter(is.na(k_ref) | k_ref == "") %>%
    glimpse()
 #  josm <- "http://127.0.0.1:8111/load_and_zoom?left=8.19&right=8.20&top=48.605&bottom=48.590&select=node413602999
  zoom <- "left=%0.5f&right=%0.5f&top=%0.5f&bottom=%0.5f"
  select <- "node%s"
  df2 <- df1 %>%
    mutate(select = sprintf(select, osm_id)) %>%
    mutate(zoom = sprintf(zoom, lon - .001, lon + .001, lat + .001, lat - .001)) %>%
    mutate(josm = sprintf("<a href='http://127.0.0.1:8111/load_and_zoom?%s&select=%s'>josm</a>", zoom, select)) %>%
    dplyr::select(-select, -zoom)

  html <- misc_html_append_df(html, df2)
#
  html <- misc_html_append(html, "<h3>k_ref en double sur les platform</h3>")
  df1 <- network.df %>%
    filter(highway == "bus_stop" & public_transport == "platform") %>%
    group_by(k_ref) %>%
    summarize(nb = n()) %>%
    filter(nb > 1) %>%
    glimpse()
  df2 <- network.df %>%
    filter(k_ref %in% df1$k_ref) %>%
    arrange(k_ref, osm_id)
  html <- misc_html_append_df(html, df2)
}
#
# comparaison osm # gtfs
# source("geo/scripts/transport.R");reseau_osm_stops_gtfs_stops()
reseau_osm_stops_gtfs_stops <- function(force_osm = TRUE) {
  library(tidyverse)
  library(data.table)
  library(janitor)
  carp()
  titre <- "reseau_osm_stops_gtfs_stops"
  html <- misc_html_titre(titre)
  html <- misc_html_append(html, "<h1>OSM</h1>")
  gtfs_stops.sf <- tidytransit_stops_sf_lire() %>%
    mutate(gtfs = "gtfs") %>%
    filter(location_type == 0) %>%
    glimpse()
  if (Reseau == "star") {
    gtfs_stops.sf <- gtfs_stops.sf %>%
      filter(!grepl("^5\\d\\d\\d", stop_id))
  }
  dsn <- osm_bus_stop_network_csv(force = force_osm)
  osm_stops.df <- fread(dsn) %>%
    rename(k_ref = Config[1, 'k_ref']) %>%
    clean_names() %>%
    mutate(k_ref = sprintf("%s", k_ref)) %>%
    glimpse()
  osm_stops.sf <- st_as_sf(osm_stops.df, coords = c("lon", "lat"), crs = 4326, remove = TRUE) %>%
    dplyr::select(stop_id = k_ref, name, osm_id = id, osm_type = type) %>%
    mutate(osm = "osm")
  Encoding(osm_stops.sf$name) <- "UTF-8"

# sans géométrie
  df <- dplyr::full_join(osm_stops.sf %>% st_drop_geometry(), gtfs_stops.sf %>% st_drop_geometry(), by=c('stop_id'='stop_id')) %>%
    glimpse()
  html <- misc_html_append(html, "<h2>les stops osm absents du gtfs</h2>")
  osm_inc.df <- df %>%
    filter(is.na(gtfs)) %>%
    dplyr::select(osm_type, osm_id, stop_id, name) %>%
    glimpse()
  transport_save(osm_inc.df, "reseau_osm_stops_gtfs_stops_osm_inc")
  html <- misc_html_append_df(html,osm_inc.df)
#
  html <- misc_html_append(html, "<h2>les stops gtfs absents d'osm</h2>")
  gtfs_inc.df <- df %>%
#    mutate(stop_id=as.numeric(stop_id)) %>%
#    filter(stop_id < 15000) %>%
    filter(is.na(osm)) %>%
    dplyr::select(stop_id, stop_name, stop_desc, lat, lon, location_type) %>%
    glimpse()
  html <- misc_html_append_df(html, gtfs_inc.df)
#
# avec la géométrie
  html <- misc_html_append(html, "<h2>distance > 50 mètres</h2>")
  carp("distance > 50 mètres")
  df <- dplyr::full_join(osm_stops.sf %>% as.data.frame(), gtfs_stops.sf %>% as.data.frame(), by=c('stop_id'='stop_id'))
  df$d <- as.numeric(st_distance(df$geometry.x, df$geometry.y, by_element = TRUE))
  df1 <- df %>%
    dplyr::select(name, stop_name, osm_id, d) %>%
    filter(d > 50)
  html <- misc_html_append_df(html, df1)
  dsn <- sprintf("%s/%s.html", webDir, titre)
  write(html, dsn)
  carp("dsn: %s", dsn)
  url <- sprintf("http://localhost/transport/%s.html", titre)
  browseURL(
    url,
    browser = "C:/Program Files/Mozilla Firefox/firefox.exe"
  )
  return(invisible(df1))
}
# source("geo/scripts/transport.R");reseau_gtfs_routes_shapes_stops()
reseau_gtfs_routes_shapes_stops <- function(force_osm = FALSE) {
  library(stringr); # pour str_glue
  df <- transport_read("tidytransit_gtfs_routes_shapes_stops") %>%
    mutate(route_color = sprintf("#%s", toupper(route_color))) %>%
    mutate(route_text_color = sprintf("#%s", toupper(route_text_color))) %>%
    glimpse()
  dsn <- osm_arrets_csv(force = force_osm)
  osm_stops.df <- fread(dsn, encoding = "UTF-8") %>%
    rename(k_ref = Config[1, 'k_ref']) %>%
    filter(public_transport == "platform") %>%
    clean_names() %>%
    mutate(type = dplyr::recode(type,
      "way" = "wy",
      "node" = "nd"
    )) %>%
    mutate(member = sprintf("  %s %s %s", type, id, "platform"))
  cols <- colnames(df)
  for (i in 1:nrow(df)) {
    stops.df <- tibble(stop = unlist(str_split(df[i, "stops"], ";")))
    stops.df <- stops.df %>%
      left_join(osm_stops.df, by = c("stop" = "k_ref"))
    platforms <- paste(stops.df$member, collapse = "\n")
    for (j in 1:ncol(df)) {
      col <- sprintf("relation_%s", cols[j])
      assign(col, df[[i, j]])
    }
    tpl <- "
  colour = {relation_route_color}
  from = {relation_first_name}
  gtfs:shape_id = {relation_shape_id}
  name = {Config_route_name} {relation_route_short_name} : {relation_first_name} - {relation_last_name}
  network = {Config_network}
  network:wikidata = {Config_wikidata}
  network:wikipedia = {Config_wikipedia}
  operator = {Config_operator}
  public_transport:version = 2
  ref = {relation_route_short_name}
  ref:network = {relation_ref_network}
  route = bus
  text_colour = {relation_route_text_color}
  to = {relation_last_name}
  type = route
  website = {Config_website}
  {platforms}
"
    level0 <- str_glue(tpl)
#    print(level0)
    dsn <- sprintf("%s/reseau_gtfs_routes_shapes_stops_%s.txt", osmDir, relation_shape_id)
    write(level0, dsn)
    carp("dsn: %s", dsn)
  }
}
# source("geo/scripts/transport.R");reseau_gtfs_routes_stops()
reseau_gtfs_routes_stops <- function(force_osm = FALSE) {
  library(stringr); # pour str_glue
  df <- transport_read("tidytransit_routes_stops") %>%
    mutate(route_color = sprintf("#%s", toupper(route_color))) %>%
    mutate(route_text_color = sprintf("#%s", toupper(route_text_color))) %>%
    group_by(ref_network) %>%
    mutate(no = row_number(ref_network)) %>%
    mutate(ref_route = sprintf("%s_%s", ref_network, no)) %>%
    glimpse()
  dsn <- osm_arrets_csv(force = force_osm)
  osm_stops.df <- fread(dsn, encoding = "UTF-8") %>%
    rename(k_ref = Config[1, 'k_ref']) %>%
    filter(public_transport == "platform") %>%
    clean_names() %>%
    mutate(type = dplyr::recode(type,
      "way" = "wy",
      "node" = "nd"
    )) %>%
    mutate(member = sprintf("  %s %s %s", type, id, "platform")) %>%
    glimpse()
  df1 <- df %>%
    ungroup() %>%
    dplyr::select(ref_route, name = route_long_name, de = first_name, à = last_name, nb = nb_stops) %>%
    glimpse()
  tex_df2kable(df1, longtable = TRUE)
  return()
  cols <- colnames(df)
  for (i in 1:nrow(df)) {
    stops.df <- tibble(stop = unlist(str_split(df[i, "stops_code"], ";")))
    stops.df <- stops.df %>%
      left_join(osm_stops.df, by = c("stop" = "k_ref"))
    platforms <- paste(stops.df$member, collapse = "\n")
    for (j in 1:ncol(df)) {
      col <- sprintf("relation_%s", cols[j])
      assign(col, df[[i, j]])
    }
#  name = {Config_route_name} {relation_route_short_name} : {relation_first_name} - {relation_last_name}
    tpl <- "
  colour = {relation_route_color}
  from = {relation_first_name}
  name = {relation_route_long_name} : {relation_first_name} - {relation_last_name}
  network = {Config_network}
  network:wikidata = {Config_wikidata}
  network:wikipedia = {Config_wikipedia}
  operator = {Config_operator}
  public_transport:version = 2
  ref = {relation_route_short_name}
  ref:network = {relation_ref_network}
  route = bus
  text_colour = {relation_route_text_color}
  to = {relation_last_name}
  type = route
  website = {Config_website}
  {platforms}
"
    level0 <- str_glue(tpl)
    dsn <- sprintf("%s/reseau_gtfs_routes_stops_%s.txt", osmDir, df[i, "ref_route"])
    write(level0, dsn)
    carp("dsn: %s", dsn)
  }
}
