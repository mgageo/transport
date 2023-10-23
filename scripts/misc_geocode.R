# <!-- coding: utf-8 -->
#
# la partie géocodage
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ==========================================================================
#
#
# géocodage direct avec https://adresse.data.gouv.fr/api
#
# https://georezo.net/forum/viewtopic.php?id=105371
#
# https://api-adresse.data.gouv.fr/search/?q=6+rue+Chateaurenault&citycode=35238
# http://demo.addok.xyz/search/?q=6+rue+Chateaurenault&citycode=35238
# http://all.demo.addok.xyz/search/?q=85+rue+de+vern&citycode=35238
# https://api-adresse.data.gouv.fr/reverse/?lat=48.563618633888&lon=-2.7583939713015
# https://demo.addok.xyz/reverse/?lat=48.563618633888&lon=-2.7583939713015
datagouv_url <- 'https://api-adresse.data.gouv.fr'
datagouv_url <- 'http://all.demo.addok.xyz'
datagouv_url <- 'https://api-adresse.data.gouv.fr'
datagouv_url <- 'https://demo.addok.xyz'
geocode_direct_datagouv <- function(adresse, force=FALSE) {
  library(RCurl)
  library(RJSONIO)
  library(urltools)
  library(tidyverse)
  carp('adresse: %s', adresse)
  dsn <- sprintf("%s/geocode_direct_datagouv.Rda", cfgDir);
  if ( file.exists(dsn) & force==FALSE) {
    load(dsn);
    if ( adresse %in% direct.df$adresse ) {
      ad <- adresse
      df <- direct.df %>%
        filter(adresse %in%  ad)
 #     View(direct.df)
 #     stop('****')
      return(invisible(df))
    }
  } else {
    carp('adresse: %s', adresse)
#    stop('***')
    direct.df <- data.frame("adresse"=character(0), "lon_lat"=character(0), "label"=character(0), stringsAsFactors=FALSE)
  }
  httpheader = c(
    Accept = "text/html",
    "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; WOW64; rv:51.0) Gecko/20100101 Firefox/51.0"
  )
#  url <- sprintf("https://api-adresse.data.gouv.fr/search/?q=%s", url_encode(as.character(adresse)))
  url <- sprintf("%s/search/?q=%s",datagouv_url, as.character(adresse))
  carp("url: %s", url)
  x <- tryCatch(
    {
      curl = getCurlHandle()
      # send query
      RCurl::getURL(URLencode(url), curl = curl, verbose = TRUE, httpheader=httpheader)
    },
    error = function(condition){
      print("erreur")
      cat(getCurlInfo(curl, "response.code")[[1]])
    }
  )
# analyse de la réponse
#    carp(x)
  ret <- RJSONIO::fromJSON(x)
#    print(ret);stop("***")
  nbfeat <- length(ret$features)
  carp("nbfeat: %s", nbfeat)
  if ( nbfeat < 1 ) {
#    carp(" direct : %s", lonlat)
    lonlat <- "***"
    label <- '***'
    city <- '***'
    citycode <- '***'
    lon <- '***'
    lat <- '***'
    importance <- '***'
    score <- '***'
  } else {
    score <- ret$features[[1]]$properties$score;
    importance <- ret$features[[1]]$properties$importance;
    label <- ret$features[[1]]$properties$name;
    city <- ret$features[[1]]$properties$city;
    citycode <- ret$features[[1]]$properties$citycode;
    lonlat <- ret$features[[1]]$geometry$coordinates
#    lonlat <- sprintf("%s %s", lonlat[1], lonlat[2])
    lon <- lonlat[1]
    lat <- lonlat[2]
  }
  df <- data.frame(adresse=adresse, citycode=citycode, city=city, lon=lon, lat=lat, label=label, importance=importance, score=score, x=x, stringsAsFactors=FALSE)
  direct.df <- rbind(direct.df, df)
  save(direct.df, file=dsn)
  return(invisible(df))
}
geocode_direct_datagouv_lire <- function() {
  dsn <- sprintf("%s/geocode_direct_datagouv.Rda", cfgDir);
  load(dsn);
  carp('dsn: %s nrow: %s', dsn, nrow(direct.df))
  return(invisible(direct.df))
}
#
geocode_search_csv_datagouv <- function(f_orig, f_dest, url = datagouv_url) {
  library(curl)
  library(RJSONIO)
  httpheader = c(
    Accept = "text/html",
    "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; WOW64; rv:51.0) Gecko/20100101 Firefox/51.0"
  )
  h <- new_handle()
  handle_setform(h,
    data = form_file(f_orig)
  )
  url <- sprintf("%s/search/csv/", url)
  req <- curl_download(url, f_dest, handle = h) %>%
    glimpse()
}
#
# géocodage direct avec nominatim
geocode_direct_nominatim <- function(adresse, countrycodes = 'FR', force = FALSE) {
  library(RCurl)
  library(jsonlite)
  library(urltools)
  library(tidyverse)
  carp('adresse: %s', adresse)
  dsn <- sprintf("%s/geocode_direct_nominatim.Rda", cfgDir);
  if ( file.exists(dsn) & force==FALSE) {
    load(dsn);
    if ( adresse %in% direct.df$adresse ) {
      return(invisible(direct.df[direct.df$adresse == adresse,]))
    }
  } else {
    carp('adresse: %s', adresse)
    direct.df <- data.frame("adresse"=character(0), "lon_lat"=character(0), "label"=character(0), stringsAsFactors=FALSE)
  }
  httpheader = c(
    Accept = "text/html",
    "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; WOW64; rv:51.0) Gecko/20100101 Firefox/51.0"
  )
  url <- sprintf("https://nominatim.openstreetmap.org/search?q=%s&countrycodes=%s&format=json&limit=1", url_encode(as.character(adresse)), countrycodes)
  carp("url: %s", url)
  tryCatch(
    {ret <- jsonlite::fromJSON(url)},
    error = function(c) return(data.frame())
  )
#  print(ret);stop("***")
  nbfeat <- length(ret)
  carp("nbfeat: %s", nbfeat)
  if ( nbfeat < 1 ) {
#    carp(" direct : %s", lonlat)
    lonlat <- "***"
    label <- '***'
    lon <- '***'
    lat <- '***'
    importance <- '***'
  } else {
    importance <- ret[1, "importance"];
    label <- ret[1, "display_name"];
    lon <- ret[1, "lon"]
    lat <- ret[1, "lat"]
  }
  df <- data.frame(adresse=adresse, lon=lon, lat=lat, label=label, importance=importance, stringsAsFactors=FALSE)
  if (countrycodes=='CH') {
#    print(df)
#    stop('***')
  }
  direct.df <- rbind(direct.df, df)
  save(direct.df, file=dsn)
  return(invisible(df))
}
geocode_reverse_v1 <- function(points, server = NULL) {
  library(RCurl)
  library(RJSONIO)
  if (is.null(server)){server <- "http://photon.komoot.de" }
  for (i in 1:length(points)) {
    lon <- points[i, 'GPSLongitude']
    lat <- points[i, 'GPSLatitude']
    url <- sprintf("%s/reverse?lon=%s&lat=%s&osm_tag=place", server, lon, lat)
    x <- tryCatch(
      {
        curl = getCurlHandle()
        # send query
        RCurl::getURL(URLencode(url), curl = curl)
      },
      error = function(condition){
        cat(getCurlInfo(curl, "response.code")[[1]])
      }
    )
# analyse de la réponse
    ret <- RJSONIO::fromJSON(x)
    nbfeat <- length(ret$features)
    print(ret);stop("***")
  }
}
#
# géocodage inverse avec http://api-adresse.data.gouv.fr
geocode_reverse_datagouv <- function(points, force = FALSE) {
  library(RCurl)
  library(RJSONIO)
  dsn <- sprintf("%s/geocode_datagouv.Rda", cfgDir);
  if ( file.exists(dsn) & force==FALSE) {
    load(dsn);
  } else {
    reverse.df <- data.frame("lon_lat"=character(0), "adresse"=character(0), stringsAsFactors=FALSE)
  }
  httpheader = c(
    Accept = "text/html",
    "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; WOW64; rv:51.0) Gecko/20100101 Firefox/51.0"
  )
#  carp(" nb : %s", nrow(points))
#  print(summary(points))
  points$adresse <- '';
  for (i in 1:nrow(points)) {
    lon <- sprintf("%0.5f", points[i, 'lon']);
    lat <- sprintf("%0.5f", points[i, 'lat'])
    lonlat <- sprintf("%s, %s", lon, lat);
    df1 <- subset(reverse.df, lon_lat == lonlat)
    if ( nrow(df1) > 0 ) {
#      View(df1);stop("***")
      points[i, 'adresse'] <- as.character(df1[1, 2]);
      next;
    }
    url <- sprintf("http://api-adresse.data.gouv.fr/reverse/?lat=%s&lon=%s", lat, lon)
    carp(url)
    x <- tryCatch(
      {
        curl = getCurlHandle()
        # send query
        RCurl::getURL(URLencode(url), curl = curl, verbose = TRUE, httpheader=httpheader)
      },
      error = function(condition){
        print("erreur")
        cat(getCurlInfo(curl, "response.code")[[1]])
      }
    )
# analyse de la réponse
#    carp(x)
    ret <- RJSONIO::fromJSON(x)
#    print(ret);stop("***")
    nbfeat <- length(ret$features)
    if ( nbfeat < 1 ) {
      carp(" lonlat : %s", lonlat)
      adrese <- "***"
    } else {
      adresse <- ret$features[[1]]$properties$label;
    }
    points[i, 'adresse'] <- adresse;
    reverse.df <- rbind(reverse.df, data.frame(lon_lat=lonlat, adresse=adresse))
  }
  points$adresse <- iconv(points$adresse, "UTF-8")
  save(reverse.df, file=dsn)
  return(invisible(points))
}
#
# géocodage inverse avec http://api-adresse.data.gouv.fr
# https://cran.r-project.org/web/packages/curl/vignettes/intro.html
# source("geo/scripts/coj.R");geocode_reverse_csv_datagouv_test()
#
geocode_reverse_csv_datagouv_test <- function() {
  csv <- 'lat,lon,name
48.670333,6.1468826,École Claude Déruet
48.6495464,6.1521676,École Gilberte Monne
48.6470103,6.2075765,École maternelle Victor Hugo
48.7277223,6.1578988,École maternelle Buffon'
  csv <- 'lat,lon,name
44.83665,-0.511508,toto'
  f_orig <- sprintf("%s/geocode_reverse.csv", cfgDir);
  F <- file(f_orig, encoding="UTF-8")
  write(csv, file=F, append = FALSE)
  close(F)
  f_reverse <- sprintf("%s/geocode_reverse_geo.csv", cfgDir);
  carp("f_reverse: %s", f_reverse)
  geocode_reverse_csv_datagouv(f_orig, f_reverse)
}
#
geocode_reverse_csv_datagouv <- function(f_orig, f_reverse, url = datagouv_url) {
  library(curl)
  library(RJSONIO)
  httpheader = c(
    Accept = "text/html",
    "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; WOW64; rv:51.0) Gecko/20100101 Firefox/51.0"
  )
  h <- new_handle()
  handle_setform(h,
    data = form_file(f_orig)
  )
  url <- sprintf("%s/reverse/csv/",datagouv_url)
  req <- curl_download(url, f_reverse, handle = h) %>%
    glimpse()
}
#
# géocodage inverse avec nominatim
geocode_reverse_nominatim <- function(points, force=FALSE) {
  library(RCurl)
  library(RJSONIO)
  dsn <- sprintf("%s/geocode_nominatim.Rda", cfgDir);
  if ( file.exists(dsn) & force==FALSE) {
    load(dsn);
  } else {
    reverse.df <- data.frame("lon_lat"=character(0), "adresse"=character(0), stringsAsFactors=FALSE)
  }
  httpheader = c(
    Accept = "text/html",
    "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; WOW64; rv:51.0) Gecko/20100101 Firefox/51.0"
  )
  carp(" nb : %s", nrow(points))
#  print(summary(points))
  points$adresse <- '';
  for (i in 1:nrow(points)) {
    lon <- sprintf("%0.5f", points[i, 'lon']);
    lat <- sprintf("%0.5f", points[i, 'lat'])
    lonlat <- sprintf("%s, %s", lon, lat);
    df1 <- subset(reverse.df, lon_lat == lonlat)
    if ( nrow(df1) > 0 ) {
#      View(df1);stop("***")
      points[i, 'adresse'] <- as.character(df1[1, 2]);
      next;
    }
    url <- sprintf("https://nominatim.openstreetmap.org/reverse?format=json&lat=%s&lon=%s&zoom=17&addressdetails=1", lat, lon)
    carp(url)
    x <- tryCatch(
      {
        curl = getCurlHandle()
        # send query
        RCurl::getURL(URLencode(url), curl = curl, verbose = TRUE, httpheader=httpheader)
      },
      error = function(condition){
        print("erreur")
        cat(getCurlInfo(curl, "response.code")[[1]])
      }
    )
# analyse de la réponse
    carp(x)
    ret <- RJSONIO::fromJSON(x)
#    carp(ret$address[1])
    nbfeat <- length(ret$features)
    adresse <- sprintf("%s, %s", ret$address[1], ret$address[2]);
    points[i, 'adresse'] <- adresse;
    reverse.df <- rbind(reverse.df, data.frame(lon_lat=lonlat, adresse=adresse))
  }
  points$adresse <- iconv(points$adresse, "UTF-8")
  save(reverse.df, file=dsn)
  return(invisible(points))
}
#
# ajout de l'adresse à un spdf
geocode_reverse_spdf <- function(spdf) {
  carp()
  spdf1 <- spTransform(spdf, CRS("+init=epsg:4326"))
  df <- as.data.frame(coordinates(spdf1))
  colnames(df) <- c("lon", "lat")
  df <- geocode_reverse_datagouv(df)
  spdf@data <- cbind(spdf@data, df)
#  print(spdf@data)
  return(invisible(spdf))
}
#
# ajout de l'adresse à un sf
geocode_reverse_sf <- function(nc) {
  carp()
  library(sf)
  df <- st_transform(nc, 4326) %>%
    mutate(lon=map_dbl(geometry, ~st_centroid(.x)[[1]]), lat=map_dbl(geometry, ~st_centroid(.x)[[2]])) %>%
    dplyr::select(lon, lat) %>%
    st_set_geometry(NULL) %>%
    glimpse()
  carp('geocodage')
  df <- geocode_reverse_nominatim(df) %>%
    print(n=20, na.print='') %>%
    glimpse()
  carp('cbind')
  nc <- st_bind_cols(nc, df) %>%
    print(n=50, na.print='') %>%
    glimpse()
#  print(spdf@data)
  return(invisible(nc))
}
#
# géocodage inverse avec openls
# https://geoservices.ign.fr/documentation/services/api-et-services-ogc/geocodage-ogc
geocode_reverse_openls <- function(df, force=FALSE) {
  carp("nb: %s", nrow(df))
  df <- df %>%
    mutate(adresse = "") %>%
    mutate(lon = sprintf("%0.5f",lon)) %>%
    mutate(lat = sprintf("%0.5f",lat)) %>%
    mutate(lonlat = sprintf("%s, %s", lon, lat))
  dsn <- sprintf("%s/geocode_reverse_openls.Rds", cfgDir);
  if ( file.exists(dsn) & force==FALSE) {
    reverse.df <- readRDS(dsn);
  } else {
    reverse.df <- data.frame("lon_lat" = character(0), "lieudit" = character(0), stringsAsFactors = FALSE)
  }
  for (i in 1:nrow(df)) {
    lonlat <- df[i, "lonlat"]
    df1 <- subset(reverse.df, lon_lat == lonlat)
    if ( nrow(df1) > 0 ) {
      lieudit <- df1[1, "lieudit"]
    } else {
      geocode_reverse_openls_point(df, i)
      lieudit <- geocode_reverse_openls_point_parse()
    }
    df[i, "lieudit"] <- lieudit
    reverse.df <- rbind(reverse.df, data.frame(lon_lat = lonlat, lieudit = lieudit))
    saveRDS(reverse.df, file = dsn)
  }
  glimpse(df)
  return(invisible(df))
}
geocode_reverse_openls_point <- function(df, i, force = FALSE) {
  library(httr)
  http_headers = c(
    Accept = "text/html",
    "Content-Type" = "application/xml",
    "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; WOW64; rv:51.0) Gecko/20100101 Firefox/51.0"
  )
  url <- "https://wxs.ign.fr/essentiels/geoportail/ols?"
  tpl <- '<?xml version="1.0" encoding="UTF-8"?><XLS xmlns:gml="http://www.opengis.net/gml" xmlns="http://www.opengis.net/xls" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" version="1.2"  xsi:schemaLocation="http://www.opengis.net/xls http://schemas.opengis.net/ols/1.2/olsAll.xsd">
<RequestHeader/><Request requestID="1" version="1.2" maximumResponses="10" methodName="GeocodeRequest">
<ReverseGeocodeRequest>
<Position>
<gml:Point><gml:pos>{{lat}} {{lon}}</gml:pos></gml:Point>
<gml:CircleByCenterPoint>
<gml:pos>{{lat}} {{lon}}</gml:pos>
<gml:radius>1000</gml:radius>
</gml:CircleByCenterPoint>
</Position>
<ReverseGeocodePreference>PositionOfInterest</ReverseGeocodePreference>
</ReverseGeocodeRequest>
</Request>
</XLS>
'
  body <- misc_dfi2tpl(df, i, tpl)
#  carp("body: %s", body)
  dsn <- sprintf("%s/geocode_reverse_openls_point.xml", varDir)
  reponse <- httr::POST(url, body = body, encode = "multipart", add_headers(http_headers), write_disk(dsn, overwrite = TRUE))
#  carp("reponse: %s", content(reponse))
}
#
# version en GET
geocode_reverse_openls_point_GET <- function(df, i, force = FALSE) {
  library(httr)
  http_headers = c(
    Accept = "text/html",
    "Content-Type" = "application/xml",
    "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; WOW64; rv:51.0) Gecko/20100101 Firefox/51.0"
  )
  url <- "https://wxs.ign.fr/essentiels/geoportail/ols?"
  tpl <- '<?xml version="1.0" encoding="UTF-8"?><XLS xmlns:gml="http://www.opengis.net/gml" xmlns="http://www.opengis.net/xls" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" version="1.2"  xsi:schemaLocation="http://www.opengis.net/xls http://schemas.opengis.net/ols/1.2/olsAll.xsd">
<RequestHeader/><Request requestID="1" version="1.2" maximumResponses="10" methodName="GeocodeRequest">
<ReverseGeocodeRequest>
<Position>
<gml:Point><gml:pos>{{lat}} {{lon}}</gml:pos></gml:Point>
<gml:CircleByCenterPoint>
<gml:pos>{{lat}} {{lon}}</gml:pos>
<gml:radius>1000</gml:radius>
</gml:CircleByCenterPoint>
</Position>
<ReverseGeocodePreference>PositionOfInterest</ReverseGeocodePreference>
</ReverseGeocodeRequest>
</Request>
</XLS>
'
  body <- misc_dfi2tpl(df, i, tpl)
  body <- str_replace_all(body, "[\\r\\n]", "")
  body <- urlEncodePath(body)
  carp("body: %s", body)
  url <- sprintf("%sxls=%s&output=json", url, body)
  dsn <- sprintf("%s/geocode_reverse_openls_point.json", varDir)
  reponse <- httr::GET(url, write_disk(dsn, overwrite = TRUE))
  carp("reponse: %s", content(reponse))
}
#
# https://urbandatapalette.com/post/2021-03-xml-dataframe-r/
# source("geo/scripts/cheveche35.R"); geocode_reverse_openls_point_parse()
geocode_reverse_openls_point_parse <- function() {
  library(xml2)
  carp()
  dsn <- sprintf("%s/geocode_reverse_openls_point.xml", varDir)
  doc <- xml2::read_xml(dsn, verbose = TRUE)
  rows <- xml2::xml_find_all(doc, "//d1:ReverseGeocodedLocation")
#  carp("rows length: %s", length(rows))
  df1 <- data.frame()
  for (row in rows) {
    items <- xml2::xml_find_all(row, ".//d1:Address")
    nodes_name <- xml_name(xml_children(items))
    nodes_attr <- xml_attr(xml_children(items), "type")
    nodes_text <- trimws(xml_text(xml_children(items)))
    df <- data.frame(nodes_name, nodes_attr, nodes_text) %>%
      filter(nodes_name == "Place") %>%
      pivot_wider(names_from = nodes_attr, values_from = nodes_text)
#        <xlsext:ExtendedGeocodeMatchCode>Toponym</xlsext:ExtendedGeocodeMatchCode>
#        <SearchCentreDistance value="163.06"/>
    code <- xml2::xml_find_first(row, ".//xlsext:ExtendedGeocodeMatchCode") %>%
      xml_text()
    distance <- xml2::xml_find_first(row, ".//d1:SearchCentreDistance") %>%
      xml_attr("value")
    df$code <- code
    df$distance <- distance
    df1 <- bind_rows(df1, df)
#    stop("*****")
  }
  df1 <- df1 %>%
    filter(code == "Toponym") %>%
    filter(grepl("Lieu", Nature))
  lieudit <- sprintf("%s / %s", df1[1, "Municipality"], df1[1, "Commune"]) %>%
    glimpse()
  return(invisible(lieudit))
}
#
# géocodage direct avec ign geoservices
geocode_direct_geoservices <- function(adresse, force = FALSE) {
  library(httr2)
  library(urltools)
  library(sf)
  library(tidyverse)
  carp("adresse: %s", adresse)
  httpheader = c(
    Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8",
    "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:109.0) Gecko/20100101 Firefox/117.0"
  )
  geoservices_url <- "https://wxs.ign.fr/essentiels/geoportail/geocodage/rest/0.1/search?index=poi&q="
  url <- sprintf("%s%s", geoservices_url, url_encode(as.character(adresse)))
  carp("url: %s", url)
  req <- httr2::request(url) %>%
    req_user_agent(httpheader["User-Agent"]) %>%
#    req_dry_run() %>%
    req_perform(verbosity = 0)
  response <- httr2::resp_body_string(req)
  dsn <- sprintf("%s/geocode_direct_geoservices.json", varDir)
  write(response, dsn, append = FALSE)
  nc <- st_read(dsn, stringsAsFactors=FALSE, quiet = TRUE)
  misc_print(nc)
  return(invisible(nc))
}