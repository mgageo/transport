# <!-- coding: utf-8 -->
#
# fonctions génériques
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
#
# https://quanti.hypotheses.org/1813
#
## quelques options générales ------------------------------------
#
options("encoding" = "UTF-8")
par(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0), mai = c(0, 0, 0, 0))
options(stringsAsFactors = FALSE)
# le timeout sur le téléchargement via download.file
options(timeout=120)
options(scipen=999) # pour désactiver l'écriture scientifique des nombres
#options(OutDec= ",")
# https://stackoverflow.com/questions/62140483/how-to-interpret-dplyr-message-summarise-regrouping-output-by-x-override
options(dplyr.summarise.inform = F)

Log <- function(msg) {
  print(msg)
  flush.console()
}
#
# pour libérer la mémoire
misc_free <- function() {
  memory.size(max=T)
  memory.size(max=F)
  rm(list = ls())
  rm(list = ls(all.names = TRUE))
  gc()
#  .rs.restartR()
  memory.size(max=T)
  memory.size(max=F)
}

#
# https://stackoverflow.com/questions/6979917/how-to-unload-a-package-without-restarting-r
detach_package <- function(pkg, character.only = FALSE) {
  if(!character.only) {
    pkg <- deparse(substitute(pkg))
  }
  search_item <- paste("package", pkg, sep = ":")
  while(search_item %in% search()) {
    detach(search_item, unload = TRUE, character.only = TRUE)
  }
}
#
## comme en perl
#
# message à la console avec indication de l'origine
carp <- function(...) {
  curcall <- as.character(deparse(sys.call(-1)))
  carp_call <<- gsub('\\(.*$', '', curcall)
  arguments <- as.list(match.call(expand.dots=FALSE))[-1]
  arguments <- arguments$...
  fncall <- deparse(sys.calls()[[sys.nframe()-1]])
  msg <- sprintf('%s %s', format(Sys.time(), "%X"), curcall)
  if (length(arguments) >= 1 ) {
    msg <- sprintf(...)
    msg <- sprintf("%s %s", curcall, msg)
  }
  print(msg)
  flush.console()
}
#
# message à la console
Carp <- function(...) {
  msg <- sprintf(...)
  print(msg)
  flush.console()
}
confess <- function(...) {
  arguments <- as.list(match.call(expand.dots=FALSE))[-1]
  arguments <- arguments$...
  msg <- ""
  if (length(arguments) >= 1 ) {
    msg <- sprintf(...)
  }
  print(msg)
  flush.console()
  stop('confess')
}
die <- function(...) {
  curcall <- as.character(deparse(sys.call(-1)))
  Curcall <<- gsub('\\(.*$', '', curcall)
  arguments <- as.list(match.call(expand.dots=FALSE))[-1]
  arguments <- arguments$...
  fncall <- deparse(sys.calls()[[sys.nframe()-1]])
  msg <- sprintf('%s %s', format(Sys.time(), "%X"), curcall)
  if (length(arguments) >= 1 ) {
    msg <- sprintf(...)
    msg <- sprintf("%s %s", curcall, msg)
  }
  print(msg)
  flush.console()
  stop('die')
}
#
## fonctions "système de fichiers"
# ===============================
# options("encoding" = "UTF-8")
# getOption("encoding")
# options("encoding" = "native.enc")
# Encoding(t2[,"a"]) <- "UTF-16"
writeLignes_v1 <- function(fic, lignes, encoding="native.enc") {
  file.create(fic, encoding="UTF-8")
  fileConn <- file(fic)
  if ( encoding == "native.enc" ) {
    msg <- iconv(lignes, to="UTF-8")
  } else {
    msg <- lignes
  }
  writeLines(msg, fileConn, useBytes=TRUE)
  close(fileConn)
  carp(" fic: %s", fic)
}
writeLignes <- function(fic, lignes, encoding="native.enc") {
  TEX <- file(fic, encoding="UTF-8")
  write(lignes, file = TEX, append = FALSE)
  carp(" fic: %s", fic)
}
#
# fonctions pour les fichiers compressés
#
# fonction générique d'extraction
extrait <- function(target_file, target_dir, overwrite=TRUE) {
  carp("target_file: %s", target_file)
  carp("target_dir: %s", target_dir)
  if ( ! file.exists(target_dir) ) {
    dir.create(target_dir, showWarnings = FALSE, recursive = TRUE)
  }
  ext <- sub('^.*\\.','', target_file, perl=TRUE)
  carp("ext:%s", ext)
  switch(ext,
    '7z' = {
      un7z(target_file, exdir=target_dir, overwrite=TRUE)
    },
    'zip' = {
      unzip(target_file, exdir=target_dir, overwrite=TRUE)
    },
    'tar' = {
      untar(target_file, exdir=target_dir)
    }
  )
}
# C:/Program Files/7-Zip/7z.exe
un7z <- function(target_file, exdir, overwrite=TRUE) {
  cmd <-sprintf('7z e -o %s %s', exdir, target_file)
  system(cmd)
}
#
# fonction pour dataframe
# ========================
#
# sauvegarde d'un dataframe en csv
df_ecrire <- function(df,f) {
  Log(sprintf("df_ecrire() f:%s",f))
  if ( ('point' %in% colnames(df)) ) {
    dfTmp <- subset(df, select = -(point) )
    Log(sprintf("df_ecrire() -point"))
  } else {
#    Log(sprintf("df_ecrire() #point"))
    dfTmp <- df
  }
  write.table(dfTmp, file = f, sep = ";", row.names=FALSE, col.names = TRUE,qmethod = "double", quote = FALSE, fileEncoding="utf8")
}
#
# version avec java
df_ecrirex_java <- function(df, dsn=FALSE, suffixe="") {
  library(xlsx)
  df <- as.data.frame(df)
  if ( dsn == FALSE ) {
    curcall <- as.character(deparse(sys.call(-1)))
    curcall <- gsub('\\(.*$', '', curcall)
    if ( suffixe != "" ) {
      suffixe <- sprintf("_%s", suffixe)
    }
    dsn <- sprintf("%s/%s%s.csv", texDir, curcall, suffixe)
  }
  if ( ('_point' %in% colnames(df)) ) {
    dfTmp <- subset(df, select = -(point) )
    Log(sprintf("df_ecrirex() -point"))
  } else {
#    Log(sprintf("df_ecrirex() #point"))
    dfTmp <- df
  }
  write.table(dfTmp, file = dsn, sep = ";", row.names=FALSE, col.names = TRUE, qmethod = "double", quote = FALSE, fileEncoding="utf8")
  dsn <- gsub("csv$", "xlsx", dsn)
  write.xlsx(dfTmp, dsn, row.names=FALSE)
  carp("df_ecrirex() dsn:%s", dsn)
}
df_ecrirex <- function(df, dsn=FALSE, suffixe="") {
  library(writexl)
  df <- as.data.frame(df)
  if ( dsn == FALSE ) {
    curcall <- as.character(deparse(sys.call(-1)))
    curcall <- gsub('\\(.*$', '', curcall)
    if ( suffixe != "" ) {
      suffixe <- sprintf("_%s", suffixe)
    }
    dsn <- sprintf("%s/%s%s.csv", texDir, curcall, suffixe)
  }
  if ( ('_point' %in% colnames(df)) ) {
    dfTmp <- subset(df, select = -(point) )
    Log(sprintf("df_ecrirex() -point"))
  } else {
#    Log(sprintf("df_ecrirex() #point"))
    dfTmp <- df
  }
  write.table(dfTmp, file = dsn, sep = ";", row.names=FALSE, col.names = TRUE, qmethod = "double", quote = FALSE, fileEncoding="utf8")
  dsn <- gsub("csv$", "xlsx", dsn)
  write_xlsx(dfTmp, dsn)
  carp("df_ecrirex() dsn:%s", dsn)
}
df_lire <- function(f) {
  carp("f:%s",f)
  df <- read.table(f, header=TRUE, sep=";", blank.lines.skip = TRUE, stringsAsFactors=FALSE, quote="", encoding="UTF-8")
  return(df)
}
df_lirex_v0 <- function(dsn) {
  library(xlsx)
  carp("f:%s", dsn)
  df <- read.xlsx(dsn, 1, header=TRUE, colClasses=NA)
  return(df)
}
df_lirex <- function(dsn) {
  library(readxl)
  carp("f:%s", dsn)
  df <- read_excel(dsn, col_names = TRUE)
  return(df)
}
df_lirexu <- function(dsn) {
  library(xlsx)
  Log(sprintf("df_lirexu f:%s", dsn))
  df <- read.xlsx(dsn, 1, header=TRUE, colClasses=NA, encoding="UTF-8", stringsAsFactors=FALSE)
  return(df)
}
zip_dl <- function(les_url, base="") {
  for(i in 1:nrow(les_url) ) {
    url <- les_url$url[i]
    zip <-sub(".*/", "", url)
    zip <- les_url$libelle[i]
    url <- sprintf("%s%s", base, url)
    Log(sprintf("zip_dl() url:%s %s %s", url, zip, les_url$libelle[i]))
    file <- sprintf("%s/%s", dl_dir, zip)
    if( ! file.exists(file) ) {
      Log(sprintf("zip_dl() dl file:%s", file ))
      download.file(url, file, quiet = FALSE, mode = "wb")
    }
    if( ! file.exists(file) ) {
      Log(sprintf("zip_dl() absent : file:%s", file))
	    stop()
    }
    zip_dir  <-sub("\\..*$", "", zip)
    zip_dir <- sprintf("%s/%s", dl_dir, zip_dir)
    if( ! file.exists(zip_dir) ) {
      Log(sprintf("zip_dl() absent : zip_dir:%s", zip_dir))
      unzip(file, overwrite = TRUE,junkpaths = TRUE, exdir = zip_dir, unzip = "internal", setTimes = TRUE)
    } else {
      Log(sprintf("zip_dl() present : zip_dir:%s", zip_dir))
    }
    files <- list.files(zip_dir, pattern = "\\.shp$", full.names = TRUE, ignore.case = TRUE)
    if ( length(files) != 1) {
      stop("zip_dl() .shp")
      next;
    }
    dsn <- files[1]
    layers <- ogrListLayers(dsn)
    Log(sprintf("zip_dl() layers:%s", layers))
#    Log(sprintf("inpn() info:%s", OGRSpatialRef(dsn,layers)))
    sp <- readOGR(dsn,layers)
    print(summary(sp))
  }
}
especes_camel <- function(df) {
  df$espece <- gsub(",.*", "", df$TAXO_VERNACUL)
  df$espece_c <- camel2(df$espece)
  df <- df[with(df, order(espece_c)), ]
}
# http://stackoverflow.com/questions/11672050/how-to-convert-not-camel-case-to-camelcase-in-r
camel2 <- function(x) {
  x <- iconv(x, to='ASCII//TRANSLIT')
  gsub("(^|[^[:alnum:]])([[:alnum:]])", "\\U\\2", x, perl = TRUE)
}
camel3 <- function(x) {
  x <- iconv(x, to='ASCII//TRANSLIT')
  x <- gsub("[^[:alnum:]]", " ", x, perl = TRUE)
  x <- tolower(x)
}
# http://stackoverflow.com/questions/11672050/how-to-convert-not-camel-case-to-camelcase-in-r
camel4 <- function(x) {
  x <- iconv(x, from='UTF-8', to='ASCII//TRANSLIT')
  x <- gsub("\\s+", " ", x, perl = TRUE)
  x <- gsub("Oe", "O", x, perl = TRUE)
  x <- gsub("oe", "o", x, perl = TRUE)
  gsub("(^|[^[:alnum:]])([[:alnum:]])", "\\U\\2", x, perl = TRUE)
}
camel5 <- function(x) {
  x <- iconv(x, from='UTF-8', to='ASCII//TRANSLIT')
  x <- gsub("\\-", " ", x, perl = TRUE)
  x <- gsub("\\.", " ", x, perl = TRUE)
  x <- gsub("\\(", " ", x, perl = TRUE)
  x <- gsub("\\)", " ", x, perl = TRUE)
  x <- gsub("\\s+", " ", x, perl = TRUE)
  x <- gsub("\\s$", "", x, perl = TRUE)
  gsub("(^|[^[:alnum:]])([[:alnum:]])", "\\U\\2", x, perl = TRUE)
}
# https://stackoverflow.com/questions/952275/regex-group-capture-in-r-with-multiple-capture-groups
regexpr_perl <- function(expr, str) {
  match <- regexpr(expr, str, perl=T)
  matches <- character(0)
  if (attr(match, 'match.length') >= 0) {
    capture_start <- attr(match, 'capture.start')
    capture_length <- attr(match, 'capture.length')
    total_matches <- 1 + length(capture_start)
    matches <- character(total_matches)
    matches[1] <- substr(str, match, match + attr(match, 'match.length') - 1)
    if (length(capture_start) > 1) {
      for (i in 1:length(capture_start)) {
        matches[i + 1] <- substr(str, capture_start[[i]], capture_start[[i]] + capture_length[[i]] - 1)
      }
    }
  }
  matches
}
# http://www.dataanalytics.org.uk/Data%20Analysis/TipsAndTricks/TTR-20150531.htm
## Transparent colors
## Mark Gardener 2015
## www.dataanalytics.org.uk
misc_col <- function(color, percent = 100, name = NULL) {
#	  color = color name
#	percent = % transparency
#	   name = an optional name for the color

## Get RGB values for named color
  rgb.val <- col2rgb(color)
#  print(sprintf("misc_col() color:%s %d %d %d", color, rgb.val[1], rgb.val[2], rgb.val[3]))
## Make new color using input color as base and alpha set by transparency
  col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
#               alpha = (100-percent)*255/100,
               alpha = percent,
               names = name)
#  print(t.col)
#  t.col = rgb(255,0,0, max=255, alpha = 100)
  return(col)
}
# http://www.dataanalytics.org.uk/Data%20Analysis/TipsAndTricks/TTR-20150531.htm
## Transparent colors
## Mark Gardener 2015
## www.dataanalytics.org.uk
t_col <- function(color, percent = 50, name = NULL) {

#	  color = color name
#	percent = % transparency
#	   name = an optional name for the color

## Get RGB values for named color
  rgb.val <- col2rgb(color)
  print(sprintf("t_col() color:%s %d %d %d", color, rgb.val[1], rgb.val[2], rgb.val[3]))
## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
#               alpha = (100-percent)*255/100,
               alpha = 60,
               names = name)
#  print(t.col)
#  t.col = rgb(255,0,0, max=255, alpha = 100)
  return(t.col)
}
add.alpha <- function(col, alpha=0.2){
  apply(sapply(col, col2rgb)/255, 2, function(x) rgb(x[1], x[2], x[3], alpha=alpha))
}
#
# https://www.earthdatascience.org/courses/earth-analytics/multispectral-remote-sensing-modis/refine-plots-report/
# jongle entre les différents formats d'image
#
plotImg <- function(img, alpha = 255, maxpixels = 50000000, new = TRUE, png = FALSE) {
  ncols <<- img@ncols
  nrows <<- img@nrows
  carp('ncols: %s nrows:%s', img@ncols, img@nrows )
  if( new==TRUE & png==FALSE) {
    dev.new( width = img@ncols, height = img@nrows, units = "px", pointsize = 12)
  }
  if(png!=FALSE) {
    png(png, width=img@ncols, height=img@nrows)
  }
  par(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0), mai = c(0, 0, 0, 0))
#  show(img)
  if ( is(img, 'RasterBrick') ) {
    carp('rasterbrick nlayers:%s', nlayers(img) )
    if(nlayers(img) < 3 ) {
      plotImgGray(img, alpha=alpha, maxpixels=maxpixels, new=FALSE)
    } else {
      plotRGB(img, axes=FALSE, add=FALSE, maxpixels=maxpixels, alpha=alpha)
    }
  }
  if ( is(img, 'RasterLayer') ) {
    carp('rasterlayer nlayers:%s', nlayers(img) )
#    plot(img, axes=FALSE, add=FALSE, maxpixels=maxpixels, alpha=alpha, legend=FALSE)
    if(nlayers(img) == 1 ) {
#      plotImgGray(img, alpha=alpha, maxpixels=maxpixels, new=FALSE)
#      par(xpd = TRUE)
      plot(img, axes=FALSE, add=FALSE, maxpixels=maxpixels, alpha=alpha, legend=FALSE, box = FALSE)
    } else {
      plot(img, axes=FALSE, add=FALSE, maxpixels=maxpixels, alpha=alpha, legend=FALSE)
    }
  }
  setSizes()
#  stop('****')
}
#
# image en gris
# en direct de https://www.neonscience.org/dc-multiband-rasters-r
plotImgGray <- function(img, alpha=120, maxpixels=5000000, new=TRUE, png=FALSE) {
  carp('png: %s', png)
  alpha <- alpha / 255
  if( new==TRUE & png==FALSE) {
    dev.new( width = img@ncols, height = img@nrows, units = "px", pointsize = 12)
  }
  if(png!=FALSE) {
    png(png, width=img@ncols, height=img@nrows)
  }
  par(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0), mai = c(0, 0, 0, 0))
#  show(img)
# create a grayscale color palette to use for the image.
  grayscale_colors <- gray.colors(100,            # number of different color levels
                                  start = 0.0,    # how black (0) to go
                                  end = 1.0,      # how white (1) to go
                                  gamma = 2.2,    # correction between how a digital
                                  # camera sees the world and how human eyes see it
                                  alpha = alpha)   #Null=colors are not transparent
  image(img, col=grayscale_colors, axes=FALSE, add=FALSE, xlab = NA, ylab = NA, maxpixels=maxpixels)
#  stop('***')
  return()
  if ( is(img, 'RasterBrick') ) {
    plotRGB(img, axes=FALSE, add=FALSE, maxpixels=maxpixels, alpha=alpha)
  }
  if ( is(img, 'RasterLayer') ) {
    plot(img, col=grayscale_colors, axes=FALSE, add=FALSE, maxpixels=maxpixels, alpha=alpha)
  }
}
# autre version https://www.johndcook.com/blog/2009/08/24/algorithms-convert-color-grayscale/
plotImgGray2 <- function(img, alpha=255, maxpixels=500000) {
  carp()
  dev.new( width = img@ncols, height = img@nrows, units = "px", pointsize = 12)
  par(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0), mai = c(0, 0, 0, 0))
  color.values <- getValues(img)
  bw.values <- color.values[,1]*0.21 + color.values[,1]*0.72 + color.values[,1]*0.07
}
# autre version https://github.com/dahtah/imager/blob/master/R/conversions.R
plotImgGray3 <- function(img, alpha=255, maxpixels=500000) {
  carp()
  library(imager)
  dev.new( width = img@ncols, height = img@nrows, units = "px", pointsize = 12)
  par(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0), mai = c(0, 0, 0, 0))
  img <- grayscale(img, method = "Luma", drop = TRUE)
  plot(img, col=grayscale_colors, axes=FALSE, add=FALSE, maxpixels=maxpixels, alpha=alpha)
}
# source("geo/scripts/onc35.R");test_gray();
test_gray <- function() {
  carp()
  library(raster)
  dsn <- 'D:/bvi35/CouchesGci35/ogc/gci35_carte.tif'
  img <- brick(dsn)
  plotImgGray3(img)
}
makeTransparent = function(..., alpha=0.5) {
  if(alpha<0 | alpha>1) stop("alpha must be between 0 and 1")
  alpha = floor(255*alpha)
  newColor = col2rgb(col=unlist(list(...)), alpha=FALSE)
  .makeTransparent = function(col, alpha) {
    rgb(red=col[1], green=col[2], blue=col[3], alpha=alpha, maxColorValue=255)
  }
  newColor = apply(newColor, 2, .makeTransparent, alpha=alpha)
  return(newColor)
}
couleurs_palette <- function(couleurs) {
  plot.new()
  xref <- 0
  yref <- 0
  width <- 20
  height <- 10
  delta <- 2
  nb <- length(couleurs)
  hauteur <- (height + delta)* (nb)
  carp('hauteur: %d', hauteur)
  plot(c(0, 40), c(0, hauteur), type = "n", xlab = "", ylab = "", axes=FALSE, frame.plot=TRUE)
  for (i in 1:nb){
    rect(xref, yref, xref + width, yref + height, col = couleurs[i])
    text(xref + width + delta , yref + height / 2, labels=couleurs[i], adj = c(0,0.5))
    yref <- yref + height + delta
  }
}
#
# détermination de la taille des polices, lignes
setSizes <- function() {
  x1 <- par()$usr[1]
  x2 <- par()$usr[2]
  y1 <- par()$usr[3]
  y2 <- par()$usr[4]
  xextent <- x2 - x1
  yextent <- y2 - y1
#  lgd <- legend(x = mean(c(xmin,xmax)), y =  mean(c(ymin,ymax)), c("Your data name"), pch = c(20), col = c("black"), plot = F)
# Add legend in the lower right corner:
#  legend(x = xmax - lgd$rect$w, y =  ymin + lgd$rect$h, c("Your data name"), pch = c(20), col = c("black"), plot = T)
  cexH1 <<- round(xextent / 300, 1)
  print(sprintf("setSizes() xextent:%s yextent:%s cexH1:%s", xextent, yextent, cexH1))
}
#
# sauvegarde en fichier pdf du graphique ggplot
ggpdf <- function(plot = last_plot(), suffixe='', dossier='',  width=0, height=0) {
  if ( width == 0 ) {
    width <- par("din")[1]
    height <- par("din")[2]
  }
#  stop("****")
  curcall <- as.character(deparse(sys.call(-1)))
  curcall <- gsub('\\(.*$', '', curcall)
  if ( suffixe != "" ) {
    suffixe <- sprintf("_%s", suffixe)
  }
  if ( dossier != "" ) {
    dossier <- sprintf("/%s", dossier)
  }
  dsn <- sprintf("%s%s/%s%s.pdf", texDir, dossier, curcall, suffixe)
  print(sprintf("%s() dsn : %s", curcall, dsn))
  ggsave(dsn, width=width, height=height)
#  ggsave(dsn, plot=plot)
}
ggpdf2 <- function(p, suffixe="", dossier='', width=7, height=7) {
#  library(cowplot)
  curcall <- as.character(deparse(sys.call(-1)))
  curcall <- gsub('\\(.*$', '', curcall)
  if ( suffixe != "" ) {
    suffixe <- sprintf("_%s", suffixe)
  }
  if ( dossier != "" ) {
    dossier <- sprintf("/%s", dossier)
  }
  dsn <- sprintf("%s%s/%s%s.pdf", texDir, dossier, curcall, suffixe)
  print(sprintf("%s() dsn : %s", curcall, dsn))
  ggsave(dsn, p, width=width, height=height)
}
#
# sauvegarde en fichier pdf d'un graphique
dev2pdf <- function(dsn='', suffixe='', dossier='', w=0, h=0) {
  carp()
  if ( w == 0 ) {
    w <- par("din")[1]
    h <- par("din")[2]
  }
  curcall <- as.character(deparse(sys.call(-1)))
  curcall <- gsub('\\(.*$', '', curcall)
  if ( suffixe != "" ) {
    suffixe <- sprintf("_%s", suffixe)
  }
  if ( dossier != "" ) {
    dossier <- sprintf("/%s", dossier)
  }
  if ( dsn == '' ) {
    dsn <- sprintf("%s%s/%s%s.pdf", texDir, dossier, curcall, suffixe)
  }
  dev.copy(pdf, dsn, width=w, height=h)
  invisible(dev.off())
  carp(" dsn: %s", dsn)
}
#
# sauvegarde en fichier png d'un graphique
dev2png <- function(dsn='', suffixe="", arbo="", w=0, h=0) {
  if ( w==0 ) {
    w <- par("din")[1]
    h <- par("din")[2]
  }
  curcall <- as.character(deparse(sys.call(-1)))
  curcall <- gsub('\\(.*$', '', curcall)
  if ( suffixe != "" ) {
    suffixe <- sprintf("_%s", suffixe)
  }
  if ( dsn == '' ) {
    dsn <- sprintf("%s%s/%s%s.png", texDir, arbo, curcall, suffixe)
  }
  dev.copy(png, dsn, width=w, height=h)
  invisible(dev.off())
  carp(" dsn: %s", dsn)
}
#
# sauvegarde en format pdf
txt2pdf <- function(txt, dsn=FALSE, suffixe="", dossier="") {
  if ( dsn == FALSE ) {
    curcall <- as.character(deparse(sys.call(-1)))
    curcall <- gsub('\\(.*$', '', curcall)
    if ( suffixe != "" ) {
      suffixe <- sprintf("_%s", suffixe)
    }
    if ( dossier != "" ) {
      dossier <- sprintf("%s/", dossier)
    }
    dsn <- sprintf("%s/%s%s%s.pdf", texDir, dossier, curcall, suffixe)
  } else {
    if ( suffixe != "" ) {
      suffixe <- sprintf("_%s", suffixe)
    }
    if ( dossier != "" ) {
      dossier <- sprintf("%s/", dossier)
    }
    dsn <- sprintf("%s/%s%s%s.pdf", texDir, dossier, dsn, suffixe)
  }
  pdf(dsn, width=6, height=3)
  plot.new()
  par(mar=c(0,0,0,0), oma=c(0,0,0,0))
  text(.1, .5, txt, font=4, cex=1, col="#F48024")
  dev.off()
  carp("dsn: %s", dsn)
}
#
# sauvegarde en format tex
txt2tex <- function(txt, dsn=FALSE, suffixe="", dossier="") {
  if ( dsn == FALSE ) {
    curcall <- as.character(deparse(sys.call(-1)))
    curcall <- gsub('\\(.*$', '', curcall)
    if ( suffixe != "" ) {
      suffixe <- sprintf("_%s", suffixe)
    }
    if ( dossier != "" ) {
      dossier <- sprintf("%s/", dossier)
    }
    dsn <- sprintf("%s/%s%s%s.tex", texDir, dossier, curcall, suffixe)
  } else {
    if ( suffixe != "" ) {
      suffixe <- sprintf("_%s", suffixe)
    }
    if ( dossier != "" ) {
      dossier <- sprintf("%s/", dossier)
    }
    dsn <- sprintf("%s/%s%s%s.tex", texDir, dossier, dsn, suffixe)
  }
  write(txt, dsn)
  carp("dsn: %s", dsn)
}
#
# sauvegarde en fichier pdf d'un graphique
tex2fic_v1 <- function(tex, suffixe="") {
  curcall <- as.character(deparse(sys.call(-1)))
  curcall <- gsub('\\(.*$', '', curcall)
  if ( suffixe != "" ) {
    suffixe <- sprintf("_%s", suffixe)
  }
  dsn <- sprintf("%s\\%s%s.tex", texDir, curcall, suffixe)
#  Encoding(tex) <- "UTF-8"
  tex <- paste(tex, collapse="\n")
  tex <- iconv(tex, to='UTF-8')
  writeBin(tex, dsn)
  carp(" dsn: %s", dsn)
}
#
# sauvegarde d'un texte pour utilsation en latex
tex2fic <- function(tex, dsn=FALSE, suffixe="", dossier="") {
  if ( dsn == FALSE ) {
    curcall <- as.character(deparse(sys.call(-1)))
    curcall <- gsub('\\(.*$', '', curcall)
    if ( suffixe != "" ) {
      suffixe <- sprintf("_%s", suffixe)
    }
    if ( dossier != "" ) {
      dossier <- sprintf("%s/", dossier)
    }
    dsn <- sprintf("%s/%s%s%s.tex", texDir, dossier, curcall, suffixe)
  }
#  Encoding(tex) <- "UTF-8"
#  tex <- iconv(tex, to='UTF-8')
  write(tex, dsn)
  carp(" dsn: %s", dsn)
}
#
# l'export avec version java
export_df2xlsx_java <- function(df, dsn=FALSE, suffixe="", dossier="", onglet='export') {
  library(xlsx)
  if ( dsn == FALSE ) {
    curcall <- as.character(deparse(sys.call(-1)))
    curcall <- gsub('\\(.*$', '', curcall)
    if ( suffixe != "" ) {
      suffixe <- sprintf("_%s", suffixe)
    }
    if ( dossier != "" ) {
      dossier <- sprintf("%s/", dossier)
    }
    dsn <- sprintf("%s/%s%s%s.xlsx", texDir, dossier, curcall, suffixe)
  }
  write.xlsx(df, file=dsn, append=FALSE, sheetName=onglet, showNA=FALSE)
  carp("dsn: %s", dsn)
}
export_df2xlsx <- function(df, dsn=FALSE, suffixe="", dossier="", onglet='export') {
  library(writexl)
#  stop('****')
  if ( dsn == FALSE ) {
    curcall <- as.character(deparse(sys.call(-1)))
    curcall <- gsub('\\(.*$', '', curcall)
    if ( suffixe != "" ) {
      suffixe <- sprintf("_%s", suffixe)
    }
    if ( dossier != "" ) {
      dossier <- sprintf("%s/", dossier)
    }
    dsn <- sprintf("%s/%s%s%s.xlsx", texDir, dossier, curcall, suffixe)
  }
  writexl::write_xlsx(df, path=dsn)
  carp("dsn: %s", dsn)
}
export_df2xlsx <- function(df, dsn=FALSE, suffixe="", dossier="", onglet='export') {
  library(rio)
#  stop('****')
  if ( dsn == FALSE ) {
    curcall <- as.character(deparse(sys.call(-1)))
    curcall <- gsub('\\(.*$', '', curcall)
    if ( suffixe != "" ) {
      suffixe <- sprintf("_%s", suffixe)
    }
    if ( dossier != "" ) {
      dossier <- sprintf("%s/", dossier)
    }
    dsn <- sprintf("%s/%s%s%s.xlsx", texDir, dossier, curcall, suffixe)
  }
  rio::export(df, file=dsn)
  carp("dsn: %s", dsn)
}
#
# sauvegarde en format rds
sauve_rds <- function(objet, dsn=FALSE, suffixe="", dossier="") {
  if ( dsn == FALSE ) {
    curcall <- as.character(deparse(sys.call(-1)))
    curcall <- gsub('\\(.*$', '', curcall)
    if ( suffixe != "" ) {
      suffixe <- sprintf("_%s", suffixe)
    }
    if ( dossier != "" ) {
      dossier <- sprintf("%s/", dossier)
    }
    dsn <- sprintf("%s/%s%s%s.Rds", cfgDir, dossier, curcall, suffixe)
  } else {
    if ( suffixe != "" ) {
      suffixe <- sprintf("_%s", suffixe)
    }
    if ( dossier != "" ) {
      dossier <- sprintf("%s/", dossier)
    }
    dsn <- sprintf("%s/%s%s%s.Rds", cfgDir, dossier, dsn, suffixe)
  }
  saveRDS(objet, file=dsn)
  carp("dsn: %s", dsn)
}
# lecture en format rds
lire_rds <- function(dsn=FALSE, suffixe="", dossier="") {
  if ( dossier != "" ) {
    dossier <- sprintf("%s/", dossier)
  }
  dsn <- sprintf("%s/%s%s%s.Rds", cfgDir, dossier, dsn, suffixe)
  carp("dsn: %s", dsn)
  object <- readRDS(dsn)
  return(invisible(object))
}
#
# template du pauvre
misc_list2tpl <- function(foo, tpl) {
  for(i in seq_along(foo)) {
    val <- foo[[i]]
    v <- names(foo)[i]
    carp("v: %s=>%s", v, val)
    re <- paste0("\\{\\{", v, "\\}\\}")
    tpl <- gsub(re, val, tpl, perl = TRUE)
  }
  return(invisible(tpl))
}
#
# template du pauvre
misc_dfi2tpl <- function(df, i, tpl) {
  for ( v in colnames(df) ) {
    if( is.na(df[i, v]) ) {
      val <- ''
    } else {
      val <- as.character(df[i, v])
    }
    re <- paste0("\\{\\{", v, "\\}\\}")
#    carp("re: %s", re)
    tpl <-  gsub(re, val, tpl, perl = TRUE)
  }
  return(invisible(tpl))
}
misc_df2tpl <- function(df, tpl) {
  library(stringr)
  t <- c()
  df$id <- -1:-nrow(df)
  for ( i in 1:nrow(df) ) {
    tpli <-  misc_dfi2tpl(df, i, tpl)
    t <- c(t, tpli)
#    print(tpli); stop("****")
  }
  return(invisible(t))
}