# <!-- coding: utf-8 -->
#
# quelques fonctions pour générer les fichiers tex
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
#
#
# les messages de warnings de tinytex
# options(tinytex.latexmk.warning = FALSE)
# la partie pour tex
tex_les <- function(df, suffixe="", dossier="images") {
  require(cowplot)
  df[is.na(df)] <- ""
  for(i in 1:nrow(df) ) {
    fonction <- df$fonction[i]
    if ( grepl('^#', fonction) ) {
      next
    }
    if ( grepl('^!', fonction) ) {
      break
    }
    tex_fonction(fonction, df$mode[i], df$args[i], suffixe, dossier)
  }
}
tex_fonction <- function(e, mode="carte", args = "", suffixe="", dossier="images") {
  library(stringr)
  print(sprintf("tex_fonction() e: %s mode: %s args: %s", e, mode, args))
  outputFic <<- sprintf("%s/%s/%s.pdf", texDir, dossier, e)
  if ( suffixe != "" ) {
    outputFic <<- sprintf("%s/%s/%s%s.pdf", texDir, dossier, e, suffixe)
    a <- list(args)
  }
  fonction <- sprintf("%s_tex", e)
  if( ! exists(fonction, mode = "function") ) {
    fonction <- e
  }
  a <- list()
  if ( args != "" ) {
    outputFic <<- sprintf("%s\\%s\\%s_%s%s.pdf", texDir, dossier, e, args, suffixe)
    a <- list(args)
  }
  w <- 4
  h <- 4
#  mode <- "gg(4,8)"
  r <- "^(\\w+)\\((\\d+),(\\d+)\\)"
  m <- str_match_all(mode, r)
  m <- m[[1]]
#  print(class(m))
  if (nrow(m) > 0) {
    mode <- m[1,2]
    w <- m[1,3]
    h <- m[1,4]
  }

  if ( interactive() ) {
    switch(mode,
      "carte" = {
        dev.new(width = w, height = h, units = "px")
      },
      "ggplot" = {
        pdf(outputFic, width = w, height = h)
      },
      "plotgg" = {
        dev.new(width = w, height = h)
      },
      "print" = {
        dev.new()
      },
      {
        print(sprintf("tex_fonction() **** %s", mode))
      }
    )
  } else {
    switch(mode,
      "carte" = {
        outputFic <<- sub("pdf$", "png", outputFic)
        png(outputFic, width = ncols, height = nrows)
      },
      "ggplot" = {
        pdf(outputFic, width = w, height = h)
      },
      "plotgg" = {
        pdf(outputFic, width = w, height = h)
      },
      "print" = {
        pdf(outputFic)
      },
      {
        print(sprintf("tex_fonction() ****"))
      }
    )
  }
#  dev.new()
  if ( mode != "print" ) {
    par(mar=c(0,0,0,0), oma=c(0,0,0,0))
  }

  p <- do.call(fonction, a)
  if ( interactive() ) {
    if ( grepl("carte", mode) ) {
      savePlot(filename=outputFic,type=c("pdf"), device=dev.cur(), restoreConsole = TRUE)
    }
    if ( grepl("print", mode) ) {
      dev.print(pdf, outputFic)
    }
    if ( grepl("ggplot", mode) ) {
      save_plot(outputFic, p)
    }
    if ( grepl("plotgg", mode) ) {
      savePlot(filename=outputFic,type=c("pdf"), device=dev.cur(), restoreConsole = TRUE)
    }
  } else {
    dev.off()
  }
  print(sprintf("tex_fonction() fin e:%s outputFic: %s", e, outputFic))
}
#
# conversion en table latex
# http://math.furman.edu/~dcs/courses/math47/R/library/xtable/html/print.xtable.html
# pas en utf-8
tex_dsn <- function(dsn = FALSE, suffixe = "", dossier = "") {
  if ( dsn == FALSE ) {
    curcall <- as.character(deparse(sys.call(-1)))
    curcall <- gsub('\\(.*$', '', curcall)
    if ( suffixe != "" ) {
      suffixe <- sprintf("_%s", suffixe)
    }
    if ( dossier != "" ) {
      dossier <- sprintf("%s/", dossier)
      dir.create(sprintf("%s/%s", texDir, dossier), showWarnings = FALSE, recursive = TRUE)
    }
    dsn <- sprintf("%s/%s%s%s.tex", texDir, dossier, curcall, suffixe)
  }
  return(invisible(dsn))
}
#
# conversion en table latex
# http://math.furman.edu/~dcs/courses/math47/R/library/xtable/html/print.xtable.html
# pas en utf-8
tex_df2table <- function(df, dsn = FALSE, suffixe = "", dossier = "", entete = FALSE, num = FALSE, nb_lignes = 50, digits = 0) {
  library(xtable)
#  library("Hmisc")
  options("encoding" = "UTF-8")
  if ( dsn == FALSE ) {
    curcall <- as.character(deparse(sys.call(-1)))
    curcall <- gsub('\\(.*$', '', curcall)
    if ( suffixe != "" ) {
      suffixe <- sprintf("_%s", suffixe)
    }
    if ( dossier != "" ) {
      dossier <- sprintf("%s/", dossier)
      dir.create(sprintf("%s/%s", texDir, dossier), showWarnings = FALSE, recursive = TRUE)
    }
    dsn <- sprintf("%s/%s%s%s.tex", texDir, dossier, curcall, suffixe)
  }
  if( nrow(df) < 1 ) {
    carp('***** %s', dsn)
    glimpse(df)
    f <- file(dsn, open="w")
    writeLines(c('%', '%'), f)
    close(f)
#    stop('***')
    return()
  }
#  View(df)
  df <- as.data.frame(df)
  ajout <- FALSE
  ligne <- 1
#  nb_lignes <- 50
  row.names(df) <- 1:nrow(df)
#  carp("entete: %s", entete)
#  carp("dsn: %s", dsn)
  while ( ligne <= nrow(df) ) {
    fin <- ligne + nb_lignes - 1
    fin <- min(fin, nrow(df))
    dfl <- df[ligne:fin, ]
#    Carp("ligne: %d nrow: %d", ligne, nrow(df))
    if ( entete != FALSE) {
#      df.table <- xtable(dfl, digits=digits, align='m|m{4cm}|m{15cm}|')
      df.table <- xtable(dfl, digits=digits)
      align(df.table) <- entete
      print(
        x = df.table,
        type="latex",
        hline.after = c(-1, 0, 1:nrow(df.table)),
        include.rownames=num,
        tabular.environment = "longtable",
        table.placement="!ht",
        floating=FALSE,
        file=dsn,
        append=ajout
      )
    } else {
      df.table <- print.xtable(
        x = dfl,
        digits = 2,
      )
      print(
        x = df.table,
        type = "latex",
        include.rownames = num,
        table.placement = "!ht",
        file = dsn,
        append = ajout
      )
    }
#    cat("\\vfill\n\\clearpage\n")
    ajout <- TRUE
    ligne <- fin +1
  }
  t <- readLines(dsn)
  return(invisible(t))
}
tex_df2kable <- function(df, dsn = FALSE, suffixe = "", dossier = "", nb_lignes = 0, num = FALSE, digits = 1, font_size = 7, longtable = TRUE, extra = FALSE, escape = TRUE) {
  library(knitr)
  library(kableExtra)
  options("encoding" = "UTF-8")
  if ( dsn == FALSE ) {
    curcall <- as.character(deparse(sys.call(-1)))[1]
    curcall <- gsub('\\(.*$', '', curcall)
    if ( suffixe != "" ) {
      suffixe <- sprintf("_%s", suffixe)
    }
    if ( dossier != "" ) {
      dossier <- sprintf("%s/", dossier)
      d <- sprintf("%s/%s", texDir, dossier)
      dir.create(d, showWarnings = FALSE, recursive = TRUE)
    }
    dsn <- sprintf("%s/%s%s%s.tex", texDir, dossier, curcall, suffixe)
  }
  if (num == TRUE) {
    df <- tibble::rowid_to_column(df, "NO")
  }
  Carp("dsn: %s", dsn)
  if (nrow(df) > 0) {
    if (escape == FALSE) {
      colnames(df) <- escapeLatexSpecials(colnames(df))
#      glimpse(df)
    }
    linesep <- c("", "", "", "", "", "", "", "", "", "\\addlinespace")
    repeat_header_text = "\\textit{(suite)}"
    if (longtable == TRUE) {
      tex <- knitr::kable(df, "latex"
        , longtable = T
        , booktabs = T
        , digits = digits
        , linesep = linesep
        , format.args = list(decimal.mark = ',')
        , escape = escape
        ) %>%
# https://thinkr.fr/les-tableaux-statiques-pour-vos-rapports-en-r/
        kable_styling(
          position = "left",
          font_size = font_size,
          latex_options = c("repeat_header"),
          repeat_header_text = repeat_header_text
        )
    } else {
      tex <- knitr::kable(df, "latex"
        , longtable = F
        , booktabs = T
        , digits = digits
        , linesep = linesep
        , format.args = list(decimal.mark = ',')
        , escape = escape
        ) %>%
# https://thinkr.fr/les-tableaux-statiques-pour-vos-rapports-en-r/
        kable_styling(
          position = "left",
          font_size = font_size,
          latex_options = c("repeat_header", "striped", "scale_down"),
          repeat_header_text = repeat_header_text
        )
    }
    if (extra != FALSE) {
#      tex <- do.call(extra, list(tex))
#      column_spec(3, width = "50em")
      column_spec(tex, 1, latex_column_spec = ">{\\\\color{red}}c")
    }
  } else {
    tex <- "

TABLEAU VIDE

"
  }
  f <- file(dsn, open = "w")
  writeLines(tex, f)
  close(f)
  return(tex)
}
#
# la version avec tabularray
# problème de performances
#
tex_df2longtblr <- function(df, dsn = FALSE, suffixe = "", dossier = "", num = FALSE, digits = 1) {
  options("encoding" = "UTF-8")
  if ( dsn == FALSE ) {
    curcall <- as.character(deparse(sys.call(-1)))[1]
    curcall <- gsub('\\(.*$', '', curcall)
    if ( suffixe != "" ) {
      suffixe <- sprintf("_%s", suffixe)
    }
    if ( dossier != "" ) {
      dossier <- sprintf("%s/", dossier)
      d <- sprintf("%s/%s", texDir, dossier)
      dir.create(d, showWarnings = FALSE, recursive = TRUE)
    }
    dsn <- sprintf("%s/%s%s%s.tex", texDir, dossier, curcall, suffixe)
  }
  if (num == TRUE) {
    df <- tibble::rowid_to_column(df, "NO")
  }
  Carp("dsn: %s", dsn)
  df1 <- df %>%
    mutate(row = sprintf("%s & %s & %s\\\\", ref, stops, names)) %>%
    glimpse()
  rows <- paste(df1$row, collapse = "\n")
  tex <- sprintf("\\begin{longtblr}[
caption = {Tableau des relations},
]{colspec={ll},hlines}
%s
\\end{longtblr}", rows)
  f <- file(dsn, open = "w")
  writeLines(tex, f)
  close(f)
  return(tex)
}

tex_texte <- function(texte, dsn = FALSE, suffixe = "", dossier = "", escape = TRUE) {
  options("encoding" = "UTF-8")
  if ( dsn == FALSE ) {
    curcall <- as.character(deparse(sys.call(-1)))[1]
    curcall <- gsub('\\(.*$', '', curcall)
    if ( suffixe != "" ) {
      suffixe <- sprintf("_%s", suffixe)
    }
    if ( dossier != "" ) {
      dossier <- sprintf("%s/", dossier)
    }
    dsn <- sprintf("%s/%s%s%s.tex", texDir, dossier, curcall, suffixe)
  }
  TEX <- file(dsn, encoding="UTF-8")
  tex <- "% <!-- coding: utf-8 -->"
  if (escape == TRUE) {
    texte <- escapeLatexSpecials(texte)
  }
  tex <- append(tex, texte)
  write(tex, file = TEX, append = FALSE)
  close(TEX)
  carp("dsn: %s", dsn)
}
#
# conversion en table latex
tex_df2table_v1 <- function(df, dsn=FALSE, suffixe="", num=FALSE, nb_lignes=50, align=FALSE) {
  library(xtable)
  stop("**********")
  if ( dsn == FALSE ) {
    curcall <- as.character(deparse(sys.call(-1)))
    curcall <- gsub('\\(.*$', '', curcall)
    if ( suffixe != "" ) {
      suffixe <- sprintf("_%s", suffixe)
    }
    dsn <- sprintf("%s\\%s%s.tex", texDir, curcall, suffixe)
  }
  carp(" dsn: %s", dsn)
#  View(df)
  df <- as.data.frame(df)
#  print(head(df))
  flush.console()
  Sys.sleep(1)
  TEX <- file(dsn, encoding="UTF-8")
  sink(TEX)
  ajout <- FALSE
  ligne <- 1
#  nb_lignes <- 50
  row.names(df) <- 1:nrow(df)
  while ( ligne < nrow(df) ) {
    fin <- ligne + nb_lignes - 1
    fin <- min(fin, nrow(df))
    dfl <- df[ligne:fin,]
    df.table <- xtable(dfl, digits=0)
    align(df.table, "lp{1in}p{3in}")
    if ( align == FALSE) {
      print(df.table, type="latex", tabular.environment="longtable", include.rownames=num, table.placement="!ht")
    } else {
      print(df.table, type="latex", tabular.environment="longtable", include.rownames=num, table.placement="!ht", align=align)
    }
#    cat("\\vfill\n\\clearpage\n")
    ajout <- TRUE
    ligne <- fin +1
  }
  sink()
#  print(sprintf("tex_df2table() dsn: %s", dsn))
}
escapeLatexSpecials <- function(x) {
#  x <- gsub("\\", "$\\backslash$", x, fixed = T)
  x <- gsub("#", "\\\\#", x)
  x <- gsub("\\$", "\\\\$", x)
  x <- gsub("%", "\\\\%", x)
#  x <- gsub("&", "***", x)
  x <- gsub("&", "\\\\&", x)
  x <- gsub("~", "\\\\~", x)
  x <- gsub("_", "\\\\_", x)
#  x <- gsub("_", "\\\\\\\\_", x)
  x <- gsub("%", "\\\\%", x)
#  x <- gsub("^", "\\\\^", x)
#  x <- gsub("\\{", "\\\\{", x)
#  x <- gsub("\\}", "\\\\}", x)
#  x <- gsub(">", "$>$", x)
#  x <- gsub("<", "$<$", x)
  return(x)
}
tex_escape <- function(x) {
  x <- gsub("#", "\\\\#", x)
  x <- gsub("\\$", "\\\\$", x)
  x <- gsub("%", "\\\\%", x)
#  x <- gsub("&", "***", x)
  x <- gsub("&", "\\\\&", x)
  x <- gsub("~", "\\\\~", x)
  x <- gsub("_", "\\\\_", x)
#  x <- gsub("_", "\\\\\\\\_", x)
  x <- gsub("%", "\\\\%", x)
  return(x)
}
tex_regex_escape <- function(x, double_backslash = FALSE) {
  if (double_backslash) {
    x <- gsub("\\\\", "\\\\\\\\", x)
  }
  x <- gsub("\\$", "\\\\\\$", x)
  x <- gsub("\\_", "\\\\_", x)
  x <- gsub("\\(", "\\\\(", x)
  x <- gsub("\\)", "\\\\)", x)
  x <- gsub("\\[", "\\\\[", x)
  x <- gsub("\\]", "\\\\]", x)
  x <- gsub("\\{", "\\\\{", x)
  x <- gsub("\\}", "\\\\}", x)
  x <- gsub("\\*", "\\\\*", x)
  x <- gsub("\\+", "\\\\+", x)
  x <- gsub("\\?", "\\\\?", x)
  x <- gsub("\\|", "\\\\|", x)
  x <- gsub("\\^", "\\\\^", x)
  return(x)
}
# cat(escapeLatexSpecials("Amb_W01"))
#
# template du pauvre
tex_dftpl2fic <- function(df, tpl, suffixe = "", dossier = "") {
  carp()
  library(tidyverse)
  library(utf8)
  library(readr)
  if ( suffixe !=  "" ) {
    suffixe <- sprintf("_%s", suffixe)
  }
  if ( dossier !=  "" ) {
    dossier <- sprintf("%s/", dossier)
  }
  texFic <- sprintf("%s/%s%s%s.tex", texDir, dossier, tpl, suffixe)
  TEX <- file(texFic)
  tex <- "% <!-- coding: utf-8 -->"
#
# le template tex
  dsn <- sprintf("%s/%s_tpl.tex", texDir, tpl)
  template <- read_file(dsn)
  carp("enc: %s", Encoding(template))
  if (nrow(df) > 0) {
    for ( i in 1:nrow(df) ) {
      tpl <- template
      tpl <- tex_df2tpl(df, i, tpl)
      tex <- append(tex, tpl)
#      break
    }
  }
  write(tex, file = TEX, append = FALSE)
  close(TEX)
  carp(" texFic: %s", texFic)
}
tex_df2tpl <- function(df, i, tpl) {
  Encoding(tpl) <- 'UTF-8'
  if (inherits(df, "sf")) {
    df <- st_drop_geometry(df)
  }
  df <- as.data.frame(df)
#  glimpse(df[i, ])
#  mga <<- df
  for ( v in colnames(df) ) {
#    carp("v: %s", v)
    if( is.na(df[i, v]) ) {
      val <- ''
    } else {
      val <- as.character(df[i, v])
    }
#    carp("val: %s", val)
    if (Encoding(val) != 'UTF_8') {
#      val <- iconv(val, to = 'UTF-8')
    }
    re <- paste0("\\{\\[", v, "\\]\\}")
#    Carp("re: %s valid: %s", re, utf8_valid(re))
#    Carp("val: %s valid: %s", val, utf8_valid(val))
    tpl <-  gsub(re, val, tpl, perl = TRUE)
#    Carp("tpl: %s valid: %s", tpl, utf8_valid(tpl))
#    Carp('val: %s tpl: %s', val, paste0(tpl, collapse = ", "));
    re <- paste0("\\{\\{", v, "\\}\\}")
#    val <- escapeLatexSpecials(val)
    val <- misc_tex(val)
#    Carp("* val: %s valid: %s", val, utf8_valid(val))
#    Carp('re: %s val:%s', re, val)
#    Encoding(val) <- 'UTF-8'
    tpl <-  gsub(re, val, tpl, perl = TRUE)
#    Carp("* tpl: %s valid: %s", tpl, utf8_valid(tpl))
#    Carp('val: %s tpl: %s', val, paste0(tpl, collapse = ", "));    stop('***')
#    Encoding(tpl) <- 'UTF-8'
  }
#  Carp('tpl: %s', paste0(tpl, collapse = ", ")); stop('***');
  return(invisible(tpl))
}
#
# setwd('d:/web');source("geo/scripts/onc35.R");tex_pdflatex()
tex_pdflatex <- function(dsn = "especes.tex", dossier = "") {
  library(tinytex)
  if (Tex == FALSE) {
    return(invisible(Tex))
  }
  if (dossier != "") {
    texDir <- sprintf("%s/%s", texDir, dossier)
  }
  carp("dsn: %s texDir: %s", dsn, texDir)
  setwd(texDir)
  pdf <- str_replace(dsn , "tex$", "pdf")
  pdf <- sprintf("%s/%s", texDir, pdf)
  if (basename(dsn) != dsn) {
    d <- dirname(dsn)
    tDir <- sprintf('%s/%s', texDir, d)
    carp('tDir: %s', tDir)
    setwd(tDir)
  }
  carp("getwd: %s", getwd())
  options(tinytex.verbose = TRUE)
#  options(tinytex.verbose = FALSE)
  x <- tryCatch(
    {
      tinytex::pdflatex(dsn)
    },
    error = function(condition){
      carp('erreur %s', condition)
    }
  )
  carp("baseDir: %s", baseDir)
  setwd(baseDir)
  misc_openFile(pdf)
  return(invisible(pdf))
}

