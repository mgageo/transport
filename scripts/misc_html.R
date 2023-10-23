# <!-- coding: utf-8 -->
#
# quelques fonctions pour générer les fichiers html
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
#
#

html_df2kable <- function(
    df,
    dsn = FALSE,
    suffixe="", dossier="",
    nb_lignes = 0,
    num = FALSE,
    digits = 1,
    font_size = 9,
    extra = FALSE,
    escape = TRUE
  ) {
  library(knitr)
  library(kableExtra)
  options("encoding" = "UTF-8")
  if ( dsn == FALSE ) {
    curcall <- as.character(deparse(sys.call(-1)))
    curcall <- gsub('\\(.*$', '', curcall)
    if ( suffixe != "" ) {
      suffixe <- sprintf("_%s", suffixe)
    }
    if ( dossier != "" ) {
      dossier <- sprintf("%s/", dossier)
      d <- sprintf("%s/%s", webDir, dossier)
      dir.create(d, showWarnings = FALSE, recursive = TRUE)
    }
    dsn <- sprintf("%s/%s%s%s.html", webDir, dossier, curcall, suffixe)
  }
  carp("dsn: %s", dsn)
  if (nrow(df) > 0){
    linesep <- c("", "", "", "", "", "", "", "", "", "\\addlinespace")
    repeat_header_text = "\\textit{(suite)}"
    html <- knitr::kable(df, "html"
      , longtable = T
      , booktabs = T
      , digits = digits
      , linesep = linesep
      , format.args = list(decimal.mark = ',')
      , escape = escape
      ) %>%
      kable_styling(
        position = "left",
        font_size = font_size,
        repeat_header_text = repeat_header_text
      )
    if (extra != FALSE) {
      html <- do.call(extra, list(tex))
  #      column_spec(3, width = "50em")
    }
  } else {
    html <- "

TABLEAU VIDE

"
  }
  f <- file(dsn, open="w")
  writeLines(html, f)
  close(f)
}
html_replace <- function(x) {
html_symbols <- data.frame(
    html = c("&copy;", "&reg;", "&trade;", "&ldquo;",
        "&rdquo;", "&lsquo;", "&rsquo;", "&bull;", "&middot;", "&sdot;",
        "&ndash;", "&mdash;", "&cent;", "&pound;", "&euro;", "&ne;",
        "&frac12;", "&frac14;", "&frac34;", "&deg;", "&larr;", "&rarr;",
        "&hellip;", "&nbsp;", "&lt;", "&gt;", "&amp;", "&quot;", "&apos;",
        "&yen;", "&laquo;", "&raquo;"
    ),
    symbol = c("(c)", "(r)", "tm", "\"", "\"", "'",
        "'", "-", "-", "[]", "-", "-", "cents", "pounds", "euro", "!=",
        "half", "quarter", "three fourths", "degrees", "<-", "->", "...",
        " ", "<", ">", "&", '"', "'", "yen", "<<", ">>"
    ),
    stringsAsFactors = FALSE
)
  stringi::stri_replace_all_fixed(x, c(">"), c("&gt;"))
}
html_df2gt <- function(df) {
  library(gt)
  html <- df %>%
    glimpse() %>%
    gt() %>%
    as_raw_html()
  return(invisible(gt))
}
