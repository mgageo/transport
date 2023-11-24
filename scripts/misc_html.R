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
    suffixe = "",
    dossier = "",
    nb_lignes = 0,
    num = FALSE,
    digits = 1,
#    font_size = 9,
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
#        font_size = font_size,
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
  return(invisible(html))
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
  stringi::stri_replace_all_fixed(x, c("<"), c("&lt;"))
}
html_df2gt <- function(df) {
  library(gt)
  html <- df %>%
    glimpse() %>%
    gt() %>%
    as_raw_html()
  return(invisible(gt))
}
html_browse <- function(dsn, titre) {
  dsn <- sprintf("%s/%s.html", webDir, titre)
  write(html, dsn)
  carp("dsn: %s", dsn)
  if (HtmlBrowse != FALSE) {
    url <- sprintf("http://localhost/%s/%s.html", WebDir, titre)
    browseURL(
      url,
      browser = "C:/Program Files/Mozilla Firefox/firefox.exe"
    )
  }
}
html_entete <- function(title = "titre") {
  html <- sprintf('<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8" />
    <title>%s</title>
    <meta name="author" content="Marc Gauthier" />
    <meta name="copyright" content="©CC BY-NC-SA 4.0 Marc Gauthier" />
    <meta name="robots" content="noindex,nofollow"/>
    <meta http-equiv="expires" content="43200"/>
  </head>
  <body>', title)
  return(invisible(html))
}
html_titre <- function(titre = "titre") {
  html <- '<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8" />
    <title>%s</title>
    <meta name="author" content="Marc Gauthier" />
    <meta name="copyright" content="©CC BY-NC-SA 4.0 Marc Gauthier" />
    <meta name="robots" content="noindex,nofollow"/>
    <meta http-equiv="expires" content="43200"/>
  </head>
  <body>
'
  sprintf(html, titre)
}
html_pied <- function() {
  html <- sprintf('  </body>
</html>')
  return(invisible(html))
}
html_pied_sort <- function(html1) {
  html <- '
    <script src="/js/sorttable/sorttable.js"></script>
  </body>
</html>
'
  html1 <- append(html1, html)
}
html_append <- function(html1, html2) {
  writeLines(html2)
  html1 <- append(html1, as.character(html2))
}
html_append_df <- function(html, df) {
  glimpse(df)
  rownames(df) <- NULL
  htm <- df %>%
    kbl(escape = F) %>%
#    kable_minimal() %>%
    kable_styling(bootstrap_options = "striped", full_width = F, position = "left", fixed_thead = T)
  html <- append(html, htm)
  return(invisible(html))
}
html_df2fic <- function(df, titre = "titre", dsn = FALSE, suffixe = "", dossier = "") {
  html <-  html_titre(titre = titre)
  rownames(df) <- NULL
  htm <- df %>%
    kbl(escape = F) %>%
#    kable_minimal() %>%
    kable_styling(bootstrap_options = "striped", full_width = F, position = "left", fixed_thead = T)
  html <- append(html, htm)
  html <- html_pied(html)
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
    dsn <- sprintf("%s/%s%s%s.html", webDir, dossier, curcall, suffixe)
  }
  write(html, dsn)
  carp("dsn: %s", dsn)
}