# <!-- coding: utf-8 -->
#
# graphique avec ggplot
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
GGPLOT_WATERMARK <- FALSE
#
#
ggplot_init <- function() {
  library(ggplot2)
  library(viridis)
# https://www.datanovia.com/en/fr/blog/ggplot-galerie-de-themes/
# https://www.shanelynn.ie/themes-and-colours-for-r-ggplots-with-ggthemr/
# https://github.com/hrbrmstr/hrbrthemesdev
  library(ggthemes)
  library(grid)
  library(gridtext)
  library(RColorBrewer)
  library(classInt)
}
#
ggplot_flip <- function(df) {
  theme.blank <- theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
  )
  mga.df <<- df
  m <- max(df$nb)
  pas <- ceiling(max(df$nb)/8)
  carp("m: %s pas: %s", m, pas)
  les_pas <<- c(0, pas, pas*2, pas*8)
  df$cut <- cut(df$nb, les_pas, include.lowest = TRUE, right = FALSE)
  df$espece <- factor(df$espece, levels = df$espece[order(df$nb)])
  gg <- ggplot(data=df, aes(x=espece, y=nb, fill=cut)) +
    geom_bar(stat="identity", width=0.7) +
    scale_y_continuous( limits = c(0, m) ) +
    theme.blank +
    scale_fill_manual(values=c("#e75e00", "#009e73", "#56b4e9")) +
    theme(legend.position = c(0.8, 0.2)) +
    theme(legend.title = element_blank()) +
#    annotate("text", x = c(2), y = c(60), label = nbj , color="black", size=5, fontface="bold") +
    coord_flip()
  return(invisible(gg))
}
ggplot_theme_blank <- function() {
  theme.blank <- theme(
    axis.line=element_blank(),
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.position="none",
    panel.background=element_blank(),
    panel.border=element_blank(),
#      panel.grid.major=element_blank(),
#      panel.grid.minor=element_blank(),
#      plot.background=element_blank()
  )
  return(invisible(theme.blank))
}
ggplot_theme_nomargins <- function() {
 theme(plot.margin = margin(t = 0,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 0)) # Left margin
}
ggplot_integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}
#
ggplot_casamance <- function() {
  carp()
  library(ggplot2)
  theme_gg <- theme(
    axis.line=element_blank(),
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  )
  return(invisible(theme_gg))
}
#
#
ggplot_chelon <- function() {
  carp("début")
  library(ggplot2)
  theme_set(theme_ipsum())
  theme_gg <- theme(axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      legend.position="none",
      panel.background=element_rect(fill = 'lightblue'),
      panel.border=element_blank(),
#      panel.grid.major=element_blank(),
#      panel.grid.minor=element_blank(),
#      plot.background=element_blank()
  )
  return(invisible(theme_gg))
}
#
#
ggplot_gci35 <- function() {
  carp()
  library(ggplot2)
  theme_gg <- theme(
#    axis.line=element_blank(),
#    axis.text.x=element_blank(),
#    axis.text.y=element_blank(),
#    axis.ticks=element_blank(),
    panel.background = element_rect(fill = "grey"),
    plot.background = element_rect(fill = "grey"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
#    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  )
  return(invisible(theme_gg))
}
ggplot_gci35_watermark <- function(gg, angle = 45) {
  library(cowplot)
  gg <- ggdraw(gg) +
    draw_label(
      "@ Marc Gauthier CC-BY-NC-ND",
      color = "white",
      alpha = 0.5,
      size = 20,
      angle = angle
    )
  return(invisible(gg))
}
#
#
ggplot_stoc <- function() {
  carp()
  library(ggplot2)
  theme_gg <- theme(
    panel.background = element_rect(fill = "grey"),
    plot.background = element_rect(fill = "grey"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  )
  return(invisible(theme_gg))
}
ggplot_stoc_watermark <- function(gg) {
  gg <- gg +
    annotate("text", x = Inf, y = -Inf, label = "@ Marc Gauthier CC-BY-NC-ND"
      , hjust=1.1, vjust=-1.1, col="white", cex=6,  fontface = "bold", alpha = 0.8
    )
  return(invisible(gg))
}
ggplot_stoc_watermark <- function(gg, angle = 30) {
  library(cowplot)
  gg <- ggdraw(gg) +
    draw_label(
      "@ Marc Gauthier CC-BY-NC-ND",
      color = "white",
      alpha = 0.5,
      size = 24,
      angle = angle
    )
  return(invisible(gg))
}
ggplot_geonature <- function() {
  carp()
  library(ggplot2)
  theme_gg <- theme(
    axis.line=element_blank(),
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank(),
    panel.background = element_rect(fill = "grey"),
    plot.background = element_rect(fill = "grey"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  )
  return(invisible(theme_gg))
}

# https://www.data-imaginist.com/2020/insetting-a-new-patchwork-version/
# https://rstudio-pubs-static.s3.amazonaws.com/92534_af09813c707b4bb382bfd539f78d4750.html
#
# en direct de https://rdrr.io/github/datarootsio/artyfarty/src/R/watermark.R
ggplot_watermark_txt <- function(txt = "@ Marc Gauthier CC-BY-NC-ND", x, y, location = "br", rot=0, ...){
  extra_gpar <- list(...)
  gpar <- grid::get.gpar()

  if(length(extra_gpar) > 0)
    gpar <- modifyList(gpar, extra_gpar)

  if(all(missing(x), missing(y))){

    if(location == "tr"){
      y = .9
      x = .9
    } else if(location == "tl"){
      y = .9
      x = .1
    } else if(location == "bl"){
      y = .1
      x = .1
    } else if(location == "br"){
      y = .1
      x = .9
    } else if(location == "center"){
      y = .5
      x = .5
    }
  }
  wm_grob <- grid::textGrob(label = txt, y=y, x=x, rot=rot, gp=gpar)
  annotation_custom(wm_grob)
}
ggplot_watermark <- function(txt = "@ Marc Gauthier CC-BY-NC-ND") {
  annotate(geom="text",
    x=df$Min,
    y=50.0,
    xend=Inf,
    yend=Inf,
    label=txt,
    color='white',
    angle=45,
    fontface='bold',
    size=25,
    alpha=0.5,
    family='Arial'
  )
}
ggplot_grid_texte <- function(texte = "@ Marc Gauthier CC-BY-NC-ND", position = "bottomright", col = "black") {
  library(readr)
  library(tidyverse)
  library(grid)
  carp("pos: %s", position)
  positions <- "pos,x,y,hjust,vjust,rot
bottomleft,0,0,0,0,0
topleft,0,1,0,1,0
topright,1,1,1,1,0
bottomright,1,0,1,0,0
left,0,0.5,0,0,0
right,1,0.5,1,0,0
top,0.5,1,0.5,1,0
bottom,0.5,0,0.5,0,0
center,0.5,0.5,0.5,0.5,0
centerup,0.5,0.5,0.5,0.5,45
"
  df <- read_csv(positions, show_col_types = FALSE) %>%
    filter(pos %in% position)
  if(nrow(df) != 1) {
    stop("******")
  }
  x <- df[1, "x"]
  y <- df[1, "y"]
  hjust <- df[1, "hjust"]
  vjust <- df[1, "vjust"]
  rot <- df[1, "rot"]
# https://www.rdocumentation.org/packages/grid/versions/3.6.2/topics/grid.text
  alpha <- 0.8
  grob <- grobTree(textGrob(texte, x = x, y = y, hjust = hjust, vjust = vjust, rot = rot,
    gp=gpar(col = col, cex = 1, fontface = "bold", alpha = alpha)
#    vp = outerBox
  ))
  return(invisible(grob))
}
ggplot_copy <- function(texte = "@ Marc Gauthier CC-BY-NC-ND", position = "bottomrigth", cex = 6) {
  carp("position: %s", position)
  gg <- annotate("text", x = Inf, y = -Inf, label = texte
      , hjust=1, vjust=-1, col="white", cex = cex,  fontface = "bold", alpha = 0.8
    )
  if (position == "topright") {
    gg <- annotate("text", x = Inf, y = Inf, label = texte
        , hjust=1, vjust=1, col="white", cex = cex,  fontface = "bold", alpha = 0.8
      )
  }
  if (position == "bottomleft") {
    gg <- annotate("text", x = -Inf, y = -Inf, label = texte
        , hjust=0, vjust=-1, col="white", cex = cex,  fontface = "bold", alpha = 0.8
      )
  }
  if (position == "topleft") {
    gg <- annotate("text", x = -Inf, y = Inf, label = texte
        , hjust=0, vjust=1, col="white", cex = cex,  fontface = "bold", alpha = 0.8
      )
  }
  return(invisible(gg))
}

# https://stackoverflow.com/questions/16422847/save-plot-with-a-given-aspect-ratio
ggplot_sizeit <- function(p, panel.size = 2, default.ar=1){
  library(ggplot2)
  library(grid)
  gb <- ggplot_build(p)
  # first check if theme sets an aspect ratio
  ar <- gb$plot$coordinates$ratio

  # second possibility: aspect ratio is set by the coordinates, which results in
  # the use of 'null' units for the gtable layout. let's find out
  g <- ggplot_gtable(gb)
  nullw <- sapply(g$widths, attr, "unit")
  nullh <- sapply(g$heights, attr, "unit")

  # ugly hack to extract the aspect ratio from these weird units
  if(any(nullw == "null"))
    ar <- unlist(g$widths[nullw == "null"]) / unlist(g$heights[nullh == "null"])

  if(is.null(ar)) # if the aspect ratio wasn't specified by the plot
       ar <- default.ar

  # ensure that panel.size is always the larger dimension
  if(ar <= 1 ) panel.size <- panel.size / ar

  g$fullwidth <- convertWidth(sum(g$widths), "in", valueOnly=TRUE) +
    panel.size
  g$fullheight <- convertHeight(sum(g$heights), "in", valueOnly=TRUE) +
    panel.size / ar

  class(g) <- c("sizedgrob", class(g))
  g
}
ggplot_ratio <- function(p, default.ar=-1){
  gb <<- ggplot_build(p)
  # first check if theme sets an aspect ratio
  ar <- gb$plot$coordinates$ratio
  # second possibility: aspect ratio is set by the coordinates, which results in
  # the use of 'null' units for the gtable layout. let's find out
  g <<- ggplot_gtable(gb)
  nullw <- sapply(g$widths, attr, "unit")
  nullh <- sapply(g$heights, attr, "unit")
  # ugly hack to extract the aspect ratio from these weird units
  if(any(nullw == "null"))
      ar <- unlist(g$widths[nullw == "null"]) / unlist(g$heights[nullh == "null"])

  if(is.null(ar)) # if the aspect ratio wasn't specified by the plot
      ar <- default.ar
  ar[1]
}
#
# sauvegarde en fichier pdf du graphique ggplot
ggplot_pdf <- function(plot = last_plot(), suffixe = '', dossier = '',  width = 0, height = 0) {
  if (GGPLOT_WATERMARK != FALSE) {
    plot <- ggplot_stoc_watermark(plot)
  }
  if ( width == 0 ) {
    width <- par("din")[1]
    height <- par("din")[2]
  }
#  stop("****")
  curcall <- as.character(deparse(sys.call(-1)))[1]
  curcall <- gsub('\\(.*$', '', curcall)
  if ( suffixe != "" ) {
    suffixe <- sprintf("_%s", suffixe)
  }
  if ( dossier != "" ) {
    dossier <- sprintf("/%s", dossier)
  }
  dsn <- sprintf("%s%s/%s%s.pdf", texDir, dossier, curcall, suffixe)
  print(sprintf("%s() dsn : %s", curcall, dsn))
#  ggsave(dsn, width=width, height=height)
  ggsave(dsn, plot = plot)
  carp("***dsn: %s", dsn)
}