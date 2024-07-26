# <!-- coding: utf-8 -->
#
# quelques fonctions pour la comptage oiseaux des jardins
# auteur: Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
# les statistiques
# !!!! obsolète !!!!!
# cf actions
# !!!!!!!!!!!!!!!!!!!!!
# les traitements à enchainer suite à une mise à jour
# source("geo/scripts/coj.R");stat_jour()
stat_jour <- function() {
  carp()
  library(tidyverse)
  dev.new()
  txDir <<- texDir
  anneeRef <<- c('2020')
  stat_jour_35()
#  stat_jour_bzh()
#  stat_jour_bzha()
}
stat_jour_35 <- function() {
  carp()
  donnees_lire(force=TRUE, filtre=FALSE)
  dept <- "35"
  deptRef <<- '35'
  libelleRef <<- '35'
  texDir <<- sprintf("%s/%s", txDir, dept)
  donnees.df <<- filter(donnees.df, dept %in% deptRef)
  stat_jour_stat()
  stat_commune_dept()
  stat_commune_annee()
  texDir <<- txDir
  tex_pdflatex(sprintf('%s_stat.tex',libelleRef))
}
stat_jour_bzh <- function() {
  carp()
  libelleRef <<- 'bzh'
  deptRef <<- c('22', '35', '29', '56', '44')
  texDir <<- sprintf("%s/%s", txDir, libelleRef)
  donnees_lire(force=TRUE, filtre=FALSE)
  stat_jour_stat()
  stat_commune_dept()
  texDir <<- txDir
  tex_pdflatex(sprintf('%s_stat.tex',libelleRef))
}
stat_jour_bzha <- function() {
  carp()
  libelleRef <<- 'bzha'
  deptRef <<- c('22', '35', '29', '56')
  texDir <<- sprintf("%s/%s", txDir, libelleRef)
  donnees_lire(force=TRUE, filtre=FALSE)
  stat_jour_stat()
  stat_commune_dept()
  texDir <<- txDir
#  tex_pdflatex(sprintf('%s.tex',libelleRef))
}
stat_jour_stat <- function() {
#  return()
  stat_annee_dept_commune_tex()
  stat_annee_commune_tex()
  stat_annee_dept()
  stat_annee_dept_juxta("m_especes")
  stat_annee_dept_juxta("m_oiseaux")
  stat_annee_critere("nb_oiseaux")
  stat_annee_critere("nb_especes")
  stat_annee_dept_tex()
  stat_annee_tex()
  stat_annee_dept_espece_tex()
  stat_espece_abondance_tex()
  stat_espece_frequence_tex()
  stat_presence_annees()
  stat_presence_annees_histo()
}
#
# préparation du dataframe des stat par année et par département
# source("geo/scripts/coj.R");stat_commune()
stat_commune <- function() {
  library(dplyr)
  df <- sqlv2_tables_lire('2020', '_2020') %>%
    glimpse()
  df1 <- df %>%
    filter(insee=='22178') %>%
    group_by(annee, admid) %>%
    summarise(nb= n()) %>%
    group_by(annee) %>%
    summarise(nb= n()) %>%
    glimpse()
}
#
# stat de 2019 pour le geoca
# source("geo/scripts/coj.R");stat_geoca()
stat_geoca <- function() {
  library(dplyr)
  df <- donnees_fusion_lire("donnees_fusion_2019.Rda") %>%
    glimpse()
  df1 <- df %>%
    filter(dept=='22') %>%
    group_by(annee) %>%
    summarise(nb= n()) %>%
    print(n=20) %>%
    glimpse()
}
#
# l'horodatage des saisies sur les deux sites
# source("geo/scripts/coj.R");stat_horodatage_jour()
stat_horodatage_jour <- function() {
  carp()
  library(ggplot2)
  library(tidyverse)
  gg1 <- stat_horodatage_coj()
  gg2 <- stat_horodatage_faune()
  gg <- grid.arrange(gg1, gg2, ncol=2)
  ggpdf(gg, dossier='stat')
  return(invisible(gg))
}
#
# pour avoir la courbe horodatage de saisie des données
# source("geo/scripts/coj.R");stat_horodatage_coj()
stat_horodatage_coj <- function() {
  library(lubridate)
  library(ggplot2)
  library(tidyverse)
  library(gridExtra)
  carp()
  gg1 <- stat_horodatage_coj_annee(anneePrec)
  gg2 <- stat_horodatage_coj_annee(anneeRef)
  gg <- grid.arrange(gg1, gg2)
  ggpdf(gg, dossier='stat')
  return(invisible(gg))
}
stat_horodatage_coj_annee <- function(annee='2019') {
  library(lubridate)
  library(ggplot2)
  library(tidyverse)
  carp()
  df <- annnee_debut_fin(annee)
  d <- df[1, 'debut']
  debut <- dmy(d)
  d <- df[1, 'fin']
  fin <- dmy(d) + days(15)
  lims <- c(floor_date(debut), floor_date(fin))
  sqlv2_tables_lire(a=annee)
  df <- entete.df %>%
    mutate(date=sprintf('%s %s', cojcreationdate, cojcreationheure)) %>%
    mutate(date=dmy_hm(date)) %>%
    filter(date >= debut & date <= fin) %>%
    glimpse()
  df1 <- df %>%
    group_by(time = floor_date(date, "2 hours")) %>%
    summarise(nb=n()) %>%
    arrange(time) %>%
    mutate(cum=cumsum(nb))
  gg <- ggplot(df1, aes(x = time, y = nb)) +
    geom_bar(stat = "identity",
      width = 60*60*2,  # one bar to be 2 hours wide
      position = position_nudge(x = 60*60),  # shift one hour to the *right*
      colour = "black"
    ) +
    geom_line(aes(y=cum/10)) +
    scale_x_time(
      name= 'jour',
      labels = function(x) strftime(x, "%a"),
      limits=lims
    ) +
    scale_y_continuous(
      name="par 2 heures",
      limit=c(0, 500),
      sec.axis = sec_axis(~ . * 10, name = "cumul")
    )
  gg <- gg +
    ggtitle(sprintf('coj %s nb: %s', annee, nrow(df))) +
    theme(plot.title = element_text(vjust = - 10))
  return(invisible(gg))
}
# source("geo/scripts/coj.R");stat_horodatage_faune()
stat_horodatage_faune <- function() {
  library(lubridate)
  library(ggplot2)
  library(tidyverse)
  library(gridExtra)
  carp()
  gg1 <- stat_horodatage_faune_annee(anneePrec)
  gg2 <- stat_horodatage_faune_annee(anneeRef)
  gg <- grid.arrange(gg1, gg2)
  ggpdf(gg, dossier='stat')
  return(invisible(gg))
}
# source("geo/scripts/coj.R"); gg<- stat_horodatage_faune_annee(); print(gg)
stat_horodatage_faune_annee <- function(annee='2020') {
  library(lubridate)
  library(ggplot2)
  library(tidyverse)
  carp()
  df <- annnee_debut_fin(annee)
  d <- df[1, 'debut']
  debut <- dmy(d)
  d <- df[1, 'fin']
  fin <- dmy(d) + days(15)
  lims <- c(floor_date(debut), floor_date(fin))
  faune.df <- faune_donnees_jardin_annee_lire(annee)
  df <- faune.df %>%
    group_by(insert_date, id_form) %>%
    summarize(nb=n()) %>%
    mutate(date=excel_numeric_to_date(as.numeric(insert_date), include_time = TRUE)) %>%
    filter(date >= debut & date <= fin) %>%
    glimpse()
  df1 <- df %>%
    group_by(time = floor_date(date, "2 hours")) %>%
    summarise(nb=n()) %>%
    arrange(time) %>%
    mutate(cum=cumsum(nb))
  gg <- ggplot(df1, aes(x = time, y = nb)) +
    geom_bar(stat = "identity",
      width = 60*60*2,  # one bar to be 2 hours wide
      position = position_nudge(x = 60*60),  # shift one hour to the *right*
      colour = "black"
    ) +
    geom_line(aes(y=cum/10)) +
    scale_x_time(
      name= 'jour',
      labels = function(x) strftime(x, "%a"),
      limits=lims
    ) +
    scale_y_continuous(
      name="par 2 heures",
      limit=c(0, 250),
      sec.axis = sec_axis(~ . * 10, name = "cumul")
    )
  gg <- gg +
    ggtitle(sprintf('faune %s nb: %s', annee, nrow(df))) +
    theme(plot.title = element_text(vjust = - 10))
  return(invisible(gg))
}
#
# les données ont été mise à jour ?
# https://www.faune-bretagne.org/index.php?m_id=1380&fid=34698
# source("geo/scripts/coj.R"); gg <- stat_validation_faune_annee()
stat_validation_faune_annee <- function(annee='2020') {
  library(lubridate)
  library(ggplot2)
  library(tidyverse)
  carp()
  faune.df <- faune_donnees_jardin_annee_lire(annee)
  df <- faune.df %>%
    group_by(county, id_form, insert_date, update_date) %>%
    summarize(nb=n()) %>%
    filter(insert_date != update_date) %>%
    filter(nb > 3) %>%
    glimpse()

}
#
# préparation du dataframe des stat par année et par département
# source("geo/scripts/coj.R");stat_df()
stat_df <- function() {
  library(dplyr)
  df <- donnees_lire()
  df1 <- df %>%
    group_by(annee, dept, admid) %>%
    summarise(nb_especes = n(), nb_oiseaux = sum(nb))
  carp("nrow:%s df1", nrow(df1))
  stat.df <- df1 %>%
    group_by(annee, dept) %>%
#    summarise(nb = n(), nb_oiseaux = sum(nb_oiseaux), nb_especes=sum(nb_especes), m_oiseaux=sprintf("%.1f", nb_oiseaux/nb), m_especes=sprintf("%.1f", nb_especes/nb))
    summarise(nb_jardins = n(), nb_oiseaux = sum(nb_oiseaux), nb_especes=sum(nb_especes), m_oiseaux=nb_oiseaux/nb_jardins, m_especes=nb_especes/nb_jardins) %>%
    arrange(dept, annee)
#  View(stat.df)
  return(invisible(stat.df))
}
# source("geo/scripts/coj.R");stat_annee_df()
stat_annee_df <- function() {
  library(dplyr)
  df <- donnees_lire()
  df1 <- df %>%
    group_by(annee, dept, admid) %>%
    summarise(nb_especes = n(), nb_oiseaux = sum(nb))
  carp("nrow:%s df1", nrow(df1))
  stat.df <- df1 %>%
    group_by(annee) %>%
    summarise(nb_jardins = n(), nb_oiseaux = sum(nb_oiseaux), nb_especes=sum(nb_especes), m_oiseaux=nb_oiseaux/nb_jardins, m_especes=nb_especes/nb_jardins)
  return(invisible(stat.df))
}
#
stat_annee_dept_commune_df <- function() {
  library(dplyr)
  df <- donnees_lire()
  df1 <- df %>%
    group_by(annee, dept, insee, admid) %>%
    summarise(nb_especes=n(), nb_oiseaux=sum(nb)) %>%
    group_by(annee, dept, insee) %>%
    summarise(nb_jardins = n(), nb_oiseaux = sum(nb_oiseaux))
  carp("nrow:%s df1", nrow(df1))
  stat.df <- df1 %>%
    group_by(annee, dept) %>%
    summarise(nb_communes=n(), nb_jardins=sum(nb_jardins), nb_oiseaux=sum(nb_oiseaux)) %>%
    arrange(dept, annee) %>%
    glimpse()
  carp('nrow: %s stat.df', nrow(stat.df))
  return(invisible(stat.df))
}
#
# stat par année et par département pour les communes
stat_annee_dept_commune_tex <- function() {
  df <- stat_annee_dept_commune_df()
  dsn <- sprintf("%s/stat_annee_dept_commune.tex", texDir)
  tex_df2table(df, dsn, nb_lignes=50)
}
#
stat_annee_commune_df <- function() {
  library(dplyr)
  df <- donnees_lire()
  df1 <- df %>%
    group_by(annee, dept, insee, admid) %>%
    summarise(nb_especes = n(), nb_oiseaux = sum(nb)) %>%
    group_by(annee, dept, insee) %>%
    summarise(nb_jardins = n(), nb_oiseaux = sum(nb_oiseaux))

  carp("nrow:%s df1", nrow(df1))
  stat.df <- df1 %>%
    group_by(annee) %>%
    summarise(nb_communes=n(), nb_jardins=sum(nb_jardins), nb_oiseaux=sum(nb_oiseaux)) %>%
    glimpse()
  carp('nrow: %s stat.df', nrow(stat.df))
  return(invisible(stat.df))
}
#
# stat par année et par département pour les communes
# source("geo/scripts/coj.R");stat_annee_commune_tex()
stat_annee_commune_tex <- function() {
  df <- stat_annee_commune_df()
  dsn <- sprintf("%s/stat_annee_commune.tex", texDir)
  tex_df2table(df, dsn, nb_lignes=50)
}
#
# stat par année
stat_annee_tex <- function() {
  df <- stat_annee_df()
  dsn <- sprintf("%s/stat_annee.tex", texDir)
  tex_df2table(df, dsn, nb_lignes=50)
}
#
# stat par année et par département
stat_annee_dept_tex <- function() {
  df <- stat_df()
  dsn <- sprintf("%s/stat_annee_dept.tex", texDir)
  tex_df2table(df, dsn, nb_lignes=50)
}
#
# stat par année et par département
stat_annee_dept_espece_tex <- function() {
  df <- donnees_lire()
  df2 <- df %>%
    filter(annee == anneeRef & dept %in% deptRef)
  df1 <- df2 %>%
    group_by(annee, dept, espece) %>%
    summarise(nb_jardins = n(), nb_oiseaux = sum(nb) )
  carp("nrow:%s df1", nrow(df1))
  dsn <- sprintf("%s/stat_annee_dept_espece.tex", texDir)
  tex_df2table(df1, dsn, nb_lignes=50, num=TRUE)
}
#
# stat par année et par département
stat_annee_dept_juxta <- function(variable="m_especes") {
  carp()
  library(ggplot2)
  stat.df <- stat_df()
  theme.blank <- theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
  )
  gg <- ggplot(data=stat.df, aes(x=annee, y=get(variable))) +
    geom_histogram(stat = "identity") +
    theme.blank +
    facet_wrap(~dept)
  print(gg)
  ggpdf(gg, suffixe=variable)
}
stat_annee_dept <- function() {
  carp()
  library(ggplot2)
  stat.df <- stat_df()
  gg <- ggplot(data=stat.df, aes(x=annee, y=nb_jardins, fill=dept, label=nb_jardins)) +
    geom_bar(stat='identity') +
    geom_text(size = 3, position = position_stack(vjust = 0.5))
  print(gg)
  ggpdf()
}
stat_annee_critere <- function(variable="nb_oiseaux") {
  carp()
  library(ggplot2)
  df <- donnees_lire(FALSE)
  df1 <- df %>%
    group_by(annee, dept, admid) %>%
    summarise(nb_especes = n(), nb_oiseaux = sum(nb) )
  df2 <- df1 %>%
    group_by(annee, dept, critere=get(variable)) %>%
    summarise(nb = n())
#  View(df2)
  theme.blank <- theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
  )
  gg <- ggplot(data=df2, aes(x=critere, y=nb, fill=annee)) +
    geom_bar(stat='identity') +
    theme.blank +
    facet_wrap(~annee)
  print(gg)
  ggpdf(gg, suffixe=variable)
}
#
# acun intérêt
stat_annee <- function(annee=2018) {
  library(dplyr)
  library(ggplot2)
  library(scales)
  df <- sql_fusion_lire()
  stat.df <- df %>%
    filter(annee == annee) %>%
    group_by(dept) %>%
    summarise(nb = n())
  ggplot(data=stat.df, aes(x=annee, y=nb, fill=dept)) + geom_bar(width = 1, stat='identity')
}
#
# stat par année et par espèce
# http://www.milanor.net/blog/aggregation-dplyr-summarise-summarise_each/
# https://thinkr.fr/utiliser-la-grammaire-dplyr-pour-triturer-ses-donnees/
# source("geo/scripts/coj.R");stat_espece()
stat_espece_abondance_tex <- function() {
  carp()
  library(tidyverse)
  df <- donnees_lire()
#  df <- df %>% filter(dept == "35")
  annees <- distinct(df, annee)
  df1 <- df %>%
    group_by(annee, espece) %>%
    summarise(nb = sum(nb))
  df2 <- df1 %>%
    group_by(espece) %>%
    mutate(total = sum(nb)) %>%
    spread(annee, nb, 0) %>%
    arrange(desc(total)) %>%
    filter(total > 0)
#  df2 <- df2[1:50, ]
  dsn <- sprintf("%s/stat_espece_abondance.tex", texDir)
  tex_df2table(df2, dsn, nb_lignes=50, num=TRUE)
  df2 <- df1 %>%
    group_by(annee) %>%
    mutate(totcells=sum(nb), # how many cells overall
      percent=round(1000*nb/totcells,0)) %>% #cells by landuse/total cells
    dplyr::select(-c(nb, totcells)) %>%
    spread(key=annee, value=percent, fill=0)
  dsn <- sprintf("%s/stat_espece_abondance_pm.tex", texDir)
  tex_df2table(df2, dsn, nb_lignes=50, num=TRUE)
}
stat_espece_frequence_tex <- function() {
  carp()
  library(tidyverse)
  df <- donnees_lire()
#  df <- df %>% filter(dept == "35")
  carp("les années")
  annees <- distinct(df, annee) %>%
    glimpse()
  df1 <- df %>%
    group_by(annee, espece) %>%
    summarise(nb = n())
  df2 <- df1 %>%
    group_by(espece) %>%
    mutate(total=sum(nb)) %>%
    spread(annee, nb, 0) %>%
    arrange(desc(total)) %>%
    filter(total > 0)
#  df2 <- df2[1:50, ]
  dsn <- sprintf("%s/stat_espece_frequence.tex", texDir)
  tex_df2table(df2, dsn, nb_lignes=50, num=TRUE)
  df2 <- df1 %>%
    group_by(annee) %>%
    mutate(totcells=sum(nb), # how many cells overall
      percent=round(1000*nb/totcells,0)) %>% #cells by landuse/total cells
    dplyr::select(-c(nb, totcells)) %>%
    spread(key=annee, value=percent, fill=0)
  dsn <- sprintf("%s/stat_espece_frequence_pm.tex", texDir)
  tex_df2table(df2, dsn, nb_lignes=50, num=TRUE)
}
# les traitements spatiaux
#
# préparation du dataframe des stat par année et par commune
# source("geo/scripts/coj.R");stat_commune_df()
stat_commune_df <- function() {
  carp()
  library(dplyr)
  df <- donnees_lire() %>%
    glimpse()
  df$insee <- gsub(' ', '', df$insee)
  if ( 1 == 2 ) {
    filter(df, dept %in% deptRef) %>%
      filter(annee == anneeRef) %>%
      filter(insee == '35362') %>%
      print(n=20, na.print = "")
    stop('***')
  }
  df1 <- df %>%
    group_by(annee, dept, insee, admid) %>%
    summarise(nb_especes = n(), nb_oiseaux = sum(nb) )
  df1 <- filter(df1, dept %in% deptRef)
  df2 <- df1 %>%
    group_by(insee, annee) %>%
    summarise(nb_jardins = n()) %>%
    arrange(-nb_jardins) %>%
    glimpse() %>%
    print(n=10)
  df2$INSEE_COM <- gsub(' ', '', df2$insee)
#  View(df2)
  carp(" nrow: %d", nrow(df2))
  return(invisible(df2))
}
stat_commune_annee <- function() {
  carp()
  library(tidyverse)
  df <- stat_commune_df()
  df1 <- df %>%
    group_by(insee) %>%
    mutate(total=sum(nb_jardins)) %>%
    spread(key=annee, value=nb_jardins, 0) %>%
    filter(total > 20)
  spdf <- couches_ign_lire(libelleRef)
  ign.df <- spdf@data[, c("INSEE_COM", "NOM_COM", "POPULATION")]
  df2 <- df1 %>%
    left_join(ign.df, by='INSEE_COM') %>%
    mutate(pc=round(10000* total / as.numeric(POPULATION),0)) %>%
    dplyr::select(-c(INSEE_COM))
#  View(df2)
  dsn <- sprintf("%s/stat_commune_annee.tex", texDir)
  tex_df2table(df2, dsn, nb_lignes=55, num=FALSE)
}
#
# source("geo/scripts/coj.R");stat_commune_dept()
stat_commune_dept <- function() {
  carp("anneeRef: %s", anneeRef)
  df <- stat_commune_df()
  carp("top communes")
  df <- filter(df, annee == anneeRef) %>%
    arrange(-nb_jardins) %>%
    print(n=10)
  vPal <- c("white", rev(heat.colors(5)))
  df$sym <- vPal[1]
  df[df$nb_jardins > 0,"sym"] <- vPal[2]
  df[df$nb_jardins > 1,"sym"] <- vPal[3]
  df[df$nb_jardins > 4,"sym"] <- vPal[4]
  df[df$nb_jardins > 10,"sym"] <- vPal[5]
  df[df$nb_jardins > 50,"sym"] <- vPal[6]
  spdf <- couches_ign_lire(libelleRef)
  spdf@data <- spdf@data[, c("INSEE_COM", "NOM_COM")]
  classes <- c(0, 1, 5, 10, 50, 100)
  spdf1 <- spdf
  spdf1@data <- data.frame(spdf@data, df[match(spdf@data$INSEE_COM, df$INSEE_COM), ])
  spdf1$nb_jardins[is.na(spdf1$nb_jardins)] <- 0
  par(mar=c(0,0,0,0), oma=c(0,0,0,0))
  plot(spdf1, col=spdf1@data$sym)
  spdf2 <- spdf1[spdf1@data$nb_jardins > 4,]
  text(coordinates(spdf2), labels=spdf2@data$nb_jardins, font = 2, cex= 1)
  dsn <- sprintf("%s/stat_commune_dept.pdf", texDir)
  dev.copy(pdf, dsn, width=par("din")[1], height=par("din")[2])
  dev.off()
  carp("dsn:%s", dsn)
}
#
# les traitements presence
# préparation du dataframe
stat_presence_annees <- function() {
  library(tidyverse)
  df <- donnees_lire()
  carp("nrow:%s", nrow(df))
  df1 <- df %>%
    mutate(ligne = 1:nrow(.)) %>%
    filter(is.na(espece))
  if (nrow(df1) > 0) {
    carp("************* espece na: %s", nrow(df1))
    glimpse(df1)
#    stop("*************")
  }
  df <- df %>%
    filter(! is.na(espece))
  annees.df <- data.frame(annee=character(), nb_donnees=character(), nb_jardins=character(), stringsAsFactors=FALSE)
  nbj <- length(unique(df$id))
  annees.df <- rbind(annees.df,list(annee="pc", nb_donnees=nrow(df), nb_jardins=nbj), stringsAsFactors=FALSE)
  df1 <- stat_presence_pc(df)
  annees <- unique(df$annee)
  nb_annees <- length(annees)
  for (a in annees ) {
    df2 <- filter(df, annee == a)
    nbj <- length(unique(df2$id))
    carp(" annee:%s nrow:%s", a, nrow(df2))
    annees.df <- rbind(annees.df, list(annee=a, nb_donnees=nrow(df2), nb_jardins=nbj), stringsAsFactors=FALSE)
    df3 <- stat_presence_pc(df2)
    colnames(df3) <- c("espece", a)
    df1 <- left_join(df1, df3, by = c("espece"))
  }
  annees.df <<- annees.df
  glimpse(df1)
  df1[is.na(df1)] <- 0
  df2 <- df1 %>%
    filter(pc>0) %>%
    arrange(desc(pc))
# http://laeti.perrierbrusle.free.fr/stat_td3_cours.pdf
  df2$et <- apply(df2[, annees], 1, sd)
  df2$moy <- apply(df2[, annees], 1, sum)/nb_annees
  df2 <- df2 %>%
    mutate(cv = round(10* et / moy,0))  %>%
    dplyr::select(-c(et, moy))
#  View(df2)
  carp(" nrow: %d", nrow(df2))
  dsn <- sprintf("%s/stat_presence_annees.tex", texDir)
  tex_df2table(df2, dsn, nb_lignes=55, num=TRUE)
  return(invisible(df2))
}
#
# un histogramme
# http://www.sthda.com/french/wiki/ggplot2-barplots-guide-de-demarrage-rapide-logiciel-r-et-visualisation-de-donnees
# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
# http://bconnelly.net/2013/10/creating-colorblind-friendly-figures/
stat_presence_annees_histo <- function() {
  library(ggplot2)
  library(dplyr)
  df <- stat_presence_annees()
  carp(" nrow:%s", nrow(df))
  print(annees.df)
  print(colnames(df))
  a <- anneeRef
  nbj <- sprintf("%s : nombre de jardins : %s", a, annees.df[annees.df$annee==a, "nb_jardins"])
  df <- df %>%
    mutate(nb = get(a))
#  df$nb[is.na(df$nb)] <- 0
#  View(df)
  df$espece <- factor(df$espece, levels = df$espece[order(df$nb)])
  df$cut <- cut(df$nb, c(0, 20, 50, 100), include.lowest = TRUE, right = FALSE)
  theme.blank <- theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
  )

  gg <- ggplot(data=df, aes(x=espece, y=nb, fill=cut)) +
    geom_bar(stat="identity", width=0.7) +
    scale_y_continuous( limits = c(0,100) ) +
    theme.blank +
    scale_fill_manual(values=c("#e75e00", "#009e73", "#56b4e9")) +
    theme(legend.position = c(0.8, 0.2)) +
    theme(legend.title = element_blank()) +
     annotate("text", x = c(2), y = c(60), label = nbj , color="black", size=5, fontface="bold") +
    coord_flip()
  print(gg)
  ggpdf()
}
#
stat_presence_pc <- function(df) {
  library(dplyr)
  df1 <- df %>%
    group_by(espece) %>%
    summarise(nb_jardins = n())
  df1$tot_jardins <- length(unique(df$admid))
  df2 <- df1 %>%
    mutate(pc=round(100*nb_jardins/tot_jardins,0)) %>%
    dplyr::select(-c(nb_jardins, tot_jardins))
#  View(df2)
  carp(" nrow: %d", nrow(df2))
  return(invisible(df2))
}
#
# les traitements  nourrissage
# préparation du dataframe
stat_nourrissage <- function() {
  library(dplyr)
  df <- donnees_lire()
  df$id <- sprintf("%s/%s", df$admid, df$annee)
  fusion.df <- sql_fusion_sainou()
  carp(" nrow:%s", nrow(df))
  df <- left_join(df, fusion.df, by="id")
  df1 <- df %>%
    group_by(espece, sainou) %>%
    summarise(nb_jardins = n()) %>%
    spread(key=sainou, value=nb_jardins, fill=0)
  nb_oui <- length(unique(df[df$sainou == "oui", "id"]))
  nb_non <- length(unique(df[df$sainou == "non", "id"]))
  df1$nb_oui <- nb_oui
  df1$nb_non <- nb_non
  df1$pc_oui <- round(100*df1$oui/df1$nb_oui,0)
  df1$pc_non <- round(100*df1$non/df1$nb_non,0)
  View(df1)
  return(invisible(df2))
}
# https://blog.exploratory.io/filter-data-with-dplyr-76cf5f1a258e
stat_dpt35 <- function() {
  library(dplyr)
  library(tidyr)
  df <- sql_fusion_lire()
  df1 <- df %>%
    filter(grepl("35 ", insee)) %>%
    group_by(insee, annee) %>%
    summarise(nb = n())
  df <- xl2012_comptage_lire()
  df$annee <- '2012'
  df2 <- df %>%
    filter(grepl("35 ", insee)) %>%
    group_by(insee, annee) %>%
    summarise(nb = n())
  colnames(df2) <- c('insee', 'annee', 'nb')
  df <- rbind(df1, df2)
# https://stackoverflow.com/questions/27354734/dplyr-mutate-rowsums-calculations-or-custom-functions
  df3 <- df %>%
    spread("annee", "nb") %>%
    replace(is.na(.), 0)
  insee.df <- insee_cog_lire()
  df4 <- df3 %>%
    filter(`2012` > 10) %>%
    left_join(insee.df)
  print(df4)
}