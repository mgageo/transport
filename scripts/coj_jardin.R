# <!-- coding: utf-8 -->
#
# quelques fonctions pour la comptage oiseaux des jardins
# auteur: Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
#
# les traitements par jardin
#
# source("geo/scripts/coj.R");jardin_test()
jardin_test <- function() {
  carp()
  library(rio)
  library(tidyverse)
  deptRef <<- c('22', '35', '29', '56')
  anneesRef <<- c('2013', '2014', '2015', '2016', '2017', '2018', '2019')
  communesRef <<- c('35051')
  donnees_lire(force=TRUE, filtre=FALSE)
  donnees.df <<- filter(donnees.df, saidep %in% deptRef)
  donnees.df <<- filter(donnees.df, annee %in% anneesRef)
#  donnees.df <<- filter(donnees.df, saicom %in% communesRef)
  dsn <- sprintf("%s/jardin_donnees.Rds", texDir)
  export(donnees.df, dsn)
  jardin_causes(donnees.df)
  dsn <- sprintf("%s/jardin_cause.Rds", texDir)
  export(cause.df, dsn)
  jardin_causes_liste()
  df <- filter(donnees.df, id %in% cause.df$id)
  dsn <- sprintf("%s/jardin_donnees_exclu.Rds", texDir)
  export(df, dsn)

  jardin_liste(df)
  carp('donnees cause.df %s', length(unique(df$id)))
#
# sur les données restantes
  df <- filter(donnees.df, ! id %in% cause.df$id)
  dsn <- sprintf("%s/jardin_donnees_inclu.Rds", texDir)
  export(df, dsn)
  jardin_jour_stat()
}
#
# les stat sur les causes
jardin_jour_causes_stat <- function() {
  carp()
  dsn <- sprintf("%s/jardin_cause.Rds", texDir)
  cause.df <- import(dsn)
#  jardin_causes_liste()
  dsn <- sprintf("%s/jardin_donnees_exclu.Rds", texDir)
  exclu.df <- import(dsn)
  dsn <- sprintf("%s/jardin_donnees.Rds", texDir)
  donnees.df <- import(dsn)
  df1 <- donnees.df %>%
    group_by(annee, saidep) %>%
    summarize(nb=n()) %>%
    mutate(type='brut')
  df2 <- exclu.df %>%
    group_by(annee, saidep) %>%
    summarize(nb=n()) %>%
    mutate(type='exclu')
  df3 <- left_join(df1, df2, by=c('annee', 'saidep')) %>%
    glimpse()
  df3 <- rbind(df1, df2) %>%
    spread(type, nb, 0) %>%
    mutate(par=round(100*exclu/brut,0)) %>%
    arrange(saidep, annee)
  View(df3)
  dsn <- sprintf("%s/jardin_causes_stat.tex", texDir)
  tex_df2table(df3, dsn, nb_lignes=50)
  tex_pdflatex(sprintf('%s.tex','jardin'))
}
#
# les stat sur les données
# source("geo/scripts/coj.R");jardin_jour_stat()
jardin_jour_stat <- function() {
  carp()
  dsn <- sprintf("%s/jardin_donnees_inclu.Rds", texDir)
  df <- import(dsn)
  jardin_especes_stat(df)
  tex_pdflatex(sprintf('%s.tex','jardin'))
  donnees.df <<- df
  libelleRef <<- 'bzha'
  if ( ! exists('txDir') ) {
    txDir <<- texDir
  }
  texDir <<- sprintf("%s/%s", txDir, libelleRef)
  stat_jour_stat()
  tex_pdflatex(sprintf('%s.tex','coj_bzha'))
}
#
# détermination des jardins par cause
jardin_causes <- function(df) {
  carp()
  cause.df <<- data.frame(id=character(), espece=character(), cause=character(),nb=integer(), stringsAsFactors=FALSE)
  jardin_cause_espece(df)
  jardin_cause_nboiseaux_max(df)
  jardin_cause_nboiseaux_min(df)
  jardin_cause_nbespeces(df)
  jardin_cause_espece_ko(df)
  jardin_cause_espece_max(df)
  glimpse(cause.df)
}
#
# liste des causes
# source("geo/scripts/coj.R");jardin_causes_liste()
jardin_causes_liste <- function() {
  carp()
  df <- cause.df %>%
    group_by(cause, espece) %>%
    summarize(nb=n()) %>%
    glimpse()
  dsn <- sprintf("%s/jardin_causes_liste.tex", texDir)
  tex_df2table(df, dsn, nb_lignes=50)
}
#
# filtrage des jardins avec une espèce > 100
jardin_cause_espece <- function(df) {
  df1 <- df %>%
    filter(nb >100)
  if(nrow(df1) < 1 ) {
    return()
  }
  df1$cause <- "espece > 100"
  cause.df <<- rbind(cause.df, df1[, c("id", "espece", "cause", "nb")])
}
#
# filtrage des jardins avec une espèce supérieure à un seuil spécifique
jardin_cause_espece_max <- function(df) {
  max.df <- especes_lire('espece_max')
  carp("nrow: %d max.df", nrow(max.df))
  df1 <- df %>%
    left_join(max.df) %>%
    filter(nb > max)
  if(nrow(df1) < 1 ) {
    return()
  }
  df1$cause <- "espece max"
  cause.df <<- rbind(cause.df, df1[, c("id", "espece", "cause", "nb")])
}
#
# filtrage des jardins avec plus de 200 oiseaux ou moins de 4
jardin_cause_nboiseaux_max <- function(df) {
  df1 <- df %>%
    group_by(id) %>%
    summarise(nb = sum(nb) )
  df1 <- df1 %>%
    filter(nb > 200)
  if(nrow(df1) < 1 ) {
    return()
  }
  df1$cause <- "oiseaux > 200"
  df1$espece <- "*"
  cause.df <<- rbind(cause.df, df1[, c("id", "espece", "cause", "nb")])
}
jardin_cause_nboiseaux_min <- function(df) {
  df1 <- df %>%
    group_by(id) %>%
    summarise(nb = sum(nb) )
  df1 <- df1 %>%
    filter(nb < 4)
  if(nrow(df1) < 1 ) {
    return()
  }
  df1$cause <- "oiseaux < 4"
  df1$espece <- "*"
  cause.df <<- rbind(cause.df, df1[, c("id", "espece", "cause", "nb")])
}
#
# filtrage des jardins avec plus de 25 espèces ou moins de 3
jardin_cause_nbespeces <- function(df) {
  df1 <- df %>%
    group_by(id, espece) %>%
    summarise(nb = sum(nb) ) %>%
    group_by(id) %>%
    summarise(nb = n() )
  df1 <- df1 %>%
    filter(nb > 25)
  if(nrow(df1) < 1 ) {
    return()
  }
  df1$cause <- "especes > 25"
  df1$espece <- "*"
  cause.df <<- rbind(cause.df, df1[, c("id", "espece", "cause", "nb")])
}
#
# pour supprimer des comptages avec erreur espèce
jardin_cause_espece_ko <- function(df) {
  carp()
  ko.df <- especes_lire('espece_ko')
  carp("nrow: %d ko.df", nrow(ko.df))
#  print(Encoding(ko.df$espece))
  df1 <- df %>%
    filter(espece %in% ko.df$espece)
  carp("df1 nrow : %s", nrow(df1))
  if(nrow(df1) < 1 ) {
    return()
  }
  df1$cause <- "espece ko"
  cause.df <<- rbind(cause.df, df1[, c("id", "espece", "cause", "nb")])
}
# source("geo/scripts/coj.R");jardin_especes_stat()
jardin_especes_stat <- function(df) {
  carp()
  library(rio)
  library(tidyverse)
  df1 <- df %>%
    group_by(espece) %>%
    summarize(jardin=n(), min=min(nb), max=max(nb)) %>%
    glimpse()
#  View(df1)
  dsn <- sprintf("%s/jardin_especes_stat.xlsx", cfgDir)
  export(df1, dsn)
  dsn <- sprintf("%s/jardin_especes_stat.tex", cfgDir)
  tex_df2table(df1, dsn)
  carp('nrow: %s', nrow(df1))
}
# source("geo/scripts/coj.R");jardin_liste()
jardin_liste <- function(df) {
  carp()
  library(rio)
  library(tidyverse)
  df <- df %>%
    mutate(admid=as.numeric(admid)) %>%
    mutate(jardin=sprintf('%s/%05d', annee, admid)) %>%
    glimpse()
  texFic <- sprintf("%s/jardin_liste.tex", texDir)
  TEX <- file(texFic, encoding="UTF-8")
  tex <- "% <!-- coding: utf-8 -->"
  jardins <- sort(unique(df$jardin))
  for (Jardin in jardins) {
#    carp('Jardin: %s', Jardin)
    df1 <- df %>%
      filter(jardin == Jardin) %>%
      arrange(espece)
    tex <- append(tex, sprintf("\\subsection*{%s}", Jardin))
    df2 <- filter(cause.df, id == Jardin)
    if (nrow(df2) > 0 ) {
#      View(df2)
      tex <- append(tex, sprintf("\\begin{itshape}"))
      for(i in 1:nrow(df2) ) {
        tex <- append(tex, sprintf("%s (%s), ", df2[i, 'cause'], df2[i, 'espece']))
      }
      tex <- append(tex, sprintf("\\end{itshape}"))
      tex <- append(tex, sprintf(""))
    }
    for(i in 1:nrow(df1) ) {
      tex <- append(tex, sprintf("%s (%s), ", escapeLatexSpecials(df1[i,'espece']), df1[i,'nb']))
    }
  }
  write(tex, file = TEX, append = FALSE)
  carp('texFic: %s', texFic)
}
