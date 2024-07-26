# <!-- coding: utf-8 -->
#
# quelques fonctions pour le comptage oiseaux des jardins
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
#
#
GGPLOT_WATERMARK <- TRUE
#
# =============================================================================================================
#
# les traitements à enchainer suite à une mise à jour du code des statistiques
# source("geo/scripts/coj.R");actions_jour()
actions_jour <- function(config = "config_yann") {
  carp()
  library(tidyverse)
  library(ggplot2)
  library(janitor)
  library(knitr)
#  actions_config('config_test'); return()
#  actions_config('faune_35'); return()
#  actions_config('config_bzh2020')
#  actions_config('sql_35')
  actions_config('config_35')
#  actions_config('config_sqlv2')
#  actions_config(config)
}
# ================================================
# la partie configuration des actions
# source("geo/scripts/coj.R");actions_config('config_bzh')
actions_config <- function(onglet = "config_test") {
  library(tidyverse)
  library(ggplot2)
  library(janitor)
  actions_dsn <<- sprintf("%s/sources_actions.xlsx", cfgDir)
  carp("actions_dsn: %s onglet: %s", actions_dsn, onglet)
  df <- actions_onglet_lire(onglet) %>%
    filter(! grepl("^#", attribut))
  for (i in 1:nrow(df)) {
    if (is.na(df[i, "attribut"])) {
      break
    }
    df2 <- actions_config_action(df[i, "attribut"], df[i, "valeur"], actions_dsn)
  }
#  tex_pdflatex('actions.tex')
}
#
# réalisation de l'action de configuration
actions_config_action <- function(attribut, valeur, dsn) {
  Carp("*****début %s / %s /%s", attribut, valeur, actions_dsn)
  switch(attribut,
    "actions" = actions_config_action_actions(valeur, dsn),
    "annee" = actions_config_action_annee(valeur),
    "dept" = actions_config_action_dept(valeur),
    "dossier" = actions_config_action_dossier(valeur),
    "lire" = actions_config_action_lire(valeur),
    "lire_rds" = actions_config_action_lire_rds(valeur),
    "pdf" = actions_config_action_pdf(valeur),
    "sauve" = actions_config_action_sauve(),
    "source" = actions_config_action_source(valeur),
    "stat" = actions_config_action_stat(valeur),
    "verifier" = actions_config_action_verifier(valeur),
    {
      carp("inconnu: %s", attribut)
      stop("$$$$$$$$$")
    }
  )
  Carp("*****fin %s / %s /%s", attribut, valeur, actions_dsn)
}
actions_config_action_actions <- function(valeur, dsn) {
  carp()
  actions_init(dsn, onglet = valeur, prefixe = "actions_")
}
actions_config_action_annee <- function(valeur) {
  carp()
  donnees.df <<- filter(donnees.df, annee == valeur)
}
actions_config_action_dept <- function(valeur) {
  carp()
  depts <- strsplit(valeur, ",")[[1]]
  donnees.df <<- filter(donnees.df, dept %in% depts)
  if (nrow(donnees.df) == 0) {
    stop("***")
  }
}
actions_config_action_dossier <- function(valeur) {
  carp("valeur: %s", valeur)
  dossier <<- valeur
  dossierDir <<- sprintf("%s/%s", cfgDir, dossier)
  dir.create(dossierDir, showWarnings = FALSE, recursive = TRUE)
}
actions_config_action_lire <- function(valeur) {
  carp("valeur: %s", valeur)
  rds <- "donnees_fusion_sources"
#  rds <- "donnees_fusion_sqlv2_sources"
  if (! is.na(valeur)) {
    rds <- valeur
  }
  donnees.df <<- lire_rds(rds) %>%
    glimpse() %>%
    filter(! is.integer(nb)) %>%
    mutate(nb = as.integer(nb)) %>%
    filter(! is.na(nb)) %>%
# formulaire renseigné avec 0
    filter(nb > 0) %>%
    mutate(admid = sprintf("%s/%s/%s", annee, source, admid)) %>%
    filter(! is.na(espece)) %>%
# pour Bergeronnette flavéole (M.f.flavissima)
    mutate(espece = gsub("\\s*\\(.*$", "", espece)) %>%
    glimpse()
#  donnees.df %>%
#    filter(grepl("Bergeronnette flav", espece)) %>%
#    glimpse()
  actions_stat("lire")
#  stop("****")
}
actions_config_action_lire_rds <- function(valeur) {
  carp("début")
  donnees.df <<- lire_rds("donnees", dossier = dossier) %>%
    glimpse()
  df1 <- donnees.df %>%
    filter(espece == "Rouge-gorge ; Rougegorge familier")
  if (nrow(df1) > 0) {
    stop("çççççççççççççççççççç")
  }
  actions_stat("lire_rds")
  carp("fin")
}
# c'est le template qui est fourni !
actions_config_action_pdf <- function(valeur) {
  carp("valeur: %s", valeur)
#
# le template tex
  dsn <- sprintf("%s/%s_tpl.tex", texDir, valeur)
  tpl <- readLines(dsn)
  val <- "dossier"
  re <- paste0("\\{\\{", val, "\\}\\}")
  tpl <- gsub(re, dossier, tpl, perl = TRUE)
  texFic <- sprintf("%s/%s_%s.tex", texDir, dossier, valeur)
  carp("dsn: %s => texFic: %s", dsn, texFic)
  write(tpl, file = texFic, append = FALSE)
  texFic <- sprintf("%s_%s.tex", dossier, valeur)
  tex_pdflatex(texFic)
}
actions_config_action_sauve <- function() {
  carp("dossier: %s", dossier)
  sauve_rds(donnees.df, dsn = "donnees", dossier = dossier)
  df1 <- donnees.df %>%
    filter(espece == "Rouge-gorge ; Rougegorge familier")
  if (nrow(df1) > 0) {
    stop("^^^^^^^^^^^^^^")
  }
  actions_stat("sauve")
}
actions_config_action_source <- function(valeur) {
  carp("valeur: %s", valeur)
  donnees.df <<- filter(donnees.df, grepl(valeur, source))
  if (nrow(donnees.df) == 0) {
    stop("***")
  }
}
actions_config_action_stat <- function(valeur) {
  carp("valeur: %s", valeur)
  actions_stat(valeur)
}
#
# vérification
actions_config_action_verifier <- function(onglet) {
  carp("donnees.df nrow:%s", nrow(donnees.df))
  df1 <- donnees.df %>%
    filter(espece == "Rouge-gorge ; Rougegorge familier")
  if (nrow(df1) > 0) {
    stop("^^^^^^^^^^^^^^")
  }
}
#
## la partie actions --------------------------------------------------------
#

#
# suppression des especes "techniques"
actions_filtrer <- function(onglet) {
  carp()
  df1 <- donnees.df %>%
    filter(! is.na(espece)) %>%
    filter(espece != "Aucune espèce")
  donnees.df <<- df1
  return(invisible(donnees.df))
}
# controler la date d'observation
actions_dates <- function(...) {
  library(lubridate)
  library(ggplot2)
  library(scales)
  carp()
# que s'il y a plusieurs sources
  df <- donnees.df %>%
    filter(annee == anneeRef) %>%
    group_by(source) %>%
    summarize(nb = n())
  if(nrow(df) < 2) {
    carp("***** une seule source")
    txt2pdf(txt = "une seule source", dossier = dossier)
    return()
  }

  df <- donnees.df %>%
    filter(annee == anneeRef) %>%
    mutate(Date = dmy(jjmmaaaa)) %>%
    group_by(Date, source, admid) %>%
    summarize(nb = n()) %>%
    group_by(Date, source) %>%
    summarize(nbf = n()) %>%
    ungroup() %>%
    pivot_wider(names_from = source, values_from = nbf, values_fill = list(nbf = NA))
  theme.blank <- theme(axis.line = element_blank(),
#      axis.text.x=element_blank(),
#      axis.text.y=element_blank(),
#      axis.ticks=element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_text(color = "darkblue"),
      axis.title.y.right = element_text(color = "red"),
      legend.position = "none",
      panel.background = element_blank(),
      panel.border = element_blank(),
#      panel.grid.major=element_blank(),
#      panel.grid.minor=element_blank(),
#      plot.background=element_blank()
  )
  gg <- ggplot(data = df, ) +
    geom_point(mapping = aes(x = Date, y = sqlv2), colour = "darkblue", size = 2) +
    geom_point(mapping = aes(x = Date, y = faune), colour = "red", size = 2) +
    scale_color_discrete(name = "nombre", labels = c("sqlv2", "faune")) +
    scale_x_date(labels = date_format("%d/%m"), date_breaks = "14 days", date_minor_breaks = "1 day") +
    scale_y_continuous(
      name = "nombre de formulaires sqlv2",
      sec.axis = sec_axis(~ . / 1, name = "nombre de formulaires faune")
    )
  gg <- gg + theme.blank
  plot(gg)
  ggplot_pdf(gg, dossier = dossier)

#  donnees.df <<- df1
}
#
# valider que la date d'observation est bien dans un intervalle autour du week-end
actions_dates_valid <- function(onglet) {
  library(lubridate)
  library(ggplot2)
  library(scales)
  carp("onglet: %s", onglet)
  df1 <- actions_onglet_lire(onglet) %>%
    mutate(d = dmy(samedi)) %>%
    mutate(debut = d - days(8)) %>%
    mutate(fin = d + days(15)) %>%
    mutate(annee = sprintf("%s", annee)) %>%
    glimpse()
  df2 <- donnees.df %>%
    group_by(annee, jjmmaaaa, admid, source) %>%
    summarize(nb = n()) %>%
    left_join(df1, by = c("annee")) %>%
    mutate(date = dmy(jjmmaaaa)) %>%
    mutate(delta = as.numeric(date - d)) %>%
    glimpse()
  df3 <<- df2 %>%
    group_by(annee, delta) %>%
    summarise(nb = n())
  theme.blank <- theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
  )
  gg <- ggplot(data = df3, aes(x = delta, y = nb, fill = annee)) +
    geom_bar(stat = "identity") +
    theme.blank +
    facet_wrap(~annee)
  print(gg)

  df3 <- df2 %>%
    filter(date < debut | date > fin) %>%
    group_by(annee, source) %>%
    summarize(nb = n()) %>%
    left_join(df1, by = c("annee")) %>%
    glimpse()
  print(knitr::kable(df3, format = "pipe"))

#  donnees.df <<- df1
}
#
# renommer des espèces
actions_renommer <- function(onglet) {
  carp()
  df <- actions_onglet_lire(onglet) %>%
    filter(! is.na(nouveau)) %>%
    filter(nouveau != "") %>%
    glimpse()
  df1 <- donnees.df
  df1 <- df1 %>%
    left_join(df, by = c("espece" = "ancien")) %>%
    mutate(espece = ifelse(is.na(nouveau), espece, nouveau)) %>%
    dplyr::select(-nouveau)
  donnees.df <<- df1
#  stop('***')
}
#
# controler le nom des espèces
actions_controler <- function(onglet) {
  carp()
  df <- actions_onglet_lire(onglet)
#
# suppression des especes "techniques"
  df1 <- donnees.df %>%
    filter(! is.na(espece)) %>%
    filter(espece != "Aucune espèce")
  df1 <- df1 %>%
    group_by(espece, source) %>%
    summarize(nb = n())
  df2 <- df1 %>%
    filter(! espece %in% df$espece)

  if (nrow(df2) > 0) {
    glimpse(df2)
    print(kable(df2, n = 200, format = "pipe"))
    carp("*** espèce inconnue ***** onglet: %s", onglet)
    stop("****")
  }
}
#
# exclusion de certaines espèces
actions_exclure <- function(onglet) {
  carp()
  df <- actions_onglet_lire(onglet)
  df1 <- donnees.df
  df2 <- df1 %>%
    group_by(espece) %>%
    summarize(nb = n()) %>%
    filter(nb < 100) %>%
    arrange(espece) %>%
    print(n = 200)
  export_df2xlsx(df2)
  stop("***")
}
#
# seuils globaux sur un jardin
actions_seuil <- function(onglet) {
  carp()
  df <- actions_onglet_lire(onglet)
  tex_df2kable(df, dossier = dossier, suffixe = onglet)
  df1 <- donnees.df %>%
    group_by(admid) %>%
    summarize(jardin_especes = n(), jardin_oiseaux = sum(nb)) %>%
    glimpse()
  df2 <- df1 %>%
    group_by(jardin_especes) %>%
    summarize(nb = n()) %>%
    glimpse()
  gg <- ggplot(data = df2, aes(x = jardin_especes, y = nb)) + geom_bar(width = 1, stat = "identity")
  ggplot_pdf(gg, dossier = dossier, suffixe = "jardin_especes")
  df2 <- df1 %>%
    group_by(jardin_oiseaux) %>%
    summarize(nb = n()) %>%
    glimpse()
  gg <- ggplot(data = df2, aes(x = jardin_oiseaux, y = nb)) + geom_bar(width = 1, stat = "identity")
  ggplot_pdf(gg, dossier = dossier, suffixe = "jardin_oiseaux")
#  stop('**')
  df3 <- data.frame()
  for (i in 1:nrow(df)) {
    df2 <- actions_seuil_test(df1, df[i, "attribut"], df[i, "valeur"], df[i, "test"])
    df3 <- rbind(df3, df2)
  }
  df4 <- df3 %>%
    glimpse() %>%
    group_by(cause) %>%
    summarize(nb = n()) %>%
    print(n = 30)
  tex_df2kable(df4, dossier = dossier, suffixe = "apres")
# pour avoir la liste par jardin
  actions_seuil_liste(donnees.df, df3)
# suppression des jardins
  ids <- unique(df3$admid)
  df3 <- donnees.df %>%
    filter(!admid %in% ids)
  donnees.df <<- df3
}
#
# un des seuils sur un jardin
actions_seuil_test <- function(df1, attribut, valeur, test) {
  cause <- sprintf("%s %s %s", attribut, valeur, test)
#  attribut <- 'jardin_especes'
  if (test == "min") {
    df2 <- df1 %>%
      filter(!!rlang::sym(attribut) < get("valeur"))
  }
  if (test == "max") {
    df2 <- df1 %>%
      filter(!!rlang::sym(attribut) > get("valeur"))
  }
  carp("%s nrow: %s", cause, nrow(df2))
  if (nrow(df2) > 0) {
    df2$cause <- cause
  }
  return(invisible(df2))
}
#
# les jardins avec trop d'especes improbables d'une liste
# source("geo/scripts/coj.R");actions_probable('probable')
actions_probable <- function(onglet) {
  carp("onglet: %s", onglet)
#  return()
  df <- actions_onglet_lire(onglet)
  tex_df2kable(df, dossier = dossier, suffixe = onglet)
# ajout du poids par espèce
  df1 <- donnees.df %>%
    left_join(df, by = c("espece" = "espece")) %>%
    mutate(poids = replace_na(poids, 0))
#
# toutes les espèces doivent avoir un poids
  df2 <- df1 %>%
    filter(poids == 0 ) %>%
    group_by(espece, source) %>%
    summarize(nb = n())
  if (nrow(df2) > 0) {
    glimpse(df2)
    print(kable(df2, n = 200, format = "pipe"))
  }
# calcul du poids par jardin
  df3 <- df1 %>%
    filter(poids > 0) %>%
    group_by(admid) %>%
    summarize(poids = sum(poids), especes = paste0(espece, collapse = ", ")) %>%
    glimpse()
  df4 <- df3 %>%
    group_by(poids) %>%
    summarize(nb = n()) %>%
    adorn_totals(where = "row", name = "Total") %>%
    print(n = 20)
  tex_df2kable(df4, dossier = dossier, suffixe = "poids")
# filtrage
  df3 <- df3 %>%
    filter(poids > 1) %>%
    glimpse()
  actions_probable_liste(donnees.df, df3)
#  txt <- faune_formulaires_liste(df3)
#  tex_texte(txt, dossier = dossier)
  ids <- unique(df3$admid)
#
# détermination de la liste des espèces improbables
# les jardins avec improbable
  df2 <- df1 %>%
    filter(admid %in% ids)
  df3 <- df2 %>%
    filter(poids > 0) %>%
    group_by(espece) %>%
    summarize(nb = n()) %>%
    glimpse()
  tex_df2kable(df3, dossier = dossier, suffixe = "especes")
#  stop("***")
#
# les jardins sans trop d'improbables
  df1 <- donnees.df %>%
    filter(!admid %in% ids)
  donnees.df <<- df1
}
actions_probable_liste <- function(donnees.df, df3) {
  ids <- unique(df3$admid)
  liste <- ""
  for (id in ids) {
    df1 <- df3 %>%
      filter(admid == id)
    df2 <- donnees.df %>%
      filter(admid == id) %>%
      mutate(espece = sprintf("%s (%s)", espece, nb)) %>%
      group_by(admid, insee) %>%
      summarize(especes = paste0(espece, collapse = ", "))
    causes <- sprintf("%s insee: %s, %s", df1[1, "admid"], df2[1, "insee"], df1[1, "especes"])
    liste <- append(liste, causes)
    liste <- append(liste, "\\smallbreak")
    liste <- append(liste, df2[[1, "especes"]])
    liste <- append(liste, "\\bigbreak")
  }
  tex_texte(liste, dossier = dossier)
}
actions_seuil_liste <- function(donnees.df, df3) {
  ids <- unique(df3$admid)
  liste <- ""
  for (id in ids) {
    df1 <- df3 %>%
      filter(admid == id) %>%
      group_by(admid) %>%
      summarize(causes = paste0(cause, collapse = ", "))
    df2 <- donnees.df %>%
      filter(admid == id) %>%
      mutate(espece = sprintf("%s (%s)", espece, nb)) %>%
      group_by(admid, insee) %>%
      summarize(especes = paste0(espece, collapse = ", "))
    causes <- sprintf("%s insee: %s, %s", df1[1, "admid"], df2[1, "insee"], df1[1, "causes"])
    liste <- append(liste, causes)
    liste <- append(liste, "\\smallbreak")
    liste <- append(liste, df2[[1, "especes"]])
    liste <- append(liste, "\\bigbreak")
  }
  tex_texte(liste, dossier = dossier)
}

#
# suppression des especes improbables d'une liste
actions_supprimer <- function(onglet) {
  carp()
#  return()
  df <- actions_onglet_lire(onglet)
# ajout du poids par espèce
  df1 <- donnees.df %>%
    left_join(df, by = c("espece" = "espece"))
  df2 <- df1 %>%
    filter(is.na(poids)) %>%
    dplyr::select(-poids)
  donnees.df <<- df2
}
#
## pour Yann -----------------------------------------------------------------
#
#
actions_yann <- function(onglet) {
  carp()
  df <- actions_onglet_lire(onglet)
  tex_df2kable(df, dossier = dossier, suffixe = "config")
  df1 <- donnees.df %>%
    group_by(espece) %>%
    summarize(nb = n()) %>%
    ungroup()
  df1 <- df1 %>%
    full_join(df, by = c("espece" = "espece")) %>%
    glimpse()
  export_df2xlsx(df1)
  df3 <- df %>%
    filter(! is.na(seuil)) %>%
    filter(seuil > 0)
  df2 <- donnees.df %>%
    group_by(espece) %>%
    summarise(x=list(enframe(quantile(nb, probs=c(0.99))[[1]][1], "quantiles", "nb"))) %>%
    unnest(x) %>%
    filter(espece %in% df3$espece) %>%
    print(n=80)
  export_df2xlsx(df2, suffixe = "seuil")
#
# application du seuil nombre d'individus
  df6 <- donnees.df %>%
    left_join(df, by = c("espece" = "espece"))
  df7 <- df6 %>%
    filter(! is.na(seuil)) %>%
    filter(seuil > 0) %>%
    filter(nb > seuil) %>%
    mutate(cause = sprintf("%s (%s#%s)", espece, nb, seuil)) %>%
    glimpse()
  carp("les hors jardin ou migrateurs")
  df8 <- df6 %>%
    filter(is.na(seuil)) %>%
    mutate(cause = sprintf("%s (hj)", espece))
  df9 <- df8 %>%
    group_by(espece) %>%
    summarize(nb = n())
  print(knitr::kable(df9, format = "pipe"))
  return()
  df5 <- rbind(df7, df8)
  actions_yann_liste(donnees.df, df5)
}
#
# la liste des espèces et des jardins
actions_yann_liste <- function(donnees.df, df3) {
  df2 <- df3 %>%
    group_by(espece) %>%
    summarize(n = n(), causes = paste0(nb, collapse = ", "))
  tex_df2kable(df2, dossier = dossier, suffixe = "espece")
#
# la liste des jardins
  ids <- unique(df3$admid)

  liste <- ""
  for (id in ids) {
    df1 <- df3 %>%
      filter(admid == id) %>%
      group_by(admid) %>%
      summarize(nb = n(), causes = paste0(cause, collapse = ", "))
    df2 <- donnees.df %>%
      filter(admid == id) %>%
      mutate(espece = sprintf("%s (%s)", espece, nb)) %>%
      group_by(admid, insee) %>%
      summarize(especes = paste0(espece, collapse = ", "))
    causes <- sprintf("%s insee: %s, %s", df1[1, "admid"], df2[1, "insee"], df1[1, "causes"])
    liste <- append(liste, causes)
    liste <- append(liste, "\\smallbreak")
    liste <- append(liste, df2[[1, "especes"]])
    liste <- append(liste, "\\bigbreak")
  }
  tex_texte(liste, dossier = dossier)
}
#
## la partie statistiques --------------------------------------------
#
# source("geo/scripts/coj.R");actions_stat(suffixe = "test")
actions_stat <- function(suffixe) {
  carp("début")
#  dossier <- ""
#  donnees.df <<- lire_rds("donnees_fusion_sources", dossier = dossier) %>%
#    glimpse()
  actions_stat_annee_dept()
#  tex_df2kable(stat.df, dossier = dossier, suffixe = suffixe)
#  stop("*****")
  carp("fin")
}
#
# préparation du dataframe des stat par année et par département
# source("geo/scripts/coj.R");actions_stat_annee_dept()
actions_stat_annee_dept <- function(...) {
  library(dplyr)
  library(janitor)
  df1 <- donnees.df %>%
    group_by(annee, dept, admid) %>%
    summarise(nb_especes = n(), nb_oiseaux = sum(nb))
  carp("nrow:%s df1", nrow(df1))
  stat.df <<- df1 %>%
    group_by(annee, dept) %>%
    summarise(nb_jardins = n(), nb_oiseaux = sum(nb_oiseaux),
      nb_especes = sum(nb_especes), m_oiseaux = nb_oiseaux / nb_jardins, m_especes = nb_especes / nb_jardins
    ) %>%
    arrange(dept, annee) %>%
    glimpse()
  df1 <- donnees.df %>%
    group_by(annee, dept, source, admid) %>%
    summarise(nb_especes = n(), nb_oiseaux = sum(nb))
  carp("nrow:%s df1", nrow(df1))
  stat_source.df <<- df1 %>%
    group_by(annee, dept, source) %>%
    summarise(nb_jardins = n(), nb_oiseaux = sum(nb_oiseaux),
      nb_especes = sum(nb_especes), m_oiseaux = nb_oiseaux / nb_jardins, m_especes = nb_especes / nb_jardins
    ) %>%
#    mutate(m_oiseaux=format(m_oiseaux, digits=3, nsmall = 0)) %>%
    arrange(dept, annee, source) %>%
    glimpse()
  df2 <- stat_source.df %>%
    dplyr::select(dept, annee, source, nb = nb_jardins) %>%
    pivot_wider(names_from = annee, values_from = nb, values_fill = list(nb = 0)) %>%
    adorn_totals()
  misc_print(df2)
  tex_df2kable(df2, dossier = dossier, suffixe = "source")
  df3 <- stat_source.df %>%
    dplyr::select(dept, annee, source, nb = nb_jardins)
  gg <- ggplot(data = df3, aes(x = annee, y = nb, fill = dept, label = nb)) +
    geom_bar(stat = "identity") +
    geom_text(size = 3, position = position_stack(vjust = 0.5))
  print(gg)
  ggplot_pdf(gg, dossier = dossier, suffixe = "source")

#  return(invisible())
  df13 <- stat_source.df %>%
    dplyr::select(dept, annee, source, nb = nb_jardins) %>%
    mutate(source = dplyr::recode(source,
      "xl2012"  = "bv",
      "sql" = "bv",
      "sqlv2" = "bv",
      "faune" = "lpo",
      "lpo44" = "lpo",
      "odj" = "lpo"
    )) %>%
    group_by(annee, source) %>%
    summarize(nb = sum(nb))
  df14 <- df13 %>%
    pivot_wider(names_from = annee, values_from = nb, values_fill = list(nb = 0)) %>%
    adorn_totals()
  misc_print(df14)
  tex_df2kable(df14, dossier = dossier, suffixe = "asso")
#  carp("levels: %s", levels(as.factor(df13$source)))
  gg <- ggplot(data = df13,
      aes(x = annee, y = nb, fill = factor(source, levels = c("lpo", "bv")), label = nb)
    ) +
    theme(legend.title = element_blank()) +
    geom_bar(stat = "identity") +
    geom_text(size = 3, position = position_stack(vjust = 0.5))
#  gg <- ggplot_stoc_watermark(gg)
  print(gg)
  ggplot_pdf(gg, dossier = dossier, suffixe = "asso")

}
#
# les statistiques comme sur le site internet www.bretagne-vivante-dev.org/coj/bilan.php
# source("geo/scripts/coj.R");actions_stat_bilan()
actions_stat_bilan <- function(...) {
  library(dplyr)
  df1 <- donnees.df %>%
    filter(source == "sqlv2") %>%
    filter(annee == !! anneeRef) %>%
    glimpse()
  df2 <- df1 %>%
    group_by(espece, dept, admid) %>%
    summarize(nb_oiseaux = sum(nb)) %>%
    group_by(espece, dept) %>%
    summarize(nbj = n(), nbo = sum(nb_oiseaux))
  df3 <- df1 %>%
    group_by(dept, admid) %>%
    summarize(nb_oiseaux = sum(nb)) %>%
    group_by(dept) %>%
    summarize(nbj = n(), nbo = sum(nb_oiseaux)) %>%
    mutate(espece = "total") %>%
    dplyr::select(espece, dept, nbj, nbo)
  df4 <- rbind(df3, df2) %>%
    pivot_longer(c(-espece, -dept), names_to = "var", values_to = "nb") %>%
    pivot_wider(names_from = c(dept, var), values_from = nb, values_fill = list(nb = 0)) %>%
    glimpse()
  print(knitr::kable(df4, format = "pipe"))
  tex_df2kable(df4, dossier = dossier, font_size = 7)
}
#
# plusieurs années, plusieurs départements
actions_stat_annee_dept_bar <- function(...) {
  carp()
  library(ggplot2)
  gg <- ggplot(data = stat.df, aes(x = annee, y = nb_jardins, fill = dept, label = nb_jardins)) +
    geom_bar(stat = "identity") +
    geom_text(size = 3, position = position_stack(vjust = 0.5))
  print(gg)
  ggplot_pdf(dossier = dossier)
}
#
# stat par année
# source("geo/scripts/coj.R");actions_stat_annee()
actions_stat_annee <- function(...) {
  carp()
  library(ggplot2)
  theme.blank <- theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
  )
  df1 <- donnees.df %>%
    group_by(annee, admid) %>%
    summarise(nb_especes = n(), nb_oiseaux = sum(nb))
  carp("nrow:%s df1", nrow(df1))
  df2 <- df1 %>%
    group_by(annee) %>%
    summarise(nb_jardins = n(), nb_oiseaux = sum(nb_oiseaux),
      nb_especes = sum(nb_especes), m_oiseaux = nb_oiseaux  / nb_jardins, m_especes = nb_especes / nb_jardins
    ) %>%
    arrange(annee) %>%
    ungroup() %>%
    glimpse()
  gg <- ggplot(data = df2) +
    geom_line(mapping = aes(x = annee, y = m_oiseaux), group = 1, linewidth = 3, color = "blue") +
    annotate("text",  x = "2012", y = 35, label = "moyenne oiseaux", vjust = 1, hjust = 0, size = 5, color = "blue") +
    geom_line(mapping = aes(x = annee, y = m_especes), group = 1, linewidth = 3, color = "green") +
    annotate("text",  x = "2020", y = 15, label = "moyenne especes", vjust = 1, hjust = 1, size = 5, color = "green") +
    ylim(0, 45) +
    theme.blank
  print(gg)
  ggplot_pdf(gg, dossier = dossier)
}
#
## pour tester une action avec fichier stat.df
# source("geo/scripts/coj.R");actions_test()
actions_test <- function() {
  dossier <- ""
  donnees.df <<- lire_rds("donnees_fusion_sources", dossier = dossier) %>%
#    filter(source == "sqlv2") %>%
    glimpse()
#  actions_stat_annee_dept()
  actions_stat_annee_dept_juxta()
}
# stat par année et par département
actions_stat_annee_dept_juxta <- function(variable="m_especes") {
  carp()
  library(ggplot2)
  theme.blank <- theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
  )
  df1 <- stat.df %>%
    mutate(y = get(variable)) %>%
    mutate(label = round(y, digits = 1))
  gg <- ggplot(data = df1, aes(x = annee, y = y)) +
    geom_col() +
    geom_text(aes(label = label), position = position_stack(vjust = 0.5), color = "white") +
    theme.blank +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    facet_wrap(~dept)
  print(gg)
  ggplot_pdf(gg, dossier = dossier, suffixe = variable)
}
actions_stat_annee_dept_insee <- function(...) {
  library(ggplot2)
  df <- donnees.df
  df1 <- df %>%
    group_by(annee, dept, insee) %>%
    summarise(nb_especes = n(), nb_oiseaux = sum(nb))
  df2 <- df1 %>%
    group_by(annee, dept) %>%
    summarise(nb = n()) %>%
    glimpse()
  df3 <- df2 %>%
    pivot_wider(names_from = dept, values_from = nb, values_fill = list(nb = 0)) %>%
    glimpse()
  tex_df2kable(df3, dossier = dossier)
#  View(df2); stop('***')
  theme.blank <- theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
  )
  ids <<- unique(df2$dept)

  if (length(ids) == 1) {
    gg <- ggplot(data = df2, aes(x = annee, y = nb, label = nb)) +
      geom_bar(stat = "identity") +
      geom_text(position = position_stack(vjust = 0.5), color = "white", size = 3)
      theme.blank
  } else {
    gg <- ggplot(data = df2, aes(x = annee, y = nb, fill = dept, label = nb)) +
      geom_bar(stat = "identity") +
      geom_text(size = 3, position = position_stack(vjust = 0.5))
  }
  print(gg)
  ggplot_pdf(gg, dossier = dossier)
}
actions_stat_critere <- function(variable = "nb_oiseaux") {
  carp("variable: %s", variable)
  library(ggplot2)
  df <- donnees.df
  df1 <- df %>%
    group_by(annee, dept, admid) %>%
    summarise(nb_especes = n(), nb_oiseaux = sum(nb))
  df2 <- df1 %>%
    group_by(annee, dept, critere = get(variable)) %>%
    summarise(nb = n()) %>%
    glimpse()
#  View(df2); stop('***')
  theme.blank <- theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
  )
  gg <- ggplot(data = df2, aes(x = critere, y = nb, fill = annee)) +
    geom_bar(stat = "identity") +
    theme.blank +
    facet_wrap(~annee)
  print(gg)
#  stop('***')
  ggplot_pdf(gg, dossier = dossier, suffixe = variable)
}
#
# stat par année et par espèce
# http://www.milanor.net/blog/aggregation-dplyr-summarise-summarise_each/
# https://thinkr.fr/utiliser-la-grammaire-dplyr-pour-triturer-ses-donnees/
#
# la part d'une espèce parmi les autres espèces
actions_stat_espece_abondance <- function(...) {
  carp()
  library(tidyverse)
  df <- donnees.df
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
  tex_df2kable(df2, dossier = dossier, nb_lignes = 60, num = TRUE, font_size = 7)
  df2 <- df1 %>%
    group_by(annee) %>%
    mutate(totcells = sum(nb), # how many cells overall
      percent = round(1000 * nb / totcells, 0)) %>% #cells by landuse / total cells
    dplyr::select(-c(nb, totcells)) %>%
    spread(key = annee, value = percent, fill = 0)

  tex_df2kable(df2, dossier = dossier, suffixe = "pm", nb_lignes = 60, num = TRUE, font_size = 7)
# avec un graphique
  df2 <- df %>%
    group_by(espece) %>%
    summarize(nb = n()) %>%
    mutate(totcells = sum(nb),
      nb = round(1000 * nb / totcells, 0)
    )
  gg <- ggplot_flip(df2)
  print(gg)
  ggplot_pdf(gg, dossier = dossier)
#  stop('***')
# pour les principales espèces
  df2 <- df1 %>%
    group_by(annee) %>%
    mutate(totcells = sum(nb), # how many cells overall
      percent = round(1000 * nb / totcells, 0)) %>% #cells by landuse / total cells
    dplyr::select(-c(nb, totcells))
  df3 <- df2 %>%
    ungroup() %>%
    glimpse() %>%
    arrange(desc(annee), desc(percent))
  df3 <- df3[1:5, ] %>%
    ungroup() %>%
    glimpse()
  df4 <- df2 %>%
    filter(espece %in% df3$espece) %>%
    glimpse() %>%
    print(n = 20)
  gg <- ggplot(df4, aes(x = annee, y = percent, group = espece, colour = espece)) +
    geom_line(linewidth = 3) +
    geom_text(aes(label = percent), hjust = 0, vjust = 0, color = "black", check_overlap = TRUE)
  plot(gg)
  ggplot_pdf(gg, dossier = dossier, suffixe = "top")
}
#
# Moyenne du nombre d’oiseaux d’une espèce par jardin
actions_stat_espece_oiseaux <- function(...) {
  carp()
  library(tidyverse)
  df <- donnees.df
  annees <- distinct(df, annee)
  df1 <- df %>%
    group_by(annee, espece) %>%
    summarise(nb = sum(nb))
  df2 <- df %>%
    group_by(annee, admid) %>%
    summarize(nb = n()) %>%
    group_by(annee) %>%
    summarize(nb_jardins = n())
  df3 <- df1 %>%
    left_join(df2, by = c("annee" = "annee")) %>%
    mutate(nb  = sprintf("%0.2f", nb / nb_jardins)) %>%
    dplyr::select(espece, annee, nb) %>%
    spread(key = annee, value = nb, fill = 0) %>%
    glimpse()
  tex_df2kable(df3, dossier = dossier, nb_lignes = 60, num = TRUE)
  theme.blank <- theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
  )
# le graphique
  admids <- distinct(df, admid)
  nb_jardins <- nrow(admids)
  df4 <- df %>%
    group_by(espece) %>%
    summarise(nb = sum(nb) / nb_jardins) %>%
    arrange(desc(nb))
  gg <- ggplot_flip(df4)
  print(gg)
  ggplot_pdf(gg, dossier = dossier)
#  stop('***')
#
# pour avoir un graphique des principales espèces
  df3 <- df1 %>%
    left_join(df2, by = c("annee" = "annee")) %>%
    mutate(nb = nb / nb_jardins) %>%
    mutate(pc = sprintf("%0.2f", nb)) %>%
    glimpse()
  df5 <- df3 %>%
    arrange(desc(annee), desc(nb))
  df5 <- df5[1:7, ]
  df4 <- df3 %>%
    filter(espece %in% df5$espece) %>%
    glimpse() %>%
    print(n = 20)
  gg <- ggplot(df4, aes(x = annee, y = nb, group = espece, colour = espece, label = pc)) +
    geom_line(linewidth = 3) +
    geom_text(hjust = 0, vjust = 0, color = "black", check_overlap = TRUE)
  plot(gg)
  ggplot_pdf(gg, dossier = dossier, suffixe = "top")
}
#
## pour tester une action avec fichier donnes.df
# source("geo/scripts/coj.R");actions_test_donnees()
actions_test_donnees <- function() {
  dossier <<- ""
  donnees.df <<- lire_rds("donnees_fusion_sources", dossier = dossier) %>%
#    filter(source == "sqlv2") %>%
    glimpse()
  actions_stat_annee_espece_oiseaux()
}
#
# Moyenne du nombre d’oiseaux d’une espèce par jardin
actions_stat_annee_espece_oiseaux <- function(...) {
  carp()
  library(tidyverse)
  df <- donnees.df
  annees <- distinct(df, annee)
  if (nrow(annees) < 2) {
    txt2pdf("une seule année", dossier = dossier, suffixe = "tendance")
    txt2tex("une seule année", dossier = dossier, suffixe = "tendance")
    return()
  }
  df1 <- df %>%
    group_by(annee, espece) %>%
    summarise(nb = sum(nb))
  df2 <- df %>%
    group_by(annee, admid) %>%
    summarize(nb = n()) %>%
    group_by(annee) %>%
    summarize(nb_jardins = n())
#
# la moyenne de la dernière année est dans la moyenne des années précédentes ?
  df3 <- df1 %>%
    left_join(df2, by = c("annee" = "annee")) %>%
    mutate(nb = nb / nb_jardins) %>%
    dplyr::select(espece, annee, nb) %>%
    pivot_wider(names_from = annee, values_from = nb, values_fill = list(nb = 0)) %>%
    glimpse()
# https://www.gertstulp.com/post/different-ways-of-calculating-rowmeans-on-selected-variables-in-a-tidyverse-framework/
# les numéros de colonnes
  select_vars <- head(grep("^20", colnames(df3)), -1) %>%
    glimpse()
# la dernière année
  anneeDer <- rev(colnames(df3))[1]
  if (anneeDer != anneeRef) {
    stop("****")
  }
  df3 <- df3 %>%
    mutate(mean = apply(select(., all_of(select_vars)), 1, mean)) %>%
    mutate(median = apply(select(., all_of(select_vars)), 1, median)) %>%
    mutate(var = apply(select(., all_of(select_vars)), 1, var)) %>%
    mutate(sd = apply(select(., all_of(select_vars)), 1, sd)) %>%
    mutate(mean_inf = mean - sd) %>%
    mutate(mean_sup = mean + sd) %>%
    mutate(annee = get(anneeRef)) %>%
#    glimpse()
    mutate(normal_inf = if_else(annee < mean_inf, "inf", "")) %>%
    mutate(normal_sup = if_else(annee > mean_sup, "sup", "")) %>%
    mutate(tendance = sprintf("%s%s", normal_inf, normal_sup)) %>%
    filter(tendance != "") %>%
    mutate(annee = sprintf("%0.2f", annee)) %>%
    mutate(moyenne = sprintf("%0.2f", mean)) %>%
    mutate(tolerance = sprintf("%0.2f %0.2f", mean_inf, mean_sup)) %>%
    dplyr::select(espece, annee, moyenne, tolerance, tendance) %>%
    glimpse()
  df4 <- df %>%
    group_by(espece) %>%
    summarize(nb = n())
  depts <- distinct(df, dept)
  seuil <- 50 * nrow(depts) * nrow(annees)
  carp("seuil: %s", seuil)
  df5 <- df3 %>%
    left_join(df4, by = c("espece")) %>%
    filter(nb > seuil) %>%
    arrange(desc(annee)) %>%
    rename(`2024` = annee) %>%
    glimpse()
  tex_df2kable(df5, dossier = dossier, suffixe = "tendance", nb_lignes = 60, num = TRUE)
#
# pour avoir un graphique
  df3 <- df1 %>%
    left_join(df2, by = c("annee" = "annee")) %>%
    mutate(nb = nb / nb_jardins) %>%
    mutate(pc = sprintf("%0.2f", nb)) %>%
    glimpse()
  df4 <- df3 %>%
    filter(espece %in% df5$espece) %>%
    glimpse() %>%
    print(n = 20)
  gg <- ggplot(df4, aes(x = annee, y = nb, group = espece, colour = espece, label = pc)) +
    geom_line(linewidth = 3) +
    geom_text(hjust = 0, vjust = 0, color = "black", check_overlap = TRUE) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  plot(gg)
  ggplot_pdf(gg, dossier = dossier, suffixe = "tendance")
}
# source("geo/scripts/coj.R");actions_stat_espece_frequence()
actions_stat_espece_frequence <- function(...) {
  carp()
  library(tidyverse)
  df <- donnees.df
  annees <- distinct(df, annee)
  df1 <- df %>%
    group_by(annee, espece) %>%
    summarise(nb = n())
  df2 <- df1 %>%
    group_by(espece) %>%
    mutate(total = sum(nb)) %>%
    spread(annee, nb, 0) %>%
    arrange(desc(total)) %>%
    filter(total > 0)
#  df2 <- df2[1:50, ]
  tex_df2kable(df2, dossier = dossier, nb_lignes = 60, num = TRUE)
# le nombre de jardins par année
  df3 <- df %>%
    group_by(annee, admid) %>%
    summarize(nb = n()) %>%
    group_by(annee) %>%
    summarize(nbj = n()) %>%
    glimpse()
  df4 <- df1 %>%
    left_join(df3, by = c("annee")) %>%
    mutate(percent = round(100 * nb / nbj, 0)) %>%
    dplyr::select(-c(nb, nbj)) %>%
    glimpse()
  df5 <- df4 %>%
    pivot_wider(names_from = annee, values_from = percent, values_fill = list(percent = 0)) %>%
    glimpse()
#  print(knitr::kable(df5, format = "pipe"))
  tex_df2kable(df5, dossier = dossier, suffixe = "pm", nb_lignes = 60, num = TRUE)
#
# pour un graphique avec les espèces
  df11 <- df4 %>%
    glimpse() %>%
    arrange(desc(annee), desc(percent))
  df11 <- df11[1:5, ] %>%
    glimpse()
  df12 <- df4 %>%
    filter(espece %in% df11$espece) %>%
    glimpse()
  gg <- ggplot(df12, aes(x = annee, y = percent, group = espece, colour = espece)) +
    geom_line(linewidth = 3) +
    geom_text(aes(label = percent), hjust = 0, vjust = 0, color = "black", check_overlap = TRUE)
  plot(gg)
  ggplot_pdf(gg, dossier = dossier)
}
# source("geo/scripts/coj.R");actions_stat_especes_nb()
actions_stat_especes_nb <- function(...) {
  carp()
  library(tidyverse)
  tex <- ""
  df <- donnees.df
  especes <- distinct(df, espece)[, "espece"] %>%
    arrange(espece)
#  dossier <- "especes"
  for (i in 1:nrow(especes)) {
    espece <- especes[[i, "espece"]]
    tex <- append(tex, actions_stat_espece_nb(espece, dossier))
  }
  tex_texte(tex, dossier = dossier, escape = FALSE)
#  tex_pdflatex("coj_especes.tex")
}
#
# l'histogramme pour une espèce
actions_stat_espece_nb <- function(espece, dossier) {
  carp("espece: %s dossier: %s", espece, dossier)
  library(ggplot2)
  library(scales)
  e <- camel5(espece)
  tex <- sprintf("\\subsection*{%s}", espece)
  tex <- append(tex, sprintf("\\includegraphics[width=0.30\\textwidth,keepaspectratio]{%s/actions_stat_espece_nb_%s.pdf}", dossier, e))
#  return(invisible(tex))
  df1 <- donnees.df %>%
    filter(espece == !! espece) %>%
    arrange(nb)
  n <- .995 * nrow(df1)
  df1 <- head(df1, n) %>%
    glimpse()
  df2 <- df1 %>%
    group_by(espece, nb) %>%
    summarize(n = n()) %>%
    glimpse()
  gg <- ggplot(data = df2, aes(x = nb, y = n )) +
    geom_col() +
    scale_x_continuous(breaks = ggplot_integer_breaks())
  theme_gg <- theme(
    panel.background = element_rect(fill = "grey"),
    plot.background = element_rect(fill = "grey"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
  )
  gg <- gg + theme_gg
  maxX <- max(df2$nb)
  maxY <- max(df2$n)
  gg <- gg +
    annotate("text", x = maxX, y = maxY, label = "@ Marc Gauthier CC-BY-NC-ND"
    , hjust=1, vjust=0, col="white", cex=3,  fontface = "bold", alpha = 0.8
    )
  print(gg)
  ggplot_pdf(gg, dossier = dossier, suffixe = e, width=8, height=4)
  return(invisible(tex))
}
#
#
## la partie carto ------------------------------------------------------------------------
#
# la version en sf avec ggplot
# source("geo/scripts/coj.R");actions_carto("Annee=")
actions_carto <- function(...) {
  carp()
  library(sf)
  library(tidyverse)
  library(ggplot2)
#  library(patchwork)
#  library(lwgeom)
  library(ggspatial)
  filtre <- ""
  z <- list(...)
  carp("z: %s length(z): %s", z, length(z))
  if (! is.na(z)) {
    filtre <- z
    params <- actions_params(...)
  } else {
    params <- list()
  }

  Annee <- params$Annee
  carp("Annee: %s class: %s", Annee, class(Annee))
#  stop("****")
  df <- donnees.df %>%
    glimpse()
  if (is.character(Annee)) {
    carp("filtrage Annee: %s", Annee)
    df <- df %>%
      filter(annee == !! Annee) %>%
      glimpse()
  }
  source <- params$souree
  if (class(source) == "character") {
    carp("source: %s", source)
    filtre <- source
    df <- df %>%
      filter(source == !! source) %>%
      glimpse()
  }
  depts.df <- group_by(df, dept) %>%
    summarize(nb = n())
  if (! exists("commune.sf")) {
    commune.sf <<- ign_adminexpress_lire_sf("COMMUNE") %>%
      glimpse()
  }
  carp("les communes")
  nc <- commune.sf %>%
    filter(INSEE_DEP %in% depts.df$dept)
  par(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0))
  stat.df <- df %>%
    group_by(annee, insee, admid) %>%
    summarise(nb_especes = n(), nb_oiseaux = sum(nb)) %>%
    group_by(insee) %>%
    summarise(nb_jardins = n(), nb_oiseaux = sum(nb_oiseaux), nb_especes = sum(nb_especes),
      m_oiseaux = nb_oiseaux / nb_jardins, m_especes = nb_especes / nb_jardins
    )
  nc <- nc %>%
    left_join(stat.df, by = c("INSEE_COM" = "insee"))
#  theme_set(theme_map(base_size = 14))

  gg <- ggplot(nc) +
    geom_sf(aes(fill = nb_jardins)) +
    theme_void() +
    coord_sf(datum = NA) +
    scale_fill_distiller(palette = "Spectral", name = "nb", na.value = "white") +
    annotation_scale(location = "br", line_width = .5) +
    theme(legend.position = c(0.1, 0.2))
  gg <- gg +
    annotate("text", -Inf, Inf, label = filtre, hjust = 0, vjust = 1)
#  gg <- gg + annotation_custom(ggplot_grid_text(position = "topright"))
  print(gg)
  if (is.character(Annee)) {
    carp("Annee: %s ggpdf", Annee)
    ggplot_pdf(gg, dossier = dossier, suffixe = Annee)
  } else {
    ggplot_pdf(gg, dossier = dossier)
  }
}
#
# conversion en named list
# http://www.r-tutor.com/r-introduction/list/named-list-members
# source("geo/scripts/coj.R");actions_params("Annee=2022;source=sql")
actions_params <- function(...) {
  filtre <- ""
  z <- list(...) %>%
    glimpse()

  v <- str_split(z[1], ";") %>%
    glimpse()
#  attached <- search()
#  attached <- attached[grepl("params", attached)]
#  for (a in attached) {
#    detach(params)
#  }
  params <- str_split(v[[1]], "=") %>%
    set_names(., map(., 1)) %>%
    map(tail, -1) %>%
    glimpse()

  return(invisible(params))
}
#
# la version en sp
actions_carto_sp <- function(...) {
  carp()
  library(sp)
  z <- list(...) %>%
    glimpse()
  v <- str_split(z[1], ";") %>%
    glimpse()
# http://www.r-tutor.com/r-introduction/list/named-list-members
  params <- str_split(v[[1]], "=") %>%
    set_names(., map(., 1)) %>%
    map(tail, -1) %>%
    glimpse()
  attach(params)
  df <- donnees.df %>%
    glimpse()
  if (exists("annee")) {
    carp("annee: %s", annee)
    df <- df %>%
      filter(annee == !! annee) %>%
      glimpse()
  }
  depts.df <- group_by(df, dept) %>%
    summarize(nb = n()) %>%
    glimpse()
  if (! exists("commune.sf")) {
    commune.sf <<- ign_adminexpress_lire_sf("COMMUNE") %>%
      glimpse()
  }
  carp("les communes")
  nc <- commune.sf %>%
    filter(INSEE_DEP %in% depts.df$dept) %>%
    glimpse()
  spdf <- as(nc, "Spatial")
  spdf@data <- spdf@data[, c("INSEE_COM", "NOM_COM")] %>%
    glimpse()
#  stop("******")
  df <- df %>%
    group_by(annee, insee, admid) %>%
    summarise(nb_especes = n(), nb_oiseaux = sum(nb)) %>%
    group_by(insee) %>%
    summarise(nb_jardins = n(), nb_oiseaux = sum(nb_oiseaux),
      nb_especes = sum(nb_especes), m_oiseaux = nb_oiseaux / nb_jardins, m_especes = nb_especes / nb_jardins
    ) %>%
    glimpse()
  carp("top communes")
  df %>%
    arrange(-nb_jardins) %>%
    print(n = 10)
  vPal <- c("white", rev(heat.colors(5)))
  df$sym <- vPal[1]
  df[df$nb_jardins > 0, "sym"] <- vPal[2]
  df[df$nb_jardins > 1, "sym"] <- vPal[3]
  df[df$nb_jardins > 4, "sym"] <- vPal[4]
  df[df$nb_jardins > 10, "sym"] <- vPal[5]
  df[df$nb_jardins > 50, "sym"] <- vPal[6]

  classes <- c(0, 1, 5, 10, 50, 100)
  spdf1 <- spdf
  spdf1@data <- data.frame(spdf@data, df[match(spdf@data$INSEE_COM, df$insee), ]) %>%
    glimpse()
  spdf1$nb_jardins[is.na(spdf1$nb_jardins)] <- 0
  par(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0))
  plot(spdf1, col = spdf1@data$sym)
  spdf2 <- spdf1[spdf1@data$nb_jardins > 4, ]
  text(coordinates(spdf2), labels = spdf2@data$nb_jardins, font = 2, cex = 1)
  if (exists("annee")) {
    dev2pdf(dossier = dossier, suffixe = annee)
  } else {
    dev2pdf(dossier = dossier)
  }
  return()
  dsn <- sprintf("%s/stat_commune_dept.pdf", texDir)
  dev.copy(pdf, dsn, width = par("din")[1], height = par("din")[2])
  dev.off()
  carp("dsn:%s", dsn)
}
#
#
## la partie geoca ------------------------------------------------------------------------
#
#
# pour la mésange bleue du geoca
# source("geo/scripts/coj.R");actions_geoca()
actions_geoca <- function() {
  carp()
  df <- lire_rds("donnees_fusion_sources") %>%
    mutate(nb = as.integer(nb)) %>%
    mutate(admid = sprintf("%s/%s/%s", annee, source, admid)) %>%
    filter(! is.na(espece)) %>%
    filter(grepl("sange bleue", espece)) %>%
    filter(nb < 60) %>%
    glimpse() %>%
    group_by(nb) %>%
    summarize(n = n()) %>%
    glimpse()
  gg <- ggplot(df, aes(x = nb, y = n)) +
    geom_line(linewidth = 3)

  plot(gg)
}
