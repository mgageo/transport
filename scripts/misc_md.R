# <!-- coding: utf-8 -->
#
# quelques fonctions pour générer les fichiers markdown
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
#
md_txt <- ''
md_entete <- function(titre = "titre") {
  md <- '---
title: "%s"
author: "Marc Gauthier"
date: "10 avril 2017"
output: html_document
---'
  md_txt <<- sprintf(md, titre)
}
md_append <- function(texte = "titre") {
  md_txt <<- append(md_txt, texte)
}
md_print <- function(dsn) {
  library(knitr)
  library(rmarkdown)
  carp("dsn: %s", dsn)
#  print(md_txt)
#  dsn <- sprintf("%s/toto.md", varDir)
  MD <- file(dsn, encoding="UTF-8")
  write(md_txt, file = MD, append = FALSE)
  close(MD)
  render(dsn, output_format = "html_document")
}