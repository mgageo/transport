# <!-- coding: utf-8 -->
#
# utilisation de docker
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
# ===============================================================
# auteur : Marc Gauthier
#
#
# source("geo/scripts/transport.R"); docker_jour(force = TRUE)
docker_jour <- function(force = TRUE) {

}
#
# source("geo/scripts/transport.R"); docker_valhalla(force = TRUE)
docker_valhalla <- function(force = TRUE) {
  system2("docker", "ps -a")
}