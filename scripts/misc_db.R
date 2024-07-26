# <!-- coding: utf-8 -->
#
# les traitements sur les données en base postgresql
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
#
# https://db.rstudio.com/best-practices/schema/
# https://www.r-bloggers.com/using-postgresql-in-r-a-quick-how-to/
#
db_postgresql_connect <- function() {
  library(DBI)
  library(RPostgreSQL)
  carp()
  db_con <<- dbConnect(RPostgres::Postgres(),
    dbname = db_name,
    host = host,
    port = db_port,
    user = db_user,
    password = db_password
  )
}
db_MySQL_killConnections <- function () {
  all_cons <- dbListConnections(MySQL())
  for(con in all_cons) {
    dbDisconnect(con)
  }
  print(paste(length(all_cons), " connections killed."))
}
#
# la lecture peut s'effecteur sur "schema, table" ou "schema.table"
# source("geo/scripts/geonature.R");postgresql_table_lire('gn_synthese', 'synthese')
postgresql_table_lire <- function(schema='gn_imports', table=FALSE) {
  library(tidyverse)
  library(dbplyr)
  if (table == FALSE) {
    st <- strsplit(schema, '\\.')[[1]]
    schema <- st[1]
    table <- st[2]
  }
  carp('%s.%s', schema, table)
  df <- tbl(db_con, in_schema(schema, table)) %>%
    head() %>%
    glimpse()
  return(invisible(df))
}
#
# plus requêtes
# SendQueries : ne retourne pas un dataframe
db_SendQueries <- function(db_con, sql) {
  carp()
#
  sql <- gsub("(^[[:space:]]+|[[:space:]]+$)", "", sql)
  if ( ! grepl(';$', sql) ) {
		sql <- sprintf('%s;', sql)
	}
  sqls <- strsplit(sql, "\\n")[[1]]
  for (i in 1:length(sqls)) {
    sql <- sqls[i]
    db_SendQuery(db_con, sql)
  }
}
db_sql2sqls <- function(sql) {
  carp()
#
  sql <- gsub("(^[[:space:]]+|[[:space:]]+$)", "", sql)
  if ( ! grepl(';$', sql) ) {
		sql <- sprintf('%s;', sql)
	}
  sqls <- strsplit(sql, "\\n")[[1]]
  return(invisible(sqls))
}
#
# un requête avec gestion des erreurs
db_SendQuery <- function(db_con, sql) {
  sql <- gsub(";+$", ";", sql)
  carp('sql: %s', sql)
  tryCatch({
    result <- DBI::dbSendQuery(db_con, sql)
    mga <<- result
    df <- dbFetch(result, n = -1)
    dbClearResult(result)
    return(invisible(df))
  },
    error = function(e) {
      print('dbSendQuery')
      print(e)
#      stop('***')
    }
  )
}
#
# un requête avec gestion des erreurs
db_SendStatement <- function(db_con, sql) {
  sql <- gsub(";+$", ";", sql)
  carp('sql: %s', sql)
  tryCatch({
    df_postgres <- DBI::dbSendStatement(db_con, sql) %>%
      glimpse()
  },
    error = function(e) {
      print('dbSendStatement')
      print(e)
      stop('***')
    }
  )
  return(invisible(df_postgres))
}
db_SendStatements <- function(con, sql) {
  carp()
  sqls <- db_sql2sqls(sql)
  for (i in 1:length(sqls)) {
    sql <- sqls[i]
    db_SendStatement(con, sql)
  }
}
#
# remplacement de champs clé, valeur à partir d'un dataframe
db_df2tpl <- function(df, tpl) {
  for ( i in 1:nrow(df) ) {
    cle <- df[i, 'cle']
    val <- df[i, 'valeur']
    re <- paste0("\\{\\{", cle, "\\}\\}")
    tpl <-  gsub(re, val, tpl, perl=TRUE)
  }
  return(invisible(tpl))
}
#
# préparation d'une requête sql INSERT INTO
db_df2sql <- function(df, tpl) {
  CLES <- paste(df$cle, collapse = ', ')
  VALEURS <- paste(df$valeur, collapse = "', '")
  VALEURS <- sprintf("'%s'", VALEURS)
  VALEURS <- gsub("'(TRUE|FALSE)'", "\\1", VALEURS)
  VALEURS <- gsub("'(\\d+)'", "\\1", VALEURS)
  df1 <- tribble(
    ~cle,~valeur
  )
  df1[1, 'cle'] <- 'VALUES'
  df1[1, 'valeur'] <- VALEURS
  df1[2, 'cle'] <- 'CLES'
  df1[2, 'valeur'] <- CLES
#  glimpse(df1)
  sql <- db_df2tpl(df1, tpl)
  return(invisible(sql))
}