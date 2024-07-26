# <!-- coding: utf-8 -->
#
# les traitements sur les données en base postgresql
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
#
# la base postgresql/postgis est dans un conteneur docker, avec un espace disque partagé
# docker run -p 5432:5432 --name pg_visionature -e POSTGRES_PASSWORD=tgbtgb -v d:/docker/postgres-data/:/tmp -d postgis/postgis
#
# https://db.rstudio.com/best-practices/schema/
# https://www.r-bloggers.com/using-postgresql-in-r-a-quick-how-to/

# https://dzone.com/articles/using-jsonb-in-postgresql-how-to-effectively-store
# https://www.postgis.us/presentations/PGOpen2018_data_loading.pdf
# https://stackoverflow.com/questions/34688465/how-do-i-run-a-sql-file-of-inserts-through-docker-run
#
psql_user_old <- FALSE
psql_connect <- function(user = "visio_root") {
  library(DBI)
  library(RPostgreSQL)
  if (psql_user_old == user) {
    return(invisible(db_con))
  }
  carp("user: %s", user)
  mga_docker_pg(user)
  db_con <<- dbConnect(RPostgres::Postgres(),
    dbname = db_name,
    host = db_host,
    port = db_port,
    user = db_user,
    password = db_password
  )
  psql_user_old <- user
  return(invisible(db_con))
}
psql_databases_liste <- function() {
  psql <- "
SELECT datname FROM pg_database
WHERE datistemplate = false;
"
  res <- db_SendQuery(db_con, psql)
  print(knitr::kable(res, format = "pipe"))
}
# source("geo/scripts/fb.R");psql_extensions_init()
psql_extensions_init <- function() {
  psql <- "
CREATE EXTENSION IF NOT EXISTS postgis;
CREATE EXTENSION IF NOT EXISTS postgis_topology;
CREATE EXTENSION IF NOT EXISTS hstore;
CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;
CREATE EXTENSION IF NOT EXISTS \"uuid-ossp\";
CREATE EXTENSION IF NOT EXISTS pg_trgm;
CREATE EXTENSION IF NOT EXISTS fuzzystrmatch;
"
  res <- db_SendQueries(db_con, psql)
  psql_extensions_liste()
}
psql_extensions_liste <- function(affiche = TRUE) {
  psql <- "
SELECT name FROM pg_available_extensions order by 1;
"
  res <- db_SendQuery(db_con, psql)
  print(knitr::kable(res, format = "pipe"))
}
psql_schema_liste <- function(schema = "public") {
  psql <- "
SELECT table_schema, table_name
FROM information_schema.tables
WHERE table_schema = '%s'
ORDER BY table_schema,table_name;
"
  psql <- sprintf(psql, schema)
  res <- db_SendQuery(db_con, psql)
  print(knitr::kable(res, format = "pipe"))
}
# https://dataedo.com/kb/query/postgresql/list-user-defined-functions
# postgresql 11
psql_functions_liste <- function(affiche = FALSE) {
  psql <- "
select n.nspname as schema_name,
       p.proname as specific_name,
       case p.prokind
            when 'f' then 'FUNCTION'
            when 'p' then 'PROCEDURE'
            when 'a' then 'AGGREGATE'
            when 'w' then 'WINDOW'
            end as kind,
       l.lanname as language,
       case when l.lanname = 'internal' then p.prosrc
            else pg_get_functiondef(p.oid)
            end as definition,
       pg_get_function_arguments(p.oid) as arguments,
       t.typname as return_type
from pg_proc p
left join pg_namespace n on p.pronamespace = n.oid
left join pg_language l on p.prolang = l.oid
left join pg_type t on t.oid = p.prorettype
where n.nspname not in ('pg_catalog', 'information_schema')
order by schema_name,
         specific_name;
"
  res <- db_SendQuery(db_con, psql)
  if (affiche == TRUE) {
    df <- res %>%
      filter(type == "TABLE")
    print(knitr::kable(df, format = "pipe"))
  }
  return(invisible(res))
}

# en direct de https://dba.stackexchange.com/questions/30061/how-do-i-list-all-tables-in-all-schemas-owned-by-the-current-user-in-postgresql/30063
psql_tables_liste <- function(affiche = FALSE) {
  psql <- "
select nsp.nspname as table_schema,
       cls.relname as table_name,
       rol.rolname as owner,
       case cls.relkind
         when 'r' then 'TABLE'
         when 'm' then 'MATERIALIZED_VIEW'
         when 'i' then 'INDEX'
         when 'S' then 'SEQUENCE'
         when 'v' then 'VIEW'
         when 'c' then 'TYPE'
         else cls.relkind::text
       end as type
from pg_class cls
  join pg_roles rol on rol.oid = cls.relowner
  join pg_namespace nsp on nsp.oid = cls.relnamespace
where nsp.nspname not in ('information_schema', 'pg_catalog')
  and nsp.nspname not like 'pg_toast%'
  and rol.rolname = current_user  --- remove this if you want to see all objects
order by nsp.nspname, cls.relname;
"
  res <- db_SendQuery(db_con, psql)
  if (affiche == TRUE) {
    df <- res %>%
      filter(type == "TABLE")
    print(knitr::kable(df, format = "pipe"))
  }
  return(invisible(res))
}
# source("geo/scripts/fb.R"); df <- psql_tables_count()
psql_tables_count <- function(affiche = FALSE) {
  psql <- "
select table_schema,
       table_name,
       (xpath('/row/cnt/text()', xml_count))[1]::text::int as row_count
from (
  select table_name,
    table_schema,
    query_to_xml(format('select count(*) as cnt from %I.%I', table_schema, table_name), false, true, '') as xml_count
  from information_schema.tables
  where table_schema not in ('information_schema', 'pg_catalog')
) t
ORDER BY table_name, table_schema;
"
  res <- db_SendQuery(db_con, psql)
  if (affiche == TRUE) {
    df <- res
    print(knitr::kable(df, format = "pipe"))
  }
  return(invisible(res))
}
#
# source("geo/scripts/fb.R");psql_table_display()
psql_table_display <- function(schema='src_vn.observations', table = FALSE, affiche = FALSE) {
  if (table == FALSE) {
    st <- strsplit(schema, '\\.')[[1]]
    schema <- st[1]
    table <- st[2]
  }
  psql <- "
SELECT
  tablename,
  indexname,
  indexdef
FROM
  pg_indexes
WHERE
  schemaname = '%s' AND tablename = '%s'
ORDER BY
  tablename,
  indexname;"
  psql <- sprintf(psql, schema, table)
  res <- db_SendQuery(db_con, psql)
  if (affiche == TRUE) {
    df <- res
    print(knitr::kable(df, format = "pipe"))
  }
  return(invisible(res))
}
#
# la lecture peut s'effecteur sur "schema, table" ou "schema.table"
# source("geo/scripts/fb.R");psql_table_lire('public', 'reptiles')
psql_table_lire <- function(schema='gn_imports', table = FALSE, affiche = FALSE, nb_lignes = 5) {
  library(tidyverse)
  library(dbplyr, quietly = TRUE, warn.conflicts = FALSE)
  if (table == FALSE) {
    st <- strsplit(schema, '\\.')[[1]]
    schema <- st[1]
    table <- st[2]
  }
  print(sprintf('table %s.%s', schema, table))
  df <- tbl(db_con, in_schema(schema, table))
  if (affiche != FALSE) {
    print(knitr::kable(head(df, nb_lignes), format = "pipe"))
  }
  return(invisible(df))
}
#
# création d'une table à partir d'un fichier json
# source("geo/scripts/fb.R");psql_json2table()
psql_json2table <- function(user = "transfer19", schema='public', table = "mga", dsn = "D:/bvi35/CouchesFauneBretagne/api/observations_diff.json" ) {
  library(tidyverse)
  library(dbplyr, quietly = TRUE, warn.conflicts = FALSE)
  library(DBI)
  library(jsonlite)
  json.df <-  jsonlite::fromJSON(dsn) %>%
    glimpse()
  table <- sprintf("%s.%s", schema, table)
  psql_connect(user = user)
  psql <- "
CREATE SCHEMA IF NOT EXISTS %s;
"
  psql <- sprintf(psql, schema)
  dbWriteTable(db_con, SQL(table), json.df, row.names = FALSE, append = FALSE, overwrite = TRUE)
}
# source("geo/scripts/fb.R");psql_json2table()
psql_json2table_v2 <- function(schema='public', table = "mga", dsn = "D:/bvi35/CouchesFauneBretagne/api/observations_diff.json" ) {
  library(tidyverse)
  library(dbplyr, quietly = TRUE, warn.conflicts = FALSE)
  library(DBI)

  data <- read_file(dsn) %>%
    glimpse()
  table <- sprintf("%s.%s", schema, table)
  psql_connect(user = "transfer25")
  psql <- "
CREATE SCHEMA IF NOT EXISTS %s;
CREATE TABLE IF NOT EXISTS %s (data VARCHAR);
TRUNCATE TABLE %s;
"
  psql <- sprintf(psql, schema, table, table)
  res <- db_SendStatements(db_con, psql)
  df <- tibble(data = data)
  dbWriteTable(db_con, SQL(table), df, row.names = FALSE, append = TRUE)
  psql <- "
ALTER TABLE mga ALTER COLUMN data TYPE JSONB USING data::JSONB;
DROP TABLE IF EXISTS mga2;
CREATE TABLE mga2 (id_sighting varchar(30), id_universal varchar(30), modification_type varchar(30));
insert into mga2 (id_sighting, id_universal, modification_type) SELECT p.* FROM mga l CROSS JOIN LATERAL JSON_POPULATE_RECORDSET(null::mga2, data::json) AS p;
 "
  psql <- sprintf(psql, table, table)
  res <- db_SendStatements(db_con, psql)

}
psql_tables_head <- function(affiche = TRUE) {
  psql_connect(user = psql_user)
  df <- psql_tables_liste(affiche = FALSE) %>%
    filter(type == "TABLE")
  for(i in 1:nrow(df)) {
    psql_table_lire(df[[i, "table_schema"]], df[[i, "table_name"]], affiche = TRUE)
  }
}
# https://stackoverflow.com/questions/25202133/how-to-get-the-triggers-associated-with-a-view-or-a-table-in-postgresql/25202347
psql_triggers_liste <- function(affiche = FALSE) {
  psql <- "
select    tgname
  ,relname
  ,tgenabled
  ,nspname    from    pg_trigger
  join    pg_class    on    (pg_class.oid=pg_trigger.tgrelid)
  join    pg_namespace    on    (nspowner=relowner);
"
  res <- db_SendQuery(db_con, psql)
  if (affiche == TRUE) {
    df <- res %>%
      filter(type == "TABLE")
    print(knitr::kable(df, format = "pipe"))
  }
  return(invisible(res))
}
psql_triggers_liste <- function(affiche = FALSE) {
  psql <- "
SELECT
  ns.nspname || '.' || tbl.relname as trigger_table,
  trg.tgname,
  pg_proc.proname
FROM pg_trigger trg
 JOIN pg_proc on trg.tgfoid = pg_proc.oid
 JOIN pg_class tbl on trg.tgrelid = tbl.oid
 JOIN pg_namespace ns ON ns.oid = tbl.relnamespace
WHERE NOT tgisinternal
ORDER BY trigger_table, tgname;
"
  res <- db_SendQuery(db_con, psql)
  if (affiche == TRUE) {
    df <- res
    print(knitr::kable(df, format = "pipe"))
  }
  psql <- "
SELECT proname, prosrc, proargnames FROM pg_proc WHERE proname like '%s';
"
  for (i in 1:nrow(df)) {
    proname <- df[[i, "proname"]]
    psql1 <- sprintf(psql, proname)
    res <- db_SendQuery(db_con, psql1)
    df1 <- res
    print(knitr::kable(df1, format = "pipe"))
  }
  return(invisible(res))
}
psql_version_liste <- function(affiche = TRUE) {
  psql <- "
SELECT version();
"
  res <- db_SendQuery(db_con, psql)
  print(knitr::kable(res, format = "pipe"))
}
#
# source("geo/scripts/fb.R");psql_triggers_display()
psql_triggers_display <- function(affiche = FALSE) {
  psql <- "
SELECT
  trigger_schema || '.' || trigger_name as trigger_table,
  event_object_table,
  event_manipulation,
  action_statement,
  action_timing
FROM information_schema.triggers
ORDER BY trigger_table, event_object_table, event_manipulation;"
  res <- db_SendQuery(db_con, psql)
  if (affiche == TRUE) {
    df <- res
    print(knitr::kable(df, format = "pipe"))
  }
  return(invisible(res))
}
#
# source("geo/scripts/fb.R");psql_fdw_setup()
psql_fdw_setup <- function(affiche = FALSE) {
  psql <- "
BEGIN;
CREATE EXTENSION IF NOT EXISTS postgres_fdw;

DROP SERVER IF EXISTS {{foreign_server}} CASCADE;
CREATE SERVER {{foreign_server}}
  FOREIGN DATA WRAPPER postgres_fdw
  OPTIONS (host '{{foreign_host}}', port '{{foreign_port}}', dbname '{{foreign_dbname}}');
CREATE USER MAPPING FOR USER
  SERVER {{foreign_server}}
  OPTIONS (user '{{foreign_user}}', password '{{foreign_password}}');

DROP SCHEMA IF EXISTS {{local_schema}};
CREATE SCHEMA {{local_schema}};
IMPORT FOREIGN SCHEMA {{foreign_schema}}
  FROM SERVER {{foreign_server}}
  INTO {{local_schema}};
COMMIT;
";
  variables <- list(
    foreign_server = "transfer26",
    foreign_host = "192.168.0.26",
    foreign_port = "5432",
    foreign_dbname = "fb_db",
    foreign_user = "fb_user",
    foreign_password = "fb_password",
    local_schema = "transfer26_public",
    foreign_schema = "public"
  )
  psql <- misc_list2tpl(variables, psql)
  dsn <- sprintf("%s/psql_fdw_setup.sql", cfgDir)
  write(psql, dsn)
  carp("dsn: %s", dsn)
}