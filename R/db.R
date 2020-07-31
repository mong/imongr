#' Low level database functions for imongr
#'
#' Database metadata are read from config. If one or more of these are
#' defined 'env' corresponding values will be fetched from the environmental
#' variables IMONGR_DB_HOST, IMONGR_DB_NAME, IMONGR_DB_USER and IMONGR_DB_PASS.
#' If these are undefined the function providing connection handles will exit
#' with an error. What the function expects from table names and variable
#' names in the data is also defined by the configuration. Thus, functions can
#' be left unchanged when the data model is altered as long as such changes
#' are reflected by configuration.
#'
#' Ordinary user interactions with data should have their own functions, but
#' may be built ontop of these ones. For instance, such functions must make
#' sure consistency (\emph{e.g.} foreign keys) between database tables are
#' kept in order.
#'
#' @param pool a database connection pool object
#' @param registry Integer defining registry id
#' @param table string defining target database table
#' @param sample Numeric in the range 0 to 1 defining the relative sub-sample
#' size, \emph{e.g.} when \code{sample = 0.1} approximately 10\% of
#' observations are returned. Default is \code{NA} which will return all data
#' @param df data frame containing data to be inserted into a database
#' @return Database pool object, data frame or status message
#' @name db
#' @aliases make_pool drain_pool insert_tab get_table get_agg_data get_data
#' get_delivery get_user get_user_registry get_indicator get_registry_name
NULL

#' @rdname db
#' @export
make_pool <- function() {

  conf <- get_config()

  host <- conf$db$host
  if (host == "env") {
    if ("IMONGR_DB_HOST" %in% names(Sys.getenv())) {
      host <- Sys.getenv("IMONGR_DB_HOST")
    } else {
      stop(paste("No database host defined in config or environment",
                 "varaible IMONGR_DB_HOST. Cannot go on."))
    }
  }

  dbname <- conf$db$name
  if (dbname == "env") {
    if ("IMONGR_DB_NAME" %in% names(Sys.getenv())) {
      dbname <- Sys.getenv("IMONGR_DB_NAME")
    } else {
      stop(paste("No database name defined in config or environment",
                 "varaible IMONGR_DB_NAME. Cannot go on."))
    }
  }

  username <- conf$db$user
  if (username == "env") {
    if ("IMONGR_DB_USER" %in% names(Sys.getenv())) {
      username <- Sys.getenv("IMONGR_DB_USER")
    } else {
      stop(paste("No database username defined in config or environment",
                 "variable IMONGR_DB_USER. Cannot go on."))
    }
  }

  password <- conf$db$pass
  if (conf$db$pass == "env") {
    if ("IMONGR_DB_PASS" %in% names(Sys.getenv())) {
      password <- Sys.getenv("IMONGR_DB_PASS")
    } else {
      stop(paste("No database password defined in config or environment",
                 "variable IMONGR_DB_PASS. Cannot go on."))
    }
  }

  pool::dbPool(
    drv = RMariaDB::MariaDB(),
    dbname = dbname,
    host = host,
    username = username,
    password = password,
    idleTimeout = 60000
  )
}


#' @rdname db
#' @export
drain_pool <- function(pool) {
  pool::poolClose(pool)
}


#' @rdname db
#' @export
insert_tab <- function(pool, table, df) {

  conf <- get_config()

  if (!table %in% names(conf$db$tab)) {
    stop(paste("Table specified is not recognized. Valid tables are:",
               paste(names(conf$db$tab), collapse = ", ")))
  }

  insert <- conf$db$tab[[table]]$insert

  if (!length(names(df)) == length(insert)) {
    stop(paste0("In 'df' the number of variables (",
                length(names(df)), ") is not equal to what was expected (",
                length(insert), ")"))
  }

  is_member <- names(df) %in% insert

  if (!all(is_member)) {
    stop(paste0("One or more variable names (",
                paste(names(df)[!is_member], collapse = ", ",
                ") do not match what was expected (", length(insert))))
  }

  pool::dbWriteTable(pool, table, df[insert], append = TRUE,
                       row.names = FALSE)
}


#' @rdname db
#' @export
get_table <- function(pool, table) {

  f <- paste0("get_", table)
  do.call(f, args = list(pool = pool))
}

#' @rdname db
#' @export
get_agg_data <- function(pool) {

  conf <- get_config()
  query <- paste0("
SELECT
  ", paste0(conf$db$tab$agg_data$insert, collapse = ",\n "), "
FROM
  agg_data;")

  pool::dbGetQuery(pool, query)
}

#' @rdname db
#' @export
get_data <- function(pool, sample = NA) {

  conf <- get_config()
  query <- paste0("
SELECT
  ", paste0("var.", conf$db$tab$data$insert, collapse = ",\n  "), "
FROM
  data var
LEFT JOIN
  delivery d
ON
  var.delivery_id = d.id
WHERE
  d.latest = 1")

  if (!is.na(sample) && sample > 0 && sample < 1) {
    query <- paste(query, "AND RAND() <", sample)
  }

  pool::dbGetQuery(pool, query)
}


#' @rdname db
#' @export
get_delivery <- function(pool, sample = NA) {

  conf <- get_config()
  query <- paste0("
SELECT
  ", paste0(conf$db$tab$delivery$insert, collapse = ",\n "), "
FROM
  delivery")

  if (!is.na(sample) && sample > 0 && sample < 1) {
    query <- paste(query, "\nWHERE\n  RAND() <", sample)
  }

  pool::dbGetQuery(pool, query)
}


#' @rdname db
#' @export
get_user <- function(pool, sample = NA) {

  conf <- get_config()
  query <- paste0("
SELECT
  ", paste0(conf$db$tab$user$insert, collapse = ",\n "), "
FROM
  user
WHERE
  valid=1")

  if (!is.na(sample) && sample > 0 && sample < 1) {
    query <- paste(query, "AND RAND() <", sample)
  }

  pool::dbGetQuery(pool, query)
}


#' @rdname db
#' @export
get_org <- function(pool, sample = NA) {

  query <- paste0("
SELECT
  *
FROM
  org")

  if (!is.na(sample) && sample > 0 && sample < 1) {
    query <- paste(query, "\nWHERE\n  RAND() <", sample)
  }

  pool::dbGetQuery(pool, query)
}


#' @rdname db
#' @export
get_user_registry <- function(pool, sample = NA) {

  query <- paste0("
SELECT
  *
FROM
  user_registry")

  if (!is.na(sample) && sample > 0 && sample < 1) {
    query <- paste(query, "\nWHERE\n  RAND() <", sample)
  }

  pool::dbGetQuery(pool, query)
}

#' @rdname db
#' @export
get_indicator <- function(pool, sample = NA) {

  conf <- get_config()
  query <- paste0("
SELECT
  ", paste0(conf$db$tab$indicator$insert, collapse = ",\n "), "
FROM
  indicator")

  if (!is.na(sample) && sample > 0 && sample < 1) {
    query <- paste(query, "\nWHERE\n  RAND() <", sample)
  }

  pool::dbGetQuery(pool, query)
}


#' @rdname db
#' @export
get_registry_name <- function(pool, registry) {

  query <- paste0("
SELECT
  name
FROM
  registry
WHERE
  id=", registry, ";")

  pool::dbGetQuery(pool, query)$name
}



#' @rdname db
#' @export
get_registry_indicators <- function(pool, registry) {

  query <- paste0("
SELECT
  IndID
FROM
  indicator
WHERE
  registry_id=", registry, ";"
  )

  pool::dbGetQuery(pool, query)
}

#' @rdname db
#' @export
get_registry <- function(pool, sample = NA) {

  conf <- get_config()
  query <- paste0("
SELECT
  ", paste0(conf$db$tab$registry$insert, collapse = ",\n "), "
FROM
  registry")

  if (!is.na(sample) && sample > 0 && sample < 1) {
    query <- paste(query, "\nWHERE\n  RAND() <", sample)
  }

  pool::dbGetQuery(pool, query)
}
