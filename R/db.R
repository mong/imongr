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
#' @param context Character string defining the environment context. Must be
#' one of \code{c("prod", "verify", "qa")}. Default value is \code{"prod"}.
#' @param table string defining target database table
#' @param sample Numeric in the range 0 to 1 defining the relative sub-sample
#' size, \emph{e.g.} when \code{sample = 0.1} approximately 10\% of
#' observations are returned. Default is \code{NA} which will return all data
#' @param df data frame containing data to be inserted into a database
#' @return Database pool object, data frame or status message
#' @name db
#' @aliases make_pool drain_pool insert_table get_table get_table_raw
NULL

#' @rdname db
#' @export
make_pool <- function(context = "prod") {
  pool::dbPool(
    drv = RMariaDB::MariaDB(),
    dbname = db_name(),
    host = db_host(context),
    username = db_username(),
    password = db_password(),
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
insert_table <- function(pool, table, df) {
  conf <- get_config()

  if (!table %in% names(conf$db$tab)) {
    stop(paste(
      "Table specified is not recognized. Valid tables are:",
      paste(names(conf$db$tab), collapse = ", ")
    ))
  }

  insert <- conf$db$tab[[table]]$insert

  if (!length(names(df)) == length(insert)) {
    stop(paste0(
      "In 'df' the number of variables (",
      length(names(df)), ") is not equal to what was expected (",
      length(insert), ")"
    ))
  }

  is_member <- names(df) %in% insert

  if (!all(is_member)) {
    stop(paste0(
      "One or more variable names (",
      paste(names(df)[!is_member], collapse = ", "),
      ") do not match what was expected (", length(insert)
    ))
  }

  pool::dbWriteTable(pool, table, df[insert],
    append = TRUE,
    row.names = FALSE
  )
}


#' @rdname db
#' @export
get_table <- function(pool, table, sample = NA) {
  # make sure we deal in proper encoding
  pool::dbExecute(pool, "SET NAMES utf8")

  conf <- get_config()
  query <- paste0("
SELECT
  ", paste0(conf$db$tab[[table]]$insert, collapse = ",\n  "), "
FROM
  ", table)

  if (!is.na(sample) && sample > 0 && sample < 1) {
    query <- paste(query, "\nWHERE\n  RAND() <", sample)
  }

  pool::dbGetQuery(pool, query)
}

#' @rdname db
#' @export
get_table_raw <- function(pool, table, sample = NA) {
  # make sure we deal in proper encoding
  pool::dbExecute(pool, "SET NAMES utf8")

  query <- paste0("
SELECT
  *
FROM
  ", table)

  if (!is.na(sample) && sample > 0 && sample < 1) {
    query <- paste(query, "\nWHERE\n  RAND() <", sample)
  }

  pool::dbGetQuery(pool, query)
}
