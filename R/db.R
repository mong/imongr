#' Database functions for imongr
#'
#' Database credentials are read from config. If one or more of these are
#' defined 'env' corresponding values will be fetched from the environmental
#' variables IMONGR_DB_USER and IMONGR_DB_PASS. If these are undefined the
#' function providing connection handles will exit with an error.
#'
#' @param pool a database connection pool object
#' @param table string defining target database table
#' @param df data frame containing data to be inserted into a database
#' @return Database pool object, data frame or status message
#' @name db
#' @aliases make_pool insert_tab get_data
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
  insert <- conf$db$tab[[table]]$insert

  pool::dbWriteTable(pool, table, df[insert], append = TRUE,
                       row.names = FALSE)
}


#' @rdname db
#' @export
get_data <- function(pool) {

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
  d.latest = 1;")

  pool::dbGetQuery(pool, query)
}
