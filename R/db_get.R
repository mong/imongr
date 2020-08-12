#' Retreiv data from imongr database
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
#' @param pool a database connection pool object
#' @param registry Integer defining registry id
#' @param full_name Logical defining if full names is to be returned
#' @return Data object from database
#' @name db_get
#' @aliases get_registry_name
NULL

#' @rdname db_get
#' @export
get_registry_name <- function(pool, registry, full_name = FALSE) {

  query <- paste0("
SELECT
  name,
  full_name
FROM
  registry
WHERE
  id=", registry, ";")

  if (full_name) {
    pool::dbGetQuery(pool, query)$full_name
  } else {
    pool::dbGetQuery(pool, query)$name
  }
}
