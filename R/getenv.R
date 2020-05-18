#' Get relevant settings either from config og environmental variables
#'
#' asdfgasd
#'
#' @return Character string with settings
#' @name getenv
#' @aliases db_host db_name db_username db_password adminer_url
NULL

#' @rdname getnev
#' @export
db_host <- function() {

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

  host
}

#' @rdname getnev
#' @export
db_name <- function() {

  conf <- get_config()

  dbname <- conf$db$name
  if (dbname == "env") {
    if ("IMONGR_DB_NAME" %in% names(Sys.getenv())) {
      dbname <- Sys.getenv("IMONGR_DB_NAME")
    } else {
      stop(paste("No database name defined in config or environment",
                 "varaible IMONGR_DB_NAME. Cannot go on."))
    }
  }

  dbname
}

#' @rdname getnev
#' @export
db_username <- function() {

  conf <- get_config()

  username <- conf$db$user
  if (username == "env") {
    if ("IMONGR_DB_USER" %in% names(Sys.getenv())) {
      username <- Sys.getenv("IMONGR_DB_USER")
    } else {
      stop(paste("No database username defined in config or environment",
                 "variable IMONGR_DB_USER. Cannot go on."))
    }
  }

  username
}

#' @rdname getnev
#' @export
db_password <- function() {

  conf <- get_config()

  password <- conf$db$pass
  if (conf$db$pass == "env") {
    if ("IMONGR_DB_PASS" %in% names(Sys.getenv())) {
      password <- Sys.getenv("IMONGR_DB_PASS")
    } else {
      stop(paste("No database password defined in config or environment",
                 "variable IMONGR_DB_PASS. Cannot go on."))
    }
  }

  password
}

#' @rdname getnev
#' @export
adminer_url <- function() {

  conf <- get_config()

  url <- conf$adminer$url
  if (url == "env") {
    if ("IMONGR_ADMINER_URL" %in% names(Sys.getenv())) {
      url <- Sys.getenv("IMONGR_ADMINER_URL")
    } else {
      warning(paste("Expected Adminer url definition in the environment",
                 "varaiable IMONGR_ADMINER_URL, but it does not exist!."))
      url <- ""
    }
  }

  url
}