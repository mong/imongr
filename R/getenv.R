#' Get relevant settings either from config og environmental variables
#'
#' @return Character string with settings
#' @name getenv
#' @aliases db_host db_name db_username db_password adminer_url
NULL

#' @rdname getenv
#' @export
get_user_name <- function() {

  user_name <- Sys.getenv("SHINYPROXY_USERNAME")
  if (user_name == "") {
    stop("No user defined!")
  }

  user_name

}


#' @rdname getenv
#' @export
get_user_groups <- function() {

  user_groups <- Sys.getenv("SHINYPROXY_USERGROUPS")
  if (user_groups == "") {
    stop("No groups defined")
  }

  user_groups
}


#' @rdname getenv
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


#' @rdname getenv
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


#' @rdname getenv
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


#' @rdname getenv
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


#' @rdname getenv
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
